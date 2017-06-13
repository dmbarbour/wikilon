{-# LANGUAGE BangPatterns, ViewPatterns #-}
-- | Wikilon Persistence Layer
--
-- Wikilon provides a simple key-value database for rooted data, but
-- the bulk of Wikilon data is based on "stowage" - use of secure 
-- hashes to reference binaries. Unlike keys, secure hash resources
-- require garbage collection.
--
-- Persistence is implemented above LMDB, but the LMDB layer is mostly
-- hidden below a lightweight optimistic concurrency transaction API.
--
module Wikilon.DB
    ( DB, TX
    , open
    , newTX, dupTX, txDB
    , readKey, readKeyDB
    , readKeys, readKeysDB
    , writeKey
    , loadRsc, loadRscDB
    , stowRsc
    , commitTX
    , checkTX
    , FilePath
    , ByteString
    , module Awelon.Hash
    ) where

import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.DeepSeq (force)
import Foreign
import Data.Function (on)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified System.IO (FilePath)
import qualified System.IO.Error as E
import qualified System.Directory as FS 
import System.FilePath ((</>))
import qualified System.IO as Sys
import qualified System.Exit as Sys
import qualified System.FileLock as FL 
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.IORef
import qualified Data.Map as M
import qualified Data.IntMap as Mi
import qualified Data.List as L
import Data.Word (Word8)
import Data.Bits ((.|.), xor)
import Data.Monoid
import Data.Maybe
import Database.LMDB.Raw
import Awelon.Syntax (validWordByte)
import Awelon.Hash
import Debug.Trace

-- these errors shouldn't appear regardless of user input
dbError :: String -> a
dbError = error . (++) "Wikilon.DB: "

-- Thoughts: I like having the thin layer to access the stowage,
-- and the robust security and distribution properties for using
-- secure hashes as capabilities. But for performance, there may
-- be some benefits to use of VCache PVars or similar, so we can
-- detect conflicts early and avoid serializing anything but the
-- final value for each variable.
--
-- In any case, if I model these, it will be in another module.

-- | Wikilon Database Object
--
-- Wikilon uses a key-value database with a special feature: values
-- may contain binary references in the form of secure hashes. These
-- secure hashes are conservatively GC'd (cf. Awelon.Hash.hashDeps).
-- This supports scalable representation of tree-structured data with
-- implicit structure sharing. Further, this fits nicely with purely
-- functional programming. We treat this binary "stowage" layer as a
-- persistence, sharing, and virtual memory model for large values.
data DB = DB 
  { db_fp       :: !FilePath -- location in filesystem
  , db_fl       :: !FL.FileLock -- resist multi-process access

    -- LMDB layer (using MDB_NOLOCK)
  , db_env      :: !MDB_env  
  , db_data     :: {-# UNPACK #-} !MDB_dbi' -- key -> value roots
  , db_stow     :: {-# UNPACK #-} !MDB_dbi' -- secureHash -> binary
  , db_rfct     :: {-# UNPACK #-} !MDB_dbi' -- secureHash -> positive count ([1-9][0-9]*)
  , db_zero     :: {-# UNPACK #-} !MDB_dbi' -- secureHash set with rfct=0

    -- Reader Locking (frame based)
  , db_rdlock   :: !(IORef R)

    -- Asynch Write Layer
  , db_signal   :: !(MVar ())               -- work available?
  , db_new      :: !(IORef Stowage)         -- pending stowage
  , db_commit   :: !(IORef [Commit])        -- commit requests
  , db_hold     :: !(IORef EphTbl)          -- ephemeron table
  } 
-- notes: Reference counts are partitioned so we can quickly locate
-- objects with zero references for purpose of incremental GC. The
-- ephemeron table can preserve some objects in the database as if
-- rooted.
--
-- If I later need multi-process access, I might need to move the
-- ephemeron table to shared memory, and use a shared writer mutex.

instance Eq DB where
    (==) = (==) `on` db_signal

instance Show DB where
    showsPrec _ db = showString "DB@" . showString (db_fp db)

type Stowage = M.Map Hash ByteString        -- ^ latent batch for DB
type KVMap = M.Map ByteString ByteString    -- ^ safe keys and values.
type Commit = ((KVMap,KVMap), MVar Bool)    -- ^ ((reads,writes),returns)
data R = R !(MVar Int) !(MVar ())           -- ^ simple reader count
type EphTbl = Mi.IntMap Int                 -- ^ resource ref counts

-- ephemeron table is a (hash,count) collection.
-- 
-- False positives are possible, but should be rare and aren't a
-- huge problem when they happen (they delay GC of some resources).
-- The hash is extracted from a base32 secure hash, so entropy is
-- not a problem. I've arbitrarily chosen FNV-1a as the hash here,
-- and at most 14 characters for ~70 bits entropy.
ephHashP :: Ptr Word8 -> Int -> IO Int
ephHashP !p = fmap fromIntegral . go basis . min rd where
    rd = min 14 stowKeyLen -- 70 bits entropy or available MDB key
    basis = 14695981039346656037
    prime = 1099511628211
    go :: Word64 -> Int -> IO Word64
    go !r !n = 
        if (0 == n) then return r else
        let ix = (n - 1) in
        peekElemOff p ix >>= \ c ->
        let r' = (r `xor` (fromIntegral c)) * prime in
        go r' ix

ephHashMDB :: MDB_val -> IO Int
ephHashMDB h = ephHashP (mv_data h) (fromIntegral (mv_size h))

ephHash :: Hash -> Int
ephHash = unsafeDupablePerformIO . flip withBSKey ephHashMDB

-- Note: I probably need a simpler ephemeron table, one that I can
-- check quickly without allocations. 

-- I assume the ephemeron table won't be my bottleneck, since it has a
-- cost proportional only to the amount of new stowage by a TX. 

dbSignal :: DB -> IO ()
dbSignal db = tryPutMVar (db_signal db) () >> return ()

dbPushCommit :: DB -> Commit -> IO ()
dbPushCommit db !task = do
    atomicModifyIORef (db_commit db) $ \ lst -> ((task:lst), ())
    dbSignal db

dbPushStow :: DB -> Stowage -> IO ()
dbPushStow db s = do
    atomicModifyIORef' (db_new db) $ \ s0 -> (M.union s0 s, ()) 
    dbSignal db

nonZero :: Int -> Maybe Int
nonZero 0 = Nothing
nonZero n = Just n

-- | release ephemeral stowage references.
dbClearEph :: DB -> EphTbl -> IO ()
dbClearEph db drop = 
    traceM ("TX releasing " ++ show (Mi.size drop) ++ " resources") >>
    if Mi.null drop then return () else
    atomicModifyIORef' (db_hold db) $ \ tbl ->
        let diff = \ l r -> nonZero (l - r) in
        let tbl' = Mi.differenceWith diff tbl drop in
        (tbl', ())

-- | Add ephemeral stowage references. 
dbAddEph :: DB -> EphTbl -> IO ()
dbAddEph db update = 
    if Mi.null update then return () else
    atomicModifyIORef' (db_hold db) $ \ tbl ->
        let tbl' = Mi.unionWith (+) tbl update in 
        (tbl', ())

-- | Perform an operation while holding a read lock.
-- 
-- LMDB is essentially a frame-buffered database. Readers don't wait,
-- they immediately read the most recent valid frame. LMDB with NOLOCK
-- has two valid frames between commits. Commit destroys the old frame
-- header and replaces it. Thus, a writer needs only to wait on readers
-- of the elder frame immediately before commit. If readers are short
-- lived (compared to the time it takes to write), our writer shouldn't
-- ever need to wait on readers.
--
-- Anyhow, readers will grab a read lock, but the read lock will be
-- advanced after each commit so our writer only needs to wait on a
-- few long-lived readers (if any).
withReadLock :: DB -> IO a -> IO a
withReadLock db = bracket acq relR . const where
    acq = readIORef (db_rdlock db) >>= \ r -> acqR r >> return r

-- | advance reader frame (separate from waiting)
advanceReadFrame :: DB -> IO R
advanceReadFrame db = 
    newR >>= \ rN -> 
    atomicModifyIORef (db_rdlock db) $ \ rO -> (rN, rO)
    
-- | type R is a simple count (of readers), together with a signaling
-- MVar that is active (full) iff the current count is zero.
newR :: IO R
newR = R <$> newMVar 0 <*> newMVar ()

-- acquire reader lock
acqR :: R -> IO ()
acqR (R ct sig) = modifyMVarMasked_ ct $ \ n -> do
    when (0 == n) $ takeMVar sig
    return $! (n + 1)

-- release reader lock
relR :: R -> IO ()
relR (R ct sig) = modifyMVarMasked_ ct $ \ n -> do
    when (1 == n) $ putMVar sig ()
    return $! (n - 1)

-- wait on R to have a zero count.
waitR :: R -> IO ()
waitR (R _ sig) = readMVar sig

-- | environment flags and reasons for them
--
-- - MDB_NOLOCK: avoid reader lock limits, simplify lightweight thread
--    issues, and optimize for very short-lived readers.
-- - MDB_NOSYNC: advance reader frame between commit and explicit sync.
-- - MDB_WRITEMAP: reduces mallocs and data copies during writes a lot.
lmdbEnvF :: [MDB_EnvFlag]
lmdbEnvF = [MDB_NOLOCK, MDB_WRITEMAP, MDB_NOSYNC]

-- | Open or Create the Database. 
--
-- The argument is simply a directory where we expect to open the
-- database, and a maximum database size in megabytes.
--
-- The implementation uses LMDB without locks and writable memory.
-- Concurrency and the ephemeron table are managed within this 
-- process, so the database mustn't be used concurrently by other
-- processes. A lockfile is used to resist accidents.
--
-- Note: at the moment, there is no clear way to 'close' this
-- database, except for allowing it to leave scope. 
--
open :: FilePath -> Int -> IO (Either SomeException DB)
open fp nMB = try $ do
    FS.createDirectoryIfMissing True fp
    lock <- tryLockE (fp </> "lockfile")
    flip onException (FL.unlockFile lock) $ do
        env <- mdb_env_create

        mks <- mdb_env_get_maxkeysize env
        unless (mks >= maxKeyLen) (fail "require LMDB compiled with larger max key size.")

        mdb_env_set_mapsize env (nMB * (1024 * 1024))
        mdb_env_set_maxdbs env 4
        mdb_env_open env fp lmdbEnvF

        flip onException (mdb_env_close env) $ do
            -- initial transaction to open databases. No special DB flags.
            txIni <- mdb_txn_begin env Nothing False
            let openDB s = mdb_dbi_open' txIni (Just s) [MDB_CREATE]
            dbData <- openDB "@"    -- named key-value data
            dbStow <- openDB "$"    -- stowed data
            dbRfct <- openDB "#"    -- non-zero persistent reference counts
            dbZero <- openDB "0"    -- hashes with only volatile references
            mdb_txn_commit txIni

            dbRdLock <- newIORef =<< newR -- readers tracking
            dbSignal <- newMVar () -- initial signal to try GC
            dbCommit <- newIORef mempty
            dbNew <- newIORef mempty
            dbHold <- newIORef mempty

            let db = DB { db_fp = fp
                        , db_fl = lock
                        , db_env = env
                        , db_data = dbData
                        , db_stow = dbStow
                        , db_rfct = dbRfct
                        , db_zero = dbZero
                        , db_rdlock = dbRdLock
                        , db_signal = dbSignal
                        , db_commit = dbCommit
                        , db_new = dbNew
                        , db_hold = dbHold
                        }

            forkIO (dbWriter db)
            return db

-- close is only performed upon full GC
close :: DB -> IO ()
close db = do
    mdb_env_sync_flush (db_env db)
    mdb_env_close (db_env db)
    FL.unlockFile (db_fl db)

-- try lock with a simple IOError
tryLockE :: FilePath -> IO FL.FileLock
tryLockE fp =
    FL.tryLockFile fp FL.Exclusive >>= \ mbLocked ->
    case mbLocked of
        Just fl -> return fl
        Nothing -> E.ioError $ E.mkIOError 
            E.alreadyInUseErrorType "exclusive file lock failed" 
            Nothing (Just fp)

-- | Transactional Database API
--
-- These transactions support optimistic concurrency, detecting conflict
-- only when it's time to attempt writing the transaction. A transaction
-- can read and write keys, and may load or stow secure hash resources.
-- Stowed data is moved directly into the database, but the transaction
-- will prevent premature GC of the data via an ephemeron table.
--
-- Concurrent, non-conflicting transactions are batched together to help
-- improve throughput and amortize the overheads of synchronization. When
-- conflicts occur, progress is guaranteed: at least one transaction will
-- succeed. But the remainder might need to be retried. It isn't difficult
-- to use queues or add an STM layer to resist conflicts.
--
-- The TX is thread safe and may be committed more than once to represent
-- ongoing progress. TX doesn't need to be aborted explicitly: just don't
-- commit. 
data TX = TX !DB !(MVar TXS)

instance Eq TX where (==) (TX _ l) (TX _ r) = (==) l r

data TXS = TXS 
    { tx_read   :: !KVMap   -- reads or assumptions
    , tx_write  :: !KVMap   -- data written since create or commit
    , tx_hold   :: !EphTbl  -- ephemeral stowage GC roots
    }

emptyTXS :: TXS
emptyTXS = TXS mempty mempty mempty 

-- | A transaction is associated with a database.
txDB :: TX -> DB
txDB (TX db _) = db

-- | Initialize a fresh transaction.
newTX :: DB -> IO TX
newTX db = do
    st <- newMVar emptyTXS
    let tx = TX db st 
    mkWeakMVar st (finiTX tx)
    return tx

-- clear ephemeral stowage.
finiTX :: TX -> IO ()
finiTX (TX db st) = modifyMVarMasked_ st $ \ s -> do
    dbClearEph db (tx_hold s)
    return emptyTXS

-- | Duplicate a transaction.
-- 
-- Fork will deep-copy a transaction object, including its relationship
-- to ephemeral stowage. This may be useful to model partial backtracking
-- for a computation.
dupTX :: TX -> IO TX
dupTX (TX db st) = do
    s <- readMVar st
    st' <- newMVar s
    let tx' = TX db st'
    mkWeakMVar st' (finiTX tx')
    dbAddEph db (tx_hold s)
    return tx'

-- preserve keys up to a reasonably large maximum size, enough
-- to model a lightweight filesystem (if desired).
maxKeyLen :: Integral a => a
maxKeyLen = 255

-- rewrite problematic keys to a collision resistant secure hash.
-- I'll look only at the first byte and the key size. Most keys
-- should be safe in practice, and thus not need any rewrite.
toSafeKey :: ByteString -> ByteString
toSafeKey s = if safe s then s else mkSafe s 
    where
    safe s = case LBS.uncons s of
        Just (c, s') -> (c > 31) && (LBS.length s' < maxKeyLen) 
        Nothing -> False -- empty key isn't considered safe
    mkSafe s = LBS.singleton 26 <> LBS.fromStrict (BS.take stowKeyLen (hashL s))
        -- using (SUB)hash. Won't alias with natural safe keys.

-- | read strict bytestring key as MDB_val
withBSKey :: BS.ByteString -> (MDB_val -> IO a) -> IO a
withBSKey (BS.PS fp off len) action = 
    withForeignPtr fp $ \ p ->
        action $ MDB_val (fromIntegral len) (p `plusPtr` off)

-- | read lazy bytestring key as MDB_val.
withLBSKey :: ByteString -> (MDB_val -> IO a) -> IO a
withLBSKey (LBS.Chunk bs LBS.Empty) action = withBSKey bs action
withLBSKey k action =
    let len = LBS.length k in
    allocaBytes (fromIntegral len) $ \ p -> do
        copyLBS p k
        action (MDB_val (fromIntegral len) p)

-- | copy a lazy bytestring to pointer destination.
--
-- Assumes sufficient space in destination for the full length.
-- I'm surprised that I couldn't find an equivalent function in 
-- Data.ByteString.Lazy.Internal. 
copyLBS :: Ptr Word8 -> LBS.ByteString -> IO ()
copyLBS !dst s = case s of
    LBS.Empty -> return ()
    (LBS.Chunk (BS.PS fp off len) more) -> do
        withForeignPtr fp $ \ src -> BS.memcpy dst (src `plusPtr` off) len
        copyLBS (dst `plusPtr` len) more

-- copy an MDB for use as a Haskell bytestring.
copyMDB_to_BS :: MDB_val -> IO BS.ByteString
copyMDB_to_BS (MDB_val len src) = 
    BS.create (fromIntegral len) $ \ dst ->
        BS.memcpy dst src (fromIntegral len)

-- | Retrieve value associated with given key.
--
-- If the key has already been read or written within a transaction,
-- this returns a value specific to the transaction. Otherwise, it 
-- will read the current value from the database. Snapshot isolation
-- for separate reads is not guaranteed, but all reads are verified
-- to be consistent upon commit. 
--
-- Security Note: keys are not guarded against timing attacks. The
-- client should provide access control or capability security.
readKey :: TX -> ByteString -> IO ByteString
readKey (TX db st) k = modifyMVarMasked st $ \ s ->
    case readKeyTXS s k of
        Just v  -> return (s, v)
        Nothing -> do
            v <- readKeyDB db k
            let r' = M.insert k v (tx_read s) 
            let s' = s { tx_read = r' }
            return (s', v)

-- Read key previously read or written by the transaction. 
readKeyTXS :: TXS -> ByteString -> Maybe ByteString
readKeyTXS s k = M.lookup k (tx_write s) <|> M.lookup k (tx_read s)

-- | Read key directly from database.
--
-- This retrieves the most recently committed value for a key. This is
-- equivalent to readKey with a freshly created transaction.
readKeyDB :: DB -> ByteString -> IO ByteString
readKeyDB db (toSafeKey -> !key) = withReadLock db $ do 
    txn <- mdb_txn_begin (db_env db) Nothing True
    val <- dbReadKey db txn key
    mdb_txn_commit txn
    return val

-- obtain a value after we have our transaction
dbReadKey :: DB -> MDB_txn -> ByteString -> IO ByteString
dbReadKey db txn k = withLBSKey k $ (dbReadKeyMDB db txn)

dbReadKeyMDB :: DB -> MDB_txn -> MDB_val -> IO ByteString
dbReadKeyMDB db txn k = do
    let toBS = maybe (return BS.empty) copyMDB_to_BS
    bs <- toBS =<< mdb_get' txn (db_data db) k
    return $! LBS.fromStrict bs

-- | Read values for multiple keys.
--
-- This reads multiple keys with a single LMDB-layer transaction. The 
-- main benefit with readKeys is snapshot isolation for keys initially
-- read together. 
readKeys :: TX -> [ByteString] -> IO [ByteString]
readKeys (TX db st) allKeys = modifyMVarMasked st $ \ s -> do
    let newKeys = L.filter (isNothing . readKeyTXS s) allKeys 
    newVals <- readKeysDB db newKeys
    let r' = M.union (tx_read s) (M.fromList (L.zip newKeys newVals))
    let s' = s { tx_read = r' }
    let allVals = fmap (fromJust . readKeyTXS s') allKeys 
    return (s', allVals)

-- | Read multiple keys directly from database.
--
-- This obtains a snapshot for a few values from the database. This
-- is equivalent to readKeys using a freshly created transaction.
readKeysDB :: DB -> [ByteString] -> IO [ByteString]
readKeysDB db (toSafeKeys -> !keys) =
    if L.null keys then return [] else 
    withReadLock db $ do 
        txn <- mdb_txn_begin (db_env db) Nothing True
        vals <- mapM (dbReadKey db txn) keys
        mdb_txn_commit txn
        return vals

toSafeKeys :: [ByteString] -> [ByteString]
toSafeKeys = force . fmap toSafeKey
  
-- | Write a key-value pair.
--
-- Writes are trivially recorded into the transaction until commit.
-- There is no risk of write-write conflicts. Subsequent reads will
-- observe the value written.
--
-- Note: Wikilon DB accepts any key, but may rewrite problematic keys
-- using a secure hash. Doing so has a moderate performance cost and
-- hinders LMDB-layer debugging (e.g. via the mdb_dump tool). A good
-- key is short (< 256 bytes) and a valid word, filename, or URL.
writeKey :: TX -> ByteString -> ByteString -> IO ()
writeKey (TX _ st) (force -> !k) (force -> !v) =
    modifyMVarMasked_ st $ \ s ->
        let w' = M.insert k v (tx_write s) in
        return $! s { tx_write = w' }

-- Key Length for Stowage
--
-- Wikilon DB uses only half of the hash for LMDB layer lookups, and
-- uses the remaining half for a constant-time comparison to resist
-- timing attacks that could otherwise leak capabilities. I assume
-- 140 bits is sufficient for practical cryptographic uniqueness, at
-- least within a single runtime.
--
-- This same key fragment is used for reference counting and other
-- features.
stowKeyLen :: Integral a => a
stowKeyLen = validHashLen `div` 2

-- | Access a stowed resource by secure hash.
--
-- This searches for a resource identified by secure hash within 
-- the Wikilon database or transaction. If not found, this returns
-- Nothing, in which case you might search elsewhere like the file
-- system or network. (These resources are provider independent.)
--
-- Loading data does not prevent GC of the data. To prevent GC, the
-- only means is to ensure it remains rooted at the persistence layer.
-- Perhaps model a history of values if it's essential.
--
-- Security Note: secure hashes are essentially object capabilities,
-- and leaking capabilities is a valid concern. Timing attacks are a
-- potential vector for leaks. This function resists timing attacks
-- by using only the first half of a hash for lookup, comparing the
-- remainder in constant time. Clients should be careful to reveal
-- no more than this.
loadRsc :: TX -> Hash -> IO (Maybe ByteString)
loadRsc (TX db _) = loadRscDB db
    -- At the moment, we don't store pending stowage in the TX.
    -- This might change later.

-- | Load resource directly from database.
loadRscDB :: DB -> Hash -> IO (Maybe ByteString)
loadRscDB db !h = 
    if (BS.length h /= validHashLen) then return Nothing else
    readIORef (db_new db) >>= \ newRscTbl ->
    withReadLock db $ 
    withBSKey h $ \ hMDB -> do
    let hKey = MDB_val stowKeyLen (mv_data hMDB)                       -- normal LMDB lookup
    let hRem = MDB_val (mv_size hMDB - stowKeyLen) 
                       (mv_data hMDB `plusPtr` stowKeyLen)             -- prefix for value
    txn <- mdb_txn_begin (db_env db) Nothing True
    v <- tryRscVal hRem =<< mdb_get' txn (db_stow db) hKey
    mdb_txn_commit txn
    return (v <|> lookupRsc h newRscTbl)

-- Accept a resource value after stripping a given prefix.
--
-- Uses constant-time comparison on prefix to resist timing attacks.
tryRscVal :: MDB_val -> Maybe MDB_val -> IO (Maybe ByteString)
tryRscVal p = maybe (return Nothing) $ \ c ->
    ctMatchPrefix p c >>= \ bHasPrefix ->
    if not bHasPrefix then return Nothing else
    let dLen = mv_size c - mv_size p in
    let dPtr = mv_data c `plusPtr` (fromIntegral (mv_size p)) in
    copyMDB_to_BS (MDB_val dLen dPtr) >>= \ bs ->
    return (Just (LBS.fromStrict bs))


-- timing attack resistant prefix matching
ctMatchPrefix :: MDB_val -> MDB_val -> IO Bool
ctMatchPrefix p d =
    if (mv_size p > mv_size d) then return False else
    ctEqMem (mv_data p) (mv_data d) (fromIntegral (mv_size p))

-- constant-time equality comparison for memory pointers.
ctEqMem :: Ptr Word8 -> Ptr Word8 -> Int -> IO Bool
ctEqMem !l !r = go 0 where
    go !b !sz = 
        if (0 == sz) then return $! (0 == b) else do
        let ix = (sz - 1)
        lB <- peekElemOff l ix
        rB <- peekElemOff r ix
        go (b .|. (lB `xor` rB)) ix

-- | Timing-attack resistant lookup for the new resource table.
--
-- I'm not particularly concerned about timing attacks on new resources.
-- If the writer is doing its job, the table shouldn't stick around long
-- enough for a timing attack. However, this is easy to implement and 
-- efficient, and won't hurt. And maybe our writer isn't doing its job.
lookupRsc :: Hash -> Stowage -> Maybe ByteString
lookupRsc h m = 
    case M.lookupGT (BS.take stowKeyLen h) m of
        Just (k,v) | ctEqBS h k -> Just v
        _ -> Nothing

-- | constant time equality comparison for bytestrings.
ctEqBS :: BS.ByteString -> BS.ByteString -> Bool
ctEqBS a b = 
    (BS.length a == BS.length b) &&
    (0 == (L.foldl' (.|.) 0 (BS.zipWith xor a b)))

-- | Move resource to database, returns secure hash (Awelon.Hash).
--
-- Stowage resources are automatically named by secure hash, which
-- is later used to load the resource from the database as needed.
-- By moving data to the database, stowage can double as a virtual
-- memory system.
--
-- Wikilon DB will garbage collect resources that are not rooted by
-- a key or TX. A transaction that stows resources should root that
-- data. Stowage is too expensive to be creating lots of nodes that
-- will shortly be GC'd, at least without a lot of consideration.
--
stowRsc :: TX -> ByteString -> IO Hash
stowRsc (TX db st) v = modifyMVarMasked st $ \ s -> do
    h <- evaluate (hashL v)
    eh <- evaluate (ephHash h)
    let ephUpd = Mi.singleton eh 1 
    let hold' = Mi.unionWith (+) (tx_hold s) ephUpd
    let s' = s { tx_hold = hold' }
    dbAddEph db ephUpd
    dbPushStow db (M.singleton h v)
    return (s', h)

-- | Commit transaction to database.
--
-- Commit will returns True if commit succeeds, False otherwise.
-- During commit, the TX object is unusable and we'll wait for the
-- success or failure result.
--
-- Transactions may be committed more than once. This may be used
-- as a simple checkpoint model, so we don't lose all the work if
-- a long transaction later fails due to concurrent writers.
commitTX :: TX -> IO Bool
commitTX (TX db st) = modifyMVar st $ \ s -> do
    v <- newEmptyMVar
    dbPushCommit db ((tx_read s, tx_write s), v)
    b <- readMVar v
    let s' = if not b then s else
             -- checkpoint transaction state
             let r' = M.union (tx_write s) (tx_read s) in
             s { tx_read = r', tx_write = mempty }
    return (s', b)

-- | Diagnose a transaction.
--
-- This function returns a list of keys whose transactional values
-- currently in conflict with database values. This check is intended
-- to diagnose contention or concurrency issues, and may be checked
-- after a transaction fails to gain more information. The result is
-- ephemeral, however, subject to change by concurrent writers.
checkTX :: TX -> IO [ByteString]
checkTX (TX db st) =
    readMVar st >>= \ s ->
    withReadLock db $ do
    txn <- mdb_txn_begin (db_env db) Nothing True
    invalidReads <- filterM (fmap not . validRead db txn) (M.toList (tx_read s))
    mdb_txn_commit txn
    return (fmap fst invalidReads)

-- | The database writer thread.
--
-- All writes in Wikilon DB are centralized to a single thread. And
-- garbage collection, too. With LMDB under the hood, we're limited
-- to at most one writer in any case. 
--
-- This is mitigated by write batching. Multiple threads contribute
-- transactions, thus a single writer can get a wide variety of work
-- done per commit, and overheads are amortized across many writers.
--
-- If developers can avoid conflicts, that's even better. 
dbWriter :: DB -> IO ()
dbWriter !db = initLoop `catches` handlers where
    handlers = [Handler onGC, Handler onError]
    onGC :: BlockedIndefinitelyOnMVar -> IO ()
    onGC _ = close db -- no external references to DB
    onError :: SomeException -> IO ()
    onError e = do
        -- What to do when the database fails? For now, just stop.
        -- Maybe we could make this more configurable, later.
        putErrLn $ "Wikilon Database (" ++ show db ++ ") writer FAILED"
        putErrLn $ indent "    " (show e)
        putErrLn $ "Aborting Program!"
        Sys.exitFailure

    -- In each loop, the writer will try available transactions, write
    -- incoming stowage, do some GC. We will wait on old readers before
    -- commit, advance the reader frame after. If no work is available,
    -- we'll just sleep until work becomes available.
    --
    -- For now, I'll process updates naively. There are a lot of possible
    -- performance improvements, e.g. reducing allocations for reference
    -- counting, elimination of conflicting concurrent transactions before
    -- they go to LMDB storage, elimination of unnecessary stowage.
    loop :: R -> IO ()
    loop !r = do
        takeMVar (db_signal db) 

        -- Obtain the transactions and resources. Order is relevant.
        txList <- L.reverse <$> atomicModifyIORef (db_commit db) (\ lst -> ([],lst))
        newRsc <- readIORef (db_new db)
        txn <- mdb_txn_begin (db_env db) Nothing False

        -- Write resources, transactions, and update reference counts.
        rcu_writes <- foldM (doCommit db txn) mempty txList
        rcu_stow <- foldM (doStow db txn) mempty (M.toList newRsc)
        let rcu = M.unionWith (+) rcu_writes rcu_stow
        doRefctUpd db txn rcu

        -- Wait for readers of old frame, then commit.
        waitR r                     
        mdb_txn_commit txn          
        r' <- advanceReadFrame db   
        mdb_env_sync_flush (db_env db)

        -- Mark successful completion of batch, then continue.
        mapM_ (flip tryPutMVar True . snd) txList
        clearStowed db newRsc
        loop r'

    -- start loop with initial read frame
    initLoop = advanceReadFrame db >>= loop


-- Mark stowage resources written.
clearStowed :: DB -> Stowage -> IO ()
clearStowed db w = atomicModifyIORef' (db_new db) $ \ m -> 
    let m' = M.difference m w in (m', ())

-- Reference count tracking.
-- 
-- For now, all reference counts are written into a simple map, using
-- only the stowKeyLen fragment of the hash string to resist possible
-- timing attacks. This is far from optimal for allocations, but it is
-- simple to use in Haskell. 
type RCU = M.Map Hash Int

addRCU :: Int -> [Hash] -> RCU -> RCU
addRCU !n = flip (L.foldl' (M.unionWith (+))) . fmap toM where
    toM h = M.singleton (BS.take stowKeyLen h) n

-- Write a single resource into stowage.
--
-- This will first check if the hash already exists. Otherwise
-- we'll perform a write immediately and update the RCU.
doStow :: DB -> MDB_txn -> RCU -> (Hash, ByteString) -> IO RCU
doStow !db !txn !rcu (!h, !v) = 
    assert (validHashLen == BS.length h) $
    let (key,rem) = BS.splitAt stowKeyLen h in
    withBSKey key $ \ mdbKey ->
        mdb_get' txn (db_stow db) mdbKey >>= \ vdb -> 
        let rscIsNewToDB = isNothing vdb in
        if not rscIsNewToDB then return rcu else
        let updRCU = addRCU 0 [h] . addRCU 1 (hashDeps v) in
        let wf = compileWriteFlags [] in
        let hv = LBS.fromStrict rem <> v in
        let sz = fromIntegral (LBS.length hv) in
        mdb_reserve' wf txn (db_stow db) mdbKey sz >>= \ dst ->
        copyLBS (mv_data dst) hv >>
        return (updRCU rcu)

-- Perform a read-write transaction.
--
-- If commit fails, we'll report failure immediately. Otherwise, we 
-- write to our database but the report of success is delayed until
-- the transaction fully commits.
-- 
doCommit :: DB -> MDB_txn -> RCU -> Commit -> IO RCU
doCommit !db !txn !rcu ((!r,!w),!ret) = 
    allM (validRead db txn) (M.toList r) >>= \ allReadsOK ->
    if not allReadsOK then tryPutMVar ret False >> return rcu else
    foldM (doWrite db txn) rcu (M.toList w)

-- Write a specific bytestring.
doWrite :: DB -> MDB_txn -> RCU -> (ByteString, ByteString) -> IO RCU
doWrite !db !txn !rcu (!k, !v) = 
    withLBSKey (toSafeKey k) $ \ mdbKey -> do
        v0 <- dbReadKeyMDB db txn mdbKey
        let updRCU = addRCU 1 (hashDeps v) . addRCU (-1) (hashDeps v0)
        let wf = compileWriteFlags []
        let sz = fromIntegral (LBS.length v)
        dst <- mdb_reserve' wf txn (db_data db) mdbKey sz
        copyLBS (mv_data dst) v
        return (updRCU rcu)

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM fn (x:xs) = fn x >>= \ b -> if not b then return False else allM fn xs
allM _ [] = return True

-- zero-copy memcmp equality comparison for database and TX values.
-- (Note: empty string is equivalent to undefined in this case.)
validRead :: DB -> MDB_txn -> (ByteString,ByteString) -> IO Bool
validRead db txn (k,vTX) = withLBSKey (toSafeKey k) $ \ mdbKey ->
    mdb_get' txn (db_data db) mdbKey >>= \ mbv ->
    maybe (return (LBS.null vTX)) (flip matchMDB_LBS k) mbv

matchMDB_LBS :: MDB_val -> LBS.ByteString -> IO Bool
matchMDB_LBS v s =
    let szMatch = fromIntegral (mv_size v) == LBS.length s in
    if not szMatch then return False else matchLBS (mv_data v) s

matchLBS :: Ptr Word8 -> LBS.ByteString -> IO Bool
matchLBS !p s = case s of
    (LBS.Chunk (BS.PS fp off len) more) -> do
        iCmp <- withForeignPtr fp $ \ s -> BS.memcmp p (s `plusPtr` off) len
        if (0 /= iCmp) then return False else matchLBS (p `plusPtr` len) more
    LBS.Empty -> return True


-- Reference counts are recorded in the `db_rfct` table as a simple
-- string of [1-9][0-9]*. Anything not in the table is assumed to have
-- zero persistent references.
dbGetRefct :: DB -> MDB_txn -> Hash -> IO Int
dbGetRefct db txn h =
    assert (BS.length h == stowKeyLen) $
    withBSKey h $ \ hMDB -> 
        mdb_get' txn (db_rfct db) hMDB >>= \ mbv ->
        maybe (return 0) readRefct mbv

readRefct :: MDB_val -> IO Int
readRefct v = go 0 (mv_data v) (mv_size v) where
    go !n !p !sz =
        if (0 == sz) then return n else
        peek p >>= \ c ->
        assert ((48 <= c) && (c < 58)) $
        let n' = (10 * n) + fromIntegral (c - 48) in
        go n' (p `plusPtr` 1) (sz - 1)


digitCt :: Int -> Int
digitCt = go 1 . div10 where
    go !ct !q = if (0 == q) then ct else go (1 + ct) (div10 q)
    div10 = flip div 10

-- For incremental GC, we may have GC pending for several resources.
-- These resources, or at least a subset thereof, must be processed
-- in addition to the writes performed by the transaction.
--
-- This function returns a list of resources pending GC, filtering
-- for those in our ephemeron table. Thus, the returned hashes will
-- be collected unless we somehow have a pending incref for them.
dbGCPend :: DB -> MDB_txn -> Int -> IO [Hash]
dbGCPend db txn nMax =
    if (nMax < 1) then return [] else
    alloca $ \ pHash -> do
    crs <- mdb_cursor_open' txn (db_zero db)
    hold <- readIORef (db_hold db)
    let loop !b !n !r =
            if ((not b) || (0 == n)) then return r else
            peek pHash >>= \ hMDB ->
            ephHashMDB hMDB >>= \ eh ->
            mdb_cursor_get' MDB_NEXT crs pHash nullPtr >>= \ b' ->
            if Mi.member eh hold then loop b' n r else
            copyMDB_to_BS hMDB >>= \ h ->
            loop b' (n - 1) (h : r)
    b0 <- mdb_cursor_get' MDB_FIRST crs pHash nullPtr
    lst <- loop b0 nMax []
    mdb_cursor_close' crs
    return lst
    

-- Challenges for Reference Counting:
--
-- - write reference counts only once per object
-- - limit cascading destruction, incremental GC
--
-- To control cascading destruction, my current proposal is this: 
-- use a soft GC quota and a breadth-first destruction model. If
-- we surpass the quota, we can simply
--
-- A reasonable way to decide the quota is based on the size of our
-- transaction. This way, GC doesn't fall behind writes and tends to
-- catch over a few frames, and the time spent on GC is proportional
-- to time spent on mutations. 
--
doRefctUpd :: DB -> MDB_txn -> RCU -> IO ()
doRefctUpd db txn = undefined
{-
            rc <- flip M.traverseWithKey rcu $ \ h u ->
                dbGetRefct db txn h >>= \ rc0 -> return (rc0 + u)
-}


-- indent all lines by w
indent :: String -> String -> String
indent w = (w ++) . indent' where
    indent' ('\n':s) = '\n' : indent w s
    indent' (c:s) = c : indent' s
    indent' [] = []

-- print to stderr
putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr
        




