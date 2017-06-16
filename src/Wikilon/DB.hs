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
-- My expectation is that we'll have many reads per write. But if I'm
-- wrong about that, I can move data to LevelDB.
--
module Wikilon.DB
    ( DB, TX
    , open
    , newTX, txDB, dupTX
    , readKey, readKeyDB
    , readKeys, readKeysDB
    , writeKey
    , loadRsc, loadRscDB
    , stowRsc
    , commitTX, commitTX_async
    , checkTX
    , FilePath
    , ByteString
    , module Awelon.Hash
    ) where

import Control.Applicative
import Control.Arrow (first)
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
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as Mi
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

-- Thoughts: I like having this thin layer to access the stowage,
-- and the robust security and distribution properties from using
-- secure hashes as capabilities. 
--
-- But for performance, there may be some benefits to mixing in
-- the use of STM for persistent 'variables' and moving conflict
-- resolution mostly into normal memory (so we only commit the
-- successful transactions).
--
-- In any case, if I model PVars, it will be from another module.

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
--
-- If necessary, I might keep some extra statistics about reads and
-- writes by our 

instance Eq DB where
    (==) = (==) `on` db_signal

instance Show DB where
    showsPrec _ db = showString "DB@" . showString (db_fp db)

type Stowage = M.Map Hash ByteString        -- ^ latent batch for DB
type KVMap = M.Map ByteString ByteString    -- ^ safe keys and values.
type Commit = ((KVMap,KVMap), MVar Bool)    -- ^ ((reads,writes),returns)
data R = R !(MVar Int) !(MVar ())           -- ^ simple reader count
type EphTbl = Mi.IntMap Int                 -- ^ resource ref counts


-- ephemeron table is a (hash,count) collection with potential for
-- false positives. The hash is an FNV-1a on the first 14 bytes of
-- our base32 secure hash.
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


-- functions to push work to our writer and signal it.
dbSignal :: DB -> IO ()
dbSignal db = tryPutMVar (db_signal db) () >> return ()

dbPushCommit :: DB -> Commit -> IO ()
dbPushCommit db !task = do
    atomicModifyIORef (db_commit db) $ \ lst -> ((task:lst), ())
    dbSignal db

dbPushStow :: DB -> Stowage -> IO ()
dbPushStow db s = do
    atomicModifyIORef (db_new db) $ \ s0 -> (M.union s0 s, ()) 
    dbSignal db

ephDiff :: EphTbl -> EphTbl -> EphTbl
ephDiff = Mi.differenceWith $ \ l r -> nz (l - r) where
    nz 0 = Nothing
    nz n = Just n

-- release ephemeral stowage references.
dbClearEph :: DB -> EphTbl -> IO ()
dbClearEph db drop = 
    traceM ("TX releasing " ++ show (Mi.size drop) ++ " resources") >>
    if Mi.null drop then return () else
    atomicModifyIORef' (db_hold db) $ \ tbl ->
        (ephDiff tbl drop, ())

-- add ephemeral stowage references. 
dbAddEph :: DB -> EphTbl -> IO ()
dbAddEph db update = 
    if Mi.null update then return () else
    atomicModifyIORef' (db_hold db) $ \ tbl ->
        let tbl' = Mi.unionWith (+) tbl update in 
        (tbl', ())

-- Perform operation while holding a read lock.
-- 
-- LMDB is essentially a frame-buffered database. Readers don't wait,
-- they immediately read a recent valid frame. LMDB with NOLOCK has two
-- valid frames between commits. Commit destroys the old frame header
-- and replaces it. Thus, a writer must wait on readers from the older
-- of the two frame headers before committing, and advance frames after
-- committing. Ideally, our writer will be concurrent with readers for
-- as long as possible.
--
-- Anyhow, readers immediately grab a read lock, and the writer will
-- only wait for readers that are absurdly long-lived.
withReadLock :: DB -> IO a -> IO a
withReadLock db = bracket acq relR . const where
    acq = readIORef (db_rdlock db) >>= \ r -> acqR r >> return r

-- advance reader frame (separate from waiting)
advanceReadFrame :: DB -> IO R
advanceReadFrame db = 
    newR >>= \ rN -> 
    atomicModifyIORef (db_rdlock db) $ \ rO -> (rN, rO)
    
-- type R is a simple count (of readers), together with a signaling
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
-- Note: at the moment, there is no operation way to 'close' the DB.
-- The normal use case is to open one DB and keep it around until the
-- process fails. But resources will be released upon GC.
open :: FilePath -> Int -> IO (Either SomeException DB)
open fp nMB = try $ do
    FS.createDirectoryIfMissing True fp
    lock <- tryLockE (fp </> "lockfile")
    flip onException (FL.unlockFile lock) $ do
        env <- mdb_env_create

        -- sanity check
        lmdbMaxKeyLen <- mdb_env_get_maxkeysize env
        unless (lmdbMaxKeyLen >= maxKeyLen) $
            fail "require LMDB compiled with larger max key size."

        -- environment setup
        mdb_env_set_mapsize env (nMB * (1024 * 1024))
        mdb_env_set_maxdbs env 4
        mdb_env_open env fp lmdbEnvF

        flip onException (mdb_env_close env) $ do
            -- initial transaction to open databases. No special DB flags.
            txIni <- mdb_txn_begin env Nothing False
            let openDB s = mdb_dbi_open' txIni (Just s) [MDB_CREATE]
            dbData <- openDB "@"    -- rooted key-value data
            dbStow <- openDB "$"    -- stowed binary resources
            dbRfct <- openDB "#"    -- non-zero persistent reference counts
            dbZero <- openDB "0"    -- resources with ephemeral references
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

-- use strict bytestring key as MDB_val
withBSKey :: BS.ByteString -> (MDB_val -> IO a) -> IO a
withBSKey (BS.PS fp off len) action = 
    withForeignPtr fp $ \ p ->
        action $ MDB_val (fromIntegral len) (p `plusPtr` off)

-- use a short, lazy bytestring key as MDB_val.
withLBSKey :: ByteString -> (MDB_val -> IO a) -> IO a
withLBSKey (LBS.Chunk bs LBS.Empty) action = withBSKey bs action
withLBSKey k action =
    let len = LBS.length k in
    allocaBytes (fromIntegral len) $ \ p -> do
        copyLBS p k
        action (MDB_val (fromIntegral len) p)

-- copy a lazy bytestring to pointer destination.
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
copyMDB_to_BS (MDB_val cLen src) =
    let len = fromIntegral cLen in 
    BS.create len $ \ dst -> 
        BS.memcpy dst src len

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
--
-- For a one-off transaction, blind writes won't cause conflicts. But
-- checkpointing transactions (multiple commits) can have write-write
-- conflicts with other transactions.
--
-- Note: While Wikilon DB accepts any key, problematic keys will be
-- rewritten to a secure hash. This has some overhead and can hinder
-- LMDB-layer debugging (mdb_dump), so developers are encouraged to
-- keep keys short (< 256 bytes) and readable as filenames or URLs.
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
    -- But this might change later.

-- | Load resource directly from database.
--
-- This may miss resources associated with a specific transaction.
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

-- | Commit transaction to database. Synchronous.
--
-- Returns True on success, False otherwise. 
--
-- Transactions may be committed more than once. In that case, each
-- commit essentially checkpoints the overall transaction, limiting
-- how much work is lost upon a future commit failure.
commitTX :: TX -> IO Bool
commitTX tx = commitTX_async tx >>= id

-- | Asynchronous Commit.
--
-- This immediately starts a commit, but does not wait for success
-- or failure. This is mostly useful for asynchronous checkpoints.
-- Commits from a single transaction are always applied in order, 
-- and multiple commits in a short period of time may coalesce into
-- a single write batch. 
commitTX_async :: TX -> IO (IO Bool)
commitTX_async (TX db st) = modifyMVarMasked st $ \ s -> do
    ret <- newEmptyMVar 
    dbPushCommit db ((tx_read s, tx_write s), ret)
    let rd' = M.union (tx_write s) (tx_read s)
    let s' = s { tx_read = rd', tx_write = mempty }
    return (s', readMVar ret)

-- | Diagnose a transaction.
--
-- This function returns a list of keys whose transactional values
-- currently in conflict with database values. This check is intended
-- to diagnose contention or concurrency issues, and may be checked
-- after a transaction fails to gain more information. The result is
-- ephemeral, however, subject to change by concurrent writes.
checkTX :: TX -> IO [ByteString]
checkTX (TX db st) =
    readMVar st >>= \ s ->
    withReadLock db $ do
    txn <- mdb_txn_begin (db_env db) Nothing True
    invalidReads <- filterM (fmap not . validRead db txn) (M.toList (tx_read s))
    mdb_txn_commit txn
    return (fmap fst invalidReads)

-- The database writer thread.
--
-- All writes in Wikilon DB are funneled into this singleton thread,
-- which may write in large batches if work has accumulated during a
-- prior write frame. Write batching helps amortize a lot of latency
-- for non-conflicting concurrent or checkpointing transactions.
--
-- The current implementation leverages LMDB's property of being a
-- memory-mapped database to avoid copying data that is about to
-- be deleted or overwritten (via unsafeMDB_to_BS). We also try to
-- avoids writing stowed data that should be immediately GC'd, or
-- writing any object more than once.
dbWriter :: DB -> IO ()
dbWriter !db = initLoop `catches` handlers where
    handlers = [Handler onGC, Handler onError]
    onGC :: BlockedIndefinitelyOnMVar -> IO ()
    onGC _ = do
        mdb_env_sync_flush (db_env db)
        mdb_env_close (db_env db)
        FL.unlockFile (db_fl db)
    onError :: SomeException -> IO ()
    onError e = do
        putErrLn $ "Wikilon Database (" ++ show db ++ ") writer FAILED"
        putErrLn $ indent "    " (show e)
        putErrLn $ "Aborting Program!"
        Sys.exitFailure

    -- start loop with initial read frame
    initLoop = advanceReadFrame db >>= writeLoop

    -- verify a read against accepted write set or LMDB
    checkRead :: MDB_txn -> KVMap -> (ByteString, ByteString) -> IO Bool
    checkRead txn ws rd@(k,vTX) = case M.lookup k ws of
        Nothing -> validRead db txn rd
        Just vW -> return (vW == vTX)

    -- aggregate proposed commits into a write set if possible
    -- otherwise, immediately fail the write
    joinWrite :: MDB_txn -> KVMap -> Commit -> IO KVMap
    joinWrite txn !ws ((r,w),ret) =
        allM (checkRead txn ws) (M.toList r) >>= \ bReadsOK ->
        if bReadsOK then return (M.union w ws) else
        tryPutMVar ret False >> return ws

        
    writeLoop :: R -> IO ()
    writeLoop !r = do
        -- wait for work
        takeMVar (db_signal db) 
        
        -- BEGIN TRANSACTION
        --  Order for reading txList, stowed, hold does matter.
        --  So atomicModifyIORef is used as a barrier.
        txn <- mdb_txn_begin (db_env db) Nothing False
        txList <- L.reverse <$> atomicModifyIORef (db_commit db) (\ lst -> ([],lst))
        stowed <- atomicModifyIORef (db_new db) (\x -> (x,x)) -- atomic to prevent reordering
        hold <- readIORef (db_hold db)
        let blockDel = flip Mi.member hold . ephHash 


        -- collapse writes to single batch
        writes <- foldM (joinWrite txn) mempty txList

        -- compute reference count updates from the writes
        overwrites <- mapKeysM (peekData db txn) writes
        let wRCU = addRCU 1 (L.concatMap hashDeps (M.elems writes))
                 $ addRCU (-1) (L.concatMap hashDeps (M.elems overwrites))
                 $ mempty

        -- filter stowage to new rooted resources.
        --   roots may be persistent or ephemeral
        writeRsc <- do
            newRsc <- filterKeysM (isNewRsc db txn) stowed
            let isShallowRoot h = 
                    if blockDel h then return True else
                    let u = fromMaybe 0 (M.lookup (BS.take stowKeyLen h) wRCU) in
                    dbGetRefct db txn h >>= \ ct -> return ((ct + u) /= 0)
            let f = maybe [] hashDeps . flip M.lookup newRsc
            rooted <- rootSet f <$> filterM isShallowRoot (M.keys newRsc)
            return $! (newRsc `M.intersection` rooted) -- new AND rooted resources

        -- determine which elements must be GC'd.
        --   cascading: follow tree-structured data, delete children
        --   incremental: use quota, search for pending GC candidates
        let txSize = M.size writes + M.size writeRsc
        let qc = 50 + (2 * txSize) -- max pending GC candidates
        let qgc = 5 * qc           -- soft max items to delete at once

        gcCand <- dbGCPend db txn hold qc -- pending GC candidates
        let rcu = addRCU 0 (M.keys writeRsc)    -- new ephemerons
                $ addRCU 0 gcCand               -- old ephemerons
                $ addRCU 1 (L.concatMap hashDeps (M.elems writeRsc)) -- internal refs
                $ wRCU                          -- rooted refs

        let initRC = mapKeysM (dbGetRefct db txn) 
        let gcLoop !gc !rc = 
                let mayGC h n = (0 == n) && not (blockDel h) in
                let (ngc,rcP) = M.partitionWithKey mayGC rc in
                let done = (qgc < M.size gc) || (M.null ngc) in
                if done then return (gc, rc) else
                mapM (peekRsc db txn) (M.keys ngc) >>= \ dd ->
                let rcu = addRCU (-1) (L.concatMap (maybe [] hashDeps) dd) mempty in
                initRC (M.difference rcu rc) >>= \ rcINI -> 
                let gc' = M.union gc ngc in
                let rc' = M.unionWith (+) rcu $ M.union rcP rcINI in
                assert (M.size gc' == (M.size ngc + M.size gc)) $
                assert (M.size rc' == (M.size rcP + M.size rcINI)) $
                gcLoop gc' rc'
        rc0 <- M.unionWith (+) rcu <$> initRC rcu
        (gc,rc) <- gcLoop mempty rc0

        -- more sanity checks
        assert (M.null (M.intersection writeRsc gc)) (return ())

        -- Reads are complete!
        --
        -- Read before write ensures we only write each elements once,
        -- and helps isolate the complexity to the purely functional 
        -- code. It also ensures safety of zero-copy reads (whereas
        -- mixed read-write may recycle pages in the transaction).
        --
        -- The main disadvantage is that it takes a lot of memory to
        -- build the write sets. But that's mitigated by zero-copy.

        -- update all the data at the DB layer
        mapM_ (dbDelRscAndRefct db txn) (M.keys gc)             -- delete GC'd resources
        mapM_ (uncurry (dbSetRefct db txn)) (M.toList rc)       -- update other refcts
        mapM_ (uncurry (dbPutRsc db txn)) (M.toList writeRsc)   -- write new resources
        mapM_ (uncurry (dbPutData db txn)) (M.toList writes)    -- write keyed data
        
        -- Commit and Synchronize
        waitR r                             -- wait on readers of the old LMDB frame
        mdb_txn_commit txn                  -- commit write data to the memory map
        r' <- advanceReadFrame db           -- acquire readers from prior LMDB frame
        mdb_env_sync_flush (db_env db)      -- commit write to disk

        -- report success then continue
        mapM_ (flip tryPutMVar True . snd) txList
        atomicModifyIORef' (db_new db) $ \ m -> 
            let m' = M.difference m stowed in (m',())
        writeLoop r'


-- zero-copy reference to an LMDB layer bytestring. This result is
-- safe only within the transaction.
unsafeMDB_to_BS :: MDB_val -> IO BS.ByteString
unsafeMDB_to_BS (MDB_val n p) =
    newForeignPtr_ p >>= \ fp -> 
        return (BS.PS fp 0 (fromIntegral n))

-- access data, given a key. Only valid within transaction.
peekData :: DB -> MDB_txn -> ByteString -> IO ByteString
peekData db txn k = withLBSKey (toSafeKey k) $ \ mdbKey ->
    let mkBS = maybe (return BS.empty) unsafeMDB_to_BS in
    let mkLBS = fmap LBS.fromStrict . mkBS in
    mkLBS =<< mdb_get' txn (db_data db) mdbKey

-- access a resource, given a key. Only valid within transaction.
peekRsc :: DB -> MDB_txn -> Hash -> IO (Maybe ByteString)
peekRsc db txn h = withBSKey (BS.take stowKeyLen h) $ \ mdbKey ->
    let hashRem = validHashLen - stowKeyLen in
    mdb_get' txn (db_stow db) mdbKey >>= \ mbv -> case mbv of
        Nothing -> return Nothing
        Just v -> unsafeMDB_to_BS (mdbSkip hashRem v) >>= \ bs ->
                  return (Just (LBS.fromStrict bs))

-- scan database for a set of objects to be collected.
-- result is invalid outside the transaction.
dbGCPend :: DB -> MDB_txn -> EphTbl -> Int -> IO [Hash]
dbGCPend db txn hold quota = alloca $ \ pHash -> do
    crs <- mdb_cursor_open' txn (db_zero db)
    let loop !b !n !r =
            if ((not b) || (0 == n)) then return r else
            peek pHash >>= \ hMDB ->
            assert (mv_size hMDB == stowKeyLen) $
            ephHashMDB hMDB >>= \ eh ->
            mdb_cursor_get' MDB_NEXT crs pHash nullPtr >>= \ b' ->
            if Mi.member eh hold then loop b' n r else
            unsafeMDB_to_BS hMDB >>= \ h ->
            loop b' (n - 1) (h : r)
    b0 <- mdb_cursor_get' MDB_FIRST crs pHash nullPtr
    lst <- loop b0 quota []
    mdb_cursor_close' crs
    return lst





-- test whether a resource is new to the LMDB layer
isNewRsc :: DB -> MDB_txn -> Hash -> IO Bool
isNewRsc db txn h = withBSKey (BS.take stowKeyLen h) $ \ mdbKey ->
    isNothing <$> mdb_get' txn (db_stow db) mdbKey

-- skip the first n bytes of an MDB_val
mdbSkip :: Int -> MDB_val -> MDB_val
mdbSkip n (MDB_val sz p) = 
    assert (sz >= fromIntegral n) $
    MDB_val (sz - fromIntegral n) (p `plusPtr` n)

sel :: a -> a -> Bool -> a
sel t f b = if b then t else f

filterKeysM :: (Applicative m) => (k -> m Bool) -> M.Map k a -> m (M.Map k a)
filterKeysM op = M.traverseMaybeWithKey $ \ k v ->
    sel (Just v) Nothing <$> op k

mapKeysM :: (Applicative m) => (k -> m b) -> M.Map k a -> m (M.Map k b)
mapKeysM op = M.traverseWithKey $ \ k _ -> op k


-- compute lightweight root set
type RootSet k = M.Map k ()
rootSet :: (Ord k) => (k -> [k]) -> [k] -> RootSet k
rootSet f = addTo mempty where
    addTo = L.foldl' $ \ r k -> if M.member k r then r else addTo r (f k) 


-- Reference count tracking.
-- 
-- For now, all reference counts are written into a simple map, using
-- only the stowKeyLen fragment of the hash string to resist possible
-- timing attacks. This is far from optimal for allocations, but it is
-- simple to use in Haskell. 
type RCU = M.Map Hash Int

addRCU :: Int -> [Hash] -> RCU -> RCU 
addRCU !n = flip (L.foldl' accum) where
    accum m h = M.insertWith (+) (BS.take stowKeyLen h) n m

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

-- Record a reference count into the database. This will record zero
-- reference counts into the `db_zero` table so we can find them again
-- quickly for incremental GC.
dbSetRefct :: DB -> MDB_txn -> Hash -> Int -> IO ()
dbSetRefct db txn h 0 = 
    assert (BS.length h == stowKeyLen) $
    withBSKey h $ \ hMDB -> do
        let wf = compileWriteFlags []
        mdb_put' wf txn (db_zero db) hMDB (MDB_val 0 nullPtr)
        mdb_del' txn (db_rfct db) hMDB Nothing
        return ()
dbSetRefct db txn h n = 
    assert ((n > 0) && (BS.length h == stowKeyLen)) $
    withNatVal n $ \ nMDB ->
    withBSKey h $ \ hMDB -> do
        let wf = compileWriteFlags []
        mdb_del' txn (db_zero db) hMDB Nothing
        mdb_put' wf txn (db_rfct db) hMDB nMDB
        return ()

withNatVal :: Int -> (MDB_val -> IO a) -> IO a
withNatVal = withAllocaBytesVal . natDigits

natDigits :: Int -> [Word8]
natDigits = go [] where
    go r n = 
        let (n', c) = n `divMod` 10 in
        let r' = (fromIntegral (c + 48)) : r in
        if (0 == n') then r' else go r' n'

withAllocaBytesVal :: [Word8] -> (MDB_val -> IO a) -> IO a
withAllocaBytesVal bytes action = 
    let len = L.length bytes in
    allocaBytes len $ \ p -> do
        putBytes p bytes
        action (MDB_val (fromIntegral len) p)

putBytes :: Ptr Word8 -> [Word8] -> IO ()
putBytes !p (c:cs) = poke p c >> putBytes (p `plusPtr` 1) cs
putBytes _ [] = return ()

-- fully delete a resource 
dbDelRscAndRefct :: DB -> MDB_txn -> Hash -> IO ()
dbDelRscAndRefct db txn h = 
    assert (BS.length h == stowKeyLen) $
    withBSKey h $ \ hMDB -> do
        mdb_del' txn (db_stow db) hMDB Nothing
        mdb_del' txn (db_rfct db) hMDB Nothing
        mdb_del' txn (db_zero db) hMDB Nothing
        return ()

dbPutRsc :: DB -> MDB_txn -> Hash -> ByteString -> IO ()
dbPutRsc db txn h v =
    assert (BS.length h == validHashLen) $
    withBSKey (BS.take stowKeyLen h) $ \ hMDB -> do
        let hv = LBS.fromStrict (BS.drop stowKeyLen h) <> v 
        let sz = fromIntegral (LBS.length hv)
        let wf = compileWriteFlags [MDB_NOOVERWRITE]
        dst <- mdb_reserve' wf txn (db_stow db) hMDB sz
        copyLBS (mv_data dst) hv

-- empty string is equivalent to deletion from API
dbDelData :: DB -> MDB_txn -> ByteString -> IO ()
dbDelData db txn k =
    withLBSKey (toSafeKey k) $ \ mdbKey -> do
        mdb_del' txn (db_data db) mdbKey Nothing
        return ()

dbPutData :: DB -> MDB_txn -> ByteString -> ByteString -> IO ()
dbPutData db txn k v = 
    if LBS.null v then dbDelData db txn k else
    withLBSKey (toSafeKey k) $ \ mdbKey -> do 
        let wf = compileWriteFlags []
        let sz = fromIntegral (LBS.length v)
        dst <- mdb_reserve' wf txn (db_data db) mdbKey sz
        copyLBS (mv_data dst) v


-- indent all lines by w
indent :: String -> String -> String
indent w = (w ++) . indent' where
    indent' ('\n':s) = '\n' : indent w s
    indent' (c:s) = c : indent' s
    indent' [] = []

-- print to stderr
putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr
        




