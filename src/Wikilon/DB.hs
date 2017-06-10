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
-- All data is rooted using a simple key-value registry, which can
-- serve as a lightweight filesystem. But Wikilon DB can support 
-- rich tree-structured values via the stowage model, using secure
-- hashes to reference between binaries.
--
-- Wikilon DB assumes that most data is managed via stowage. And
-- stowed data can be shared between keys or garbage collected as
-- needed.
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
  , db_newrsc   :: !(IORef Stowage)         -- pending stowage
  , db_commit   :: !(IORef [Commit])        -- commit requests

    -- Stowage GC Layer
  , db_gc_last  :: !(IORef (Maybe Hash)) -- for incremental GC
  , db_gc_hold  :: !(IORef EphTbl) -- ephemeron table
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
type EphTbl = Mi.IntMap Int                 -- ^ table of (hash,count)

-- | My ephemeron table is a simple (hash,count) collection.
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

dbSignal :: DB -> IO ()
dbSignal db = tryPutMVar (db_signal db) () >> return ()

dbPushCommit :: DB -> Commit -> IO ()
dbPushCommit db !task = do
    atomicModifyIORef (db_commit db) $ \ lst -> ((task:lst), ())
    dbSignal db

dbPushStow :: DB -> Stowage -> IO ()
dbPushStow db s = do
    atomicModifyIORef' (db_newrsc db) $ \ s0 -> (M.union s0 s, ()) 
    dbSignal db

nonZero :: Int -> Maybe Int
nonZero 0 = Nothing
nonZero n = Just n

-- | release ephemeral stowage references.
dbClearEph :: DB -> EphTbl -> IO ()
dbClearEph db drop = 
    traceM ("TX releasing " ++ show (Mi.size drop) ++ " resources") >>
    if Mi.null drop then return () else
    atomicModifyIORef' (db_gc_hold db) $ \ tbl ->
        let diff = \ l r -> nonZero (l - r) in
        let tbl' = Mi.differenceWith diff tbl drop in
        (tbl', ())

-- | Add ephemeral stowage references. 
dbAddEph :: DB -> EphTbl -> IO ()
dbAddEph db update = 
    if Mi.null update then return () else
    atomicModifyIORef' (db_gc_hold db) $ \ tbl ->
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

        let oneMB = (1024 * 1024)
        unless ((maxBound `div` oneMB) > nMB) (fail "database size overflow")
        mdb_env_set_mapsize env (nMB * oneMB)
        
        mkl <- mdb_env_get_maxkeysize env
        unless (mkl >= maxKeyLen) (fail "expecting LMDB to support larger keys!")

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
            dbNewRsc <- newIORef mempty
            dbGCLast <- newIORef mempty
            dbGCHold <- newIORef mempty

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
                        , db_newrsc = dbNewRsc
                        , db_gc_last = dbGCLast
                        , db_gc_hold = dbGCHold
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

-- | Clear ephemeral stowage.
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

-- | Max accepted key length.
-- 
-- To simplify debugging with mdb_dump keys are kept intact up to a
-- limited size. OTOH, beyond some point, it's just wasted memory.
maxKeyLen :: Integral a => a
maxKeyLen = 255

-- | Test for problematic key.
--
-- Anything oversized, the empty string, or keys that start with SUB (26)
-- will be rewritten to the secure hash. 
validKey :: ByteString -> Bool
validKey s = case LBS.uncons s of
    Nothing -> False -- reject empty key
    Just (c, s') -> (26 /= c) && 
                    (maxKeyLen > LBS.length s')

-- | Substitute problematic keys with a collision resistant secure hash.
--
-- Safe keys will start with SUB, and any key starting with SUB will be
-- rewritten to a safe key to prevent aliasing. This won't affect normal
-- operations, but safe keys can hinder debugging with mdb_dump.
--
-- Safe keys will be used starting within the TX read and write KVMaps.
toSafeKey :: ByteString -> ByteString
toSafeKey s = if validKey s then s else 
    LBS.singleton 26 <> -- start with SUB (26)
    LBS.fromStrict (BS.take stowKeyLen (hashL s))

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
    (LBS.Chunk (BS.PS fp off len) more) ->
        withForeignPtr fp $ \ src -> do
            BS.memcpy dst (src `plusPtr` off) len
            copyLBS (dst `plusPtr` len) more

-- | copy an MDB for use as a Haskell bytestring.
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
-- Security Note: It's up to the client to provide access control for
-- keys. This may be done by partitioning access to keys by prefix,
-- similar to directories in a filesystem. Or the client could use
-- HMACs to share and validate key strings. Whatever. Wikilon DB does
-- not secure normal keys.
readKey :: TX -> ByteString -> IO ByteString
readKey (TX db st) (toSafeKey -> !k) = modifyMVarMasked st $ \ s ->
    case readSafeKeyTXS s k of
        Just v  -> return (s, v)
        Nothing -> do
            v <- readSafeKeyDB db k
            let r' = M.insert k v (tx_read s) 
            let s' = s { tx_read = r' }
            return (s', v)

-- | Read key from transaction state if it has been read or written prior. 
readSafeKeyTXS :: TXS -> ByteString -> Maybe ByteString
readSafeKeyTXS s k = M.lookup k (tx_write s) <|> M.lookup k (tx_read s)

-- | Read key directly from database.
--
-- This retrieves the most recently committed value for a key. This is
-- equivalent to readKey with a freshly created transaction.
readKeyDB :: DB -> ByteString -> IO ByteString
readKeyDB db = readSafeKeyDB db . toSafeKey

readSafeKeyDB :: DB -> ByteString -> IO ByteString
readSafeKeyDB db !key = withReadLock db $ do 
    txn <- mdb_txn_begin (db_env db) Nothing True
    val <- dbReadKey db txn key
    mdb_txn_commit txn
    return val

-- | Obtain a value after we have our transaction. Assumes safe key.
dbReadKey :: DB -> MDB_txn -> ByteString -> IO ByteString
dbReadKey db txn k = withLBSKey k $ \ mdbKey -> do
    let toBS = maybe (return BS.empty) copyMDB_to_BS
    bs <- toBS =<< mdb_get' txn (db_data db) mdbKey
    return $! LBS.fromStrict bs

toSafeKeys :: [ByteString] -> [ByteString]
toSafeKeys = force . fmap toSafeKey

-- | Read values for multiple keys.
--
-- This reads multiple keys with a single LMDB-layer transaction. The 
-- main benefit with readKeys is snapshot isolation for keys initially
-- read together. 
readKeys :: TX -> [ByteString] -> IO [ByteString]
readKeys (TX db st) (toSafeKeys -> !allKeys) = modifyMVarMasked st $ \ s -> do
    let newKeys = L.filter (isNothing . readSafeKeyTXS s) allKeys 
    newVals <- readSafeKeysDB db newKeys
    let r' = M.union (tx_read s) (M.fromList (L.zip newKeys newVals))
    let s' = s { tx_read = r' }
    let allVals = fmap (fromJust . readSafeKeyTXS s') allKeys 
    return (s', allVals)

-- | Read multiple keys directly from database.
--
-- This obtains a snapshot for a few values from the database. This
-- is equivalent to readKeys using a freshly created transaction.
readKeysDB :: DB -> [ByteString] -> IO [ByteString]
readKeysDB db = readSafeKeysDB db . toSafeKeys

readSafeKeysDB :: DB -> [ByteString] -> IO [ByteString]
readSafeKeysDB db keys = 
    if L.null keys then return [] else 
    withReadLock db $ do 
        txn <- mdb_txn_begin (db_env db) Nothing True
        vals <- mapM (dbReadKey db txn) keys
        mdb_txn_commit txn
        return vals
  
-- | Write a key-value pair.
--
-- Writes are trivially recorded into the transaction until commit.
-- There is no risk of write-write conflicts. Subsequent reads will
-- observe the value written.
--
-- Note: Wikilon DB may rewrite problematic keys using a secure hash.
-- This can affect debugging via mdb_dump and LMDB layer tools. Favor
-- keys that are short and sensible as words, URLs, or filenames.
writeKey :: TX -> ByteString -> ByteString -> IO ()
writeKey (TX _ st) (toSafeKey -> !k) (force -> !v) = 
    modifyMVarMasked_ st $ \ s ->
        let w' = M.insert k v (tx_write s) in
        return $! s { tx_write = w' }

-- | Key Length for Stowage
--
-- Wikilon DB uses only half of the hash for LMDB layer lookups,
-- and uses the remaining half for a constant-time comparison.
-- This helps resist timing attacks. 
stowKeyLen :: Integral a => a
stowKeyLen = validHashLen `div` 2

-- | Access a stowed resource by secure hash.
--
-- This searches for a resource identified by secure hash within 
-- the Wikilon database or transaction. If not found, this returns
-- Nothing, in which case you might search elsewhere like the file
-- system or network. (These resources are provider independent.)
--
-- Note: Loading stowed data doesn't prevent GC of the resource. But
-- for a successful transaction, keys read won't change before commit.
-- So failure to load a resource might indicate transaction failure,
-- or that the resource was never known to the database.
--
-- Security Note: secure hashes are essentially object capabilities,
-- and leaking capabilities is a valid concern. Timing attacks are
-- a likely vector for such leaks. This function exposes the first
-- half of a hash to timing attacks. That leaves plenty of bits for
-- security, but clients should be careful to not expose them at 
-- any other layer.
loadRsc :: TX -> Hash -> IO (Maybe ByteString)
loadRsc (TX db _) = loadRscDB db

-- | Load resource directly from database.
loadRscDB :: DB -> Hash -> IO (Maybe ByteString)
loadRscDB db !h = 
    assert (validHashLen > stowKeyLen) $
    if (BS.length h /= validHashLen) then return Nothing else
    readIORef (db_newrsc db) >>= \ newRscTbl ->
    withReadLock db $ 
    withBSKey h $ \ hMDB -> do
    let hKey = MDB_val stowKeyLen (mv_data hMDB)                       -- normal LMDB lookup
    let hRem = MDB_val (mv_size hMDB - stowKeyLen) 
                       (mv_data hMDB `plusPtr` stowKeyLen)             -- prefix for value
    txn <- mdb_txn_begin (db_env db) Nothing True
    v <- tryRscVal hRem =<< mdb_get' txn (db_stow db) hKey
    mdb_txn_commit txn
    return (v <|> lookupRsc h newRscTbl)

-- | constant-time equality comparison for memory pointers.
ctEqMem :: Ptr Word8 -> Ptr Word8 -> Int -> IO Bool
ctEqMem !l !r = go 0 where
    go !b !sz = 
        if (0 == sz) then return $! (0 == b) else do
        let ix = (sz - 1)
        lB <- peekElemOff l ix
        rB <- peekElemOff r ix
        go (b .|. (lB `xor` rB)) ix

-- | timing attack resistant prefix matching
ctMatchPrefix :: MDB_val -> MDB_val -> IO Bool
ctMatchPrefix p d =
    if (mv_size p > mv_size d) then return False else
    ctEqMem (mv_data p) (mv_data d) (fromIntegral (mv_size p))

-- | Accept a resource value after stripping a given prefix.
tryRscVal :: MDB_val -> Maybe MDB_val -> IO (Maybe ByteString)
tryRscVal _ Nothing = return Nothing
tryRscVal p (Just c) =
    ctMatchPrefix p c >>= \ bHasPrefix ->
    if not bHasPrefix then return Nothing else
    let dLen = mv_size c - mv_size p in
    let dPtr = mv_data c `plusPtr` (fromIntegral (mv_size p)) in
    copyMDB_to_BS (MDB_val dLen dPtr) >>= \ bs ->
    return (Just (LBS.fromStrict bs))

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
-- is also used to load the resource from the database as needed.
-- The data is pushed quickly to the database, enabling stowage to
-- also double as a virtual memory system.
--
-- Wikilon DB will garbage collect resources that are not rooted by
-- a key, so a transaction that stows a resource should also try to
-- root it. Meanwhile, there are ephemeral roots associated with
-- the transaction object (via System.Mem.Weak) to prevent loss of
-- data that might later be rooted.
--
-- An obvious caveat:
-- 
-- Stowage resources are slow and expensive compared to memory, so
-- we can't afford to create too much garbage at the stowage layer.
-- Try to use batching, staging, or intermediate representations to
-- avoid short-lived stowage resources. Study data structures such
-- as the LSM tree that are designed for use in slower media.
--
stowRsc :: TX -> ByteString -> IO Hash
stowRsc (TX db st) (force -> !v) = 
    evaluate (hashL v) >>= \ h ->
    modifyMVarMasked st $ \ s -> do
        let ephUpd = Mi.singleton (ephHash h) 1 
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
-- Transactions may be committed more than once. Each commit acts as
-- a checkpoint, so a future failure won't affect prior checkpoints.
-- Conflicts are still determined relative to transaction creation.
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
dbWriter db = initLoop `catches` handlers where
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

    -- In each loop, the writer will try available transactions,
    -- write incoming stowage, do some GC. We will wait on old
    -- readers before commit, advance the reader frame after.
    -- If no work is available, we'll sleep on an MVar.
    loop :: R -> IO ()
    loop !oldReaders = do
        takeMVar (db_signal db) 

        txList <- atomicModifyIORef (db_commit db) (\ lst -> ([],lst))
        newRsc <- readIORef (db_newrsc db)

        undefined

    -- get started by treating any current readers as old readers.
    initLoop = advanceReadFrame db >>= loop






-- indent all lines by w
indent :: String -> String -> String
indent w = (w ++) . indent' where
    indent' ('\n':s) = '\n' : indent w s
    indent' (c:s) = c : indent' s
    indent' [] = []

-- print to stderr
putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr
        

