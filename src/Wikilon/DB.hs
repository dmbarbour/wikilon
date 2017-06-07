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
    , open, close

    , newTX, txDB
    , readKey, writeKeyVal
    , loadRsc, stowRsc, dropRsc
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
import Data.Function (on)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified System.IO (FilePath)
import qualified System.IO.Error as E
import qualified System.Directory as FS 
import System.FilePath ((</>))
import qualified System.FileLock as FL 
import Data.IORef
import qualified Data.Map as M
import Data.Word (Word8)
import Data.Monoid
import Database.LMDB.Raw
import System.IO.Unsafe (unsafeInterleaveIO)
import Awelon.Syntax (validWordByte)
import Awelon.Hash
import Debug.Trace

-- these errors shouldn't appear regardless of user input
dbError :: String -> a
dbError = error . (++) "Wikilon.DB: "

-- | Database Resource
--
-- The database supports both key-value binaries and stowage where we
-- reference binaries via secure hashes. Much logic is related to GC
-- of secure hash resources. 
--
-- Wikilon DB assumes that most data is managed via stowage, that keys
-- are relatively small and transactions rarely interact with more than
-- a few distinct keys. 
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
  , db_rdlock   :: !(IORef (R,R))

    -- Asynch Write Layer
  , db_signal   :: !(MVar ())               -- work available?
  , db_newrsc   :: !(IORef Stowage)         -- pending stowage
  , db_commit   :: !(IORef [TXCommit])      -- commit requests

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
-- ephemeron table to shared memory, and use a shared write mutex.

instance Eq DB where
    (==) = (==) `on` db_signal

instance Show DB where
    showsPrec _ db = showString "DB@" . showString (db_fp db)

type EphTbl = M.Map Hash Int            -- ^ precise but expensive
type Stowage = M.Map Hash ByteString    -- ^ latent batch for DB
type TXCommit = (TX, MVar Bool)         -- ^ MVar for future result
data R = R !(MVar Int) !(MVar ())       -- ^ simple reader count

dbSignal :: DB -> IO ()
dbSignal db = tryPutMVar (db_signal db) () >> return ()

dbPushCommit :: DB -> TXCommit -> IO ()
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
    traceM ("TX releasing " ++ show (M.size drop) ++ " resources") >>
    if M.null drop then return () else
    atomicModifyIORef' (db_gc_hold db) $ \ tbl ->
        let diff = \ l r -> nonZero (l - r) in
        let tbl' = M.differenceWith diff tbl drop in
        (tbl', ())

-- | Add ephemeral stowage references. 
dbAddEph :: DB -> EphTbl -> IO ()
dbAddEph db update = 
    if M.null update then return () else
    atomicModifyIORef' (db_gc_hold db) $ \ tbl ->
        let tbl' = M.unionWith (+) tbl update in 
        (tbl', ())

-- | Perform an operation while holding a read lock.
-- 
-- LMDB with MDB_NOLOCK essentially uses a double-buffering model.
-- Between writes, we have two safe frames: the recently committed
-- frame, and one frame prior. During writes, we have only one safe
-- frame, since the prior frame may be concurrently dismantled.
--
-- Thus, we can simply have our writer wait for the readers of the
-- prior frame before we risk dismantling it. Readers never wait on
-- the writer, and our writer waits only on long lived readers. By
-- design, this API will resist long-lived readers since each read
-- only grabs a small number of elements.
--
-- We'll model two reader frames - (R,R) - such that we guarantee
-- all readers of the prior frame are in the second R. (There may
-- be some overlap, where some readers of the current frame also
-- are in the second R. But that's still safe.)
withReadLock :: DB -> IO a -> IO a
withReadLock db = bracket acq relR . const where
    acq = readIORef (db_rdlock db) >>= \ (r,_) -> acqR r >> return r

advanceReadFrame :: DB -> IO R
advanceReadFrame db = 
    newR >>= \ rN' ->
    atomicModifyIORef (db_rdlock db) $ \ (rN, rO) -> 
        ((rN',rN), rO)

writerWaitOnReaders :: DB -> IO ()
writerWaitOnReaders db = advanceReadFrame db >>= waitR
    
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


-- | Open or Create the Database. 
--
-- The argument is simply a directory where we expect to open the
-- database, and a maximum database size in megabytes.
--
-- The current implementation uses LMDB with the MDB_NOLOCK option,
-- and also uses process-local ephemeron tables to prevent GC of 
-- recently stowed resources. Thus, it is not safe to open the 
-- database from multiple processes. To resist accidents, a lockfile
-- is used.
open :: FilePath -> Int -> IO (Either SomeException DB)
open fp nMB = try $ do
    FS.createDirectoryIfMissing True fp
    lock <- tryLockE (fp </> "lockfile")
    flip onException (FL.unlockFile lock) $ do
        env <- mdb_env_create
        mdb_env_set_mapsize env (nMB * (1024 * 1024))
        mdb_env_set_maxdbs env 4
        mdb_env_open env fp [MDB_NOLOCK]
        flip onException (mdb_env_close env) $ do
            -- initial transaction to open databases. No special DB flags.
            txIni <- mdb_txn_begin env Nothing False
            let openDB s = mdb_dbi_open' txIni (Just s) [MDB_CREATE]
            dbData <- openDB "@"    -- named data
            dbStow <- openDB "$"    -- stowed data
            dbRfct <- openDB "#"    -- non-zero persistent reference counts
            dbZero <- openDB "0"    -- hashes with only volatile references
            mdb_txn_commit txIni

            rN <- newR
            rO <- newR
            dbRdLock <- newIORef (rN,rO) -- readers tracking
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

dbWriter :: DB -> IO ()
dbWriter _ = return ()

-- try lock with a simple IOError
tryLockE :: FilePath -> IO FL.FileLock
tryLockE fp =
    FL.tryLockFile fp FL.Exclusive >>= \ mbLocked ->
    case mbLocked of
        Just fl -> return fl
        Nothing -> E.ioError $ E.mkIOError 
            E.alreadyInUseErrorType "exclusive file lock failed" 
            Nothing (Just fp)

-- | Close the Database.
--
-- The caller must ensure the database is not used during or after the
-- close operation. So this is only useful for a graceful shutdown. In
-- normal usage, Wikilon DB expects to close by crashing, e.g. process
-- killed or sudden power failure.
close :: DB -> IO ()
close db = do
    mdb_env_sync_flush (db_env db)
    mdb_env_close (db_env db)
    FL.unlockFile (db_fl db)

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
-- to use queues to avoid or control conflicts.
--
-- The TX is thread safe and may be committed more than once to represent
-- ongoing progress. TX doesn't need to be aborted explicitly: just don't
-- commit. 
data TX = TX !DB !(MVar TXS)
type KVMap = M.Map ByteString ByteString

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

-- | Max accepted key length.
-- 
-- To simplify debugging with mdb_dump keys are kept intact up to a
-- limited size. OTOH, beyond some point, it's just wasted memory.
-- I've decided to limit preserved key sizes to 255 bytes.
maxKeyLen :: Integral a => a
maxKeyLen = 255

-- | To prevent aliasing, we distinguish safe keys from normal keys.
safeKeyPrefix :: Word8
safeKeyPrefix = 35

-- | Ideally, keys are short and human meaningful. But I'll leave that
-- to the client. Here, I just test for okay size and that we won't 
-- alias with keys rewritten for safety.
validKey :: ByteString -> Bool
validKey s = case LBS.uncons s of
    Nothing -> False -- empty key isn't okay for LMDB
    Just (c, s') -> (safeKeyPrefix /= c) && 
                    (maxKeyLen > LBS.length s')

-- | Rewrite problematic keys if necessary.
safeKey :: ByteString -> ByteString
safeKey s = if validKey s then s else 
    LBS.singleton safeKeyPrefix <>
    LBS.fromStrict (BS.drop 1 (hashL s))

-- | withKey: access key for use in MDB
withKey :: ByteString -> (MDB_val -> IO a) -> IO a
withKey = withSafeKey . safeKey where
    withSafeKey k action = do
        let len = LBS.length k 
        undefined

-- | Retrieve value associated with given key.
--
-- If the key has already been read or written, this will retrieve
-- the current value. Otherwise, it will copy the data from the DB.
-- Unfortunately, this means we do not ensure snapshot consistency
-- within a transaction (you might see inconsistent data). Only on
-- commit will consistency be verified. 
readKey :: TX -> ByteString -> IO ByteString
readKey (TX db st) (force -> !k) =  modifyMVarMasked st $ \ s -> do
    let keyInTX = M.lookup k (tx_write s) <|>   -- prior write of key
                  M.lookup k (tx_read s)        -- prior read of key
    case keyInTX of
        Just val -> return (s, val)
        Nothing  -> withKey k $ \ mdbKey -> withReadLock db $ do
                txn <- mdb_txn_begin (db_env db) Nothing True
                undefined

-- | Write a key-value pair.
--
-- Writes are simply recorded into the transaction until commit,
-- so this becomes a relatively lightweight operation. 
writeKeyVal :: TX -> ByteString -> ByteString -> IO ()
writeKeyVal (TX _ st) (force -> !k) (force -> !v) = 
    modifyMVarMasked_ st $ \ s ->
        let w' = M.insert k v (tx_write s) in
        return $! s { tx_write = w' }
        

-- | Access a stowed resource by secure hash.
--
-- If the resource is not in the DB or TX, we return Nothing. In that
-- case, you might search outside the DB - the file system or network.
-- Any means of locating a binary with the given hash is essentially
-- the same.
--
-- Unlike keys, resources are immutable and needn't be validated when
-- we commit the transaction. It's best if most data in a Wikilon DB
-- is managed in terms of these stowage resources, as it results in
-- lighter weight transactions.
loadRsc :: TX -> Hash -> IO (Maybe ByteString)
loadRsc tx (force -> !h) = undefined

-- | Move resource to database, returning secure hash (Awelon.Hash).
--
-- Stowage resources are automatically named by secure hash, which
-- is also used to look up the resource later as needed. The data
-- is immediately moved to the database. Normally objects without
-- persistent references may be garbage collected, but the TX will
-- hold onto stowed resources via an ephemeron table. (This only 
-- affects resources stowed through that specific TX.)
--
-- Note: It's important to avoid stowing short-lived, intermediate
-- resources. E.g. for constructing a trie one key at a time, we
-- could naively stow a new root node and a few more at each step.
-- But 99% of those nodes would later be GC'd. Try to use batching
-- or latent stowage or similar.
stowRsc :: TX -> ByteString -> IO Hash
stowRsc tx (force -> !v) = undefined

-- | Release ephemeral root produced by stowRsc.
--
-- If you determine that you no longer need a resource, you can drop
-- it. This allows GC of the resource, assuming there are no other
-- references than from this transaction.
--
-- This operation might be useful for long-running computations or
-- an explicit cache manager, but normally you should just wait for
-- GC and allow the to finalizer clear ephemeral references.
dropRsc :: TX -> Hash -> IO ()
dropRsc (TX db st) (force -> !h) = modifyMVarMasked_ st $ \ s ->
    case M.lookup h (tx_hold s) of
        Nothing -> return s
        Just n  -> do
            dbClearEph db (M.singleton h n)
            let hold' = M.delete h (tx_hold s)
            return $! s { tx_hold = hold' }

-- | Commit transaction to database.
--
-- Commit will returns True if commit succeeds, False otherwise.
-- The actual write is asynchronous, but we'll validate our 
-- This uses a lazy, asynchronous, boolean result so you won't wait
-- for commit to succeed until you must know whether it succeeded.
-- (Use `commitTX >>= evaluate` for synchronous commit.)
--
-- While commi
commitTX :: TX -> IO Bool
commitTX tx@(TX db _) = do
    v <- newEmptyMVar
    dbPushCommit db (tx,v)
    unsafeInterleaveIO (readMVar v)

