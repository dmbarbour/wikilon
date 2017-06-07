{-# LANGUAGE BangPatterns #-}
-- | Wikilon Persistence Layer
--
-- Wikilon provides a simple key-value database for rooted data, but
-- the bulk of Wikilon data is based on "stowage" - use of secure 
-- hashes to reference binaries. Unlike keys, secure hash resources
-- require garbage collection.
--
-- Persistence is implemented above LMDB, but the LMDB layer is mostly
-- hidden below a lightweight transaction API.
--
module Wikilon.DB
    ( DB, TX
    , open, close
    , newTX, txDB

    , readKey, readKeys
    , assumeKeyVal, writeKeyVal
    , loadRsc, stowRsc, pushRsc
    , commit, testCommit
    
    , FilePath
    , ByteString
    , module Awelon.Hash
    ) where

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Function (on)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified System.IO (FilePath)
import qualified System.IO.Error as E
import qualified System.Directory as FS 
import System.FilePath ((</>))
import qualified System.FileLock as FL 
import Data.IORef
import qualified Data.Map as M
import Database.LMDB.Raw
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

    -- LMDB layer
  , db_env      :: !MDB_env  
  , db_data     :: {-# UNPACK #-} !MDB_dbi' -- key -> value roots
  , db_stow     :: {-# UNPACK #-} !MDB_dbi' -- secureHash -> binary
  , db_rfct     :: {-# UNPACK #-} !MDB_dbi' -- secureHash -> positive count ([1-9][0-9]*)
  , db_zero     :: {-# UNPACK #-} !MDB_dbi' -- secureHash set with rfct=0

    -- Asynch Write Layer
  , db_signal   :: !(MVar ())               -- work available?
  , db_tasks    :: !(IORef [DBTask])        -- tasks to perform

    -- Stowage GC Layer
  , db_gc_last  :: !(IORef (Maybe Hash)) -- for incremental GC
  , db_gc_hold  :: !(IORef EphTbl) -- ephemeron table
  } 
-- notes: Reference counts are partitioned so we can quickly locate
-- objects with zero references for purpose of incremental GC. The
-- ephemeron table can preserve some objects in the database as if
-- rooted.

dbPushTask :: DB -> DBTask -> IO ()
dbPushTask !db !task = do
    atomicModifyIORef (db_tasks db) $ \ lst -> ((task:lst), ())
    dbSignal db

dbSignal :: DB -> IO ()
dbSignal db = tryPutMVar (db_signal db) () >> return ()

-- | Ephemeron table.
--
-- At the moment, the table is simple and precise, but probably is a
-- lot more expensive than it really ought to be. It is feasible to
-- later optimize this table... but it is not a high priority.
type EphTbl = M.Map Hash Int

-- | An asynchronous DB writer task is either a transaction commit
-- or a push of stowage resources. Note: if we commit with concurrent 
-- users of a transaction, there are no guarantees of exactly where
-- the transaction is committed (because can't guarantee how far the
-- concurrent processes run in any case).
data DBTask 
    = DBCommit !TX !(MVar Bool)  -- synchronous commit
    | DBStow !(MVar TXS)         -- asynchronous stowage

instance Eq DB where
    (==) = (==) `on` db_signal

instance Show DB where
    showsPrec _ db = showString "DB@" . showString (db_fp db)


ephDiff :: EphTbl -> EphTbl -> EphTbl
ephDiff = M.differenceWith $ \ l r ->
    case (l - r) of 
        0 -> Nothing
        n -> Just n

ephAdd :: EphTbl -> EphTbl -> EphTbl
ephAdd = M.unionWith (+)

-- | release ephemeral stowage associated with a transaction.
dbClearEph :: DB -> EphTbl -> IO ()
dbClearEph db drop = 
    traceM ("TX releasing " ++ show (M.size drop) ++ " resources") >>
    if M.null drop then return () else
    atomicModifyIORef' (db_gc_hold db) $ \ tbl ->
        let tbl' = ephDiff tbl drop in (tbl', ())

-- | Open or Create the Database. 
--
-- The argument is simply a directory where we expect to open the
-- database, and a maximum database size in megabytes.
open :: FilePath -> Int -> IO (Either SomeException DB)
open fp nMB = runInBoundThread $ try $ do
    FS.createDirectoryIfMissing True fp
    lock <- tryLockE (fp </> "lockfile")
    flip onException (FL.unlockFile lock) $ do
        env <- mdb_env_create
        mdb_env_set_mapsize env (nMB * (1024 * 1024))
        mdb_env_set_maxdbs env 4
        mdb_env_open env fp [] -- no special environment flags
        flip onException (mdb_env_close env) $ do
            -- initial transaction to open databases. No special DB flags.
            txIni <- mdb_txn_begin env Nothing False
            let openDB s = mdb_dbi_open' txIni (Just s) [MDB_CREATE]
            dbData <- openDB "@"
            dbStow <- openDB "$"
            dbRfct <- openDB "#"
            dbZero <- openDB "0"
            mdb_txn_commit txIni

            dbSignal <- newMVar () -- initial signal to perform GC
            dbTasks <- newIORef mempty
            gcHold <- newIORef mempty
            gcLast <- newIORef mempty

            let db = DB { db_fp = fp
                        , db_fl = lock
                        , db_env = env
                        , db_data = dbData
                        , db_stow = dbStow
                        , db_rfct = dbRfct
                        , db_zero = dbZero
                        , db_signal = dbSignal
                        , db_tasks = dbTasks
                        , db_gc_hold = gcHold
                        , db_gc_last = gcLast
                        }
            forkOS (dbWriter db)
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

-- | Close the Database. It's up to the client to not use it after.
-- In most use cases, the database would never be closed. A program
-- simply opens the database then runs until killed. But this does
-- offer a graceful shutdown option.
close :: DB -> IO ()
close db = do
    mdb_env_sync_flush (db_env db)
    mdb_env_close (db_env db)
    FL.unlockFile (db_fl db)

-- | Transactional Database API
--
-- The transactions presented here support optimistic concurrency. Any
-- transaction may read, write, stow, or load data. Stowage resources
-- may be published to the database before a full commit, to act as a
-- virtual memory... but that shouldn't be necessary except for long
-- running transactions. Transactions double as ephemeral GC roots for
-- stowed resources.
--
-- Overheads of LMDB write transactions are mitigated by write batching.
-- That is, non-conflicting concurrent writes will be composed into one
-- big LMDB write. In case of conflicts, some transactions will fail. To
-- avoid conflicts, isolate control over contentious keys via queues.
--
data TX = TX 
  { tx_db       :: !DB
    -- read-write and stowage are separated to simplify 
    -- concurrency and ephemeron table management.
  , tx_rw       :: !(MVar TXRW)
  , tx_s        :: !(MVar TXS)
  }

instance Eq TX where (==) = (==) `on` tx_rw

data TXS = TXS 
    { txs_stow :: !(M.Map Hash ByteString) -- pending stowage
    , txs_hold :: !EphTbl                  -- GC rooted hashes
    }
data TXRW = TXRW
    { txrw_reads  :: !(M.Map ByteString ByteString) -- reads (actual or assumed)
    , txrw_writes :: !(M.Map ByteString ByteString) -- writes (local to TX)
    }

txDB :: TX -> DB
txDB = tx_db

-- | Initialize a fresh transaction.
newTX :: DB -> IO TX
newTX db = do
    txs <- newMVar (TXS mempty mempty)
    txrw <- newMVar (TXRW mempty mempty)
    mkWeakMVar txrw (finiTXS db txs)
    return (TX db txrw txs)

-- | Clear ephemeral stowage. 
--
-- Note: if a `DBStow` task is active, we'll try to clear it before
-- our writer thread reaches it.
finiTXS :: DB -> (MVar TXS) -> IO ()
finiTXS db txs = modifyMVar_ txs $ \ s -> do
    dbClearEph db (txs_hold s)
    return (TXS mempty mempty)

-- | Retrieve value associated with given key.
--
-- Reads are not guaranteed to be snapshot consistent, but a transaction
-- will remember a previously read or written value for a key. While no
-- specific attempt is made to preserve secure hashes, a valid transaction
-- will still hold values initially read upon commit so we can assume the
-- resources remain valid.
readKey :: TX -> ByteString -> IO ByteString
readKey tx k = 
    let ks = [k] in
    readKeys tx ks >>= \ vs ->
    case vs of
        [v] -> return v
        _   -> dbError "expecting single value"

-- | Read many keys.
--
-- This performs a batch read on the database, which can easily be
-- optimized. It still doesn't guarantee snapshot consistency, since
-- prior reads or assumed key-values will take precedent. But if it's
-- the first read for every key, we do get snapshot consistency.
readKeys :: TX -> [ByteString] -> IO [ByteString]
readKeys = undefined

-- | Control assumptions about a Key's value.
--
-- Reading a key will add that key's value to our assumptions to be
-- tested upon commit. But in context of long-running transactions,
-- either to continue or repair a transaction, it can be useful to
-- control the assumptions. It's useful for testing, too.
--
-- You may assume a key has Just a specific value, or you may assume
-- Nothing about a key to effectively remove it from the read log.
assumeKeyVal :: TX -> ByteString -> Maybe ByteString -> IO ()
assumeKeyVal tx k v = modifyMVar_ (tx_rw tx) $ \ st ->
    let rd' = M.alter (const v) k (txrw_reads st) in
    return $! st { txrw_reads = rd' }

-- | Write a single key-value pair.
--
-- Note there is no need to create or delete keys. All keys exist
-- implicitly, simply defaulting to the empty bytestring. Writing
-- an empty string is thus equivalent to deleting the key.
writeKeyVal :: TX -> ByteString -> ByteString -> IO ()
writeKeyVal tx k v = undefined

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
loadRsc tx h = undefined

-- | Store a resource, returning the secure hash.
--
-- Stowage resources are automatically named using a secure hash, and
-- are garbage collected, and may be treated as plain old values for 
-- the most part. Structure sharing is also automatic, since equal 
-- binaries have the same secure hash.
--
-- In this case, our stowage may be held within the transaction until
-- we commit or use `pushRsc`.
stowRsc :: TX -> ByteString -> IO Hash
stowRsc tx v = undefined

-- | Push resources from transaction memory into database.
--
-- For long-running computations, stowage doubles as virtual memory.
-- That is, it gives us a way to recover RAM and return to the data
-- later. But this paging is expensive, so do be explicit about it.
--
-- This only pushes newly stowed data. If the transaction has not
-- produced any, this reduces to a non-operation. The transaction
-- will utilize an ephemeron table to continue holding the resource
-- as a GC root. 
pushRsc :: TX -> IO ()
pushRsc tx = undefined

-- | Commit key-value writes to the database.
--
-- This will return a boolean - true if the transaction succeeds,
-- false if there was a conflict or other problem. When commit 
-- returns successfully, the data will be durable (or at least
-- as far as LMDB can take it).
-- 
commit :: TX -> IO Bool
commit tx = undefined


-- | Test for transaction conflicts.
--
-- The testCommit operation will lists keys whose current values
-- differ from the value read or assumed. An atomic operation.
-- With the model of optimistic concurrency, conflicts due to 
-- writes to these keys should be the only source of conflict.
testCommit :: TX -> IO [ByteString]
testCommit tx = undefined



{-
-- | Read a key and obtain the recent value.
--
-- Wikilon treats every key as having a definition, with the empty
-- bytestring as the default value. If the key has been written by
-- this transaction, you'll get the written value back.
--
-- Note: Read is not guaranteed to be snapshot consistent with other
-- reads. Isolation is verified upon commit. Or use `readMany`.
read :: TX -> ByteString -> IO ByteString
read tx k = readMany tx [k] >>= \ r -> case r of
    [v] -> return v
    _   -> dbError "expecting exactly one result"



-}


-- thoughts:
-- 
-- Do I want to support *hierarchical* transactions? Doing so would
-- allow for lightweight partial rollback, a try-catch behavior. But
-- it doesn't seem necessary. OTOH, if transactions will mostly be
-- operating in IO, then perhaps all TX state should be in IO? 

-- read, write, load, stow will all use TX and operate in IO
--   e.g. read :: TX -> ByteString -> IO ByteString
--
-- Turning this into a monadic operation for Wikilon.CX is left to
-- other modules.




