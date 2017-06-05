{-# LANGUAGE BangPatterns #-}
-- | Wikilon LMDB Layer
--
-- LMDB is an embedded key-value database with transaction support.
-- All the data is maintained in a single file, memory mapped with
-- caching only at the OS layer. LMDB is read-optimized, but writes
-- also have pretty good performance.
--
-- Wikilon uses the normal key-value lookup for rooted data, but it
-- also leverages a notion of 'stowage' where binaries are referenced
-- by secure hash. Stowage requires GC, which introduces some unusual
-- challenges. Wikilon favors conservative reference counting GC for
-- this case: it's acyclic and relatively precise for secure hashes.
--
-- Wikilon will also utilize memoized cached computations. But I plan
-- at the moment to represent this within the key-value and stowage
-- system.
--
module Wikilon.DB
    ( DB
    , FilePath
    , open, sync, close
    , module Awelon.Hash
    ) where

import Control.Monad
import Control.Exception
import Control.Concurrent
import qualified System.IO (FilePath)
import qualified System.IO.Error as E
import qualified System.Directory as FS 
import System.FilePath ((</>))
import qualified System.FileLock as FL 
import Data.IORef
import qualified Data.Map as M
import Database.LMDB.Raw
import Awelon.Hash

-- | Database Resource
--
-- The database supports both key-value binaries and stowage via
-- secure hash references. Most of the logic is related to GC of
-- stowage, performed incrementally per write transaction.
data DB = DB 
  { db_fp       :: !FilePath -- location in filesystem
  , db_fl       :: !FL.FileLock -- resist multi-process access

  , db_env      :: !MDB_env  
  , db_data     :: {-# UNPACK #-} !MDB_dbi' -- key -> value roots
  , db_stow     :: {-# UNPACK #-} !MDB_dbi' -- secureHash -> binary
  , db_rfct     :: {-# UNPACK #-} !MDB_dbi' -- secureHash -> positive count ([1-9][0-9]*)
  , db_zero     :: {-# UNPACK #-} !MDB_dbi' -- secureHash set with rfct=0

  , db_eph      :: !(IORef (M.Map Hash Int))
  } 

-- Note: I might need to introduce an ephemeron table to hold onto
-- unrooted stowage references. However, it might be better to avoid
-- this feature if feasible. It isn't very compatible with RESTful
-- architectures, and it's difficult to scale beyond one process or
-- one machine. But simple key-value storage and stowage can scale.

instance Show DB where
    showsPrec _ db = showString "DB@" . showString (db_fp db)

-- | Open or Create the Database. 
--
-- The argument is simply a directory where we expect to open the
-- database, and a maximum database size.
open :: FilePath -> Int -> IO (Either SomeException DB)
open fp nMaxSize = runInBoundThread $ try $ do
    FS.createDirectoryIfMissing True fp
    lock <- tryLockE (fp </> "lockfile")
    env <- mdb_env_create
    mdb_env_set_mapsize env nMaxSize
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

        eph <- newIORef mempty
        return $! DB 
            { db_fp = fp
            , db_fl = lock
            , db_env = env
            , db_data = dbData
            , db_stow = dbStow
            , db_rfct = dbRfct
            , db_zero = dbZero
            , db_eph = eph
            }

-- try lock with a simple IOError
tryLockE :: FilePath -> IO FL.FileLock
tryLockE fp =
    FL.tryLockFile fp FL.Exclusive >>= \ mbLocked ->
    case mbLocked of
        Just fl -> return fl
        Nothing -> E.ioError $ E.mkIOError 
            E.alreadyInUseErrorType "exclusive file lock failed" 
            Nothing (Just fp)

-- | Ensure recent writes are fully persistent.
sync :: DB -> IO ()
sync = mdb_env_sync_flush . db_env

-- | Close the Database. It's up to the client to not use it after. 
close :: DB -> IO ()
close db = do
    sync db
    mdb_env_close (db_env db)
    FL.unlockFile (db_fl db)

-- | Transactional Database API
--
-- The transactions presented here support optimistic concurrency. Any
-- transaction may read or write or stow data. But we don't hold onto
-- an LMDB transaction long term, instead we record our expectations in
-- the transaction and verify upon commit. In case of stowage, we may
-- also write stowed values before commit if they consume a significant
-- amount of working memory (to support the virtual memory role). 
-- 
-- Optimistic concurrency does risk rework upon conflict, but clients
-- may avoid conflict by partitioning responsibility for keys between
-- separate threads.
-- 
-- Write batching is a feasible extension, that is we could commit many
-- concurrent transactions using a single LMDB-layer transaction. Doing
-- so would mitigate LMDB's limitation of a single writer.
--
-- NOTE: This transaction API assumes that most data is in stowage, that
-- our root key-value data is relatively small, perhaps a few dozen kB.
-- Stowage doesn't need transactional verification, since it's immutable.
data TX = TX !DB 

-- read, write, load, stow will all use TX and operate in IO
--   e.g. read :: TX -> ByteString -> IO ByteString
--
-- Turning this into a monadic operation for Wikilon.CX is left to
-- other modules.




