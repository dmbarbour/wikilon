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
-- wrong about that, I can export and import with alternative backends.
--
-- Due to memory mapping, LMDB does offer zero-copy access to data. At
-- the moment, this is only supported for stowage resources.
--
module Wikilon.DB
    ( DB, TX
    , open
    , newTX, txDB, dupTX
    , readKey, readKeyDB
    , readKeys, readKeysDB
    , writeKey, assumeKey
    , loadRsc, loadRscDB
    , withRsc, withRscDB
    , stowRsc
    , clearRsc, clearRsc'
    , commit, commit_async
    , check
    , gcDB, gcDB_async
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
import Control.DeepSeq (force, ($!!))
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
import qualified Data.Map.Strict as M
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

-- Thoughts: It would be convenient to ensure reads are also protected
-- by the ephemeron tables. This would require delay of GC for resources
-- potentially viewed through a reader lock.
--
-- A viable option is to delay new candidates for GC by a few write frames,
-- enough that our highest latency readers still have time to add ephemeral
-- roots, and also to track ephemerons added on every new read.
--
-- Obviously this would raise the cost of reads by a small amount. But this
-- is probably acceptable: the whole premise of Wikilon DB is to push most
-- read-write costs to the stowage layer. In practice, we should only be
-- reading a few roots.
--
-- More importantly, it would give me much of peace-of-mind to know that
-- all stowage references I've read are 'safe' for duration of a transaction,
-- modulo only those referenced from external sources.

-- | Wikilon Database Object
--
-- Wikilon uses a key-value database with a special feature: binaries
-- may reference other binaries using secure hashes (via Awelon.Hash).
-- Further, un-rooted binary resources will be garbage collected. This
-- enables representation of persistent, immutable data structures that
-- may be larger than active memory. Further, these resources support
-- simplified structure sharing, and are friendly in context of durable
-- data or network lookups.
--
-- Wikilon Database uses an optimistic concurrency model. Writes are
-- performed via transactions, which are verified to be serializable
-- upon commit. Concurrent transactions may conflict and be rejected,
-- but progress is guaranteed: at least one transaction must succeed
-- for conflict to exist. Transactions also serve as volatile roots
-- to resist garbage collection of secure hash resources.
--
-- The Wikilon database makes a performance assumption that most data
-- is represented at the secure hash resources (aka stowage) layer.
--
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
  , db_rdlock   :: !(MVar R)

    -- Asynch Write Layer
  , db_signal   :: !(MVar ())              -- work available?
  , db_new      :: !(MVar Stowage)         -- pending stowage
  , db_commit   :: !(MVar [Commit])        -- commit requests
  , db_hold     :: !(MVar RCU)             -- ephemeron table
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
type EphTbl = RCU                           -- ^ prevent GC of resources

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

-- Reference count tracking.
-- 
-- For now, all reference counts are written into a simple map, using
-- only the stowKeyLen fragment of the hash string to resist possible
-- timing attacks. This is far from optimal for allocations, but it is
-- simple to use in Haskell. 
type RCU = M.Map Hash Int                   -- ^ rsc ref counts (shortHash!)

shortHash :: Hash -> Hash
shortHash = BS.take stowKeyLen

mkRCU :: Int -> [Hash] -> RCU 
mkRCU !n = L.foldl' accum mempty where
    accum m h = M.insertWith (+) (shortHash h) n m

-- functions to push work to our writer and signal it.
dbSignal :: DB -> IO ()
dbSignal db = tryPutMVar (db_signal db) () >> return ()

dbPushCommit :: DB -> Commit -> IO ()
dbPushCommit db !task = do
    modifyMVarMasked_ (db_commit db) $ \ lst -> return (task:lst)
    dbSignal db

dbPushStow :: DB -> Stowage -> IO ()
dbPushStow db !s = do
    modifyMVarMasked_ (db_new db) $ \ s0 -> return $! (M.union s0 s)
    dbSignal db

ephDiff :: EphTbl -> EphTbl -> EphTbl
ephDiff = M.differenceWith $ \ l r -> nz (l - r) where
    nz 0 = Nothing
    nz n = Just n

-- release ephemeral stowage references.
dbClearEph :: DB -> EphTbl -> IO ()
dbClearEph db !drop = 
    --traceIO ("TX releasing resources " ++ show (M.keys drop)) >>
    if M.null drop then return () else
    modifyMVarMasked_ (db_hold db) $ \ hold ->
        return $! (ephDiff hold drop)

-- add ephemeral stowage references. 
dbAddEph :: DB -> EphTbl -> IO ()
dbAddEph db !added = 
    if M.null added then return () else
    modifyMVarMasked_ (db_hold db) $ \ hold ->
        return $! (M.unionWith (+) hold added)

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
withReadLock :: DB -> (MDB_txn -> IO a) -> IO a
withReadLock db action = bracket acq rel (action . snd) where
    acq = do
        r <- dbAcqR db -- note: r must be acquired before txn begins
        txn <- mdb_txn_begin (db_env db) Nothing True
        return (r,txn)
    rel (r, txn) = do
        mdb_txn_commit txn
        relR r

-- advance reader frame (separate from waiting)
advanceReadFrame :: DB -> IO R
advanceReadFrame db = 
    newR >>= \ rNew -> 
    modifyMVarMasked (db_rdlock db) $ \ rOld -> return (rNew, rOld)

-- acquire current read-lock, ensured current by holding db_rdlock
-- so the writer cannot advance reader frame while acquiring.
dbAcqR :: DB -> IO R
dbAcqR db = withMVarMasked (db_rdlock db) $ \ r -> acqR r >> return r
    
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
    lock <- tryFileLockE (fp </> "lockfile")
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

            dbRdLock <- newMVar =<< newR -- readers tracking
            dbSignal <- newMVar () -- initial signal to try GC
            dbCommit <- newMVar mempty
            dbNew <- newMVar mempty
            dbHold <- newMVar mempty

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
tryFileLockE :: FilePath -> IO FL.FileLock
tryFileLockE fp =
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
    -- , tx_stow   :: !Stowage -- batched stowage resources 
    , tx_hold   :: !EphTbl  -- rooted stowage resources
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
finiTX (TX db st) = do
    s <- swapMVar st emptyTXS
    dbClearEph db (tx_hold s)

-- | Duplicate a transaction.
-- 
-- Fork will deep-copy a transaction object, including its relationship
-- with ephemeral stowage resources.
dupTX :: TX -> IO TX
dupTX (TX db st) = do
    s <- readMVar st
    st' <- newMVar s
    let tx' = TX db st'
    dbAddEph db (tx_hold s)
    mkWeakMVar st' (finiTX tx')
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
    mkSafe s = LBS.singleton 26 <> LBS.fromStrict (shortHash (hashL s))
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
-- If a key is already known to a TX because it has been read or written,
-- the appropriate value will be returned. Otherwise, we'll look up a
-- recent value from the DB. A weakness of this model is a lack of snapshot
-- consistency. That is, we do not guarantee all readKeys reference the same
-- database snapshot. But see readKeys.
--
-- The TX does guard secure hash resources discovered through reads against
-- GC even if the value is later overwritten by a concurrent transaction.
--
readKey :: TX -> ByteString -> IO ByteString
readKey (TX db st) (force -> !k) = modifyMVarMasked st $ \ s ->
    case readKeyTXS s k of
        Just v  -> return (s, v)
        Nothing -> withReadLock db $ \ txn -> do
            v <- dbReadKey db txn k
            let ephUpd = mkRCU 1 (hashDeps v) 
            dbAddEph db ephUpd
            let r' = M.insert k v (tx_read s)
            let h' = M.unionWith (+) (tx_hold s) ephUpd
            let s' = s { tx_read = r', tx_hold = h' }
            return (s', v)

-- Read key previously read or written by the transaction. 
readKeyTXS :: TXS -> ByteString -> Maybe ByteString
readKeyTXS s k = M.lookup k (tx_write s) <|> M.lookup k (tx_read s)

-- | Read key directly from database.
--
-- This retrieves the most recently committed value for a key. This is
-- equivalent to readKey with a freshly created transaction.
readKeyDB :: DB -> ByteString -> IO ByteString
readKeyDB db (force -> !key) = 
    withReadLock db $ \ txn -> 
        dbReadKey db txn key

-- obtain a value after we have our transaction
dbReadKey :: DB -> MDB_txn -> ByteString -> IO ByteString
dbReadKey db txn k = withLBSKey (toSafeKey k) (dbReadKeyMDB db txn)

dbReadKeyMDB :: DB -> MDB_txn -> MDB_val -> IO ByteString
dbReadKeyMDB db txn k = do
    let toBS = maybe (return BS.empty) copyMDB_to_BS
    bs <- toBS =<< mdb_get' txn (db_data db) k
    return $! LBS.fromStrict bs

-- | Read values for multiple keys.
--
-- This reads multiple keys with a single LMDB-layer transaction. The 
-- main benefit with readKeys is snapshot isolation for keys initially
-- read together. This is weaker than full snapshot isolation, but can
-- be leveraged to prevent specific problems, especially for smaller
-- transactions.
readKeys :: TX -> [ByteString] -> IO [ByteString]
readKeys (TX db st) (force -> !allKeys) = modifyMVarMasked st $ \ s -> do
    s' <- let newKeys = L.filter (isNothing . readKeyTXS s) allKeys in
          if L.null newKeys then return s else
          withReadLock db $ \ txn -> do
            newVals <- mapM (dbReadKey db txn) newKeys
            let ephUpd = mkRCU 1 (L.concatMap hashDeps newVals)
            dbAddEph db ephUpd
            let r' = M.union (tx_read s) (M.fromList (L.zip newKeys newVals))
            let h' = M.unionWith (+) (tx_hold s) ephUpd
            let s' = s { tx_read = r', tx_hold = h' }
            return $! s'
    let allVals = fmap (fromJust . readKeyTXS s') allKeys
    return (s', allVals)

-- | Read multiple keys directly from database.
--
-- This obtains a snapshot for a few values from the database. This
-- is equivalent to readKeys using a freshly created transaction.
readKeysDB :: DB -> [ByteString] -> IO [ByteString]
readKeysDB db (force -> !keys) =
    if L.null keys then return [] else 
    withReadLock db $ \ txn -> 
        mapM (dbReadKey db txn) keys
  
-- | Write a key-value pair.
--
-- Writes are trivially recorded into the transaction until commit.
-- Our assumption is that individual transactions should be relatively
-- small, modulo stowage resources.
-- 
-- Wikilon database may rewrite some problematic keys under the hood
-- using a secure hash. This may hinder LMDB-layer debugging and hurt
-- performance for the problem keys. To avoid this, favor keys that
-- make short (< 256 bytes), sensible filenames, URLs, Awelon words.
writeKey :: TX -> ByteString -> ByteString -> IO ()
writeKey (TX _ st) (force -> !k) (force -> !v) =
    modifyMVarMasked_ st $ \ s ->
        let w' = M.insert k v (tx_write s) in
        return $! s { tx_write = w' }

-- | Adjust the read assumption for a key.
--
-- This sets or clears the read assumption for a key within a TX.
-- A read assumption will be verified upon a subsequent commit or
-- check, and also determines the value `readKey` will return if
-- the key has not also been written. If cleared (assume Nothing),
-- the key is not checked.
--
-- Explicit assumption is convenient for testing and can reduce the
-- isolation levels or help resolve conflicts for any long-running
-- transactions.
assumeKey :: TX -> ByteString -> Maybe ByteString -> IO ()
assumeKey (TX _ st) (force -> !k) (force -> !mbv) =
    modifyMVarMasked_ st $ \ s ->
        let r' = M.alter (const mbv) k (tx_read s) in
        return $! s { tx_read = r' }

-- | Access a stowed resource by secure hash.
--
-- This searches for a resource identified by secure hash within 
-- the Wikilon database or transaction. If not found, this returns
-- Nothing, in which case you might search elsewhere like the file
-- system or network. (These resources are provider independent.)
--
-- Transactions do not root loaded data. The assumption is that all
-- secure hashes loaded by a transaction were either discovered via
-- readKey or written via stowRsc. If this is not the case, loading
-- may fail due to concurrent GC. See also `clearRsc`.
--
-- Security Note: secure hashes serve as capabilities and must be
-- guarded against leaks such as timing attacks. The Wikilon database
-- promises to leak no more than the first half of a hash via timing
-- attacks, which leaves a sufficient 140 bits for security. But the
-- client should take care to leak no more than the database does. 
loadRsc :: TX -> Hash -> IO (Maybe ByteString)
loadRsc (TX db _) = loadRscDB db
    -- At the moment, we don't store pending stowage in the TX.
    -- But this might change later.

-- | Load resource directly from database.
loadRscDB :: DB -> Hash -> IO (Maybe ByteString)
loadRscDB db h = 
    -- peeking at recently stowed resources first. This should be
    -- a negligible overhead.
    readMVar (db_new db) >>= \ nrsc -> case lookupRsc h nrsc of
        Nothing -> withRscMDB db h (fmap LBS.fromStrict . copyMDB_to_BS)
        Just v -> return (Just v)

withRscMDB :: DB -> Hash -> (MDB_val -> IO a) -> IO (Maybe a)
withRscMDB db !h !action = 
    if (BS.length h /= validHashLen) then return Nothing else
    withBSKey h $ \ hMDB -> 
    withReadLock db $ \ txn -> do 
    let hKey = MDB_val stowKeyLen (mv_data hMDB)
    let hRem = mdbSkip stowKeyLen hMDB
    mbv <- mdb_get' txn (db_stow db) hKey
    case mbv of
        Nothing -> return Nothing
        Just val -> 
            ctMatchPrefix hRem val >>= \ bPrefixOK ->
            if not bPrefixOK then return Nothing else
            let rscData = mdbSkip (validHashLen - stowKeyLen) val in
            Just <$> action rscData

-- | Min-copy access to resource.
--
-- With the LMDB implementation layer, we can avoid a lot of copying
-- when accessing data. Other implementations might similarly use a 
-- local cache.
--
-- This operation may block background processes of the database, such
-- as the writer thread. So it's important to ensure any `withRsc` action
-- is relatively short-lived.
--
-- If the resource isn't available, this returns immediately with Nothing.
--
withRsc :: TX -> Hash -> (ByteString -> IO a) -> IO (Maybe a)
withRsc (TX db _) = withRscDB db

-- | Ephemeral access to resource in DB, see withRsc.
withRscDB :: DB -> Hash -> (ByteString -> IO a) -> IO (Maybe a)
withRscDB db h action =
    readMVar (db_new db) >>= \ mbv -> case lookupRsc h mbv of
        Just v -> Just <$> action v
        Nothing -> withRscMDB db h $ \ mdb ->
            unsafeMDB_to_BS mdb >>= \ bs ->
            action (LBS.fromStrict bs)

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

-- Timing-attack resistant lookup for newly allocated resources.
--
-- I'm not particularly concerned about timing attacks on new resources,
-- since they should be moved to the database quickly enough to resist
-- the attack. But it's also easy to make this resistant.
lookupRsc :: Hash -> Stowage -> Maybe ByteString
lookupRsc h m = 
    case M.lookupGT (shortHash h) m of
        Just (k,v) | ctEqBS h k -> Just v
        _ -> Nothing

-- | constant time equality comparison for bytestrings.
ctEqBS :: BS.ByteString -> BS.ByteString -> Bool
ctEqBS a b = 
    (BS.length a == BS.length b) &&
    (0 == (L.foldl' (.|.) 0 (BS.zipWith xor a b)))

-- | Move resource to database, returns secure hash (Awelon.Hash).
--
-- Stowed resources are moved to the database immediately, returning
-- a secure hash that may later be used to identify and access the 
-- resource. 
--
-- See 'clearRsc'. 
--
stowRsc :: TX -> ByteString -> IO Hash
stowRsc (TX db st) v = modifyMVarMasked st $ \ s -> do
    h <- evaluate (hashL v)
    let ephUpd = mkRCU 1 [h] 
    let hold' = M.unionWith (+) (tx_hold s) ephUpd
    let s' = s { tx_hold = hold' }
    dbAddEph db ephUpd
    dbPushStow db (M.singleton h v)
    return (s', h)

-- Note: I might introduce a later `batchRsc` and `pushRscBatch` to
-- support intermediate stowage within the TX.

-- | Release stale ephemeral resources.
--
-- A TX uses System.Mem.Weak to guard a set of stowage resources from
-- garbage collection. In particular, the following resources will be
-- preserved:
--
-- * resources referenced through readKey(s)
-- * new resources allocated through stowRsc
--
-- We assume hashes written were either discovered through prior reads
-- or allocated via stowRsc, and hence remain rooted by one of the above
-- cases.
--
-- Preserving resources monotonically simplifies reasoning, but it easily
-- overestimates what we really need to keep available. For a short-lived
-- TX, this is a non-issue. But for a long-lived TX, it might become a 
-- concern. In that context, `clearRsc` can help explicitly manage the
-- ephemeron table, clearing anything that is not rooted by the TX keys.
clearRsc :: TX -> IO ()
clearRsc = flip clearRsc' []

-- | As clearRsc, but also accepting a short list of extra roots.
--
-- These roots aren't checked, and may come from another TX.
clearRsc' :: TX -> [Hash] -> IO ()
clearRsc' (TX db st) (force -> !hs) = do
    (h,h') <- modifyMVarMasked st $ \ s -> do
        let roots = M.union (tx_write s) (tx_read s) -- favor writes over reads
        let deps = hs ++ L.concatMap hashDeps (M.elems roots) -- all the hashes
        let h' = mkRCU 1 deps
        let h = tx_hold s
        let s' = s { tx_hold = h' }
        return (s', (h, h'))
    -- order matters: add before removing
    dbAddEph db h'      -- add new ephemerons 
    dbClearEph db h     -- clear old ephemerons

-- | Commit transaction to database. Synchronous.
--
-- A read-only transaction will only verify that the all reads are
-- consistent with the current database (see also, `check`). But a
-- writer transaction may coalesce its updates in a batch with all
-- concurrent writes.
--
-- A transaction may be committed more than once. In that case, each
-- commit essentially checkpoints the transaction, limiting how much
-- will be lost if a future commit fails. If you do use checkpointing
-- transactions, you should also consider use of `clearRsc`.
commit :: TX -> IO Bool
commit tx = commit_async tx >>= id

-- | Asynchronous Commit.
--
-- This immediately submits transaction to the writer for commit, but
-- does not wait for completion. This is most useful in context of a
-- checkpointing transaction. Multiple commits from a single transaction
-- may potentially coalesce into a single write batch.
--  
commit_async :: TX -> IO (IO Bool)
commit_async (TX db st) = modifyMVarMasked st $ \ s ->
    if M.null (tx_write s) 
        -- handle read-only transactions immediately
      then do ok <- verifyReadsDB db (M.toList (tx_read s))
              return (s, return ok)
        -- asynchronous read-write transactions
      else do ret <- newEmptyMVar
              dbPushCommit db ((tx_read s, tx_write s), ret)
              let r' = M.union (tx_write s) (tx_read s)
              let s' = s { tx_read = r', tx_write = mempty }
              return (s', readMVar ret)

verifyReadsDB :: DB -> [(ByteString,ByteString)] -> IO Bool
verifyReadsDB db rd = 
    if L.null rd then return True else
    withReadLock db $ \ txn -> allM (validRead db txn) rd

-- | Force GC of the database.
--
-- At the moment, GC is performed as part of the normal write duties,
-- so this forces GC by committing an faux transaction. Committing an
-- empty transaction doesn't do the same because read-only transactions 
-- are optimized.
gcDB :: DB -> IO ()
gcDB db = gcDB_async db >>= id

-- | asynchronous variant of gcDB
gcDB_async :: DB -> IO (IO ())
gcDB_async db = do
    ret <- newEmptyMVar
    dbPushCommit db ((mempty,mempty),ret)
    return (readMVar ret >>= \ b -> assert b $ return ())

-- | Diagnose a transaction.
--
-- This function returns a list of keys whose transactional values
-- currently in conflict with database values. This check is intended
-- to diagnose contention or concurrency issues, and may be checked
-- after a transaction fails to gain more information. The result is
-- ephemeral, however, subject to change by concurrent writes.
check :: TX -> IO [ByteString]
check (TX db st) =
    readMVar st >>= \ s ->
    withReadLock db $ \ txn -> do
    fmap fst <$> filterM (fmap not . validRead db txn) (M.toList (tx_read s))

-- The database writer thread.
--
-- All writes in Wikilon DB are funneled into this singleton thread,
-- which may write in large batches if work has accumulated during a
-- prior write frame. Write batching helps amortize a lot of latency
-- for non-conflicting concurrent or checkpointing transactions.
--
-- The current implementation leverages LMDB's property of being a
-- memory-mapped database to avoid copying data that is about to
-- be deleted or overwritten (via unsafeMDB_to_BS).
-- 
-- There is also a two frame latency between decref of any resource 
-- and subsequent GC. This ensures we don't eliminate any reference
-- that an active reader (such as readKey) might be observing. The
-- reader may add a reference to db_hold to hold it for longer. This
-- latency is mitigated by potentially running multiple GC frames 
-- while progress is steady.
-- 
-- All new resources are written. Due to db_hold of new stowage, it's
-- almost never the case that we can filter new resources based on a
-- root set, and the attempt adds complications I'd rather avoid. So 
-- it's up to the client to avoid `stowRsc` for volatile data.
--
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
    initLoop = advanceReadFrame db >>= writeLoop mempty

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

        
    writeLoop :: EphTbl -> R -> IO ()
    writeLoop !rHold !r = do
        -- wait for work
        takeMVar (db_signal db) 
        
        -- BEGIN TRANSACTION
        --  Read requests, order does matter here
        txList <- L.reverse <$> swapMVar (db_commit db) [] -- arrival order commits
        stowed <- readMVar (db_new db)                     -- all recent stowage
        hold <- readMVar (db_hold db)                      -- volatile roots
        txn <- mdb_txn_begin (db_env db) Nothing False

        writes <- foldM (joinWrite txn) mempty txList       -- the full write batch
        overwrites <- mapKeysM (peekData db txn) writes     -- all overwritten data
        writeRsc <- filterKeysM (isNewRsc db txn) stowed    -- write the new stowage

        let wRCU = mkRCU 1 (L.concatMap hashDeps (M.elems writes))
        let owRCU = mkRCU (-1) (L.concatMap hashDeps (M.elems overwrites))
        let rscRCU = M.unionWith (+) (mkRCU 0 (M.keys writeRsc)) -- potential new db_zero
                                     (mkRCU 1 (L.concatMap hashDeps (M.elems writeRsc)))
        let rcu0 = M.unionsWith (+) [owRCU, wRCU, rscRCU]

        -- Garbage Collection and Refct Management
        --   cleanup must be proportional to write effort (to keep up)
        --   compute reference counts in volatile memory before writing
        let writeEffort = M.size writes + M.size rcu0
        let qc = 50 + (2 * writeEffort) -- initial GC candidates
        let qgc = 5 * qc                -- soft limit for cascading GC
        let blockDel h = -- not everything may be GC'd.
                let memb = M.member (shortHash h) in
                memb rcu0 || memb hold || memb rHold
        let initRC = mapKeysM (dbGetRefct db txn)
        let gcLoop !gc !rc ngc = 
                let done = (qgc < M.size gc) || (M.null ngc) in
                if done then return (gc, rc) else
                mapM (peekRsc db txn) (M.keys ngc) >>= \ dd -> -- dropped deps
                let rcu = mkRCU (-1) (L.concatMap (maybe [] hashDeps) dd) in
                initRC (M.difference rcu rc) >>= \ nrc -> -- new reference counts
                let rc' = M.unionsWith (+) [(M.difference rc ngc), rcu, nrc] in
                let gc' = M.union gc ngc in
                assert (M.size gc' == (M.size gc + M.size ngc)) $
                let mayGC h = case M.lookup h rc' of
                        Nothing -> False
                        Just ct -> (0 == ct) && not (blockDel h)
                in
                let ngc' = filterKeys mayGC rcu in -- next GC is subset of rcu
                gcLoop gc' rc' ngc'
        
        rc0 <- M.unionWith (+) rcu0 <$> initRC rcu0
        ngc0 <- mkRCU 0 <$> dbGCPend db txn (not . blockDel) qc
        (gc,rc) <- gcLoop mempty rc0 ngc0

        assert (M.null (M.intersection gc rc)) $ return () -- sanity check
        when (M.size gc >= qc) $ dbSignal db -- heuristically signal more GC
        -- traceIO ("GC: " ++ show (M.keys gc))

        -- Reads are complete!
        -- 
        -- Read before write ensures we only write each elements once,
        -- and helps isolate the complexity to the purely functional 
        -- code. It also ensures safety for the zero-copy reads without
        -- needing to know about LMDB page recycling within a write. 
        --
        -- The main disadvantage is that it takes a lot of memory to
        -- build the write sets. But that's mitigated by zero-copy.

        -- Write all the Updates to LMDB
        mapM_ (dbDelRscAndRefct db txn) (M.keys gc)             -- delete GC'd resources
        mapM_ (uncurry (dbSetRefct db txn)) (M.toList rc)       -- update other refcts
        mapM_ (uncurry (dbPutRsc db txn)) (M.toList writeRsc)   -- write new resources
        mapM_ (uncurry (dbPutData db txn)) (M.toList writes)    -- write keyed data
        
        -- Commit and Synchronize
        waitR r                             -- wait on readers of the old LMDB frame
        mdb_txn_commit txn                  -- commit write data to the memory map
        r' <- advanceReadFrame db           -- acquire readers from prior LMDB frame
        let rHold' = owRCU                  -- hold decref'd resources one more frame
        mdb_env_sync_flush (db_env db)      -- commit write to disk

        -- report success, release completed stowage, continue
        mapM_ (flip tryPutMVar True . snd) txList
        modifyMVarMasked_ (db_new db) $ \ m -> return $! (M.difference m stowed)
        writeLoop rHold' r' 


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
peekRsc db txn h = withBSKey (shortHash h) $ \ mdbKey ->
    let hashRem = validHashLen - stowKeyLen in
    mdb_get' txn (db_stow db) mdbKey >>= \ mbv -> case mbv of
        Nothing -> return Nothing
        Just v -> unsafeMDB_to_BS (mdbSkip hashRem v) >>= \ bs ->
                  return (Just (LBS.fromStrict bs))

-- scan database for a set of objects to be collected.
-- result is invalid outside the transaction.
dbGCPend :: DB -> MDB_txn -> (Hash -> Bool) -> Int -> IO [Hash]
dbGCPend db txn accept quota = alloca $ \ pHash -> do
    crs <- mdb_cursor_open' txn (db_zero db)
    let loop !b !n !r =
            if ((not b) || (0 == n)) then return r else
            peek pHash >>= \ hMDB ->
            unsafeMDB_to_BS hMDB >>= \ h ->
            mdb_cursor_get' MDB_NEXT crs pHash nullPtr >>= \ b' ->
            assert (mv_size hMDB == stowKeyLen) $
            if accept h then loop b' (n - 1) (h : r)
                        else loop b' n r
    b0 <- mdb_cursor_get' MDB_FIRST crs pHash nullPtr
    lst <- loop b0 quota []
    mdb_cursor_close' crs
    return lst

-- test whether a resource is new to the LMDB layer
isNewRsc :: DB -> MDB_txn -> Hash -> IO Bool
isNewRsc db txn h = withBSKey (shortHash h) $ \ mdbKey ->
    isNothing <$> mdb_get' txn (db_stow db) mdbKey

-- skip the first n bytes of an MDB_val
mdbSkip :: Int -> MDB_val -> MDB_val
mdbSkip n (MDB_val sz p) = 
    assert (sz >= fromIntegral n) $
    MDB_val (sz - fromIntegral n) (p `plusPtr` n)

filterKeys :: (k -> Bool) -> M.Map k a -> M.Map k a
filterKeys fn = M.filterWithKey $ \ k _ -> fn k

filterKeysM :: (Applicative m) => (k -> m Bool) -> M.Map k a -> m (M.Map k a)
filterKeysM op = M.traverseMaybeWithKey $ \ k v -> sel (Just v) Nothing <$> op k
    where sel t f b = if b then t else f

mapKeysM :: (Applicative m) => (k -> m b) -> M.Map k a -> m (M.Map k b)
mapKeysM op = M.traverseWithKey $ \ k _ -> op k

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
        




