{-# LANGUAGE OverloadedStrings, Rank2Types, TypeFamilies #-}
-- | The root data type for the Wikilon state, based on a set of root
-- PVars in VCache. Wikilon can be extended with new features by adding
-- new roots. 
module Wikilon.Store.Root
    ( WikilonStore(..)
    , loadWikilonStore
    , adminCode, isAdminCode
    , runModelTransaction
    ) where

import Control.Arrow (second)
import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.List as L
import Database.VCache
import Wikilon.Model
import Wikilon.Dict.Object (wrapDictObj, DictObj(..))
import Wikilon.Store.Secret
import Wikilon.Store.Branch (BranchSet)
import qualified Wikilon.Store.Branch as Br
import qualified Wikilon.Store.Dict as D
import Wikilon.SecureHash
import Wikilon.Store.TimeVar
import qualified Awelon.Base16 as B16

-- | holistic Wikilon state.
data WikilonStore = WikilonStore
    { wikilon_store     :: !VCache      -- ^ persistence layer features
    , wikilon_home      :: !FilePath    -- ^ e.g. in case I later want GHC-layer plugins
    , wikilon_dicts     :: !(PVar BranchSet) -- ^ dictionary contents (no security, etc.)
    , wikilon_uniqueSrc :: !(PVar Integer)   -- ^ predictable source of unique values
    , wikilon_loadCount :: !Integer     -- ^ how many times has Wikilon been loaded?
    , wikilon_secret    :: !Secret      -- ^ a secret value for administration
    , wikilon_time      :: !TimeVar     -- ^ current estimated time
    }

-- NEEDED:
--  caches for types, compiles, partial evals, etc.
--  event logs per-dictionary (words updated, renames, etc.)
--  event logs globally
--  event logs for dictionaries and wikilon

-- TODO: 
-- given WikilonStore, provide a ModelRunner.
-- consider exporting logs, stats, issue trackers, etc. as a dictionary

loadWikilonStore :: VCache -> FilePath -> IO WikilonStore
loadWikilonStore vc _home = do
    _loadCount <- incPVar =<< loadRootPVarIO vc "loadCount" 0
    _secret <- readPVarIO =<< loadRootPVarIO vc "secret" =<< newSecret
    _uniqueSrc <- loadRootPVarIO vc "uniqueSrc" 10000000
    _dicts <- loadRootPVarIO vc "dicts" (Br.empty (vcache_space vc))
    _time <- newTimeVar
    return $! WikilonStore
        { wikilon_home = _home
        , wikilon_store = vc
        , wikilon_dicts = _dicts
        , wikilon_uniqueSrc = _uniqueSrc
        , wikilon_loadCount = _loadCount
        , wikilon_secret = _secret
        , wikilon_time = _time
        }

incPVar :: PVar Integer -> IO Integer
incPVar v = runVTx (pvar_space v) $ do
    n <- readPVar v
    writePVar v $! (n+1)
    return n

-- | a code that allows users to temporarily become administrators.
-- This works until Wikilon is restarted.
adminCode :: WikilonStore -> UTF8.ByteString
adminCode w =
    BS.take 32 $ BS.pack $ B16.encode $ BS.unpack $ 
    sigBytes $ hmac (wikilon_secret w) $ UTF8.fromString $ 
    "adminCode " ++ show (wikilon_loadCount w)

-- | test whether a provided administrative code is valid
isAdminCode :: WikilonStore -> UTF8.ByteString -> Bool
isAdminCode w s = (Signature (adminCode w)) == (Signature s)
    -- Signature wrapper for constant time comparison

runModelTransaction :: WikilonStore -> ModelRunner -- (forall m . W m a) -> IO a
runModelTransaction ws tx = runVTx vc $ run ws tx where
    vc = vcache_space $ wikilon_store ws

-- TODO: adjust Branch type to use dedicated PVar.
--  This would improve performance for the current API, and would
--  reduce interference between importing a dictionary on a branch
--  and concurrent operations on other branches.
type instance DictRep WikilonStore = D.Dict
type instance Branch WikilonStore = (PVar BranchSet, BranchName)

_readBranch :: Branch WikilonStore -> VTx Br.Branch
_readBranch (v,n) = Br.lookup' n <$> readPVar v

readWikilonTime :: WikilonStore -> VTx T
readWikilonTime = liftSTM . readTimeVar . wikilon_time

type Hist a = (a, [(T, a)])

-- obtain a history between two given time values.
_sliceHistory :: T -> T -> Hist a -> Hist a
_sliceHistory tA tB = _sliceTail . uncurry _sliceHead where
    tHead = max tA tB
    tTail = min tA tB
    _sliceTail = second (L.takeWhile ((> tTail) . fst))
    _sliceHead _ ((t,d):tl) | (t > tHead) = _sliceHead d tl
    _sliceHead d lst = (d,lst)

run :: WikilonStore -> W WikilonStore a -> VTx a 
run ws (Bind op f) = run ws op >>= run ws . f
run _ (Return x) = return x
run ws (LoadBranch n) = return (wikilon_dicts ws, n)
run _ (BranchHead b) = (wrapDictObj . Br.head) <$> _readBranch b
run _ (BranchModified b) = (maybe minBound id . Br.modified) <$> _readBranch b
run _ (BranchHistory b tA tB) = 
    _readBranch b >>= \ br ->
    let hd = wrapDictObj $ Br.head br in
    let tl = second wrapDictObj <$> Br.hist br in
    return (_sliceHistory tA tB (hd,tl))
run ws (BranchUpdate (v,n) (DictObj d' _)) = 
    readPVar v >>= \ bset ->
    readWikilonTime ws >>= \ tNow ->
    let b' = Br.update (tNow,d') (Br.lookup' n bset) in
    let bset' = Br.insert n b' bset in
    writePVar v bset' >>
    markDurable
run ws ListBranches = Br.keys <$> readPVar (wikilon_dicts ws)
run ws NewEmptyDictionary = return (wrapDictObj d) where
    d = D.empty $ vcache_space $ wikilon_store ws
run ws GetTransactionTime = readWikilonTime ws
run ws (CacheBytes _key action) = run ws action -- todo: implement cache!


-- TODO:
--
-- evaluation and typechecking
-- cached content (not persistent)
-- users and user model data
-- machines and networks models
--  should users own machines? (or some of them?)
-- logs
-- decay for dictionaries (low priority)
--
-- branch security models 
--   e.g. per-branch guest policies
--        per-branch access capabilities
--        revocation of capabilities
--
-- branch subscription models
-- Wikilon's AVM-layer bindings
-- AVM bindings to HTTP services
-- 
