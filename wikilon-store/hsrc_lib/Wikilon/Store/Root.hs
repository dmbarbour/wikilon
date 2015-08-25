{-# LANGUAGE OverloadedStrings, Rank2Types #-}
-- | The root data type for the Wikilon state, based on a set of root
-- PVars in VCache. Wikilon can be extended with new features by adding
-- new roots. 
module Wikilon.Store.Root
    ( WikilonStore(..)
    , loadWikilonStore
    , adminCode, isAdminCode
    , runModelTransaction
    ) where

import Control.Exception (SomeException, Exception)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Database.VCache
import Data.VCache.LoB (LoB)
import qualified Data.VCache.LoB as LoB
import Wikilon.Model
import Wikilon.Store.Secret
import Wikilon.Store.Branch (BranchSet)
import Wikilon.SecureHash
import Wikilon.Store.Time (T, getTime)
import qualified Wikilon.Store.Branch as Br
import qualified Awelon.Base16 as B16

-- | holistic Wikilon state.
data WikilonStore = WikilonStore
    { wikilon_store     :: !VCache      -- ^ persistence layer features
    , wikilon_home      :: !FilePath    -- ^ e.g. in case I later want GHC-layer plugins
    , wikilon_dicts     :: !(PVar BranchSet) -- ^ dictionary contents (no security, etc.)
    , wikilon_uniqueSrc :: !(PVar Integer)   -- ^ predictable source of unique values
    , wikilon_loadCount :: !Integer     -- ^ how many times has Wikilon been loaded?
    , wikilon_secret    :: !Secret      -- ^ a secret value for administration
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
    return $! WikilonStore
        { wikilon_home = _home
        , wikilon_store = vc
        , wikilon_dicts = _dicts
        , wikilon_uniqueSrc = _uniqueSrc
        , wikilon_loadCount = _loadCount
        , wikilon_secret = _secret
        }

{-
-- | Access the error log. I'm not going to bother keeping this one
-- in active memory at the moment. 
_wikilon_errlog :: WikilonStore -> PVar (LoB (T,UTF8.ByteString))
_wikilon_errlog w = loadRootPVar (wikilon_store w) "errlog" (LoB.empty vc 16) where
    vc = vcache_space $ wikilon_store w

-}

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

runModelTransaction :: WikilonStore -> (forall m . W m a) -> IO a
runModelTransaction ws actions = runVTx vc $ runTransaction ws actions where
    vc = vcache_space $ wikilon_store ws

runTransaction :: WikilonStore -> W WikilonStore a -> VTx a 
runTransaction _ _ = fail "TODO! runActions"


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
