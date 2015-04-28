{-# LANGUAGE OverloadedStrings #-}
-- | The root data type for the Wikilon state, based on a set of root
-- PVars in VCache. Wikilon can be extended with new features by adding
-- new roots. 
module Wikilon.Root
    ( Wikilon(..)
    , loadWikilon
    , adminCode
    , isAdminCode
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Database.VCache
import Wikilon.Secret
import Wikilon.SecureHash
import Wikilon.Branch (BranchSet)
import qualified Wikilon.Branch as Br
import qualified Awelon.Base16 as B16

-- | holistic Wikilon state.
data Wikilon = Wikilon
    { wikilon_home      :: !FilePath    -- ^ in case I later want plugins?
    , wikilon_vcache    :: !VCache      -- ^ persistence layer features
    , wikilon_dicts     :: !(PVar BranchSet) -- ^ all the dictionaries
    , wikilon_uniqueSrc :: !(PVar Integer)   -- ^ predictable source of unique values
    , wikilon_loadCount :: !Integer     -- ^ how many times has Wikilon been loaded?
    , wikilon_secret    :: !Secret      -- ^ a secret value for administration
    } 

loadWikilon :: FilePath -> VCache -> IO Wikilon
loadWikilon _home _vcache = do
    _loadCount <- incPVar =<< loadRootPVarIO _vcache "loadCount" 0
    _secret <- readPVarIO =<< loadRootPVarIO _vcache "secret" =<< newSecret
    _uniqueSrc <- loadRootPVarIO _vcache "uniqueSrc" 10000000
    _dicts <- loadRootPVarIO _vcache "dicts" (Br.empty (vcache_space _vcache))
    return $! Wikilon
        { wikilon_home = _home
        , wikilon_vcache = _vcache
        , wikilon_dicts = _dicts
        , wikilon_uniqueSrc = _uniqueSrc
        , wikilon_loadCount = _loadCount
        , wikilon_secret = _secret
        }

incPVar :: PVar Integer -> IO Integer
incPVar v = runVTx (pvar_space v) $ do
    n <- readPVar v
    writePVar v $! (n+1)
    return n

-- | a code that allows users to become primary administrators;
-- this works until Wikilon is reset.
adminCode :: Wikilon -> UTF8.ByteString
adminCode w =
    BS.take 32 $ BS.pack $ B16.encode $ BS.unpack $ 
    sigBytes $ hmac (wikilon_secret w) $ UTF8.fromString $ 
    "adminCode " ++ show (wikilon_loadCount w)

-- | test whether a provided administrative code is valid
isAdminCode :: Wikilon -> UTF8.ByteString -> Bool
isAdminCode w s = (Signature (adminCode w)) == (Signature s)
    -- Signature wrapper for constant time comparison

-- TODO:
--
-- evaluation and typechecking
-- cached content (not persistent)
-- users and user model data
-- machines and networks models
--  should users own machines? (or some of them?)
-- logs
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
