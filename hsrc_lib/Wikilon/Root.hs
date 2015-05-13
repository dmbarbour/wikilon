{-# LANGUAGE OverloadedStrings #-}
-- | The root data type for the Wikilon state, based on a set of root
-- PVars in VCache. Wikilon can be extended with new features by adding
-- new roots. 
module Wikilon.Root
    ( Args(..), defaultArgs
    , Wikilon(..)
    , wikilon_errlog
    , logSomeException, logException, logErrorMessage
    , loadWikilon
    , adminCode
    , isAdminCode
    ) where

import Control.Exception (SomeException, Exception)
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Database.VCache
import Data.VCache.LoB (LoB)
import qualified Data.VCache.LoB as LoB
import Wikilon.Secret
import Wikilon.SecureHash
import Wikilon.Branch (BranchSet, BranchName)
import Wikilon.Time (T, getTime)
import qualified Wikilon.Branch as Br
import qualified Awelon.Base16 as B16

-- | Arguments for loading a Wikilon instance.
data Args = Args 
    { args_store :: !VCache
    , args_home  :: !FilePath
    , args_httpRoot :: !ByteString
    , args_master :: !BranchName
    } 

-- | minimally, you must provide a VCache.
defaultArgs :: VCache -> Args
defaultArgs vc = Args
    { args_store = vc
    , args_home = ""
    , args_httpRoot = ""
    , args_master = "master"
    }

-- | holistic Wikilon state.
data Wikilon = Wikilon
    { wikilon_store     :: !VCache      -- ^ persistence layer features
    , wikilon_home      :: !FilePath    -- ^ in case I later want plugins?
    , wikilon_httpRoot  :: !ByteString  -- ^ raw URI root for web services
    , wikilon_master    :: !BranchName  -- ^ master branch controls toplevel pages
    , wikilon_dicts     :: !(PVar BranchSet) -- ^ dictionary contents (no security, etc.)
    , wikilon_uniqueSrc :: !(PVar Integer)   -- ^ predictable source of unique values
    , wikilon_loadCount :: !Integer     -- ^ how many times has Wikilon been loaded?
    , wikilon_secret    :: !Secret      -- ^ a secret value for administration
    } 

loadWikilon :: Args -> IO Wikilon
loadWikilon args = do
    let vc = args_store args
    _loadCount <- incPVar =<< loadRootPVarIO vc "loadCount" 0
    _secret <- readPVarIO =<< loadRootPVarIO vc "secret" =<< newSecret
    _uniqueSrc <- loadRootPVarIO vc "uniqueSrc" 10000000
    _dicts <- loadRootPVarIO vc "dicts" (Br.empty (vcache_space vc))
    return $! Wikilon
        { wikilon_home = args_home args 
        , wikilon_store = args_store args
        , wikilon_httpRoot = wrapSlash $ args_httpRoot args
        , wikilon_master = args_master args
        , wikilon_dicts = _dicts
        , wikilon_uniqueSrc = _uniqueSrc
        , wikilon_loadCount = _loadCount
        , wikilon_secret = _secret
        }

-- | Access the error log. I'm not going to bother keeping this one
-- in active memory at the moment. 
wikilon_errlog :: Wikilon -> PVar (LoB (T,UTF8.ByteString))
wikilon_errlog w = loadRootPVar (wikilon_store w) "errlog" (LoB.empty vc 16) where
    vc = vcache_space $ wikilon_store w

logSomeException :: Wikilon -> SomeException -> IO ()
logSomeException = logException

logException :: (Exception e) => Wikilon -> e -> IO ()
logException w = logErrorMessage w . UTF8.fromString . show

logErrorMessage :: Wikilon -> UTF8.ByteString -> IO ()
logErrorMessage w msg =
    let var = wikilon_errlog w in
    let vc = pvar_space var in
    getTime >>= \ tNow ->
    runVTx vc $ modifyPVar var $ LoB.cons (tNow, msg)

-- initial root will always start and end with '/'. The
-- empty string is modified to just "/". This simplifies
-- construction of 'base' and alternative masters.
wrapSlash :: ByteString -> ByteString
wrapSlash = finiSlash . initSlash

finiSlash :: ByteString -> ByteString
finiSlash s = case BS.unsnoc s of
    Just (_, 47) -> s
    _ -> s <> "/"

initSlash :: ByteString -> ByteString
initSlash s = case BS.uncons s of
    Just (47, _) -> s
    _ -> "/" <> s

incPVar :: PVar Integer -> IO Integer
incPVar v = runVTx (pvar_space v) $ do
    n <- readPVar v
    writePVar v $! (n+1)
    return n

-- | a code that allows users to temporarily become administrators.
-- This works until Wikilon is restarted.
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
