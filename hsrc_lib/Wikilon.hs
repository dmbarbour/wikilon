{-# LANGUAGE OverloadedStrings #-}

-- | Wikilon is a persistent web server implemented above Warp and
-- Acid-State, providing a wiki-based development environment and
-- live software platform for Awelon Object (AO) code. Developers
-- can create flexible web applications and long-running services by
-- defining words appropriately.
--
-- For now, each Wikilon instance will host just one Wiki. But long
-- term, I like the idea of Wikis themselves as distributed objects,
-- with a many-to-many relationship between web servers and wikis.
-- 
module Wikilon
    ( loadInstance
    , Args(..)
    , App(..)
    ) where

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as UTF8L
import Data.Char
import Database.VCache
import Wikilon.Secret
import Wikilon.SecureHash
import qualified Wikilon.Base16 as B16

-- | Currently, Wikilon doesn't take many arguments. 
-- Just a place to persist its data.
data Args = Args
    { store :: !VCache
    }

-- | A web app and a volatile administrative code. 
data App = App 
    { waiApp    :: Wai.Application
    , adminCode :: String
    }

-- | Load an existing Wikilon instance, or initialize a new one, 
-- whose identity and persistence is associated with the given 
-- directory. 
loadInstance :: Args -> IO App
loadInstance args = do
    let vc = store args
    nCt <- incPVar =<< loadRootPVarIO vc "loadCount" 0 -- 
    sv <- readPVarIO =<< loadRootPVarIO vc "secret" =<< newSecret -- persistent secret
    let adcText = UTF8.fromString $ "adminCode " ++ show nCt 
    let adc = L.take 32 $ sigString $ hmac sv adcText -- volatile admin code
    return (App helloApp adc)

incPVar :: PVar Int -> IO Int
incPVar v = runVTx (pvar_space v) $ do
    n <- readPVar v
    writePVar v (n+1)
    return n


sigString :: Signature -> String
sigString = fmap (chr . fromIntegral) . B16.encode . B.unpack . sigBytes

helloApp :: Wai.Application
helloApp req reply = reply response where
    response = Wai.responseLBS status plainText body
    body = txt2bin $ show (Wai.requestHeaders req)
    status = HTTP.status200
    plainText = [(HTTP.hContentType,"text/plain")]
    txt2bin = UTF8L.fromString

