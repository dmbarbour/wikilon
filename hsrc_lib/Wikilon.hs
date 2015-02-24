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
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString as B
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
    sv <- newSecret -- volatile secret
    sp <- readPVarIO =<< loadRootPVarIO vc "secret" =<< newSecret -- persistent secret
    let adc = L.take 32 $ sigString $ hmac sv "adminCode" -- volatile admin code
    return (App helloApp adc)

sigString :: Signature -> String
sigString = fmap (chr . fromIntegral) . B16.encode . B.unpack . sigBytes

helloApp :: Wai.Application
helloApp req reply = reply response where
    response = Wai.responseLBS status plainText body
    body = txt2bin $ show (Wai.requestHeaders req)
    status = HTTP.status200
    plainText = [(HTTP.hContentType,"text/plain")]
    txt2bin = T.encodeUtf8 . T.pack

