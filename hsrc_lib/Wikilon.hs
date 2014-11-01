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
    , WikilonApp(..)
    ) where

import qualified Network.Wai as Wai
import qualified System.IO as Sys
import qualified Network.HTTP.Types as HTTP
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

data WikilonApp = WikilonApp 
    { waiApp    :: Wai.Application
    , adminCode :: String
    }

-- | Load an existing Wikilon instance, or initialize a new one, 
-- whose identity and persistence is associated with the given 
-- directory. 
loadInstance :: Sys.FilePath -> IO WikilonApp
loadInstance fp =
    -- create and initialize a Wiki with the given fp.
    return (WikilonApp helloApp "useless password")


helloApp :: Wai.Application
helloApp req reply = reply response where
    response = Wai.responseLBS status plainText body
    body = txt2bin $ show (Wai.requestHeaders req)
    status = HTTP.status200
    plainText = [(HTTP.hContentType,"text/plain")]
    txt2bin = T.encodeUtf8 . T.pack

