{-# LANGUAGE OverloadedStrings #-}

-- | Wikilon is a persistent web server implemented above Warp and
-- Acid-State, aiming to provide a live programming environment for
-- Awelon Object code.
--
-- A Wikilon server will provide a few built-in applications and
-- features, but much of the interesting web service logic will be
-- provided through the AO dictionary.
-- 
module Wikilon
    ( loadInstance
    ) where

import qualified Network.Wai as Wai
import qualified System.IO as Sys
import qualified Network.HTTP.Types as HTTP
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

-- | Prepare a new instance of Wikilon, with a given directory for
-- persistence and identity. It is possible to run more than one
-- wikilon instance, so long as each has a different directory.
--
-- The application is not actually started at this point, but any
-- initialization will have been performed.
--
-- Note: In addition to the WAI app, I should probably return some
-- variety of 'master' capability URL for the initial transaction.
--  
loadInstance :: Sys.FilePath -> IO Wai.Application
loadInstance _fp = return helloApp

helloApp :: Wai.Application
helloApp req reply = reply response where
    response = Wai.responseLBS status plainText body
    body = txt2bin $ show (Wai.requestHeaders req)
    status = HTTP.status200
    plainText = [(HTTP.hContentType,"text/plain")]
    txt2bin = T.encodeUtf8 . T.pack

