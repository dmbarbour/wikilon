{-# LANGUAGE OverloadedStrings #-}

-- | Wikilon is a persistent web server implemented above Warp and
-- Acid-State, providing a wiki-based development environment and
-- live software platform for Awelon Object (AO) code. As a live 
-- platform, developers can implement arbitrary web-applications as
-- words in the AO dictionary and observe immediate results.
--
-- A Wikilon web-server will typically *host* multiple wikis.
--
-- When first initialized, a Wikilon instance will create an admin
-- wiki and print a web-key URL to stdout. The Wikilon instance is
-- then configured through a browser, generally by installing more 
-- wikis (perhaps leveraging an external dictionary). The admin has
-- special influence on the toplevel namespace, permissions, quotas,
-- and so on.
-- 
-- Each Wiki provides a few default functions and web-applications.
-- But, in the tradition of emacs and Smalltalk, the vast majority
-- of a Wiki's behavior can be overridden.
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
    { waiApp :: Wai.Application
    , webKey :: String
    }


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
loadInstance :: Sys.FilePath -> IO WikilonApp
loadInstance _fp = return (WikilonApp helloApp "much to do...")

helloApp :: Wai.Application
helloApp req reply = reply response where
    response = Wai.responseLBS status plainText body
    body = txt2bin $ show (Wai.requestHeaders req)
    status = HTTP.status200
    plainText = [(HTTP.hContentType,"text/plain")]
    txt2bin = T.encodeUtf8 . T.pack

