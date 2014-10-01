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
import qualified Data.Acid as DB
import qualified Data.SafeCopy as DB


loadInstance :: Sys.FilePath -> IO Wai.Application
loadInstance _fp = return helloApp

helloApp :: Wai.Application
helloApp _request reply = action where
    action = hurrah >> reply response 
    hurrah = Sys.putStrLn ("yay! someone's talking to me!")
    response = Wai.responseLBS status plainText "Hello, Wikilon!"
    status = HTTP.status200
    plainText = [(HTTP.hContentType,"text/plain")]


-- next step: add some silly persistent state! figure out how to configure AcidState.
--   I may eventually need to handle a few arguments, such as port and home.



