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
    ( defaultMain
    ) where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified System.IO as Sys
import qualified Network.HTTP.Types as HTTP

port :: Warp.Port
port = 3000

defaultMain :: IO ()
defaultMain = 
    Sys.putStrLn ("wikilon listening on port: " ++ show port) >>
    -- TODO: emit administrative capability URL for recovery.
    Warp.run port app

app :: Wai.Application
app _request reply = hurrah >> reply response where
    hurrah = Sys.putStrLn ("yay! someone's talking to me!")
    response = Wai.responseLBS status plainText "Hello, Wikilon!"
    status = HTTP.status200
    plainText = [(HTTP.hContentType,"text/plain")]

-- next step: add some silly persistent state! figure out how to configure AcidState.
--   I may eventually need to handle a few arguments, such as port and home.



