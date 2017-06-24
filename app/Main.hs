{-# LANGUAGE ViewPatterns, PatternGuards, OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Monad.Loops (anyM)
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as U8 
import qualified System.IO as Sys
import qualified System.Exit as Sys
import qualified System.Environment as Env
import qualified Data.List as L
import qualified System.EasyFile as FS
import qualified System.Entropy as Sys
import qualified Network.Wai.Handler.Warp as WS
import qualified Network.Wai.Handler.WarpTLS as WS
import qualified Network.Wai as Wai
import Awelon.Hash (hash)
import qualified Wikilon.DB as DB
import qualified Wikilon as Wiki

helpMsg :: String
helpMsg =
  "Expected Usage:\n\
  \\n\
  \    wikilon [-pPORT] [-dbMB] [-admin] \n\
  \     -pPORT listen for HTTP requests on given port (default 3000) \n\
  \     -dbMB  configure maximum database file size (default 4T) \n\
  \     -admin print admin password (valid until process restart)\n\
  \\n\
  \    Environment variables:\n\
  \      WIKILON_HOME: directory for persistent data.\n\
  \        defaults to OS dependent user app directory.\n\
  \\n\
  \    For TLS and HTTPS:\n\
  \      add 'wiki.key' and 'wiki.crt' files to WIKILON_HOME\n\
  \      if TLS is enabled, insecure connections are denied\n\
  \\n\
  \The wikilon web server is primarily accessed and configured through a\n\
  \normal browser. Documentation online. Initial configuration requires\n\
  \an -admin password, but persistent administrators may be configured.\n\
  \"

-- should I allow users to *provide* an admin password? Probably not.
-- It would likely be a dreadful source of insecurity, with passwords
-- ending up within scripts.


-- TLS files
crt, key :: FS.FilePath
crt = "wiki.crt"
key = "wiki.key"

data Args = Args
 { a_port :: Int
 , a_dbsz :: Int
 , a_admin :: Bool
 , a_help :: Bool
 , a_bad  :: [String]
 }
defaultArgs :: Args 
defaultArgs = Args
 { a_port = 3000
 , a_dbsz = 4 * 1000 * 1000
 , a_admin = False
 , a_help = False
 , a_bad  = []
 }

readSizeMul :: String -> Maybe (Int -> Int)
readSizeMul "" = Just id
readSizeMul "K" = Just $ max 1 . (`div` 1000)
readSizeMul "M" = Just id
readSizeMul "G" = Just $ (* (1000))
readSizeMul "T" = Just $ (* (1000 * 1000))
readSizeMul _ = Nothing

readSize :: String -> Maybe Int
readSize s = case reads s of
    [(a,m)] | (a > 0) -> 
        readSizeMul m >>= \ f ->
        return (f a)
    _ -> Nothing

readPort :: String -> Maybe Int
readPort s = case reads s of
    [(a,"")] | (a > 0) -> Just a
    _ -> Nothing

procArgs :: [String] -> Args
procArgs = L.foldr (flip pa) defaultArgs where
    pa a ('-': 'p' : (readPort -> Just p)) = a { a_port = p }
    pa a ('-': 'd' : 'b': (readSize -> Just mb)) = a { a_dbsz = mb }
    pa a "-admin" = a { a_admin = True }
    pa a "-?" = a { a_help = True }
    pa a s = let bad' = s : a_bad a in a { a_bad = bad' }

useWikilonHome :: IO ()
useWikilonHome = do
    e <- Env.getEnvironment
    home <- case L.lookup "WIKILON_HOME" e of
        Just h -> return h
        Nothing -> FS.getAppUserDataDirectory "wikilon"
    FS.createDirectoryIfMissing True home
    FS.setCurrentDirectory home

-- create fresh admin password, as needed
mkAdminPass :: IO BS.ByteString
mkAdminPass = BS.take 24 . hash <$> Sys.getEntropy 48

putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr

-- run Warp using TLS if avaialable, otherwise insecure
runWarp :: Int -> Wai.Application -> IO ()
runWarp port app = do
    bUseTLS <- anyM FS.doesFileExist [crt, key]
    let tlsOpt = WS.tlsSettings crt key
    let warpOpt = WS.setPort port
                $ WS.setTimeout 120 -- allow longer heartbeats
                $ WS.defaultSettings
    if bUseTLS then WS.runTLS tlsOpt warpOpt app
               else WS.runSettings warpOpt app

haltOnError :: SomeException -> IO a
haltOnError e = do
    putErrLn "Wikilon halted with exception:"
    putErrLn (show e)
    putErrLn "Aborting."
    Sys.exitFailure

-- | This essentially just passes the buck to Wikilon.
main :: IO ()
main = body `catch` haltOnError where
    getArgs = procArgs <$> Env.getArgs
    body = 
        getArgs >>= \ args ->
        if a_help args then Sys.putStrLn helpMsg else
        let badArgs = not (L.null (a_bad args)) in
        if badArgs then failWithBadArgs args else do
        admin <- if not (a_admin args) then return Nothing else do
                 pass <- mkAdminPass
                 Sys.putStrLn ("admin:" ++ U8.toString pass)
                 return (Just pass)
        runServer args admin
    runServer args admin = do
        useWikilonHome
        let wikiOpts = Wiki.setAdmin admin
                     $ Wiki.defaultOpts
        db <- DB.open "db" (a_dbsz args)
        app <- Wiki.mkWaiApp wikiOpts db
        runWarp (a_port args) app
    failWithBadArgs args = do
        putErrLn $ "Unrecognized arguments (try `-?` for help): "
        putErrLn $ show (a_bad args)
        Sys.exitFailure
        
    


