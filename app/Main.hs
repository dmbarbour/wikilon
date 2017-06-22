{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Monad.Loops (anyM)
import Control.Exception
import qualified System.IO as Sys
import qualified System.Exit as Sys
import qualified System.Environment as Env
import qualified Data.List as L
import qualified System.EasyFile as FS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import qualified Network.Wai as Wai
import qualified Web.Scotty as Scotty
import qualified Wikilon.DB as DB
import qualified Wikilon

helpMsg :: String
helpMsg =
  "Expected Usage:\n\
  \\n\
  \    wikilon [-pPORT] [-dbMB] \n\
  \     -pPORT listen for HTTP requests on given port (default 3000) \n\
  \     -dbMB  configure maximum database file size (default 40TB) \n\
  \\n\
  \    Environment variables:\n\
  \      WIKILON_HOME: directory for persistent data.\n\
  \        defaults to OS dependent user app directory.\n\
  \\n\
  \    For TLS and HTTPS:\n\
  \      add 'wiki.key' and 'wiki.crt' files to WIKILON_HOME\n\
  \      if TLS is enabled, insecure connections are denied\n\
  \\n\
  \Once initialized, the wikilon web server can be accessed through a\n\
  \normal browser, and documentation will be provided online.\n\
  \"

-- TLS files
crt, key :: FS.FilePath
crt h = "wiki.crt"
key h = "wiki.key"

data Args = Args
 { args_port :: Int
 , args_dbsz :: Int
 , args_help :: Bool
 , args_bad  :: [String]
 }
defaultArgs :: Args 
defaultArgs = Args
 { args_port = 3000
 , args_dbsz = 40 * 1000 * 1000
 , args_help = False
 , args_bad  = []
 }

tryRead :: (Read a) => String -> Maybe a
tryRead s = case reads s of
    [(a,"")] -> Just a
    _ -> Nothing

procArgs :: [String] -> Args
procArgs = L.foldr (flip pa) defaultArgs where
    pa a "-?" = a { args_help = True }
    pa a ('-': 'p' : (tryRead -> Just p)) = a { args_port = p }
    pa a ('-': 'd' : 'b': (tryRead -> Just mb)) = a { args_dbsz = mb }
    pa a s = let bad' = s : args_bad a in a { args_bad = bad' }

useWikilonHome :: IO ()
useWikilonHome = do
    e <- Env.getEnvironment
    home <- case L.lookup "WIKILON_HOME" e of
        Just h -> return h
        Nothing -> FS.getAppUserDataDirectory "wikilon"
    FS.createDirectoryIfMissing True home
    FS.setCurrentDirectory home

putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr

-- run Warp using TLS if avaialable, otherwise insecure
runWarp :: Int -> Wai.Application -> IO ()
runWarp port app =
    bUseTLS <- anyM FS.doesFileExist [crt, key]
    let tlsOpts = Warp.tlsSettings crt key
    let warpOpts = Warp.setPort port
                 $ Warp.setTimeout 600
                 $ Warp.defaultSettings
    if bUseTLS then Warp.runTLS tlsOpts warpOpts app
               else Warp.runSettings warpOpts app

-- defaulting to a hello world app for now.
mkDummyApp :: IO Wai.Application
mkDummyApp = Scotty.scottyApp $ do
    Scotty.get "/:word" $ do
        name <- Scotty.param "word"
        Scotty.html $ mconcat ["<h1>Hello, ", name, "!</h1>"]

runWikilon :: IO ()
runWikilon = body `catches` handlers where
    handlers = [Handler onError]
    onError :: SomeException -> IO ()
    onError e = do
        putErrLn "Wikilon halted with exception:"
        putErrLn (show e)
        putErrLn "Aborting."
        Sys.exitFailure
    body = 
        fmap procArgs Env.getArgs >>= \ args ->
        if args_help args then Sys.putStrLn helpMsg else
        if not (L.null (args_bad args)) then failWithBadArgs args else
        useWikilonHome >>
        DB.open "db" (args_dbsz args) >>= \ db ->
        mkWikilonApp db >>= \ app ->
        runWarp (args_port args) app
        

        createAppDir >>= \ home ->
        mkDummyApp >>= \ app ->
        serve home app
        
    failWithBadArgs args = do
        putErrLn $ "Unrecognized arguments (try `-?` for help): "
        mapM_ (putErrLn . ((++) "  ")) (args_bad args)
        Sys.exitFailure
        

main :: IO ()
main = runWikilon


Env.getArgs >>= runWikilonInstance . procArgs 
    


