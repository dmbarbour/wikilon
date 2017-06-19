{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module Main (main) where

import Control.Monad
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
  \      when TLS is enabled, insecure connections are disabled\n\
  \\n\
  \Once initialized, the wikilon web server can be accessed through a\n\
  \normal browser, and documentation will be provided online.\n\
  \"

data Args = Args
 { args_port :: Int
 , args_db   :: Int
 , args_help :: Bool
 , args_bad  :: [String]
 }
defaultArgs :: Args 
defaultArgs = Args
 { args_port = 3000
 , args_db   = 40 * 1000 * 1000
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
    pa a ('-': 'd' : 'b': (tryRead -> Just mb)) = a { args_db = mb }
    pa a s = let bad' = s : args_bad a in a { args_bad = bad' }

wiki_crt, wiki_key :: FS.FilePath -> FS.FilePath
wiki_crt h = h FS.</> "wiki.crt"
wiki_key h = h FS.</> "wiki.key"

putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr

-- defaulting to a hello world app for now.
-- 
mkDummyApp :: IO Wai.Application
mkDummyApp = Scotty.scottyApp $ do
    Scotty.get "/:word" $ do
        name <- Scotty.param "word"
        Scotty.html $ mconcat ["<h1>Hello, ", name, "!</h1>"]

getAppDir =
    Env.getEnvironment >>= \ env ->
    case L.lookup "WIKILON_HOME" env of 
        Just h -> return h
        Nothing -> FS.getAppUserDataDirectory "wikilon"

createAppDir =
    getAppDir >>= \ fp ->
    FS.createDirectoryIfMissing True fp >>
    FS.canonicalizePath fp


runWikilonInstance :: Args -> IO ()
runWikilonInstance args = body `catches` handlers where
    handlers = [Handler onError]
    onError :: SomeException -> IO ()
    onError e = do
        putErrLn "Wikilon halted with exception:"
        putErrLn (show e)
        putErrLn "Aborting."
        Sys.exitFailure

    shouldUseTLS home = do
        hasKey <- FS.doesFileExist $ wiki_key home
        hasCRT <- FS.doesFileExist $ wiki_crt home
        return (hasKey || hasCRT)
    
    serve home app = do
        hasKey <- FS.doesFileExist $ wiki_key home
        hasCRT <- FS.doesFileExist $ wiki_crt home
        let bUseTLS = (hasKey || hasCRT)
        let tlsOpts = WarpTLS.tlsSettings (wiki_crt home) (wiki_key home)
        let warpOpts = Warp.setPort (args_port args) 
                     $ Warp.setTimeout 600 
                     $ Warp.defaultSettings
        let serve = if bUseTLS then WarpTLS.runTLS tlsOpts warpOpts
                               else Warp.runSettings warpOpts
        serve app

    body = 
        if args_help args then Sys.putStrLn helpMsg else
        if not (L.null (args_bad args)) then failWithBadArgs else
        createAppDir >>= \ home ->
        mkDummyApp >>= \ app ->
        serve home app
        
    failWithBadArgs = do
        putErrLn $ "Unrecognized arguments (try `-?` for help): "
        mapM_ (putErrLn . ((++) "  ")) (args_bad args)
        Sys.exitFailure
        

main :: IO ()
main = Env.getArgs >>= runWikilonInstance . procArgs 
    


