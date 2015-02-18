{-# LANGUAGE ViewPatterns, PatternGuards, OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Applicative
import Control.Concurrent (threadDelay)
import qualified System.IO as Sys
import qualified System.Exit as Sys
import qualified System.Environment as Env
import qualified System.EasyFile as FS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import qualified Network.HTTP.Types.Status as HTTP
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as UTF8
import Database.VCache 
import Database.VCache.Cache
import qualified Wikilon

helpMessage :: String
helpMessage =
 "The `wikilon` executable will initiate a Wikilon web service.\n\
 \\n\
 \    wikilon [-pPort] [-cMB] [-s] [-init] \n\
 \      -pPort: listen on given port (def 3000; saved) \n\
 \      -cMB: configure parse-cache size in megabytes; (def 10; saved) \n\
 \      -init: initialize & greet only; do not start. \n\
 \      -s: silent mode; do not greet or display admin code. \n\
 \\n\
 \    Environment Variables:\n\
 \      WIKILON_HOME: directory for persistent data\n\
 \        defaults to OS dependent user app directory\n\
 \\n\
 \    For TLS and HTTPS:\n\
 \      add 'wiki.key' and 'wiki.crt' files to WIKILON_HOME\n\
 \      when TLS is enabled, insecure connections are denied\n\
 \\n\
 \Wikilon is a wiki-inspired software platform and development environment\n\
 \for the Awelon project. Pages are portable, reusable functions. Name and\n\
 \type conventions enable pages to model data, docs, apps, and services.\n\
 \\n\
 \Most configuration of Wikilon is performed through a browser, mostly in\n\
 \terms of editing wiki pages. For security, some pages require an admin\n\
 \code to edit. This code is printed to console with the greeting.\n\
 \"

data Args = Args 
    { _port  :: Maybe Int
    , _cacheSize :: Maybe Int
    , _silent   :: Bool
    , _justInit :: Bool
    , _help     :: Bool
    , _badArgs  :: [String]
    }
defaultArgs :: Args
defaultArgs = Args 
    { _port = Nothing
    , _cacheSize = Nothing
    , _silent = False
    , _justInit = False
    , _help = False
    , _badArgs = []
    }

defaultPort :: Int
defaultPort = 3000

defaultCacheSize :: Int
defaultCacheSize = 10 {- megabytes -}

wiki_crt,wiki_key :: FS.FilePath
wiki_crt = "wiki.crt"
wiki_key = "wiki.key"

main :: IO ()
main = (procArgs <$> Env.getArgs) >>= runWikilonInstance

procArgs :: [String] -> Args
procArgs = L.foldr (flip pa) defaultArgs where
    pa a (helpArg -> True) = a { _help = True }
    pa a (portArg -> Just n) = a { _port = Just n }
    pa a (cacheArg -> Just n) = a { _cacheSize = Just n }
    pa a "-s" = a { _silent = True }
    pa a "-init" = a { _justInit = True }
    pa a v = 
        let badArgs' = v : _badArgs a in
        a { _badArgs = badArgs' }

wikilonWarpSettings :: Int -> Warp.Settings
wikilonWarpSettings port =
    Warp.setPort (fromIntegral port) $
    -- Warp.setHost "*" $
    -- Warp.setTimeout 300 $
    Warp.defaultSettings

wikilonWarpTLSSettings :: FS.FilePath -> WarpTLS.TLSSettings
wikilonWarpTLSSettings h = WarpTLS.tlsSettings crt key where
    crt = h FS.</> wiki_crt
    key = h FS.</> wiki_key

shouldUseTLS :: FS.FilePath -> IO Bool
shouldUseTLS h = (||) <$> haveKF <*> haveCF where
    haveKF = FS.doesFileExist $ h FS.</> wiki_key
    haveCF = FS.doesFileExist $ h FS.</> wiki_crt

runWikilonInstance :: Args -> IO ()
runWikilonInstance a = mainBody where
    mainBody = 
        if hasBadArgs then failWithBadArgs else
        if askedForHelp then helpAndExit else
        createWIKILON_HOME >>= \ home ->
        openVCache szMax (home FS.</> "db") >>= \ vc ->
        loadCache vc (_cacheSize a) >>= \ cacheSize ->
        loadPort vc (_port a) >>= \ port ->
        setVRefsCacheSize (vcache_space vc) (cacheSize * 1000 * 1000) >>
        let appArgs = Wikilon.Args { Wikilon.store = vcacheSubdir "wiki/" vc } in
        Wikilon.loadInstance appArgs >>= \ app ->
        shouldUseTLS home >>= \ bUseTLS ->
        unless (_silent a) (greet home port cacheSize app bUseTLS) >>
        if _justInit a then sync vc >> return () else
        let tlsSettings = wikilonWarpTLSSettings home in
        let run = if bUseTLS then WarpTLS.runTLS tlsSettings warpSettings
                             else Warp.runSettings warpSettings 
        in
        run (Wikilon.waiApp app) >> 
        Sys.putStrLn "Halting." >>
        sync vc
    badArgs = _badArgs a
    hasBadArgs = not $ L.null badArgs
    failWithBadArgs = err badArgMsg >> err helpMessage >> Sys.exitFailure
    badArgMsg = "unrecognized arguments: " ++ show badArgs 
    askedForHelp = _help a
    helpAndExit = Sys.putStrLn helpMessage >> Sys.exitSuccess
    greet home port cache app bUseTLS =
        Sys.putStrLn ("Wikilon:") >>
        Sys.putStrLn ("  Home:  " ++ showFP home) >>
        Sys.putStrLn ("  Port:  " ++ show port) >>
        Sys.putStrLn ("  Cache: " ++ show cache ++ " MB") >>
        Sys.putStrLn ("  HTTPS: " ++ show bUseTLS) >>
        Sys.putStrLn ("  Admin: " ++ Wikilon.adminCode app)

showFP :: FS.FilePath -> String
showFP = T.unpack . either id id . FS.toText

loadCache :: VCache -> Maybe Int -> IO Int
loadCache = ldpvar "cacheSize" defaultCacheSize

loadPort :: VCache -> Maybe Int -> IO Int
loadPort = ldpvar "port" defaultPort

loadPVar :: (VCacheable a) => String -> a -> VCache -> Maybe a -> IO a
loadPVar var vc def mbArg =
    loadRootPVarIO vc (UTF8.fromString var) def >>= \ r ->
    case mbArg of
        Nothing -> readPVarIO r
        Just a -> runVTx (pvar_space r) $
            writePVar r a >>
            return a

err :: String -> IO ()
err = Sys.hPutStrLn Sys.stderr

portArg :: String -> Maybe Int
portArg ('-':'p': s) = tryRead s
portArg _ = Nothing

cacheArg :: String -> Maybe Int
cacheArg ('-':'m':s) = tryRead s
cacheArg _ = Nothing

tryRead :: (Read a) => String -> Maybe a
tryRead (reads -> [(a,[])]) = Just a
tryRead _ = Nothing

helpArg :: String -> Bool
helpArg "?" = True
helpArg "help" = True
helpArg ('-':s) = helpArg s
helpArg _ = False

-- use WIKILON_HOME or OS based app directory.
getWIKILON_HOME :: IO FS.FilePath
getWIKILON_HOME =
    Env.getEnvironment >>= \ env ->
    case L.lookup "WIKILON_HOME" env of
        Just h -> return h
        Nothing -> FS.getAppUserDataDirectory "wikilon"

-- getWIKILON_HOME then create directory if needed
createWIKILON_HOME :: IO FS.FilePath
createWIKILON_HOME =
    getWIKILON_HOME >>= \ h0 ->
    FS.createDirectoryIfMissing True h0 >>
    FS.canonicalizePath h0

