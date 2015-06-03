{-# LANGUAGE ViewPatterns, PatternGuards, BangPatterns, OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Applicative
import Control.Exception
import qualified System.IO as Sys
import qualified System.Exit as Sys
import qualified System.Environment as Env
import qualified System.EasyFile as FS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import qualified Data.List as L
import qualified Data.ByteString.UTF8 as UTF8
import GHC.Conc (getNumProcessors, setNumCapabilities)
import Database.VCache 
import Database.VCache.Cache
import qualified Wikilon.Store.Root as Wikilon
import qualified Wikilon.WAI as Wikilon

helpMessage :: String
helpMessage =
 "The `wikilon` executable will initiate a Wikilon web service.\n\
 \\n\
 \    wikilon [-pPort] [-cMB] [-Ddict] [-dbMB] [-s] [-init] \n\
 \      -pPort: listen on given port (def 3000; saved) \n\
 \      -cMB: configure parse-cache size; (default 10MB; saved) \n\
 \      -init: initialize & greet only; do not start. \n\
 \      -Ddict: set primary dictionary (default 'master'; saved) \n\
 \      -s: silent mode; do not greet or display admin code. \n\
 \      -dbMB: configure maximum database size; (default 100TB; not saved) \n\
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
 \Most configuration of Wikilon is performed through a browser. For security\n\
 \some content requires an administrator to edit. A volatile admin code is\n\
 \printed with the greeting to manage administrative accounts.\n\
 \"

data Args = Args 
    { _port  :: Maybe Int
    , _cacheSize :: Maybe Int
    , _master :: Maybe UTF8.ByteString
    , _dbMax    :: Int
    , _silent   :: Bool
    , _justInit :: Bool
    , _help     :: Bool
    , _badArgs  :: [String]
    }
defaultArgs :: Args
defaultArgs = Args 
    { _port = Nothing
    , _cacheSize = Nothing
    , _master = Nothing
    , _dbMax = defaultMaxDBSize
    , _silent = False
    , _justInit = False
    , _help = False
    , _badArgs = []
    }

defaultPort :: Int
defaultPort = 3000

defaultCacheSize :: Int
defaultCacheSize = 10 {- megabytes -}

defaultMaster :: UTF8.ByteString
defaultMaster = "master"

--
-- 60TB was selected here because it fits into about a quarter of
-- a common 48-bit CPU address space. Assuming half the address 
-- space is reserved for the OS, this would allow enough space
-- to copy and compact the database if we need to. Also, this
-- will usually push the limit to the disk or filesystem.
defaultMaxDBSize :: Int
defaultMaxDBSize = 60 * 1000 * 1000 {- megabytes -}

wiki_crt,wiki_key :: FS.FilePath
wiki_crt = "wiki.crt"
wiki_key = "wiki.key"

main :: IO ()
main = do
    getNumProcessors >>= setNumCapabilities -- use multiple processors
    args <- procArgs <$> Env.getArgs -- command line arguments
    runWikilonInstance args -- doesn't return unless interrupted

procArgs :: [String] -> Args
procArgs = L.foldr (flip pa) defaultArgs where
    pa a (helpArg -> True) = a { _help = True }
    pa a (portArg -> Just n) = a { _port = Just n }
    pa a (masterArg -> Just name) = a { _master = Just name }
    pa a (cacheArg -> Just n) = a { _cacheSize = Just n }
    pa a (dbMaxArg -> Just n) = a { _dbMax = n }
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
        shouldUseTLS home >>= \ bUseTLS ->
        openVCache (_dbMax a) (home FS.</> "db") >>= \ vc ->
        loadCache vc (_cacheSize a) >>= \ cacheSize ->
        setVRefsCacheLimit (vcache_space vc) (cacheSize * 1000 * 1000) >>
        loadPort vc (_port a) >>= \ port ->
        loadMaster vc (_master a) >>= \ master ->
        (`finally` vcacheSync (vcache_space vc)) $ -- sync on normal exit
            let appArgs = Wikilon.Args { Wikilon.args_store = vcacheSubdir "wiki/" vc
                                       , Wikilon.args_home = home FS.</> "wiki"
                                       , Wikilon.args_httpRoot = ""
                                       , Wikilon.args_master = master
                                       }
            in
            Wikilon.loadWikilon appArgs >>= \ wiki ->
            unless (_silent a) (greet home port cacheSize wiki bUseTLS) >>
            if _justInit a then return () else
            let tlsSettings = wikilonWarpTLSSettings home in
            let warpSettings = wikilonWarpSettings port in
            let run = if bUseTLS then WarpTLS.runTLS tlsSettings warpSettings
                                 else Warp.runSettings warpSettings 
            in
            let app = Wikilon.wikilonWaiApp wiki in
            run app
    badArgs = _badArgs a
    hasBadArgs = not $ L.null badArgs
    failWithBadArgs = err badArgMsg >> err helpMessage >> Sys.exitFailure
    badArgMsg = "unrecognized arguments: " ++ show badArgs 
    askedForHelp = _help a
    helpAndExit = Sys.putStrLn helpMessage >> Sys.exitSuccess
    greet home port cache wiki bUseTLS = do
        Sys.putStrLn ("Wikilon:")
        Sys.putStrLn ("  Home:   " ++ showFP home)
        Sys.putStrLn ("  Port:   " ++ show port) 
        Sys.putStrLn ("  Cache:  " ++ show cache ++ " MB")
        Sys.putStrLn ("  HTTPS:  " ++ show bUseTLS)
        Sys.putStrLn ("  Admin:  " ++ UTF8.toString (Wikilon.adminCode wiki))
        Sys.putStrLn ("  Master: " ++ UTF8.toString (Wikilon.wikilon_master wiki)) 

showFP :: FS.FilePath -> String
showFP = id

loadCache :: VCache -> Maybe Int -> IO Int
loadCache = ldPVar "csize" defaultCacheSize

loadPort :: VCache -> Maybe Int -> IO Int
loadPort = ldPVar "port" defaultPort

loadMaster :: VCache -> Maybe UTF8.ByteString -> IO UTF8.ByteString
loadMaster = ldPVar "master" defaultMaster

--
ldPVar :: (VCacheable a) => String -> a -> VCache -> Maybe a -> IO a
ldPVar var def vc mbArg =
    loadRootPVarIO vc (UTF8.fromString var) def >>= \ r ->
    case mbArg of
        Nothing -> readPVarIO r
        Just !a -> runVTx (pvar_space r) $
            writePVar r a >>
            return a

err :: String -> IO ()
err = Sys.hPutStrLn Sys.stderr

portArg :: String -> Maybe Int
portArg ('-':'p': s) = tryRead s
portArg _ = Nothing

cacheArg :: String -> Maybe Int
cacheArg ('-':'c':s) = tryRead s
cacheArg _ = Nothing

masterArg :: String -> Maybe UTF8.ByteString
masterArg ('-':'D':s) = Just $ UTF8.fromString s
masterArg _ = Nothing

dbMaxArg :: String -> Maybe Int
dbMaxArg ('-':'d':'b':s) = tryRead s
dbMaxArg _ = Nothing

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



