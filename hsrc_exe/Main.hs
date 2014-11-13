{-# LANGUAGE ViewPatterns, PatternGuards, OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Applicative
import Control.Concurrent (threadDelay)
import qualified System.IO as Sys
import qualified System.Exit as Sys
import qualified System.Environment as Env
import qualified System.Random as Sys
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import qualified Network.Wai.Middleware.Gzip as Wai
import qualified Network.HTTP.Types.Status as HTTP
import qualified Data.List as L
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as UTF8
import qualified Wikilon

helpMessage :: String
helpMessage =
 "The `wikilon` executable will initiate a Wikilon web service.\n\
 \\n\
 \    wikilon [-pPort] [-mMB] [-s] [-init] \n\
 \      -pPort: listen on given port (def 3000; saved) \n\
 \      -mMB: Berkeley DB cache size in megabytes (def 5; saved) \n\
 \      -init: initialize & greet, but do not start. \n\
 \      -s: silent mode; do not display greeting or admin URL. \n\
 \\n\
 \    Environment Variables:\n\
 \      WIKILON_HOME: directory for persistent data\n\
 \        default is OS based application data directory\n\
 \\n\
 \    For TLS and HTTPS:\n\
 \      add 'wiki.key' and 'wiki.cert' files to WIKILON_HOME\n\
 \      if TLS is enabled, insecure connections are denied\n\
 \\n\
 \Wikilon is a wiki, a software platform, an integrated development\n\
 \environment for the Awelon project. Pages consist of Awelon Object\n\
 \code, and may define new web applications or services.\n\
 \\n\
 \Most configuration of Wikilon should be performed through a browser.\n\
 \After starting Wikilon, open a browser to http(s)://localhost:Port\n\
 \to get started. An administrative code is printed to console."

data Args = Args 
    { _port  :: Maybe Int
    , _silent   :: Bool
    , _justInit :: Bool
    , _help     :: Bool
    , _badArgs  :: [String]
    }
defaultArgs :: Args
defaultArgs = Args 
    { _port = Nothing
    , _silent = False
    , _justInit = False
    , _help = False
    , _badArgs = []
    }

defaultPort :: Int
defaultPort = 3000

defaultCache :: Int
defaultCache = 5

wiki_crt,wiki_key :: FS.FilePath
wiki_crt = "wiki.crt"
wiki_key = "wiki.key"

main :: IO ()
main = (procArgs <$> Env.getArgs) >>= runWikilonInstance

procArgs :: [String] -> Args
procArgs = L.foldr (flip pa) defaultArgs where
    pa a (helpArg -> True) = a { _help = True }
    pa a (portArg -> Just n) = a { _port = Just n }
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
wikilonWarpTLSSettings h = WarpTLS.tlsSettings (tof wiki_crt) (tof wiki_key) where
    tof = FS.encodeString . (h FS.</>)

wikilonMiddleware :: Wai.Middleware
wikilonMiddleware = ss . gz where
    gz = Wai.gzip Wai.def
    ss = secSleep 30 370

-- | Add a little extra resistance to timing attacks.
--
-- Timing attacks involve statistical analysis of times involved
-- with various operations on secret values. The best way to guard
-- against these is to favor constant time cryptographic behavior.
-- But it can be difficult to find all the vulnerabilities. Adding
-- some variance to responses caused by malformed inputs should
-- help reduce rate of information leaks.
--
-- Times here are given in milliseconds. 
secSleep :: Int -> Int -> Wai.Middleware
secSleep lo hi app request respond = app request respond' where
    respond' r = when (secReject r) (doSleep) >> respond r
    secReject = HTTP.statusIsClientError . Wai.responseStatus 
    doSleep = threadDelay =<< rndTime 
    rndTime = Sys.getStdRandom (Sys.randomR (millisec lo, millisec hi))
    millisec = (*) 1000

shouldUseTLS :: FS.FilePath -> IO Bool
shouldUseTLS h = (||) <$> haveKF <*> haveCF where
    haveKF = FS.isFile $ h FS.</> wiki_key
    haveCF = FS.isFile $ h FS.</> wiki_crt

runWikilonInstance :: Args -> IO ()
runWikilonInstance a = mainBody where
    mainBody = 
        if hasBadArgs then failWithBadArgs else
        if askedForHelp then helpAndExit else
        createWIKILON_HOME >>= \ home ->
        Wikilon.loadInstance (FS.encodeString home) >>= \ app ->
        loadPort home (_port a) >>= \ port ->
        shouldUseTLS home >>= \ bUseTLS ->
        unless (_silent a) (greet home port app bUseTLS) >>
        if _justInit a then return () else
        let waiApp = wikilonMiddleware (Wikilon.waiApp app) in
        let tlsSettings = wikilonWarpTLSSettings home in
        let warpSettings = wikilonWarpSettings port in
        if bUseTLS then WarpTLS.runTLS tlsSettings warpSettings waiApp
                   else Warp.runSettings warpSettings waiApp
        -- does not return
    badArgs = _badArgs a
    hasBadArgs = not $ L.null badArgs
    failWithBadArgs = err badArgMsg >> err helpMessage >> Sys.exitFailure
    badArgMsg = "unrecognized arguments: " ++ show badArgs 
    askedForHelp = _help a
    helpAndExit = Sys.putStrLn helpMessage >> Sys.exitSuccess
    greet home port app bUseTLS =
        Sys.putStrLn ("Wikilon:") >>
        Sys.putStrLn ("  Home:  " ++ showFP home) >>
        Sys.putStrLn ("  Port:  " ++ show port) >>
        Sys.putStrLn ("  HTTPS: " ++ show bUseTLS) >>
        Sys.putStrLn ("  Admin: " ++ Wikilon.adminCode app)

showFP :: FS.FilePath -> String
showFP = T.unpack . either id id . FS.toText

-- load a file assuming read/show as a viable serialization format.
-- This will work for numbers, but not for everything.
loadRS :: (Read a, Show a) => FS.FilePath -> a -> Maybe a -> IO a
loadRS fp _ (Just a) = 
    let bytes = UTF8.fromString (show a) in
    FS.writeFile fp bytes >>
    return a
loadRS fp def Nothing = 
    FS.isFile fp >>= \ bFile ->
    if not bFile then loadRS fp def (Just def) else
    (UTF8.toString <$> FS.readFile fp) >>= \ fileContents ->
    case tryRead fileContents of
        Just a -> return a
        Nothing -> 
            let fpTxt = T.unpack (either id id $ FS.toText fp) in
            fail $ "could not read " ++ fpTxt 

-- select and save wikilon port information
loadPort :: FS.FilePath -> Maybe Int -> IO Int
loadPort h = loadRS (h FS.</> "port") defaultPort

loadCache :: FS.FilePath -> Maybe Int -> IO Int
loadCache h = loadRS (h FS.</> "cacheSize") defaultCache

err :: String -> IO ()
err = Sys.hPutStrLn Sys.stderr

portArg :: String -> Maybe Int
portArg ('-':'p': s) = tryRead s
portArg _ = Nothing

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
        Just h -> return (FS.decodeString h)
        Nothing -> FS.getAppDataDirectory "wikilon"

-- getWIKILON_HOME then create directory if needed
createWIKILON_HOME :: IO FS.FilePath
createWIKILON_HOME =
    getWIKILON_HOME >>= \ h0 ->
    FS.createTree h0 >>
    FS.canonicalizePath h0

