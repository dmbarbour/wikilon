{-# LANGUAGE ViewPatterns, PatternGuards, OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Applicative
import qualified System.IO as Sys
import qualified System.Exit as Sys
import qualified System.Environment as Env
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import qualified Network.Wai.Middleware.Gzip as Wai
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
 \    wikilon [-pPort] [-cCert] [-s] [-init] [-SCGI] \n\
 \      -pPort: listen on port & save as new default (def 3000) \n\
 \      -s: silent mode; do not display greeting or admin URL. \n\
 \      -init: initialize & greet, but do not start. \n\
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
 \environment for the Awelon project. Pages contain Awelon Object (AO)\n\
 \code, and may define new web applications or services.\n\
 \\n\
 \Most configuration of Wikilon should be performed through a browser.\n\
 \After starting Wikilon, open a browser to http(s)://localhost:Port\n\
 \to get started. An admin session key will be printed to console."

data Args = Args 
    { _port  :: Maybe Int
    , _silent   :: Bool
    , _justInit :: Bool
    , _help     :: Bool
    , _badArgs  :: [String] -- reverse order
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


wikilonMiddleware :: Wai.Application -> Wai.Application
wikilonMiddleware = gz where
    gz = Wai.gzip Wai.def


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
        Sys.putStrLn ("  Admin: " ++ Wikilon.webKey app)

showFP :: FS.FilePath -> String
showFP = T.unpack . either id id . FS.toText

-- select and save wikilon port information
loadPort :: FS.FilePath -> Maybe Int -> IO Int
loadPort h (Just p) =
    let fp = h FS.</> "port" in
    let bytes = (UTF8.fromString (show p)) in
    FS.writeFile fp bytes >>
    return p
loadPort h Nothing =
    let fp = h FS.</> "port" in
    FS.isFile fp >>= \ bFile ->
    if not bFile then loadPort h (Just defaultPort) else
    (UTF8.toString <$> FS.readFile fp) >>= \ sPort ->
    case tryRead sPort of
        Just p -> return p
        Nothing -> fail "could not read WIKILON_HOME/port file"

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

