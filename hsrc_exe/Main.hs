{-# LANGUAGE ViewPatterns, PatternGuards #-}

module Main (main) where

import Control.Monad (when)
import Control.Applicative
import qualified System.IO as Sys
import qualified System.Exit as Sys
import qualified System.Environment as Env
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.List as L
import qualified Data.Acid as DB
import System.FilePath ((</>))
import qualified Wikilon
import PortDB

helpMessage :: String
helpMessage =
 "The `wikilon` executable will initiate a Wikilon web service.\n\
 \\n\
 \    wikilon [-pPortNumber] [-s] [-init] \n\
 \      port number: listen on this port and save as default \n\
 \          initial default port is 3000 \n\
 \      -s: silent mode; no visible output. \n\
 \      -init: initialize & greet, but do not start. \n\
 \\n\
 \    Environment Variables:\n\
 \      WIKILON_HOME: directory for persistent data\n\
 \          defaults to $HOME/.local/share/wikilon/\n\
 \          use different homes for different instances\n\
 \\n\
 \Wikilon is a wiki, a software platform, an integrated development\n\
 \environment for the Awelon project. Pages contain Awelon Object (AO)\n\
 \code, and may define new web applications or services.\n\
 \\n\
 \Most configuration of Wikilon should be performed through browser.\n\
 \After starting up the instance, navigate to the root page for more\n\
 \instructions. (You can change the root page later.)\n\
 \"

data Args = Args 
    { _setPort  :: Maybe Int
    , _silent   :: Bool
    , _justInit :: Bool
    , _help     :: Bool
    , _badArgs  :: [String] -- reverse order
    }
defaultArgs :: Args
defaultArgs = Args 
    { _setPort = Nothing
    , _silent = False
    , _justInit = False
    , _help = False
    , _badArgs = []
    }

defaultPort :: Int
defaultPort = 3000

main :: IO ()
main = (procArgs <$> Env.getArgs) >>= runWikilonInstance

procArgs :: [String] -> Args
procArgs = rba . foldl pa defaultArgs where
    pa a (portArg -> Just n) = a { _setPort = Just n }
    pa a (helpArg -> True) = a { _help = True }
    pa a "-s" = a { _silent = True }
    pa a "-init" = a { _justInit = True }
    pa a v = 
        let badArgs' = v : _badArgs a in
        a { _badArgs = badArgs' }
    rba a = a { _badArgs = L.reverse (_badArgs a) }


runWikilonInstance :: Args -> IO ()
runWikilonInstance a = tryRun where
    tryRun = 
        if hasBadArgs then failWithBadArgs else
        if askedForHelp then helpAndExit else
        getWIKILON_HOME >>= \ home ->
        getPort home >>= \ port ->
        Wikilon.loadInstance home >>= \ app ->
        greet home port app >>
        run port app
    badArgs = _badArgs a
    hasBadArgs = not $ L.null badArgs
    failWithBadArgs = err badArgMsg >> err helpMessage >> Sys.exitFailure
    badArgMsg = "unrecognized arguments: " ++ show badArgs 
    askedForHelp = _help a
    helpAndExit = Sys.putStrLn helpMessage >> Sys.exitSuccess
    getPort home =
        getWikilonPort home >>= \ p ->
        case (_setPort a) of
            Just p' | (p /= p') -> setWikilonPort home p' >> return p'
            _ -> return p
    greet _ _ _ | _silent a = return ()
    greet home port app =
        Sys.putStrLn ("Wikilon:") >>
        Sys.putStrLn ("  Home:  " ++ home) >>
        Sys.putStrLn ("  Port:  " ++ show port) >>
        Sys.putStrLn ("  Admin: " ++ Wikilon.webKey app)
    run _ _ | _justInit a = return ()
    run port app = Warp.run (fromIntegral port) (Wikilon.waiApp app)

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

-- assuming *nix environment for now
getWIKILON_HOME :: IO Sys.FilePath
getWIKILON_HOME = Env.getEnvironment >>= return . h where
    h (L.lookup "WIKILON_HOME" -> Just home) = home
    h (L.lookup "HOME" -> Just home) = home </> ".local/share/wikilon"
    h _ = "./wikilon"

getWikilonPort :: Sys.FilePath -> IO Int
getWikilonPort h = wp $ \ db -> DB.query db GetPort where
    wp = withPortDB h False

setWikilonPort :: Sys.FilePath -> Int -> IO ()
setWikilonPort h p = wp $ \ db -> DB.update db (SetPort p) where
    wp = withPortDB h True 

withPortDB :: Sys.FilePath -> Bool -> (DB.AcidState Port -> IO a) -> IO a
withPortDB home cp action = do
    db <- DB.openLocalStateFrom (home </> "port") (Port defaultPort)
    result <- action db
    when cp (DB.createCheckpoint db)
    DB.closeAcidState db
    return result
