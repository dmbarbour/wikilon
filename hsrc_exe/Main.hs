{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Monad (when)
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
 \    wikilon [-pPortNumber]\n\
 \      port number: listen on this port and save as default \n\
 \          initial default port is 3000 \n\
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

-- MAIN PROGRAM BEHAVIOR


main :: IO ()
main = Env.getArgs >>= processArgs >> runWikilonInstance

runWikilonInstance :: IO ()
runWikilonInstance =
    getWIKILON_HOME >>= \ home ->
    getWikilonPort >>= \ p ->
    Wikilon.loadInstance home >>= \ app ->
    Sys.putStrLn ("Wikilon") >>
    Sys.putStrLn ("  Data: " ++ home) >>
    Sys.putStrLn ("  Port: " ++ show p) >>
    Warp.run (fromIntegral p) app

processArgs :: [String] -> IO ()
processArgs = mapM_ pa where
  pa (portArg -> Just n) = setWikilonPort n
  pa (helpArg -> True) = Sys.putStrLn helpMessage >> Sys.exitSuccess
  pa arg = badArg arg >> err helpMessage >> Sys.exitFailure
  badArg arg = err $ "unrecognized argument: " ++ arg
  err = Sys.hPutStrLn Sys.stderr

portArg :: String -> Maybe Int
portArg ('-':'p': s) = tryRead s
portArg _ = Nothing

tryRead :: (Read a) => String -> Maybe a
tryRead (reads -> [(a,[])]) = Just a
tryRead _ = Nothing

helpArg :: String -> Bool
helpArg ('-':s) = helpArg s
helpArg "?" = True
helpArg "help" = True
helpArg _ = False

-- assuming *nix environment for now
getWIKILON_HOME :: IO Sys.FilePath
getWIKILON_HOME = Env.getEnvironment >>= return . h where
    h (L.lookup "WIKILON_HOME" -> Just home) = home
    h (L.lookup "HOME" -> Just home) = home </> ".local/share/wikilon"
    h _ = "./wikilon"

getWikilonPort :: IO Int
getWikilonPort = withPortDB False $ \ db -> DB.query db GetPort

setWikilonPort :: Int -> IO ()
setWikilonPort n = withPortDB True $ \ db -> DB.update db (SetPort n)

withPortDB :: Bool -> (DB.AcidState Port -> IO a) -> IO a
withPortDB cp action = do
    home <- getWIKILON_HOME
    db <- DB.openLocalStateFrom (home </> "port") (Port 3000)
    result <- action db
    when cp (DB.createCheckpoint db)
    DB.closeAcidState db
    return result
