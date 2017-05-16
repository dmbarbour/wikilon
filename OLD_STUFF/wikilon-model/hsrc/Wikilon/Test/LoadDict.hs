
-- a quick loader for a dictionary
module Wikilon.Test.LoadDict
    ( loadDict
    , module Wikilon.Dict
    ) where

import Data.Monoid
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import Database.VCache
import Wikilon.Dict
import Wikilon.AODict
import qualified System.IO as Sys

-- | given a '.ao' file, load it as a dictionary. Report parse errors
-- after constructing the dictionary.
loadDict :: VSpace -> Sys.FilePath -> IO Dict
loadDict vc f = 
    LBS.readFile f >>= \ s ->
    let (errors,dict) = loadAODict (dictCreate vc) s in
    let showErr = putErrLn . mappend "parse error: " . LazyUTF8.toString in
    mapM_ showErr errors >>
    return dict

putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr
