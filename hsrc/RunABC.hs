
import qualified System.IO as Sys
import qualified Data.ByteString.Lazy as LBS
import qualified Wikilon.ABC as P
import qualified Wikilon.ABC.Fast as ABC
import Wikilon.ABC.Eval 

defaultQuota :: Quota
defaultQuota = 1000000000

main :: IO ()
main = 
    LBS.readFile Sys.stdin >>= \ prog ->
    case P.decode prog of
        Left stuck -> 
            Sys.hPutStrLn Sys.stderr $ "parse error:\n" ++ show stuck
        Right abc -> case eval defaultQuota abc of
            _ -> undefined
