{-# LANGUAGE OverloadedStrings #-}

-- | runABC receives a ∀e.(e→(v*e)) program on STDIN
--   and generates a similar program on STDOUT
--   albeit, after an attempt to evaluate the value.

import qualified System.IO as Sys
import Data.Bits ((.|.))
import Data.Monoid (mempty)
import qualified Data.ByteString.Lazy as LBS
import qualified Wikilon.ABC as P
import qualified Wikilon.ABC.Fast as ABC
import Wikilon.ABC.Fast (ABC, V(..), Op(..), ExtOp(..))
import qualified Wikilon.ABC.Eval as ABC

defaultQuota :: ABC.Quota
defaultQuota = 100000000

runABC :: ABC.Quota -> ABC -> ABC 
runABC q p = ABC.compactOps $ quoteEvalResult $ ABC.eval q p' v0 where
    sealer   = ":runABC"
    unsealer = ".runABC"
    v0 = S sealer linearIdentity
    linearIdentity = B mempty (ABC.f_aff .|. ABC.f_rel)
    p' = [ ABC_Val (ABC.block p)
         , ABC_Ext ExtOp_Inline
         , ABC_Ext ExtOp_Swap
         , ABC_Tok unsealer
         , ABC_Ext ExtOp_Inline
         ]

quoteEvalResult :: ([Op], V) -> [Op]
quoteEvalResult ([], v) = [ABC_Val v]
quoteEvalResult (cc, v) = [ABC_Val v, ABC_Val (ABC.block abc), ABC_Prim P.ABC_apply]
    where abc = ABC.compactOps cc

main :: IO ()
main = 
    LBS.hGetContents Sys.stdin >>= \ prog ->
    case P.decode prog of
        Left stuck -> 
            Sys.hPutStrLn Sys.stderr $ "parse error:\n" ++ show stuck
        Right abc -> 
            let result = runABC defaultQuota (ABC.fromPureABC abc) in
            Sys.hPutStrLn Sys.stdout $ show result

