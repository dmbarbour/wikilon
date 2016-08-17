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
defaultQuota = 10000000

runABC :: ABC.Quota -> ABC.OnTok -> ABC -> ABC 
runABC = run where
    run quota ontok prog 
        = ABC.compactOps 
        $ (:[]) . ABC_Val . quoteResult 
        $ ABC.eval quota ontok (wrap prog) v0
    sealer   = ":runABC"
    unsealer = ".runABC"
    v0 = S sealer (B mempty linear)
    linear = (ABC.f_aff .|. ABC.f_rel)
    wrap p = 
        [ ABC_Val (ABC.block p)
        , ABC_Ext ExtOp_Inline
        , ABC_Ext ExtOp_Swap
        , ABC_Tok unsealer
        , ABC_Ext ExtOp_Inline
        ]
    quoteResult ([], v) = v
    quoteResult (ops, v) = Z (ABC.compactOps ops) v

main :: IO ()
main = 
    preserveNewlines >> 
    LBS.hGetContents Sys.stdin >>= \ src ->
    case P.decode src of
        Left stuck -> 
            Sys.hPutStrLn Sys.stderr $ "parse error:\n" ++ show stuck
        Right abc -> do
            let quota = defaultQuota 
            let onTok = ABC.defaultOnTok 
            let prog = ABC.fromPureABC abc 
            let result = runABC quota onTok prog 
            Sys.hPutStrLn Sys.stdout $ show result

preserveNewlines :: IO ()
preserveNewlines = 
    mapM_ (flip Sys.hSetNewlineMode Sys.noNewlineTranslation) $
    [Sys.stdout, Sys.stderr, Sys.stdin]


