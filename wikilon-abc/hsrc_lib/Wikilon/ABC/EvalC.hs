
-- | (under construction)
-- | I would like to test whether bypassing the 'decodeOp' step would
-- offer a significant performance advantage for interpreting Wikilon
-- code. Further, we might benefit from tweaking the evaluator to 
-- use more arguments, and use all of them.
--
-- This will need to be compared against the current evaluator.
module Wikilon.ABC.EvalC 
    ( evaluateC
    ) where

import Control.Applicative
import Data.Monoid
import Data.Int
import Data.Char
import Data.Bits
import qualified Data.Array.IArray as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8

import Awelon.ABC (abcDivMod)
import Wikilon.ABC.Code hiding (null)
import qualified Wikilon.ABC.Code as ABC 
import Wikilon.ABC.Util
import Wikilon.ABC.Value 

import Wikilon.ABC.Eval


-- | evaluateC is an experimental evaluator that is intended to be
-- faster than evaluate by virtue of skipping an intermediate type
-- and case from decodeOp. Instead, evaluateC operates directly on
-- the underlying code. 

{-


        '[' ->
            sizedSliceFby ']' bs >>= \ (bcode, _code') ->
            takeBBlock (abc_data abc) >>= \ (bdata, _data') ->
            let blockABC = ABC { abc_code = bcode, abc_data = bdata } in
            let op = ABC_Block blockABC in
            let abc' = ABC { abc_code = _code', abc_data = _data' } in
            return (op, abc')
        '"' ->
            sizedSliceFby '~' bs >>= \ (txt, _code') ->
            let op = ABC_Text txt in
            let abc' = abc { abc_code = _code' } in
            return (op, abc')
        '{' ->
            sizedSliceFby '}' bs >>= \ (lzTok, _code') ->
            let op = ABC_Tok (LBS.toStrict lzTok) in
            let abc' = abc { abc_code = _code' } in
            return (op, abc')
        '»' ->
            takeBQuote (abc_data abc) >>= \ (v, vs) ->
            let op = ABC_Quote v in
            let abc' = ABC { abc_code = bs, abc_data = vs } in
            return (op, abc')

-}
 