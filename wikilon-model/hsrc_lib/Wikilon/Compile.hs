{-# LANGUAGE OverloadedStrings #-}

-- | Compile bytecode for evaluation. Or link it. I'll probably
-- need several sophisticated compilers in the long term with 
-- good support for caching. But for the moment I'll create some
-- bare minimum compilers to get content running ASAP.
module Wikilon.Compile
    ( referenceCompile
    , basicEval
    ) where

import Prelude hiding (lookup)
import Control.Monad
import Data.Monoid
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.List as L
import qualified Data.ByteString.Char8 as BS

import Awelon.ABC (ABC)
import qualified Awelon.ABC as ABC
import Awelon.ABC.Eval
import Wikilon.Dict

-- | referenceCompile takes a dictionary, some bytecode, and a quota 
-- for per-word compilation. All {%word} tokens whose definitions can
-- be computed within the given quota are replaced by their definition.
-- All other tokens are simply left as is.
--
-- referenceCompile does assume the dictionary is acyclic at least for
-- all the words used in the compiled code.
--
referenceCompile :: (DictView dict) => dict -> Quota -> ABC -> ABC
referenceCompile d q abc = 
    let lu = lookup d in
    let c = L.foldl' (rccWord (lookup d) q) mempty $ abcWords abc in
    rewriteFromCache c abc

type RCC = Map Word ABC

-- | rewrite words from the cache.
rewriteFromCache :: RCC -> ABC -> ABC
rewriteFromCache cache = ABC.rewriteTokens rw where
    rw tok = maybe [ABC.ABC_Tok tok] ABC.abcOps $ rwWord tok
    rwWord tok =
        BS.uncons tok >>= \ (prefix,w) ->
        guard ('%' == prefix) >>
        Map.lookup (Word w) cache

-- assuming 
rccWord :: (Word -> ABC) -> Quota -> RCC -> Word -> RCC
rccWord lu q c w =
    if Map.member w c then c else -- compile a word at most once
    let def = lu w in  
    let lWords = abcWords def in
    let c' = L.foldl' (rccWord lu q) c lWords in
    let def' = rewriteFromCache c' def in
    let wtok = ABC.mkABC [ABC.ABC_Tok ("%" <> wordToUTF8 w)] in
    let meaning = maybe wtok id $ tryCompileDef q def' in
    Map.insert w meaning c'

-- evaluate a definition
--  of type Def a b = ∃v.∀e.(e→([v→[a→b]]*(v*e))
-- this may fail due to quotas, bad types, etc..
tryCompileDef :: Quota -> ABC -> Maybe ABC
tryCompileDef q def = 
    -- sealed environment that cannot be copied or dropped 
    let e = Sealed "" (Block mempty 0xff) in
    let c = Cont (ABC.abcOps def) $ -- build intermediate structure
            Apply Unit $ Cont (ABC.abcOps "c$") $ -- apply compiler
            Return -- done
    in
    case basicEval e c q of
        Right (Pair (Block abc _) e') | (e == e') -> Just abc
        _ -> Nothing

 
-- | Basic eval:
--
-- * applies discretionary sealers and unsealers
-- * ignores all annotations
--
-- That's it for now.
basicEval :: Evaluator
basicEval v cc q = error "todo"



