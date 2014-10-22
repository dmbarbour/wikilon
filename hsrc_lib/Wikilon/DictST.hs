{-# LANGUAGE PatternGuards, ViewPatterns #-}

-- | The Awelon Object (AO) dictionary is a primary data structure
-- of Wikilon. AO words form functions, modules, tests, documentation,
-- directives, web-applications, and wiki pages. But, essentially, the
-- content of each word is just some AO code. Documentation is modeled
-- as a word that computes a document.
--
-- This module describes the dictionary state.
--
-- In addition to a set of words, a dictionary tracks some useful
-- metadata about when changes were made, potentially who made them
-- and for which projects, etc.
-- 
module Wikilon.DictST
    ( DictST
    , DefinitionMap, ReverseLookup, WordSearchIndex
    , emptyDict, updateDict
    , dictDefs, dictSearch, dictRLU
    ) where

-- todo: 
--  dictionary state - update and access
--  serialization of dictionary

import qualified Data.List as L

import Wikilon.WordMap (WordMap)
import Wikilon.WordSet (WordSet)
import Wikilon.WordSearch (WordSearchIndex)
import qualified Wikilon.WordMap as WM
import qualified Wikilon.WordSearch as WSI

import Wikilon.DictTX
import Wikilon.AO

-- | The DictST represents the current state of a dictionary, and it
-- synchronizes three views:
--
--   a map from words to definitions
--   a map from words to a set of clients of that word
--   a word search index for all words in the dictionary
--
-- In addition, it enforces a weak validity property: that words are
-- well defined and acyclic. There is no assurance that words will
-- typecheck or that tests will pass. But they'll at least compile
-- to Awelon bytecode.
--
-- Updates to the DictST are performed by DictTX.
data DictST = DST
    { _defs :: !DefinitionMap
    , _rlu  :: !ReverseLookup
    , _wsi  :: !WordSearchIndex
    }
type DefinitionMap = WordMap AO_Code
type ReverseLookup = WordMap WordSet

-- | The main use of a dictionary: given a word, find its definition.
-- The DictST structure ensures words are properly defined, i.e. no
-- missing words, no cycles.
dictDefs :: DictST -> DefinitionMap
dictDefs = _defs

-- | To correct spelling errors, support tab completion, and other
-- tasks that involve searching for words by their structure, you
-- should use the WordSearchIndex.
dictSearch :: DictST -> WordSearchIndex
dictSearch = _wsi

-- | If you want to find all uses of a word within the dictionary,
-- try a ReverseLookup.
dictRLU :: DictST -> ReverseLookup
dictRLU = _rlu

-- | Create a new, empty dictionary.
emptyDict :: DictST
emptyDict = DST WM.empty WM.empty WSI.empty

-- | Attempt to update a dictionary with a transaction. The update
-- will be rejected if it leads to an invalid dictionary state, with
-- some human explanation for why.
updateDict :: DictTX -> DictST -> Either [String] DictST
updateDict _dtx _ds = eds where
    eds = if L.null es then Right ds' else Left es 
    es = ["error: TODO!"]
    ds' = error "TODO!"


