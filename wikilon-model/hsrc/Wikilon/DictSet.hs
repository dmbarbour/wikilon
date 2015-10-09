{-# LANGUAGE DeriveDataTypeable #-}

-- | Wikilon hosts more than one dictionary. This is necessary for
-- DVCS-style forking and merging, to model 'working' dictionaries
-- apart from the more stable versions. Individually, each named
-- dictionary has a history. 
--
-- In terms of the web service, a dictionary has a URL \/d\/dictName.
-- And an individual word has URL \/d\/dictName\/w\/word. 
--
module Wikilon.DictSet
    ( DictSet
    , DictRef
    , module Wikilon.Time
    , module Wikilon.Dict
    ) where

import Data.Typeable (Typeable)
import qualified Data.ByteString as BS
import Data.VCache.Trie (Trie)
import qualified Data.VCache.Trie as Trie
import Database.VCache
import Data.VCache.LoB (LoB) 
import Wikilon.Dict
import Wikilon.Time

-- | Wikilon hosts a finite collection of named dictionaries.
newtype DictSet = DictSet (Trie DictRef)
    deriving (Eq, Typeable)

-- | Each named dictionary has a head, a history, and potentially a 
-- bunch of metadata and cached computations and similar. 
--
-- The history for a dictionary shall utilize exponential decay after
-- it grows too large (much larger than a few hundred point samples).
-- So what I'm really after is more like a breadcrumb trail to provide
-- greater robustness against loss of information.
--
-- For cached computations, I eventually want:
--
--  * fast access to a list of 'undefined' words, with reasons
--  * fast access to a list of badly typed words
--  * index for fuzzy find of words, substrings of words
--  * index for finding words from type
--  * access to 'compiled' versions of words
--
-- Some of the
data DictRef = DictRef
    { dref_head :: PVar Dict
    , dref_hist :: PVar (LoB (T, Dict))
    }

