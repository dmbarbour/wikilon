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
import Wikilon.DictRLU
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
-- For cached computations, I want:
--
--  * reverse lookup index to find words by which tokens they use.
--  * fast access to precompiled words
--  * index for fuzzy find for words by substring
--  * quick list of undefined, cyclic, or badly typed words
--  * index for finding words by type (combine with prior?)
--
-- Fortunately, it's easy to add cached computations later, as needed.
--
type DictRef = D1

data D1 = D1
    { d_head :: PVar DictHead
    , d_hist :: PVar (LoB (T, Dict))   
    } deriving (Typeable)

-- 
data DictHead = DictHead
    { dh_dict :: Dict
    -- , dh_rlu  :: DictRLU
    , dh_tmod :: {-# UNPACK #-} !T -- a time
    } deriving (Typeable)


-- Thoughts:
--
-- For caching, I have at least a couple options. One option is to
-- indirect caching through a secure hash. Then I don't need to
-- worry about cache invalidation (simple expiration would work). 
-- And I could theoretically leverage a common cache across multiple
-- versions and branches of the dictionary.
--
-- OTOH, with VCache, I already gain a lot of implicit structure 
-- sharing. The lack of indirection might involve recomputing the
-- structure between branches and versions, but is simple.






