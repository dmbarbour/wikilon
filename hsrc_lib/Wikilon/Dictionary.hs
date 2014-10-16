
-- | The Awelon Object (AO) dictionary is a primary data structure
-- of Wikilon. AO words form functions, modules, tests, documentation,
-- directives, web-applications, and wiki pages. But, essentially, the
-- content of each word is just some AO code. Documentation is modeled
-- as a word that computes a document.
--
-- This module describes the dictionary database and transactions,
-- and enforces a few useful properties:
--
--   * every word parses
--   * words are fully defined
--   * defined words are acyclic
--   * distinguish define vs. update
-- 
-- Additionally, the dictionary supports *reactive* observation. A
-- developer can track which words are updated based on any change
-- in the dictionary. This will make it easier to automatically run
-- tests whose behavior may have changed, or alert users when the 
-- dictionary is shifting beneath them.
--
-- Wikilon may enforce higher level policies to keep the dictionary
-- hygienic, e.g. validating that words typecheck or tests pass.
--
-- The dictionary model also tracks some metadata, e.g. when the 
-- transactions are applied.
-- 
module Wikilon.Dictionary
    ( Dict, DictDB, DictTX
    , DictUpd(..), DTXMeta(..)
    , dictDecayParams
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Wikilon.Time
import Wikilon.Database
import Wikilon.AO
import Wikilon.Word

type DictDB = DB DictTX

-- | A dictionary has a list of transactions and a valid state.
--
-- 'Valid' here means that words parse, are acyclic, and that no
-- dependencies are undefined. However, there may be other errors in
-- the dictionary, such as words that will not pass a typecheck, or 
-- tests that fail. Higher layers may guard the dictionary against
-- transactions that leave the dictionary unhealthy.
--
data Dict = Dict
    { dict_db :: !DictDB
    , dict_st :: !DictST
    }

readDictDB :: Dict -> DictDB
readDictDB = dict_db

readDictST :: Dict -> DictST
readDictST = dict_st

-- | A dictionary state supports up-to-date views of the dictionary,
-- compilation of words to Awelon bytecode, and similar features.
newtype DictST = DictST { dict_wordMap :: M.Map Word DictEnt }

-- | Each word has a definition and a reverse-lookup model.
data DictEnt = DictEnt !AO_Code !RLU

-- | Our reverse lookup is a set of words that should be re-evaluated
-- (transitively) after an update to a given word. 
--
-- RLU should not directly include transitive dependencies. For example:
--
--    @define bar ...
--    @define baz x bar
--    @define foo bar baz
--
-- In this case, both baz and foo depend on bar. However, since foo 
-- also depends on baz, we don't want to include foo in the reverse 
-- lookup of bar. This helps keep our dependency sets smaller, with
-- less redundant information in memory, and simplifies topological
-- sort of reactive updates, avoiding redundant computations.
-- 
type RLU = S.Set Word

-- | A transaction on a dictionary is modeled as an ordered list of
-- updates to the dictionary, applied atomically, plus some metadata.
data DictTX = DictTX
    { dtx_meta    :: {-# UNPACK #-} !DTXMeta
    , dtx_actions :: [DictUpd] -- individual updates to dictionary.
    }

-- | Updates on the dictionary are relatively simplistic for now.
-- If it proves highly valuable, we could easily add a few more.
-- But I'm favoring a 'keep it simple and straightforward' design.
data DictUpd 
    = Define Word AO_Code   -- ^ @define foo 1 2 3; initial definition
    | Update Word AO_Code   -- ^ @update foo 1 2 3 + +; modify existing word
    | Rename Word Word      -- ^ @rename foo bar; foo must exist, bar must not.
    | Delete Word           -- ^ @delete bar; bar must exist

-- Thoughts: an `@append foo + +` feature might be useful, e.g. for
-- modeling of embedded objects. But I'm taking a 'wait and see' tactic
-- on that, I'm not confident it's a good idea yet. I suspect it could 
-- be modeled with a simple indirection, e.g.
--
--    @define foo s1$foo
--    (...)
--    @define s2$foo s1$foo + +
--    @update foo s2$foo
--

-- | Transaction metadata includes a little information about who,
-- what, when, where, why, and how much information loss has been
-- accrued due to merges. 
--
-- Time has special handling. Other metadata will consist of a simple
-- set of words, whose meanings are usually implicit to human users,
-- e.g. `user:david` or `project:awelon`. This metadata is for words
-- associated with the update, but not defined in the same update.
--
-- Renamed or deleted words will be processed appropriately. I'm 
-- avoiding information like e-mail addresses and such in metadata
-- because it would hinder keeping that information up-to-date, or
-- later eliminating it.
-- 
data DTXMeta = DTXMeta
    { dtx_tm_ini :: {-# UNPACK #-} !T -- when (start)
    , dtx_tm_fin :: {-# UNPACK #-} !T -- when (last merged)
    , dtx_count  :: {-# UNPACK #-} !Int -- count (1 for initial transaction)
    , dtx_wmeta  :: !(S.Set Word) -- who, where, why, etc.
    }

dictDecayParams :: Decay DictTX
dictDecayParams = Decay
    { db_maxlen = 1000
    , db_merge  = dictMergeFn
    , db_freq   = 8
    , db_keep   = 8
    }

dictMergeFn :: DictTX -> DictTX -> (DictTX, Int)
dictMergeFn a b = error "todo: merge DTX" -- (mappend a b, dictMergeQuality a b)

