
-- | Wikilon will track some relationships between branches of the
-- dictionary, enough to support rendering of a versions graph. At
-- this time, it isn't clear how or whether to perform exponential
-- decay on the geneology.
--
-- TODO: support rename, delete
--
-- Thoughts: would it be better to just support a generic log and
-- to process it, compared to specializing for the geneology?
module Wikilon.Geneology 
    ( Geneology
    , empty
    , addChild
    , forks
    , merges
    , vspace
    ) where

import qualified Data.ByteString.UTF8 as UTF8

import Database.VCache
import Data.VCache.LoB (LoB)
import qualified Data.VCache.LoB as LoB
import Data.VCache.Trie (Trie)
import qualified Data.VCache.Trie as Trie

import Wikilon.Time

type Name = UTF8.ByteString

-- | A geneology tracks simple merge or fork relationships between
-- branches. 
type Geneology = G0

-- versioned representation for VCache
data G0 = G0
    { g_fork  :: !(Trie ForkHist)  -- ~ list of children
    , g_merge :: !(Trie MergeHist) -- ~ list of parents
    }

type MergeHist = LoB (T, Name)
type ForkHist = LoB (T, Name)

-- | Create an empty geneology
empty :: VSpace -> Geneology
empty vc = G0 (Trie.empty vc) (Trie.empty vc)

-- | Access associated VCache space
vspace :: Geneology -> VSpace
vspace = Trie.trie_space . g_fork

-- | addChild parent child time
--
-- Add information to the geneology that the named parent branch has
-- contributed to the named child branch. This corresponds to the 
-- parent being merged into the child, or forked to create a new
-- child, or bookmarked using the child name as a bookmark name.
--
-- Note: Entries are time-sorted. This is only efficient if adding
-- recent history.
--
addChild :: Name -> Name -> T -> Geneology -> Geneology
addChild parent child time g0 = gf where
    vc = g_space g0
    add label = Just . _insHist (time,label) . maybe (_newHist vc) id
    fork' = Trie.adjust (add parent) (g_fork g0)
    merge' = Trie.adjust (add child) (g_merge g0)
    gf = G0 { g_space = vc, g_fork = fork', g_merge = merge' } 

_newHist :: VSpace -> LoB (T, Name)
_newHist vc = LoB.empty vc 16

-- a time-sorted insert with recent history near head
_insHist :: (T, Name) -> LoB (T, Name) -> LoB (T, Name)
_insHist e l = case LoB.uncons l of
    Nothing -> LoB.cons e l 
    Just (e0, l') -> 
        let bDone = (fst e) >= (fst e0) in
        if bDone then LoB.cons e l else
        LoB.cons e0 (_insHist e l')

-- | Find all direct child relationships from named branch.
childrenOf :: BranchName -> Geneology -> [(T, Name)]
childrenOf n = maybe [] LoB.toList . Trie.lookup n . g_fork

-- | Find all direct parent relationships from named branch.
parentsOf :: BranchName, -> Geneology -> [(T, Name)] 
parentsOf n g = maybe [] LoB.toList . Trie.lookup n . g_merge


