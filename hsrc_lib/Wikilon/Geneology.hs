
-- | Wikilon will track some relationships between branches of the
-- dictionary, enough to support rendering of a versions graph. At
-- this time, it isn't clear how or whether to perform exponential
-- decay on the geneology.
--
-- At the moment, our geneology is a very basic append-only data
-- structure. Ideas such as renaming or deleting a 
module Wikilon.Geneology 
    ( Geneology
    , empty
    , addChild
    , parentsOf
    , childrenOf
    , vspace

    , addSource
    , addSink
    ) where

import qualified Data.ByteString.UTF8 as UTF8

import Database.VCache
import Data.VCache.LoB (LoB)
import qualified Data.VCache.LoB as LoB
import Data.VCache.Trie (Trie)
import qualified Data.VCache.Trie as Trie

import Wikilon.Time

type Name = UTF8.ByteString
type Label = Name

-- | A geneology tracks simple merge or fork relationships between
-- branches. 
type Geneology = G0

-- versioned representation for VCache
data G0 = G0
    { g_outgoing :: !(Trie Hist)  -- ~ list of children
    , g_incoming :: !(Trie Hist) -- ~ list of parents
    }

type Hist = LoB (T, Name)

-- | Create an empty geneology
empty :: VSpace -> Geneology
empty vc = G0 (Trie.empty vc) (Trie.empty vc)

-- | Access associated VCache space
vspace :: Geneology -> VSpace
vspace = Trie.trie_space . g_outgoing

-- | addChild parent child time
--
-- Add information to the geneology that the named parent branch has
-- contributed to the named child branch. This corresponds to both 
-- a source label and a sink label.
addChild :: Name -> Name -> T -> Geneology -> Geneology
addChild parent child t = 
    addSource child (t,parent) . 
    addSink parent (t,child)

-- | Add an origin tag to a named branch. In addition to parent names,
-- this could be something like "#RENAME" or "#CREATE", or another tag.
-- A source label is visible via 'parentsOf'.
addSource :: Name -> (T, Label) -> Geneology -> Geneology
addSource n v g0 = gf where
    addLbl = Just . _insHist v . maybe (_newHist (vspace g0)) id
    merge' = Trie.adjust addLbl n (g_incoming g0)
    gf = g0 { g_incoming = merge' }

-- | Add a destination tag to a named branch. In addition to child 
-- names, this could be something like "#DELETE" or "#EXPORT:URL", etc.
-- A sink label is visible via 'childrenOf'.
addSink :: Name -> (T, Label) -> Geneology -> Geneology
addSink n v g0 = gf where
    addLbl = Just . _insHist v . maybe (_newHist (vspace g0)) id
    fork' = Trie.adjust addLbl n (g_outgoing g0)
    gf = g0 { g_outgoing = fork' }

_newHist :: VSpace -> Hist
_newHist vc = LoB.empty vc 16

-- a time-sorted insert with recent history near head
_insHist :: (T, Name) -> Hist -> Hist
_insHist e l = case LoB.uncons l of
    Nothing -> LoB.cons e l 
    Just (e0, l') -> 
        let bDone = (fst e) >= (fst e0) in
        if bDone then LoB.cons e l else
        LoB.cons e0 (_insHist e l')

-- | Find all direct child relationships from named branch.
childrenOf :: Name -> Geneology -> [(T, Name)]
childrenOf n = maybe [] LoB.toList . Trie.lookup n . g_outgoing

-- | Find all direct parent relationships from named branch.
parentsOf :: Name -> Geneology -> [(T, Name)] 
parentsOf n = maybe [] LoB.toList . Trie.lookup n . g_incoming

