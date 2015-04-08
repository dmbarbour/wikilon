{-# LANGUAGE DeriveDataTypeable #-}
-- | Wikilon keeps a branching history for dictionaries. By hosting
-- many dictionaries, Wikilon can address multiple curation levels,
-- protected versions, and users can more easily perform atomic update
-- to multiple words. 
--
-- I expect a large number of branches, scaling more or less linearly
-- with the number of users. 
-- 
-- This module does not touch access control, curation, workflows, etc..
module Wikilon.Branch
    ( Branch
    , BranchSet
    , BranchName
    , DictHist
    , MergeHist
    ) where

import Data.Monoid
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as BS
import Data.Typeable (Typeable)
import Database.VCache
import Data.VCache.Trie (Trie)
import qualified Data.VCache.Trie as Trie

import Wikilon.Dict
import Wikilon.Hist
import Wikilon.StateHist

-- | Branch names are simple Utf8 strings. We might enforce also
-- that they are suitable as words in our dictionaries.
type BranchName = UTF8.ByteString

-- | A BranchSet is a collection of named branches. Since we possibly
-- have a very large branchset (e.g. thousands of branches for as many
-- users or projects), a trie is appropriate here.
type BranchSet = Trie Branch

-- | Every Branch includes a dictionary history and a merge history.
--
-- NOTE: Wikilon doesn't keep metadata on deltas, such as commit messages
-- or change logs, because those are an awkward fit for lossy, logarithmic
-- history. 
type Branch = Branch0

-- versioned branching model
data Branch0 = Branch0
    { b_dict    :: !DictHist  -- ^ snapshots and head 
    , b_merge   :: !MergeHist -- ^ merge history
    , b_count   :: {-# UNPACK #-} !Int -- total number of branches
    , b_size    :: {-# UNPACK #-} !Int -- total number of dictionaries
    , b_create  :: !T         -- ^ when was branch created?
    , b_delete  :: !(Maybe T) -- ^ has branch been deleted? if so, when?
    } deriving (Typeable)

-- | The dictionary history consists of snapshots of the dictionary
-- over time. This history is subject to exponential decay, resulting
-- in log-scale views into the past, losing intermediate states.
type DictHist = StateHist Dict

-- | A merge history tracks when a branch receives information from other
-- branches. The goal here is to have just enough information to render a
-- pretty picture of the larger branch and merge graph. But this data may
-- prove useful for other operations.
--
-- A branch will not name itself in its own merge history. Most updates
-- are not considered to be merges. Initially forking a dictionary does
-- count as a merge.
type MergeHist = Hist BranchName


instance VCacheable Branch0 where
    put b = do 
        putWord8 0 -- version
        put (b_count b)
        put (b_size b)
        put (b_create b)
        put (b_delete b)
        put (b_merge b)
        put (b_dict b)
    get = getWord8 >>= \ v -> case v of
        0 -> do
            ct <- get
            sz <- get
            t0 <- get
            tf <- get
            hm <- get
            hd <- get
            return $! Branch0
                { b_count = ct
                , b_size = sz
                , b_create = t0
                , b_delete = tf
                , b_merge = hm
                , b_dict = hd
                }
        _ -> fail $ branchErr $ "unrecognized version " ++ show v




branchErr :: String -> String
branchErr = (++) "Wikilon.Branch: "

-- Regarding design:
--
-- I plan to implement an exponential decay model, i.e. we lose some
-- information about our histories while keeping a useful degree of
-- intermediate information. I have some simple ways to do this.
--
-- The difficulty a lossy history introduces is: how shall we model
-- commit comments, issue trackers, merge histories, and similar?  
-- If we align our metadata with our commits or snapshots, we too 
-- easily lose information due to the decay model.
--
-- I am tempted to push metadata directly into the dictionary. This
-- would include issues, change logs, merge histories, categories. A
-- few simple name conventions could help, but also we could leverage
-- some type conventions, e.g. using something like `:foo.onMerge` to
-- guide merge of sealed values of type foo (hence allowing language
-- or purpose specific merge behaviors).
--
-- Pushing metadata into the dictionary could improve accessibility,
-- flexibility, and extensibility of the metadata. And it would give
-- dictionaries more of a filesystem feel.
--
-- However, I would like to keep enough metadata to reliably render
-- the versions graph. So I'll keep an additional merge history per
-- branch, which may be more reliable. (I can potentially collect a
-- history, but it probably isn't essential.)
-- 


