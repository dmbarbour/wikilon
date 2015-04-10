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

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as BS
import Data.Typeable (Typeable)
import Database.VCache
import Data.VCache.Trie (Trie)
import qualified Data.VCache.Trie as Trie
import Data.VCache.LoB (LoB)
import qualified Data.VCache.LoB as LoB
import Wikilon.Dict
import Wikilon.Time

-- | Branch names are simple Utf8 strings. We might enforce also
-- that they use the same naming heuristics as dictionary words.
type BranchName = UTF8.ByteString

-- | A BranchSet is a collection of named branches. 
type BranchSet = BranchSet0

-- versioned branchset model
data BranchSet0 = BranchSet0
    { b_root :: !(Trie Branch)
    , b_bct  :: {-# UNPACK #-} !Int -- total number of branches
    , b_dct  :: {-# UNPACK #-} !Int -- total number of dictionaries
    } deriving (Typeable, Eq)

-- | Every Branch includes a dictionary history and a merge history.
--
-- NOTE: Wikilon doesn't keep metadata on deltas, such as commit messages
-- or change logs, because those are an awkward fit for lossy, logarithmic
-- history. 
type Branch = Branch0

-- versioned branching model
data Branch0 = Branch0
    { b_head    :: !Dict       -- head dictionary
    , b_hist    :: !DictHist   -- history of dictionary
    , b_merge   :: !MergeHist  -- merge history
    , b_create  :: !T          -- when was branch created?
    , b_delete  :: !(Maybe T)  -- has branch been deleted? if so, when?
    } deriving (Typeable)

-- | The dictionary history consists of snapshots of the dictionary
-- over time. This history is subject to exponential decay, resulting
-- in log-scale views into the past, losing intermediate states.
type DictHist = Hist Dict

-- | A merge history tracks enough information to render a geneology
-- of the branch set. Each entry indicates that some information was
-- received from another branch as a result of merge or fork. Normal
-- updates to a dictionary are not considered to be merges.
type MergeHist = Hist BranchName

-- A history is just a list of (time,value) pairs, usually ordered.
-- In this case we'll limit the number of history elements directly
-- held by our branches via LoB. 
--
-- The history is held indirectly because it isn't something we'll
-- usually access.
type Hist a = VRef (LoB (T, a))

instance VCacheable Branch0 where
    put b = do 
        putWord8 0 -- version
        put (b_create b)
        put (b_delete b)
        put (b_head b)
        put (b_hist b)
        put (b_merge b)
    get = getWord8 >>= \ v -> case v of
        0 -> do
            _create <- get
            _delete <- get
            _head <- get
            _hist <- get
            _merge <- get
            return $! Branch0
                { b_create = _create
                , b_delete = _delete
                , b_head = _head
                , b_hist = _hist
                , b_merge = _merge
                }
        _ -> fail $ branchErr $ "unrecognized Branch version " ++ show v

instance VCacheable BranchSet0 where
    put b = do
        putWord8 0 -- version
        put (b_bct b)
        put (b_dct b)
        put (b_root b)
    get = getWord8 >>= \ v -> case v of
        0 -> do
            _bct <- get
            _dct <- get
            _root <- get
            return $! BranchSet0 
                { b_bct = _bct
                , b_dct = _dct
                , b_root = _root
                }
        _ -> fail $ branchErr $ "unrecognized BranchSet version " ++ show v


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


