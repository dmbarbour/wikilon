{-# LANGUAGE DeriveDataTypeable #-}
-- | Wikilon keeps a branching history for dictionaries. Branches are
-- named by simple unicode strings. Individually, each branch has a
-- dictionary and a history.
--
-- Note: Branches may need a fair amount of associated metadata, for
-- access control and geneology and related purposes. I'm not sure
-- where this data should go. But I don't believe it should go here.
--
-- Todo: delete specific versions from a branch's history
module Wikilon.Branch
    ( BranchSet
    , BranchName
    , Branch

    , empty
    , vspace
    , width
    , volume
    , lookup
    , insert
    , delete
    , adjust

    , head
    , hist
    , update

    , decay
    , decayBranch
    
    ) where

import Prelude hiding (lookup, head)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as BS
import Data.Typeable (Typeable)
import qualified Data.List as L
import Database.VCache
import Data.VCache.LoB (LoB)
import qualified Data.VCache.LoB as LoB
import Data.VCache.Trie (Trie)
import qualified Data.VCache.Trie as Trie

import Wikilon.Dict (Dict)
import qualified Wikilon.Dict as Dict
import Wikilon.Time

-- | Branches are named in their BranchSet. These names are simple
-- strings. These names are also used in branch histories.
type BranchName = UTF8.ByteString

-- | a BranchSet is a collection of named branches, plus information
-- to track total size of the branch set. 
type BranchSet = BSet0

-- versioned branchset model
data BSet0 = BSet0
    { s_width    :: {-# UNPACK #-} !Int
    , s_volume   :: {-# UNPACK #-} !Int
    , s_data     :: !(Trie Branch)
    } deriving (Typeable, Eq, Show)

-- | Every Branch includes a dictionary history and a little metadata.
type Branch = Branch0

-- versioned branching model
data Branch0 = Branch0
    { b_head    :: !Dict            -- head dictionary
    , b_hist    :: !(LoB (T, Dict)) -- history of dictionary
    } deriving (Typeable, Eq)

-- | VCache space associated with BranchSet
vspace :: BranchSet -> VSpace
vspace = Trie.trie_space . s_data

-- | How many active branches?
width :: BranchSet -> Int
width = s_width

-- | How many dictionary samples? 
volume :: BranchSet -> Int
volume = s_volume

-- | Lookup a branch by name in a branch set. 
lookup :: BranchName -> BranchSet -> Maybe Branch
lookup n = Trie.lookupc CacheMode0 n . s_data

b_empty :: VSpace -> Branch
b_empty vc = Branch0 (Dict.empty vc) (LoB.empty 16 vc)

-- | Insert a branch into the branch set.
insert :: BranchName -> Branch -> BranchSet -> BranchSet
insert n = adjust n . const . Just

-- | Adjust a branch in the branch set.
adjust :: BranchName -> (Maybe Branch -> Maybe Branch) -> BranchSet -> BranchSet
adjust n fn s0 = sf where
    sf = BSet0 { s_width = wf, s_volume = vf, s_data = df }
    wf = s_width s0 + wbf - wb0
    vf = s_volume s0 + vbf - vb0
    df = Trie.adjust (const mbf) (s_data s0)
    mb0 = lookup n s0
    wb0 = maybe 0 (const 1) mb0
    vb0 = maybe 0 b_volume mb0
    mbf = fn mb0 
    wbf = maybe 0 (const 1) mbf
    vbf = maybe 0 b_volume mbf

b_volume :: Branch -> Int
b_volume = (1 +) . LoB.length . deref' . b_hist

-- | read the head version of a branch
head :: Branch -> Dict
head = b_head

-- | read the history for a branch. This history is generally incomplete,
-- subject to exponential decay models. This also excludes the head. The
-- history can be read as each (Time,Dict) pair saying that the dictionary
-- was used up until the given time.
hist :: Branch -> [(T, Dict)]
hist = LoB.toList . deref' . b_hist

-- | update the dictionary for a branch. This returns the original 
-- dictionary unless there is an actual change.
b_update :: (T,Dict) -> Branch -> Branch 
b_update (t,d) b = 
    let d0 = b_head b in
    if (d0 == d) then b else
    let h = deref' (b_hist b) in
    let h' = LoB.cons (t,d0) h in
    let hist' = vrefc CacheMode0 (b_space d0) h' in
    Branch0 d hist'

b_space :: Branch -> VSpace
b_space = Dict.dict_space . b_head

-- | construct an empty branch set
empty :: VSpace -> BranchSet
empty vc = BSet0 0 0 (Trie.empty vc)

-- | Apply exponential decay to a branch. This will roughly decimate
-- a branch's history (i.e. drop every tenth item), though will skip
-- the first ten entries if feasible (if there are more than ten). At
-- least one item is removed from the history.
decayBranch :: Branch -> Branch
decayBranch b = b' where
    b' = b { b_hist = vref' (b_space b) h' }
    h = deref' (b_hist b) 
    h' = _decayHist (b_head b) tgt0
    len = LoB.length h
    tgt0 = if len < 10 then (len - 1) else (9 + (len `mod` 10))

_decayHist :: Dict -> Int -> LoB (T, Dict) -> LoB (T, Dict)
_decayHist d n l = case LoB.uncons l of
    Nothing -> l
    Just (e, l') ->
        if (d == snd e) then _decayHist d n l' else -- collapse d,e (no change)
        if (n <= 0) then _decayHist d 9 l' else -- drop e (due to decay)
        LoB.cons e $ _decayHist (snd e) (n-1) l' -- keep e

-- | apply an exponential decay function to all the branches of the
-- branch set. A branch is fully removed from the branch set if it
-- has an empty dictionary and no history. 
decay :: BranchSet -> BranchSet
decay s0 = sf where
    branchList = Trie.keys (s_data s0) 
    sf = error "TODO"

instance VCacheable Branch0 where
    put b = do 
        putWord8 0 -- version
        put (b_head b)
        put (b_hist b)
    get = getWord8 >>= \ v -> case v of
        0 -> do
            _head <- get
            _hist <- get
            return $! Branch0
                { b_head = _head
                , b_hist = _hist
                }
        _ -> fail $ branchErr $ "unrecognized version " ++ show v

instance VCacheable BSet0 where
    put s = do
        putWord8 0 -- version
        put (s_width s)
        put (s_volume s)
        put (s_data s)
    get = getWord8 >>= \ v -> case v of
        0 -> do
            _width <- get
            _volume <- get
            _data <- get
            return $! BSet0 
                { s_width = _width
                , s_volume = _volume
                , s_data = _data
                }
        _ -> fail $ branchSetErr $ "unrecognized version " ++ show v

branchErr :: String -> String
branchErr = (++) "Wikilon.Branch (Branch): "

branchSetErr :: String -> String
branchSetErr = (++) "Wikilon.Branch (BranchSet): "

