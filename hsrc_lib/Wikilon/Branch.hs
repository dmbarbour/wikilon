{-# LANGUAGE DeriveDataTypeable #-}
-- | Wikilon keeps a branching history for dictionaries. Branches are
-- named by simple unicode strings. Individually, each branch has a
-- dictionary and a history.
--
-- Branch names must be valid Word names, i.e. to have the same URL
-- friendly and text friendly constraints.
module Wikilon.Branch
    ( BranchSet
    , BranchName
    , Branch

    , empty
    , vspace
    , width
    , volume
    , lookup
    , lookup'
    , keys
    , toList
    , insert
    , delete
    , adjust

    , isValidBranchName

    , head
    , hist
    , update

    , decay

    , decayBranch
    , emptyBranch
    , branchSize

    , unsafeBranchSetAddr
    
    ) where

import Prelude hiding (lookup, head)
import Control.Monad
import qualified Data.ByteString.UTF8 as UTF8
import Data.Typeable (Typeable)
import Data.Word (Word64)
import qualified Data.List as L
import Database.VCache
import Data.VCache.LoB (LoB)
import qualified Data.VCache.LoB as LoB
import Data.VCache.Trie (Trie)
import qualified Data.VCache.Trie as Trie

import Wikilon.Dict (Dict)
import Wikilon.Dict.Word (isValidWord, Word(..))
import qualified Wikilon.Dict as Dict
import Wikilon.Time

-- | Branches are named in their BranchSet. These names are simple
-- unicode strings. These names are also used in branch histories.
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

-- | Lookup a branch by name, or return an empty branch
lookup' :: BranchName -> BranchSet -> Branch
lookup' n s = case lookup n s of
    Nothing -> emptyBranch (vspace s)
    Just b -> b

keys :: BranchSet -> [BranchName]
keys = Trie.keys . s_data

toList :: BranchSet -> [(BranchName, Branch)]
toList = Trie.toList . s_data

emptyBranch :: VSpace -> Branch
emptyBranch vc = Branch0 (Dict.empty vc) (LoB.empty vc 8)

-- | Insert a branch into the branch set.
insert :: BranchName -> Branch -> BranchSet -> BranchSet
insert n = adjust n . const . Just

-- | Test whether a BranchName is suitable for Wikilon.
isValidBranchName :: BranchName -> Bool
isValidBranchName = isValidWord . Word

-- | Delete a named branch from the branch set.
delete :: BranchName -> BranchSet -> BranchSet
delete n = adjust n (const Nothing)

-- | Adjust a branch in the branch set.
adjust :: BranchName -> (Maybe Branch -> Maybe Branch) -> BranchSet -> BranchSet
adjust n fn s0 = sf where
    sf = BSet0 { s_width = wf, s_volume = vf, s_data = df }
    wf = s_width s0 + wbf - wb0
    vf = s_volume s0 + vbf - vb0
    df = Trie.adjust (const mbf) n (s_data s0)
    mb0 = lookup n s0
    wb0 = maybe 0 (const 1) mb0
    vb0 = maybe 0 b_volume mb0
    mbf = fn mb0 
    wbf = maybe 0 (const 1) mbf
    vbf = maybe 0 b_volume mbf

b_volume :: Branch -> Int
b_volume = (1 +) . LoB.length . b_hist

-- | read the head version of a branch
head :: Branch -> Dict
head = b_head

-- | read the history for a branch. This history is generally incomplete,
-- subject to exponential decay models. This also excludes the head. The
-- history can be read as each (Time,Dict) pair saying that the dictionary
-- was used up until the given time.
hist :: Branch -> [(T, Dict)]
hist = LoB.toList . b_hist

-- | update the dictionary for a branch. This returns the original 
-- dictionary unless there is an actual change.
update :: (T,Dict) -> Branch -> Branch 
update (t,d) b = 
    let d0 = b_head b in
    if (d0 == d) then b else
    let h = b_hist b in
    let h' = LoB.cons (t,d0) h in
    Branch0 d h'

b_null :: Branch -> Bool
b_null b = Dict.null (b_head b) && LoB.null (b_hist b)

-- | construct an empty branch set
empty :: VSpace -> BranchSet
empty vc = BSet0 0 0 (Trie.empty vc)

-- | heuristic size for a branch 
branchSize :: Branch -> Int
branchSize b = szHd + szTl where
    szHd = if Dict.null (b_head b) then 0 else 1
    szTl = LoB.length (b_hist b)


-- | Apply exponential decay to a branch. This will roughly decimate
-- a branch's history (i.e. drop every tenth item), though will skip
-- the first ten entries if feasible (if there are more than ten). At
-- least one item is removed from the history.
--
-- regarding performance: for Wikilon I'm assuming that no particular
-- branch has enormous histories. This can be enforced on a per-branch
-- basis, e.g. by using branchSize and decaying individual branches when
-- they reach some threshold, in addition to limiting total wiki sizes.
decayBranch :: Branch -> Branch
decayBranch b = b' where
    b' = b { b_hist = h' }
    h' = _decayHist (b_head b) tgt0 (b_hist b)
    len = LoB.length (b_hist b)
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
-- has an empty dictionary and history. 
--
-- Thoughts: it might be necessary to shift this loop into a more
-- iterative method over time, if the number of branches is very
-- large.
decay :: BranchSet -> BranchSet
decay s0 = sf where
    sf = L.foldl' updSet s0 lBranches
    lBranches = Trie.keys (s_data s0)
    updSet s n = adjust n (>>= updBr) s
    updBr b =
        let b' = decayBranch b in
        if b_null b' then mzero else 
        return b'

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

-- | using for easy e-tags
unsafeBranchSetAddr :: BranchSet -> Word64
unsafeBranchSetAddr = Trie.unsafeTrieAddr . s_data


