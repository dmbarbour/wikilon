{-# LANGUAGE DeriveDataTypeable #-}
-- | Support for a reverse token lookup index associated with a 
-- dictionary. Find where any token or word is used from the whole
-- dictionary. 
module Wikilon.DictRLU
    ( DictRLU
    , rluCreate
    , rluUpdate
    , rluTokenClients
    , rluWordClients
    , rluTransitiveWordClientsList
    ) where

import Control.Applicative
import Control.Arrow (first, second)
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as L
import Data.Typeable (Typeable)
import Database.VCache
import Data.VCache.Trie (Trie)
import qualified Data.VCache.Trie as Trie
import Wikilon.Word
import Wikilon.Token
import Wikilon.Dict (DictDiff, Diff(..))
import Wikilon.AODef

-- | The reverse lookup index allows us to quickly find clients of
-- any word or token in the dictionary. It must be kept up to date
-- with the dictionary if it's to be used for invalidating cached
-- computations and similar. 
newtype DictRLU = RLU (Trie (Trie ()))
    deriving (Typeable)

rluCreate :: VSpace -> DictRLU
rluCreate = RLU . Trie.empty

-- | Find all direct clients of a token.
rluTokenClients :: DictRLU -> Token -> [Word]
rluTokenClients (RLU trie) (Token t) = 
    maybe [] (fmap Word . Trie.keys) (Trie.lookup t trie)

-- | Find all direct clients of a word.
rluWordClients :: DictRLU -> Word -> [Word]
rluWordClients ix = rluTokenClients ix . wtok

-- token associated with a word
wtok :: Word -> Token
wtok = Token . BS.cons '%' . unWord

-- | Find transitive clients of a list of words, ordered such that a
-- well defined word appears before any of its clients. This is useful
-- for updating or invalidating cache.
rluTransitiveWordClientsList :: DictRLU -> [Word] -> [Word]
rluTransitiveWordClientsList rlu = L.reverse . accum mempty mempty where
    -- accum (visited) (cycle prevention)
    accum _ _ [] = []
    accum v c ws@(w:ws') = 
        if Set.member w v then accum v c ws' else -- already listed w
        let lClients = L.filter (`Set.notMember` v) (rluWordClients rlu w) in
        let bAddWord = L.null lClients || Set.member w c in
        if bAddWord then w : accum (Set.insert w v) (Set.delete w c) ws'
                    else accum v (Set.insert w c) (lClients ++ ws)

-- | update based on a map diff. Assumes `InL` is the new map
-- and `InR` is the old one.
rluUpdate :: DictDiff -> DictRLU -> DictRLU
rluUpdate = flip adjRoot . rluDiff where
    -- update all tokens
    adjRoot (RLU t0) = RLU . L.foldl' updTok t0 . Map.toList
    -- update for specified token
    updTok tr (Token tok, (wi,wd)) = Trie.adjust adj tok tr where
        adj = fin . ins wi . del wd . fromMaybe noClients
        noClients = Trie.empty (Trie.trie_space tr)
    ins = Trie.insertList . fmap (flip (,) () . unWord)  -- insertions
    del = Trie.deleteList . fmap unWord                 -- deletions
    -- drop token from RLU if it has no clients
    fin t | Trie.null t = Nothing   
          | otherwise   = Just t
        
-- Map of Tokens to (clients added, clients removed)
type RLUDiff = Map Token ([Word],[Word])

-- given a difference in definition, find the difference in
-- which tokens are used on each side.
tokDiff :: Diff AODef -> ([Token],[Token])
tokDiff (InL a) = (L.nub (aodefTokens a), []) -- tokens added
tokDiff (InR b) = ([], L.nub (aodefTokens b)) -- tokens removed
tokDiff (Diff a b) = (insTok, delTok) where
    lTok = L.nub (aodefTokens a)
    rTok = L.nub (aodefTokens b)
    insTok = L.filter (`L.notElem` rTok) lTok
    delTok = L.filter (`L.notElem` lTok) rTok
   
-- given an update to the dictionary, compute an update to the RLU.
rluDiff :: DictDiff -> RLUDiff
rluDiff = L.foldl' (flip accum) Map.empty where
    accum (w,d) = addClients w lIns . delClients w lDel where
        (lIns, lDel) = tokDiff d
    addClients = flip . L.foldl' . addTokClient
    delClients = flip . L.foldl' . delTokClient
    addTokClient = adjTokClient . first . (:)
    delTokClient = adjTokClient . second . (:)
    adjTokClient f m t = Map.alter adj t m where
        adj = Just . f . fromMaybe mempty 

instance VCacheable DictRLU where
    put (RLU t) = putWord8 1 >> put t
    get = getWord8 >>= \ v -> case v of
        1 -> RLU <$> get
        _ -> fail "Wikilon.DictRLU - unknown DictRLU version"

