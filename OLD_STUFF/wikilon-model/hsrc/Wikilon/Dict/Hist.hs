{-# LANGUAGE DeriveDataTypeable #-}
-- | A simple history model with exponential decay.
module Wikilon.Dict.Hist
    ( DictHist
    , createDictHist
    , updateDictHist
    , readDictHist
    ) where

import Control.Applicative
import Data.Monoid
import Data.Typeable (Typeable)
import qualified Data.List as L
import Database.VCache
import Wikilon.Dict (Dict)
import Wikilon.Time 

-- How many historical samples to keep? I'll perform one decay
-- pass whenever we notice we've surpassed this number. 
--
-- Users can increase effective histories by:
--
--  * modeling multi-versioned words within the dictionary
--  * modeling metadata tasks, todos, sessions, etc. in dictionary
--  * favoring monotonic command patterns for dictionary apps
--  * separate branches to tag versions and low-frequency updates
--
-- So I'm not too concerned about under-shooting a little, so 
-- long as we have enough history for convenience in cases like
-- recovering from vandalism and mining through history. If we
-- overshoot, the cost is some extra storage.
--
maxHistorySamples :: Int
maxHistorySamples = 180

-- | The 'DictHist' is a representation for [(T,Dict)] where each 
-- pair reads: "before time T we had dictionary Dict". Time will
-- decrease monotonically in the list.
--
-- The history is for archival and recovery purposes. Properties
-- and indices are not preserved, though they may be regenerated
-- by forking from a historical dictionary.
newtype DictHist = DictHist HRep
    deriving (Typeable)

-- At the moment the max history samples is small enough that it's
-- fine to simply record the entire list in one VCache node without
-- a fancy data structure. Users may always explicitly manage more
-- versions via hierarchical structures, tagging, etc.
type HRep = [(T, Dict)]

hrep :: DictHist -> HRep
hrep (DictHist l) = l

createDictHist :: VSpace -> DictHist
createDictHist = const $ DictHist []

readDictHist :: DictHist -> [(T,Dict)]
readDictHist = hrep

updateDictHist :: (T,Dict) -> DictHist -> DictHist
updateDictHist p = DictHist . (:) p . hrep . decayHistIfFull

decayHistIfFull :: DictHist -> DictHist
decayHistIfFull h | fullHist h = decayHist h
                  | otherwise  = h

fullHist :: DictHist -> Bool
fullHist = (>= maxHistorySamples) . L.length . hrep

decayHist :: DictHist -> DictHist
decayHist = DictHist . decimate . hrep where

-- decimate, in the literal sense of eliminating one tenth.
-- This is the basis for exponential decay. 
--
-- The strategy here is to destroy one in every group of ten
-- such that decimation is distributed fairly over the entire
-- history. Each group uses heuristics to decide which to kill.
decimate :: [(T,a)] -> [(T,a)]
decimate = mconcat . fmap decayGroup . groupsOf 10

-- take groups of at least one element
groupsOf :: Int -> [a] -> [[a]]
groupsOf = go . subtract 1 where
    go k (x:xs) = (x : L.take k xs) : go k (L.drop k xs)
    go _ _ = []

-- Eliminate one element from a small group. The element is selected
-- heuristically: (1) select the pair of elements in the group that
-- has the smallest age gap, (2) keep the younger of the pair.
-- 
-- This isn't optimal for generating a clean sampling distribution
-- (i.e. eliminating outliers and clusters). But it is good enough
-- when applied over multiple decay passes with implicit regrouping
-- between passes.
decayGroup :: [(T,a)] -> [(T,a)]
decayGroup lst = lst' where
    ageGap a b = abs $ fst a `diffTime` fst b
    minAgeGap = (L.foldl' min maxBound . gaps ageGap) lst
    lst' = elim lst 
    elim (x1 : xs@(x2 : xs')) =
        let bTargetGap = ageGap x1 x2 == minAgeGap in
        if not bTargetGap then x1 : elim xs else
        let x1_younger = fst x1 > fst x2 in
        let x_kept = if x1_younger then x1 else x2 in
        x_kept : xs'
    elim _ = [] 

gaps :: (a -> a -> b) -> [a] -> [b]
gaps fn lst = L.zipWith fn lst (L.drop 1 lst)

instance VCacheable DictHist where
    put (DictHist l) = putWord8 0 >> put l
    get = getWord8 >>= \ vn -> case vn of
        0 -> DictHist <$> get
        _ -> fail $ dictHistErr $ "unrecognized DictHist version"

dictHistErr :: String -> String
dictHistErr = (++) "Wikilon.Dict.Hist: "
