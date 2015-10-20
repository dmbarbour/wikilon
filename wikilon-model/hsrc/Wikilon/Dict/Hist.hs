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
import Data.VCache.LoB (LoB) 
import qualified Data.VCache.LoB as LoB
import Wikilon.Dict (Dict)
import Wikilon.Time 

-- How many historical samples to keep? We'll perform one decay
-- pass whenever we notice we've surpassed this number. This is
-- a tuning parameter, currently hard-coded. This should be good
-- enough for most use cases of Wikilon.
maxHistorySamples :: Int
maxHistorySamples = 1000

createHRep :: VSpace -> HRep
createHRep = flip LoB.empty 32

-- | The 'DictHist' is a representation for [(T,Dict)] where each 
-- pair reads: "we had the dictionary before the given time".
newtype DictHist = DictHist HRep
    deriving (Typeable)
type HRep = LoB (T, Dict)

hrep :: DictHist -> HRep
hrep (DictHist l) = l

createDictHist :: VSpace -> DictHist
createDictHist = DictHist . createHRep 

readDictHist :: DictHist -> [(T,Dict)]
readDictHist = LoB.toList . hrep

updateDictHist :: (T,Dict) -> DictHist -> DictHist
updateDictHist p = DictHist . LoB.cons p . hrep . decayHistIfFull

decayHistIfFull :: DictHist -> DictHist
decayHistIfFull h | fullHist h = decayHist h
                  | otherwise  = h

fullHist :: DictHist -> Bool
fullHist = (>= maxHistorySamples) . LoB.length . hrep

decayHist :: DictHist -> DictHist
decayHist (DictHist l) = DictHist (decay l) where
    rebuild = L.foldl' (flip LoB.cons) (createHRep (LoB.lob_space l))
    decay = rebuild . decimate . L.reverse . LoB.toList
    decimate = mconcat . fmap decayGroup . groupsOf 10 

-- take groups of at least one element
groupsOf :: Int -> [a] -> [[a]]
groupsOf n (x:xs) = (x : L.take (n-1) xs) : groupsOf n (L.drop (n-1) xs)
groupsOf _ _ = []

-- Eliminate at one element from a small group, favoring
-- elimination of the element that produces the smallest
-- time gap (i.e. to gradually move towards more uniformly
-- wide gaps between time samples).
--
-- This is applied to each group of ten samples.
decayGroup :: [(T,a)] -> [(T,a)]
decayGroup lst = lst' where
    timeGap a b = abs $ fst a `diffTime` fst b
    minTimeGap = (L.foldl' min maxBound . gaps timeGap) lst
    lst' = elim lst 
    elim (x1 : xs@(x2:_)) =
        if (timeGap x1 x2) == minTimeGap 
            then xs -- drop x1
            else x1 : elim xs
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
