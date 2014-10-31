{-# LANGUAGE TypeFamilies #-}
-- | A Wikilon database is a bounded length list of transactions. A
-- simple exponential decay model is applied [1], such that a flood
-- of updates doesn't erase interesting information about historical
-- conditions.
--
-- Note that this module describes the database model and structure,
-- not persistent storage thereof. 
--
-- [1] <http://awelonblue.wordpress.com/2014/10/08/logarithmic-history-v3/>
-- 
module Wikilon.Database
    ( Decay(..), Merged(..)
    , decay, decayDiff
    ) where

import Control.Parallel.Strategies (parMap, rseq)
import Data.Maybe (catMaybes)
import qualified Data.List as L


-- | We tune a database with a set of exponential decay parameters.
--
--    decay_merge: returns a merged transaction and a heuristic for
--      'quality' of the merge. The order of arguments to merge 
--      is `merge old new`. We favor merges of greater quality.
--      Merges should be associative and result in the same final
--      observable database state (for significant observations).
--
--    decay_freq: determines granularity and frequency of collections.
--       minimum: 2. reasonable default: 8. Affects performance and
--       how much information is preserved across a decay phase.
--
--    decay_keep: mixed mode with windowed history; keep K recent
--       elements. Optimally a multiple of frequency
--
-- Note: half-life is a linear function of database size.
--
data Decay tx = Decay
    { decay_merge  :: tx -> tx -> (tx, Int)
    , decay_freq   :: {-# UNPACK #-} !Int
    , decay_keep   :: {-# UNPACK #-} !Int
    }

-- | The function `decayDiff` will return a list of merge records
-- together with the final result. Each merge is recorded as a 
data Merged tx = Merged 
    { merge_newer  :: tx
    , merge_older  :: tx
    , merge_result :: tx
    , merge_score  :: Int
    } deriving (Show)

    
-- | A database is, trivially, a list of transactions, with recent
-- transactions near the head of the list. The user may decay this
-- list as often as he or she wishes, with each decay removing a 
-- fraction of items depending on the decay_freq setting. 
--
-- Half life depends on how large you allow the database to grow
-- before collecting. It is independent of frequency, though a 
-- lower frequency will result in more decay in each step.
--
-- To minimize alignment issues, decay groups are collected from
-- the rear of the list (oldest transactions first). Thus, only
-- transactions nearest the head are impacted if a group is not
-- completely filled.
decay :: Decay tx -> [tx] -> [tx]
decay dp txs = fst $ decayDiff dp txs

-- | In some cases, it might be useful to have a quick summary of
-- the merges performed during decay.
decayDiff :: Decay tx -> [tx] -> ([tx],[Merged tx])
decayDiff dp txs | (decay_freq dp < 2) = (txs,[])
decayDiff dp txs = (txs',diff) where
    (txsKeep,txsDecay) = L.splitAt (decay_keep dp) txs
    lgs = groupsOf (decay_freq dp) (L.reverse txsDecay)
    dgs = parMap rseq (decayGroup (decay_merge dp)) lgs
    diff = L.reverse $ catMaybes $ fmap snd dgs
    txsDecay' = L.reverse $ L.concatMap fst dgs
    txs' = txsKeep ++ txsDecay'

-- take as many groups of a given size as possible.
-- the last group might be smaller.
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = group : groupsOf n xs' where
    (group, xs') = L.splitAt n xs

-- decayGroup operates on *short* lists, e.g. of ten items. The list
-- size is decided by db_freq. At the moment, the lists are reverse
-- ordered, i.e. older elements first in list. We'll try to merge 
-- two elements in each group, if possible.
--
-- This is a relatively sophisticated function. We're searching for
-- an optimal candidate according to a heuristic merge function. 
decayGroup :: (tx -> tx -> (tx,Int)) -> [tx] -> ([tx], Maybe (Merged tx))
decayGroup mergeFn (t1:t2:txs') = 
    let (t1m2,caScore) = mergeFn t1 t2 in
    let ca0 = (t1m2:txs') in
    let mr0 = Merged { merge_older = t1
                     , merge_newer = t2
                     , merge_result = t1m2
                     , merge_score = caScore
                     }
    in
    let (caf,mrf) = decayGroupC ca0 mr0 mergeFn [t1] (t2:txs') in
    (caf,Just mrf)
decayGroup _ txs = (txs,Nothing) -- not enough in group to decay

-- decayGroup with a candidate! We'll stick to the candidate unless
-- we can find a strictly better merge option later on. This will
-- therefore favor decay closer to root in case of a tie.
decayGroupC :: [tx] -> Merged tx -> (tx -> tx -> (tx,Int)) -> [tx] -> [tx] -> ([tx],Merged tx)
decayGroupC ca rec mergeFn rt1s (t2:t3:txs) =
    let (t2m3,t2m3score) = mergeFn t2 t3 in
    let skip = decayGroupC ca rec mergeFn (t2:rt1s) (t3:txs) in
    if (merge_score rec >= t2m3score) then skip else
    let rec' = Merged { merge_older = t2
                      , merge_newer = t3
                      , merge_result = t2m3
                      , merge_score = t2m3score 
                      }
    in
    let ca' = (L.reverse rt1s) ++ (t2m3:txs) in
    decayGroupC ca' rec' mergeFn (t2:rt1s) (t3:txs)
decayGroupC ca rec _mrg _rt1s _txs = (ca,rec) -- all merges tested.

