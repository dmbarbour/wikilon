{-# LANGUAGE TypeFamilies #-}
-- | A Wikilon database consists of a bounded length sequence of
-- transactions. A simple exponential decay model is applied [1], such
-- that a flood of updates doesn't erase much interesting information 
-- about historical conditions. Databases may be efficiently forked
-- and synchronized (including their histories).
--
-- Note that this module describes the database model and structure,
-- not persistent storage thereof. 
--
-- [1] <http://awelonblue.wordpress.com/2014/10/08/logarithmic-history-v3/>
-- 
module Wikilon.Database
    ( DB, Decay(..), DBDiff(..)
    , validDecay
    , dbRead, dbLen, dbDecay
    , dbKeep, dbFreq, dbMergeFn, dbMaxLen
    , dbSaturated
    , emptyDB, insertDB, insertManyDB
    , forceDecayDB
    ) where

import Control.Parallel.Strategies (parMap, rseq)
import Data.Monoid
import qualified Data.List as L

-- | A Database is a bounded-length sequence of transactions with
-- a uniform, batch-processed, exponential decay model. 
data DB tx = DB
    { db_content :: [tx] -- the sequence of transactions
    , db_decay   :: {-# UNPACK #-} !(Decay tx)
    , db_length  :: {-# UNPACK #-} !Int -- cached length
    }

-- show instance (e.g. for debugging)
instance (Show tx) => Show (DB tx) where
    showsPrec _ db = showString "DB " . shows (dbRead db)

-- | read the full database contents
dbRead :: DB tx -> [tx]
dbRead = db_content 

-- | read the length of the database (cached)
dbLen :: DB tx -> Int
dbLen = db_length

-- | read the database decay parameters
dbDecay :: DB tx -> Decay tx
dbDecay = db_decay

-- | We tune a database with a set of exponential decay parameters.
--
--    db_maxlen: how large may this database grow, e.g. 1000.
--       A larger database has a longer half-life. Optimally a
--       multiple of frequency.
--
--    db_merge: returns a merged transaction and a heuristic for
--      'quality' of the merge. The order of arguments to merge 
--      is `merge old new`. We favor merges of greater quality.
--      Merges should be associative and result in the same final
--      observable database state (for significant observations).
--
--    db_freq: determines granularity and frequency of collections.
--       minimum: 2. reasonable default: 8. Affects performance and
--       how much information is preserved across a decay phase.
--
--    db_keep: mixed mode with windowed history; keep last K elements
--       Optimally a multiple of frequency, and smaller than maxlen.
--
data Decay tx = Decay
    { db_maxlen :: {-# UNPACK #-} !Int
    , db_merge  :: tx -> tx -> (tx, Int)
    , db_freq   :: {-# UNPACK #-} !Int
    , db_keep   :: {-# UNPACK #-} !Int
    }

validDecay :: Decay tx -> Bool
validDecay db = okayMaxLen && okayFreq && okayKeep where
    okayMaxLen = db_maxlen db >= (db_freq db + db_keep db)
    okayFreq = db_freq db >= 2
    okayKeep = db_keep db >= 0 

-- | test whether next insert requires decay phase
dbSaturated :: DB tx -> Bool
dbSaturated db = (dbLen db >= dbMaxLen db)

dbMaxLen :: DB tx -> Int
dbMaxLen = db_maxlen . db_decay

dbKeep :: DB tx -> Int
dbKeep = db_keep . db_decay

dbFreq :: DB tx -> Int
dbFreq = db_freq . db_decay

dbMergeFn :: DB tx -> (tx -> tx -> (tx,Int))
dbMergeFn = db_merge . db_decay


-- | For persistence and other purposes, it can be convenient to gain
-- a quick summary about what has changed in a database. We'll model
-- this as a pair of lists.
data DBDiff tx = DBDiff
    { db_added   :: [tx]
    , db_removed :: [tx]
    } deriving (Show)

instance Monoid (DBDiff tx) where
    mempty = DBDiff [] []
    mappend (DBDiff aL rL) (DBDiff aR rR) = DBDiff (aL++aR) (rL++rR)

-- | Create an empty database with the given decay parameters.
emptyDB :: Decay tx -> DB tx
emptyDB dp | validDecay dp = DB [] dp 0
           | otherwise = error "attempt to create database with invalid decay parameters"

-- | Add many transactions, preserving order. E.g. if we add
-- [1..10], then 10 is the oldest of these transactions.
insertManyDB :: DB tx -> [tx] -> (DB tx, DBDiff tx)
insertManyDB db0 txs = L.foldr ins (db0,mempty) txs where
    ins tx (db,diffR) =
        let (db',diffL) = insertDB db tx in
        let diff = diffL `mappend` diffR in
        (db', diff)

-- | Add transaction to the database. 
insertDB :: DB tx -> tx -> (DB tx, DBDiff tx)
insertDB db tx | dbSaturated db = insDecay db tx
               | otherwise = insEasy db tx

-- there's room left; just add the transaction.
insEasy :: DB tx -> tx -> (DB tx, DBDiff tx)
insEasy db tx =
    let content' = tx : db_content db in
    let len' = 1 + db_length db in
    let db' = db { db_content = content', db_length = len' } in
    let diff = mempty { db_added = [tx] } in
    (db',diff)

-- make some room, then try to insert again
insDecay :: DB tx -> tx -> (DB tx, DBDiff tx)
insDecay db tx =
    let (db',diffDecay) = forceDecayDB db in
    let (dbIns,diffIns) = insertDB db' tx in
    (dbIns, diffIns `mappend` diffDecay)

-- | Forcibly decay the database by one pass.
forceDecayDB :: DB tx -> (DB tx, DBDiff tx)
forceDecayDB db = (db',diff) where
    (txsKeep,txsDecay) = L.splitAt (dbKeep db) (db_content db)
    lgs = groupsOf (dbFreq db) (L.reverse txsDecay)
    dgs = parMap rseq (decayGroup (dbMergeFn db)) lgs
    diff = mconcat $ L.reverse $ fmap snd dgs
    txsDecay' = L.reverse $ L.concatMap fst dgs
    content' = txsKeep ++ txsDecay'
    length' = L.length content'
    db' = DB { db_content = content'
             , db_length = length'
             , db_decay = (db_decay db)
             }

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
decayGroup :: (tx -> tx -> (tx,Int)) -> [tx] -> ([tx],DBDiff tx)
decayGroup mergeFn (t1:t2:txs') = 
    let (t1m2,caScore) = mergeFn t1 t2 in
    let ca = ((t1m2:txs'),DBDiff [t1m2] [t2,t1]) in
    decayGroupC ca caScore mergeFn [t1] (t2:txs')
decayGroup _ txs = (txs,mempty) -- not enough in group to decay

-- decayGroup with a candidate! We'll stick to the candidate unless
-- we can find a strictly better merge option later on. This will
-- therefore favor decay closer to root in case of a tie.
decayGroupC :: ([tx],DBDiff tx) -> Int -> (tx -> tx -> (tx,Int)) -> [tx] -> [tx] -> ([tx],DBDiff tx)
decayGroupC ca best mrg rt1s (t2:t3:txs) =
    let skip = decayGroupC ca best mrg (t2:rt1s) (t3:txs) in
    let (t2m3,score) = mrg t2 t3 in
    if (score < best) then skip else
    let g'    = L.reverse rt1s ++ (t2m3:txs) in
    let diff' = DBDiff [t2m3] [t3,t2] in
    let ca'   = (g',diff') in
    decayGroupC ca' score mrg (t2:rt1s) (t3:txs) -- t2m3 merge is new candidate
decayGroupC ca _best _mrg _rt1s _txs = ca -- all merges tested.

