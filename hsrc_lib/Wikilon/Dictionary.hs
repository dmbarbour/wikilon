{-# LANGUAGE PatternGuards, ViewPatterns #-}

-- | The Awelon Object (AO) dictionary is a primary data structure
-- of Wikilon. AO words form functions, modules, tests, documentation,
-- directives, web-applications, and wiki pages. But, essentially, the
-- content of each word is just some AO code. Documentation is modeled
-- as a word that computes a document.
--
-- This module describes the dictionary database and transactions,
-- and supplies a type to enforce a few useful properties:
--
--   * every word parses
--   * words are fully defined
--   * defined words are acyclic
--   * distinguish define vs. update
-- 
-- Additionally, the dictionary supports *reactive* observation. A
-- developer can track which words are updated based on any change
-- in the dictionary. This will make it easier to automatically run
-- tests whose behavior may have changed, or alert users when the 
-- dictionary is shifting beneath them.
--
-- Wikilon may enforce higher level policies to keep the dictionary
-- hygienic, e.g. validating that words typecheck or tests pass.
--
-- The dictionary model also tracks some metadata, e.g. when the 
-- transactions are applied.
-- 
module Wikilon.Dictionary
    ( DictTX(..), DictUpd(..), DTXMeta(..)
    , UpdateMap, RenameMap, AnnoWords
    , mapDictUpd

    , dictDecayParams

    -- 
    , renameDTX, mergeDTX
    , heuristicDTX, mergeHeuristicDTX
    , mergeRenameMaps
    
    ) where

import Control.Arrow ((***))
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Wikilon.Time
import Wikilon.Database
import Wikilon.AO


type DictDB = DB DictTX

-- | A dictionary has a list of transactions and a valid state.
--
-- 'Valid' here means that words parse, are acyclic, and that no
-- dependencies are undefined. However, there may be other errors in
-- the dictionary, such as words that will not pass a typecheck, or 
-- tests that fail. Higher layers may guard the dictionary against
-- transactions that leave the dictionary unhealthy.
--
data Dict = Dict
    { dict_db :: !DictDB             -- ^ 
    , dict_st :: !DictST             -- ^ current state of database
    }

readDictDB :: Dict -> DictDB
readDictDB = dict_db

readDictST :: Dict -> DictST
readDictST = dict_st

-- | A dictionary state supports up-to-date views of the dictionary,
-- compilation of words to Awelon bytecode, and similar features.
newtype DictST = DictST { dict_wordMap :: M.Map Word DictEnt }

-- | Each word has a definition and a reverse-lookup model.
data DictEnt = DictEnt !AO_Code !RLU

-- | Our reverse lookup is a set of words that should be re-evaluated
-- (transitively) after an update to a given word. 
--
-- RLU should not directly include transitive dependencies. For example:
--
--    @define bar ...
--    @define baz x bar
--    @define foo bar baz
--
-- In this case, both baz and foo depend on bar. However, since foo 
-- also depends on baz, we don't want to include foo in the reverse 
-- lookup of bar. This helps keep our dependency sets smaller, with
-- less redundant information in memory, and simplifies topological
-- sort of reactive updates, avoiding redundant computations.
-- 
type RLU = S.Set Word

type RenameMap = M.Map Word Word
type UpdateMap = M.Map Word DictUpd

-- | A transaction on a dictionary has some metadata and a few words
-- to update. Each word can have at most one update. Note that rename
-- applies strictly to words defined *before* this transaction.
data DictTX = DictTX
    { dtx_meta    :: !DTXMeta
    , dtx_rename  :: !RenameMap
    , dtx_update  :: !UpdateMap
    }

-- | Dictionaries have a simple set of update operations:
--
--    define a new word
--    update an existing word
--    append an existing word
--    delete an existing word
--
-- A design constraint is that transactions merges be associative
-- without losing information about the final database state, and
-- without peeking at the database state (i.e. blind merge).
--
-- Append is experimental. It's unclear whether there is a strong
-- use-case for append. I hope it might prove useful for modeling
-- long-lived objects embedded within the dictionary.
--
data DictUpd 
    = Define AO_Code    -- @define word code
    | Update AO_Code    -- @update word code
    | Append AO_Code    -- @append word code
    | Delete            -- @delete word

mapDictUpd :: (AO_Code -> AO_Code) -> DictUpd -> DictUpd
mapDictUpd f (Define ao) = Define (f ao)
mapDictUpd f (Update ao) = Update (f ao)
mapDictUpd f (Append ao) = Append (f ao)
mapDictUpd _ Delete = Delete

isDef :: DictUpd -> Bool
isDef (Define _) = True
isDef _ = False

-- | Transaction metadata includes a little information about who,
-- when, where, why, and how many transactions have been performed.
--
-- Time gets special handling. But other metadata will have a simple
-- representation: a set of words. These words can represent some 
-- ad-hoc content, e.g. `user:dave` might be a describing a user
-- who performed the edit. And `project:awelon` might describe the
-- project associated with the edit. These words may be renamed or
-- deleted.
-- 
data DTXMeta = DTXMeta
    { dtx_tm_ini :: {-# UNPACK #-} !T -- when (start)
    , dtx_tm_fin :: {-# UNPACK #-} !T -- when (last merged)
    , dtx_count  :: {-# UNPACK #-} !Int -- count (1 for initial transaction)
    , dtx_anno   :: !AnnoWords -- words for who, where, why, etc.
    }
type AnnoWords = S.Set Word

-- | Applying rename operations is one of the more challenging 
-- features for dictionary transactions. We must:
--
--   * rename words in our annotations
--   * rename mentions of a word in our dictionary
--   * subtract newly defined words from our rename map
--   * carefully compose rename maps
-- 
renameDTX :: RenameMap -> DictTX -> DictTX
renameDTX renameMap dtx =
    if M.null renameMap then dtx else
    let rn w = maybe w id $ M.lookup w renameMap in
    -- rename the annotations
    let meta = dtx_meta dtx in
    let anno' = S.map rn (dtx_anno meta) in
    let meta' = meta { dtx_anno = anno' } in
    -- rename words within the updates!
    let rnu  = rn *** mapDictUpd (aoMapWords rn) in
    let update' = M.fromList $ fmap rnu $ M.toList (dtx_update dtx) in
    -- filter newly defined words from renameMap
    let newWords = M.keys $ M.filter isDef update' in
    let renameMap' = L.foldr M.delete renameMap newWords in
    -- compose a new rename function.
    let rename' = mergeRenameMaps (dtx_rename dtx) renameMap' in
    -- generate the new transaction
    DictTX { dtx_meta = meta'
           , dtx_rename = rename'
           , dtx_update = update'
           }

-- | mergeRenameMaps old new; combine two rename maps!
-- 
-- The basic idea here is that we can collapse a sequence of renames.
-- For example, if we yesterday renamed foo to bar, and today we rename
-- bar to baz, we can collapse this to a single rename from foo to baz.
--
--    old map:  fromList [(foo,bar)]
--    new map:  fromList [(bar,baz)]
--    result:   fromList [(foo,baz)]
--
-- Of course, maps are only indexed on the first element. The only
-- way to use the index here is to search the new map for 'bar'.
-- 
-- The converse, where we rename 'foo' in the new map, should be 
-- impossible, since 'foo' does not exist after we've renamed it
-- to 'bar'. We won't check for that here, however; it's up to
-- a higher level policy to test that transactions are valid.
--
-- Also, it's possible that yesterday we renamed foo to bar, and
-- today we renamed bar back to foo (changed our mind!). In that
-- case, we want to eliminate the identity rename (foo,foo).
--
mergeRenameMaps :: RenameMap -> RenameMap -> RenameMap
mergeRenameMaps old new =
    if M.null new then old else
    L.foldr mrn1 new (M.toList old)

mrn1 :: (Word,Word) -> RenameMap -> RenameMap
mrn1 (foo,bar) newMap =
    case M.lookup bar newMap of
        Nothing ->  M.insert foo bar newMap
        Just baz ->
            let m' = M.delete bar newMap in
            if (foo == baz) then m' else
            M.insert foo baz m'

-- | mergeDTX old new; compose two transactions into a third.
--
-- To merge two transactions, we'll first apply any name updates,
-- then combine the updates and annotations. A word that is both
-- defined and deleted will be fully removed at this point, and 
-- also removed from the set of annotations. 
mergeDTX :: DictTX -> DictTX -> DictTX
mergeDTX old new = fst $ mergeHeuristicDTX old new

-- | For Wikilon, I'm aiming for the following characteristics:
--
--   1) It's preferable that we see many values for each word.
--      Therefore: subtract 1 for each overlapping word in update.
--
--   2) It's preferable that we combine updates from the same user.
--      Therefore: subtract 1 for each difference in annotations
--                 add 1 for each overlap in annotations
--
--   3) It's preferable that history be evenly sampled over real time.
--      Therefore: subtract 1 for every 12 hours in scope of merge
--
-- A typical score will be negative. We'll favor the highest score, 
-- i.e. the merge that's least obviously problematic. Due to the 
-- nature of the exponential decay model, even a weak tendency to
-- select decent merges should result in favorable long-term behavior.
--
-- We'll compute the heuristic AFTER applying renames.
heuristicDTX :: DictTX -> DictTX -> Int
heuristicDTX old new = snd $ mergeHeuristicDTX old new

-- heuristic, assuming that no rename is needed.
heuristicDTX' :: DictTX -> DictTX -> Int
heuristicDTX' old new = score where
    score = wordScore + annoScore + timeScore
    wordScore = 
        let m1 = dtx_update old in
        let m2 = dtx_update new in
        negate $ M.size $ M.intersection m1 m2
    annoScore =
        let s1 = dtx_anno (dtx_meta old) in
        let s2 = dtx_anno (dtx_meta new) in
        let sn = S.intersection s1 s2 in
        -- subtract overlap twice, but add thrice 
        (3 * S.size sn) - (S.size s1 + S.size s2)
    timeScore =
        let tm_ini = dtx_tm_ini (dtx_meta old) in
        let tm_fin = dtx_tm_fin (dtx_meta new) in
        let dt = tm_fin `diffTime` tm_ini in
        let p = dtToPicos dt in
        let h12 = 12 * picosInHour in
        negate $ fromIntegral $ p `div` h12

picosInHour :: Integer
picosInHour = picosPerSec * secsPerHour where
    picosPerSec = 1000 * 1000 * 1000 * 1000
    secsPerHour = 3600 
    
-- | Compute both a merged value and heuristic together, in the
-- format required by Wikilon.Database.
mergeHeuristicDTX :: DictTX -> DictTX -> (DictTX, Int)
mergeHeuristicDTX old new = (m, h) where
    old' = renameDTX (dtx_rename new) old
    new' = new { dtx_rename = M.empty }
    m = mergeDTX'     old' new'
    h = heuristicDTX' old' new'

-- | Default parameters for Wikilon.Database.
dictDecayParams :: Decay DictTX
dictDecayParams = Decay
    { db_maxlen = 1000
    , db_merge  = mergeHeuristicDTX
    , db_freq   = 8
    , db_keep   = 8
    }

-- same as mergeDTX, but assumes that the new transaction does not
-- rename anything, and thus that names are consistent.
mergeDTX' :: DictTX -> DictTX -> DictTX
mergeDTX' = error "TODO!"


{-

 old new = merged where
    rename' = dtx_rename old
    oldmeta = dtx_meta old
    newmeta = dtx_meta new
    tm_ini' = dtx_tm_ini oldmeta
    tm_fin' = dtx_tm_fin newmeta
    count'  = dtx_count old + dtx_count new
    
    
    anno' = (oldAnno `Set.union` newAnno) `Set.difference` lostWords
    allAnno = Set.union (dtx_anno oldMeta) (dtx_anno newMeta)
    lostWords = newWords oldAct `Set.difference` newWords actions'
    
    
    
    
    

    oldRN = renameDTX old (dtx_rename new) -- old + renaming
    rename' = dtx_rename oldRN -- rename maps already merged
    oldMeta = dtx_meta oldRN   -- old metadata
    oldAnno = dtx_anno oldMeta -- old annotations (after rename)
    oldAct  = dtx_update oldRN -- old actions
    newMeta = dtx_meta new
    newAnno = dtx_anno newMeta
    newAct  = dtx_update new
    
    upd' = mergeUpdates (dtx_updates oldRN) (dtx_updates new)
    


dictMergeFn :: DictTX -> DictTX -> (DictTX, Int)
dictMergeFn old new = (merged,score) where
    merged = DictTX meta' actions'
    oldMeta = dtx_meta old
    oldAct = dtx_actions old
    oldAnno = fmap (renameWith newAct) (dtx_anno oldMeta)
    newMeta = dtx_meta new
    newAct = dtx_actions new
    newAnno = dtx_anno newMeta
    
    count = dtx_count old + dtx_count new




-}


