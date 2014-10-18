{-# LANGUAGE PatternGuards, ViewPatterns #-}

-- | The Awelon Object (AO) dictionary is a primary data structure
-- of Wikilon. AO words form functions, modules, tests, documentation,
-- directives, web-applications, and wiki pages. But, essentially, the
-- content of each word is just some AO code. Documentation is modeled
-- as a word that computes a document.
--
-- This module describes the dictionary database and transactions.
-- The database enforces a few useful properties:
--
--   * every word parses
--   * words are fully defined
--   * defined words are acyclic
--   * distinguish define vs. update
--
-- In addition to a set of words, a dictionary tracks some useful
-- metadata about when changes were made, potentially who made them
-- and for which projects, etc.
-- 
module Wikilon.Dictionary
    ( DictTX(..), DTXMeta(..), DictUpd(..)
    , UpdateMap, RenameMap, AnnoWords
    , renameDTX, deepRenameDTX
    , mergeDTX
    , heuristicDTX, mergeHeuristicDTX
    , mergeRenameMaps
    , mapDictUpd
    

    , dictDecayParams
    
    ) where

import Control.Arrow ((***))
import qualified Data.List as L
import qualified Wikilon.WordMap as WM
import qualified Wikilon.WordSet as WS

import Wikilon.Time
import Wikilon.Database
import Wikilon.AO

-- | A transaction on a dictionary has some metadata and a few words
-- to update. Each word can have at most one update. Note that rename
-- applies strictly to words defined *before* this transaction.
data DictTX = DictTX
    { dtx_meta    :: !DTXMeta
    , dtx_rename  :: !RenameMap
    , dtx_update  :: !UpdateMap
    }
type RenameMap = WM.WordMap Word
type UpdateMap = WM.WordMap DictUpd

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
    , dtx_count  :: {-# UNPACK #-} !Int -- number of merged transactions
    , dtx_anno   :: !AnnoWords -- words for who, where, why, etc.
    }
type AnnoWords = WS.WordSet

-- | Dictionaries have a simple set of update operations:
-- 
--    define a new word
--    update an existing word
--    delete an existing word
--
-- The distinction between 'define' and 'update' exists so we can
-- halt propagation of 'rename' and 'delete' operations. To define a
-- word that already exists is an error. With only define, update, 
-- and delete, transactions have a nice property: idempotence. 
-- 
data DictUpd 
    = Define AO_Code    -- @define word code
    | Update AO_Code    -- @update word code
    | Delete            -- @delete word

-- ASIDE:
--
-- An 'append' operator has been contemplated for embedding objects
-- in the dictionary. But I'd lose idempotence, and I'm beginning to
-- envision effective ways to embed objects as loose coalitions of
-- independent words structured in an editor by naming conventions 
-- (L2$foo might be a cell in a spreadsheet or line in a notebook).
-- 

mapDictUpd :: (AO_Code -> AO_Code) -> DictUpd -> DictUpd
mapDictUpd f (Define ao) = Define (f ao)
mapDictUpd f (Update ao) = Update (f ao)
mapDictUpd _ Delete = Delete

isDef :: DictUpd -> Bool
isDef (Define _) = True
isDef _ = False

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
    if WM.null renameMap then dtx else
    let rn w = maybe w id $ WM.lookup w renameMap in
    -- rename the annotations
    let meta = dtx_meta dtx in
    let anno' = WS.map rn (dtx_anno meta) in
    let meta' = meta { dtx_anno = anno' } in
    -- rename words within the updates!
    let rnu  = rn *** mapDictUpd (aoMapWords rn) in
    let update' = WM.fromList $ fmap rnu $ WM.toList $ dtx_update dtx in
    -- filter newly defined words from renameMap
    let newWords = WM.keys $ WM.filter isDef update' in
    let renameMap' = L.foldl' (flip WM.delete) renameMap newWords in
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
-- It's possible a map contains (foo,bar) and (bar,baz), indicating
-- two words change at the same time. This results from (bar,baz)
-- performed before (foo,bar). It should work without changes, since
-- renameDTX applies only a single step.
--
-- Also, it's possible that yesterday we renamed foo to bar, and
-- today we renamed bar back to foo (changed our mind!). In that
-- case, we want to eliminate the identity rename (foo,foo).
--
mergeRenameMaps :: RenameMap -> RenameMap -> RenameMap
mergeRenameMaps old new =
    if WM.null new then old else
    L.foldl' mrn1 new (WM.toList old)

mrn1 :: RenameMap -> (Word,Word) -> RenameMap
mrn1 newMap (foo,bar) =
    case WM.lookup bar newMap of
        Nothing ->  WM.insert foo bar newMap
        Just baz ->
            let m' = WM.delete bar newMap in
            if (foo == baz) then m' else
            WM.insert foo baz m'

-- | deepRenameDTX will take a full database and apply all renames, 
-- and then return any leftover rename operations. If this is a
-- valid and complete dictionary, the leftover map will be empty.
--
-- It is unclear whether this offers any performance advantages over
-- the more incremental processes. deepRenameDTX doesn't leverage
-- any sort of reverse lookup index.
--
deepRenameDTX :: [DictTX] -> ([DictTX], RenameMap)
deepRenameDTX = dr [] WM.empty where
    dr z rn (t:ts) =
        let tRn = renameDTX rn t in 
        let rn' = dtx_rename tRn in
        let t' = tRn { dtx_rename = WM.empty } in
        t' `seq` dr (t':z) rn' ts
    dr z rn [] = (L.reverse z, rn)


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
--   1) prefer many values for every word
--      Therefore: subtract 1 for each overlapping word in update.
--
--   2) prefer to merge updates from a single user or project
--      Therefore: subtract 1 for each difference in annotations
--                 add 1 for each overlap in annotations
--
--   3) prefer long term history be evenly sampled over real time
--      Therefore: subtract 1 for every 12 hours in scope of merge
--
-- The merge heuristic weakly adjusts which merges we favor in each
-- decay group. Hopefully, this results in an interesting history.
--
heuristicDTX :: DictTX -> DictTX -> Int
heuristicDTX old new = snd $ mergeHeuristicDTX old new

-- heuristic, assuming that no rename is needed.
heuristicDTX' :: DictTX -> DictTX -> Int
heuristicDTX' old new = score where
    score = wordScore + annoScore + timeScore
    wordScore = 
        let m1 = dtx_update old in
        let m2 = dtx_update new in
        negate $ WM.size $ WM.intersection m1 m2
    annoScore =
        let s1 = dtx_anno (dtx_meta old) in
        let s2 = dtx_anno (dtx_meta new) in
        let sn = WS.intersection s1 s2 in
        -- subtract overlap twice, but add thrice 
        (3 * WS.size sn) - (WS.size s1 + WS.size s2)
    timeScore =
        let tm_ini = dtx_tm_ini (dtx_meta old) in
        let tm_fin = dtx_tm_fin (dtx_meta new) in
        let dt = tm_fin `diffTime` tm_ini in
        let hours12 = 12 * picosInHour in
        negate $ fromIntegral $ dtToPicos dt `div` hours12

picosInHour :: Integer
picosInHour = picosPerSec * secsPerHour where
    picosPerSec = 1000 * 1000 * 1000 * 1000
    secsPerHour = 3600 
    
-- | Compute both a merged value and heuristic together, in the
-- format required by Wikilon.Database.
mergeHeuristicDTX :: DictTX -> DictTX -> (DictTX, Int)
mergeHeuristicDTX old new = (m, h) where
    old' = renameDTX (dtx_rename new) old
    new' = new { dtx_rename = WM.empty }
    m = mergeDTX'     old' new'
    h = heuristicDTX' old' new'

-- | Default parameters for Wikilon.Database.
dictDecayParams :: Decay DictTX
dictDecayParams = Decay
    { decay_merge  = mergeHeuristicDTX
    , decay_freq   = 8
    , decay_keep   = 8
    }

-- same as mergeDTX, but assumes that the new transaction does not
-- rename anything, and thus that names are consistent.
mergeDTX' :: DictTX -> DictTX -> DictTX
mergeDTX' _old _new = error "TODO!"


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

{-


type DictDB = [DictTX]

-- cf. wikilon/docs/DictionaryStructure.md


-- | A dictionary has a list of transactions and a final state.
--
-- This dictionary limits itself to a 'valid' state. In particular,
-- words in this dictionary will be fully defined and acyclic. There
-- is no promise at this layer that words are type-safe or will pass
-- their tests.
--
data Dict = Dict
    { dict_db :: !DictDB  -- ^ historical state of database
    , dict_st :: !DictST  -- ^ current state of database
    }

-- cf wikilon/doc/DictionaryValidation.md


readDictDB :: Dict -> DictDB
readDictDB = dict_db

readDictST :: Dict -> DictST
readDictST = dict_st

-- | A dictionary state supports up-to-date views of the dictionary,
-- compilation of words to Awelon bytecode, and similar features.
type DictST = M.Map Word DictEnt

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

-}



