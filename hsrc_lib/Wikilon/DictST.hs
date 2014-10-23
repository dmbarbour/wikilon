{-# LANGUAGE PatternGuards, ViewPatterns #-}

-- | The Awelon Object (AO) dictionary is a primary data structure
-- of Wikilon. AO words form functions, modules, tests, documentation,
-- directives, web-applications, and wiki pages. But, essentially, the
-- content of each word is just some AO code, which ideally compiles,
-- is well typed, and passes all tests. 
--
-- This module describes the dictionary state.
--
-- In addition to a set of words, a dictionary tracks some useful
-- metadata about when changes were made, potentially who made them
-- and for which projects, etc.
-- 
module Wikilon.DictST
    ( DictST
    , DefinitionMap, ReverseLookup
    , emptyDict, updateDict
    , dictDefs, dictRLU
    ) where

-- todo: 
--  dictionary state - update and access
--  serialization of dictionary

import Control.Monad ((>=>))
import Control.Exception (assert)
import qualified Data.List as L

import Wikilon.WordMap (WordMap)
import Wikilon.WordSet (WordSet)
import qualified Wikilon.WordMap as WM
import qualified Wikilon.WordSet as WS

import Wikilon.DictTX
import Wikilon.AO

-- | The DictST represents the current state of a dictionary, and it
-- synchronizes two views:
--
--   a map from words to definitions
--   an inverted index from words to clients thereof
--
-- In addition, it enforces a weak validity property: that words are
-- well defined and acyclic. There is no assurance that words will
-- typecheck or that tests will pass. But they'll at least compile
-- to Awelon bytecode.
--
-- Updates to the DictST are performed by DictTX.
data DictST = DictST
    { _defs :: !DefinitionMap
    , _rlu  :: !ReverseLookup
    }
type DefinitionMap = WordMap AO_Code
type ReverseLookup = WordMap WordSet

-- | The main use of a dictionary: given a word, find its definition.
-- The DictST structure ensures words are properly defined, i.e. no
-- missing words, no cycles.
dictDefs :: DictST -> DefinitionMap
dictDefs = _defs

-- | If you want to find all uses of a word within the dictionary,
-- try a ReverseLookup.
dictRLU :: DictST -> ReverseLookup
dictRLU = _rlu

-- | Create a new, empty dictionary.
emptyDict :: DictST
emptyDict = DictST { _defs = WM.empty, _rlu = WM.empty }

-- | Attempt to update a dictionary with a transaction. The update
-- will be rejected with a human meaningful error if it would lead
-- to an invalid dictionary state or database.
updateDict :: DictTX -> DictST -> Either String DictST
updateDict dtx = rn >=> upd where
    rn = applyRN (dtx_rename dtx)
    upd = applyUPD (dtx_update dtx)
    -- metadata is NOT used to update dictionary state

-- | Renaming Notes:
--
-- It's important to apply renames atomically, because our rename map
-- may contain cycles like [(foo,bar),(bar,baz),(baz,foo)]. This can
-- arise even from single rename actions; see mergeRenameMaps.
-- 
-- The main restrictions on renaming:
-- 
-- 1) we will only rename words that are in the dictionary
-- 2) we must reject 'collisions' where a word has two defs
--  (a) rename many words to the same target; (foo,baz),(bar,baz)
--  (b) target word exists in dictionary AND is not itself renamed
--
-- Feasibly, I could accept renames where the definitions match, e.g.
-- if `foo` and `bar` have the same definition, then renaming foo→bar
-- wouldn't cause any change in observable behavior. However, I think
-- it would be better to treat this 'merge' explicitly, via update. I
-- will reject even such benign collisions.
-- 
applyRN :: RenameMap -> DictST -> Either String DictST
applyRN rnm st0 = 
    -- if we aren't renaming anything, skip step
    if (WM.null rnm) then Right st0 else
    -- rename only words that exist in dictionary
    let fNID w = not (WM.member w (_defs st0)) in
    let lNID = L.filter fNID (WM.keys rnm) in
    let emsgNID = "rename word not in dict: " ++ show lNID in
    if not (L.null lNID) then Left emsgNID else
    -- reject rename collisions within map.
    let lDups = L.nub $ findDuplicates (WM.elems rnm) in
    let emsgDups = "rename collision, dups: " ++ show lDups in
    if not (L.null lDups) then Left emsgDups else
    -- reject rename collisions with dictionary.
    let fCol w = WM.member w (_defs st0) && not (WM.member w rnm) in
    let lCol = L.filter fCol (WM.elems rnm) in
    let emsgCol = "rename collision, dict: " ++ show lCol in
    if not (L.null lCol) then Left emsgCol else 
    -- everything seems to be in order, so rename
    Right $ applyRN' rnm st0

findDuplicates :: (Eq a) => [a] -> [a]
findDuplicates = fr [] where
    fr rp (a:as) | L.elem a as = fr (a:rp) as
                 | otherwise   = fr rp as
    fr rp [] = rp

-- | applyRN' - apply rename map ASSUMING it is valid 
--
-- Two steps: content before index.
--
-- Content Rename, e.g. renaming 'foo'
--   Compute a set of words whose definitions must be rewritten.
--     E.g. if 'foo' is directly used by 'qux' (via RLU), then update qux. 
--   Compute a set of words whose reverse-lookups must be rewritten.
--     E.g. if 'foo' is defined as 'bar baz', then rewrite RLU for bar & baz
--   Apply renames to body elements in both sets.
--
-- Index Rename:
--   Remove all renamed elements into separate list.
--   Apply rename function to key elements of list.
--   Insert the list back into the index.
--
-- I'm not 100% confident in this design, so it might be worth 
-- validating if bugs are found in renaming.
-- 
applyRN' :: RenameMap -> DictST -> DictST
applyRN' rnm st0 = st' where
    rnf w = maybe w id $ WM.lookup w rnm -- simple rename function
    defsToUpd = -- set of words whose defs need updating
        let rlu = maybe WS.empty id . flip WM.lookup (_rlu st0) in
        let lUseRenamedWords = fmap rlu (WM.keys rnm) in
        L.foldl' WS.union WS.empty lUseRenamedWords
    rlusToUpd = -- set of words whose RLUs need updating
        let defWords = maybe [] aoWords . flip WM.lookup (_defs st0) in
        WS.fromList $ L.concatMap defWords $ WM.keys rnm
    rnDef defMap w = case WM.lookup w defMap of
        Just ao -> WM.insert w (aoMapWords rnf ao) defMap
        Nothing -> assertImpossible $ defMap
    rnRLU rluMap w = case WM.lookup w rluMap of
        Just ws -> WM.insert w (rewriteRLU rnm ws) rluMap
        Nothing -> rluMap -- w is a 'root' word, nobody uses it
    defs' = rewriteIdx rnm $ L.foldl' rnDef (_defs st0) $ WS.toList defsToUpd 
    rlu' = rewriteIdx rnm $ L.foldl' rnRLU (_rlu st0) $ WS.toList rlusToUpd
    st' = DictST { _defs = defs', _rlu = rlu' }

-- | Update an RLU word set via rename map.
--
-- E.g. if `foo` is defined as `bar` and `baz`, and we rename `foo`
-- to `foo2`, then we must poke into the reverse lookup for `bar`
-- and `baz` and replace `foo` with `foo2`. 
--
-- In case of a rename cyle like (foo,bar),(bar,baz),(baz,foo), we
-- separate removing keys [foo,bar,baz] from adding the newer
-- names [bar,baz,foo]. Some renames are potentially neutral on 
-- their final effect.
--
-- Performance of the current implementation is O(R*lgN) where R is
-- the number of renamed elements and N is the dictionary size. R is
-- itself constrained by dictionary size, but will usually be much
-- smaller.
rewriteRLU :: RenameMap -> WordSet -> WordSet
rewriteRLU rnm ws =
    let relRNs = L.filter (flip WS.member ws . fst) (WM.toList rnm) in
    let wsDel = L.foldl' (flip WS.delete) ws (fmap fst relRNs) in
    L.foldl' (flip WS.insert) wsDel (fmap snd relRNs)

-- | Rename keys in a map. This is achieved by deleting all the
-- old keys, then inserting all the new keys (in two steps due
-- to possible overlap between inserts and deletes).
rewriteIdx :: RenameMap -> WordMap a -> WordMap a
rewriteIdx rnm m0 = m' where
    mWithoutRenamedKeys = L.foldl' (flip WM.delete) m0 (WM.keys rnm)
    m' = L.foldl' rw mWithoutRenamedKeys (WM.toList rnm)
    rw m (foo,bar) = assert (not (WM.member bar m)) $ 
        case WM.lookup foo m0 of
            Just a -> WM.insert bar a m
            Nothing -> assertImpossible $ WM.delete bar m

-- for cases that shouldn't happen (but are well defined anyway)...
assertImpossible :: a -> a
assertImpossible = assert impossible where
    impossible = False

applyUPD :: UpdateMap -> DictST -> Either String DictST
applyUPD = error "TODO!"


