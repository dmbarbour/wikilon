{-# LANGUAGE PatternGuards, ViewPatterns #-}

-- | The Awelon Object (AO) dictionary is a primary data structure
-- of Wikilon. AO words form functions, modules, tests, documentation,
-- directives, web-applications, and wiki pages. But, essentially, the
-- content of each word is just some AO code.
--
-- This module describes the dictionary state, and enforces useful 
-- structural properties on the code: that words are fully defined, 
-- acyclic. Higher level properties, such as requiring type safety,
-- tests pass, protecting frozen words, must be achieved at a higher
-- layer.
--
-- In addition to a set of words, a dictionary tracks some useful
-- metadata about when changes were made, potentially who made them
-- and for which projects, etc.
-- 
module Wikilon.DictST
    ( DictST
    , DefinitionMap, ReverseLookup
    , emptyDictST, updateDict
    , dictDefs, dictRLU
    ) where

-- todo: 
--  dictionary state - update and access
--  serialization of dictionary

import Control.Monad ((>=>), foldM)
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
emptyDictST :: DictST
emptyDictST = DictST { _defs = WM.empty, _rlu = WM.empty }

-- | Attempt to update a dictionary with a transaction. The update
-- will be rejected with a human meaningful error if it would lead
-- to an invalid dictionary state or database.
updateDict :: DictTX -> DictST -> Either String DictST
updateDict dtx = rn >=> vda >=> upd >=> vdt where
    rn  = applyRN (dtx_rename dtx)
    vda = validAnno dtx
    upd = applyUPD (dtx_update dtx)
    vdt = validUPD (dtx_update dtx)

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
-- E.g. if `foo` is defined as `bar baz`, and we rename `foo`
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

-- | Apply a set of updates to the dictionary.
--
-- At this point, renaming has already been done, and the updates
-- can be applied one at a time (a lot easier to implement!). The
-- possible errors we can recognize at this point are:
--
--   (a) we define a word that already exists
--   (b) we update or delete a word that does not exist
--   
applyUPD :: UpdateMap -> DictST -> Either String DictST
applyUPD upm st0 = foldM (flip $ uncurry tryUpd) st0 (WM.toList upm)

tryUpd :: Word -> DictUpd -> DictST -> Either String DictST
tryUpd foo (Define ao) (DictST defs rlu) =
    case WM.lookup foo defs of
        Just _code -> Left ("cannot define " ++ show foo ++ " (already has def)")
        Nothing ->
            let defs' = WM.insert foo ao defs in
            let ws = WS.fromList $ aoWords ao in
            let rlu' = insertRLU foo ws rlu in
            Right $ DictST defs' rlu'
tryUpd foo (Update newAO) (DictST defs rlu) =
    case WM.lookup foo defs of
        Nothing -> Left ("cannot update " ++ show foo ++ " (not already defined)")
        Just oldAO ->
            let defs' = WM.insert foo newAO defs in
            let wsOld = WS.fromList $ aoWords oldAO in
            let wsNew = WS.fromList $ aoWords newAO in
            let rluD = deleteRLU foo (wsOld `WS.difference` wsNew) rlu in
            let rlu' = insertRLU foo (wsNew `WS.difference` wsOld) rluD in
            Right $ DictST defs' rlu' 
tryUpd foo Delete (DictST defs rlu) =
    case WM.lookup foo defs of
        Nothing -> Left ("cannot delete " ++ show foo ++ " (not defined)")
        Just code ->
            let defs' = WM.delete foo defs in
            let ws = WS.fromList $ aoWords code in
            let rlu' = deleteRLU foo ws rlu in
            Right $ DictST defs' rlu'

-- insertRLU foo [words]; indicate that foo uses word.
-- This should be the first 
insertRLU :: Word -> WordSet -> ReverseLookup -> ReverseLookup
insertRLU foo = flip (L.foldl' addFooTo) . WS.toList where
    addFooTo rlu w =
        let s0 = maybe WS.empty id $ WM.lookup w rlu in
        assert (not $ WS.member foo s0) $
        let s' = WS.insert foo s0 in
        WM.insert w s' rlu

-- deleteRLU foo [words]; indicate foo no longer uses words.
deleteRLU :: Word -> WordSet -> ReverseLookup -> ReverseLookup
deleteRLU foo = flip (L.foldl' remFooFrom) . WS.toList where
    remFooFrom rlu w = 
        let s0 = maybe WS.empty id $ WM.lookup w rlu in
        assert (WS.member foo s0) $
        let s' = WS.delete foo s0 in
        if WS.null s' then WM.delete w rlu else
        WM.insert w s' rlu

-- | Validate the dictionary!
--
-- We've already modified the dictionary according to these updates.
-- But it might be that, after all is said and done, some of these
-- words are left in a bad state. At this time, we'll recognize two
-- kinds of errors:
--
--  (a) deleted word still has clients
--  (b) we've introduced a cyclic definition
--
-- Basically, this is as far as we'll go with validating in this
-- module. However, downstream modules may further insist that 
-- words have no obvious type errors, tests pass, etc..
--   
validUPD :: UpdateMap -> DictST -> Either String DictST
validUPD upm st@(DictST defs rlu) =
    -- deleted words in active use
    let lDel = L.filter (flip WM.member rlu) $ WM.keys $ WM.filter isDelete upm in
    let emsgDel = "cannot delete words in use: " ++ show lDel in
    if not (L.null lDel) then Left emsgDel else 
    -- cycle search via partial compile
    let updWords = WM.keys $ WM.filter (not . isDelete) upm in
    let lCyc = findCycle defs updWords in
    let emsgCyc = "update would introduce cycle: " ++ show lCyc in
    if not (L.null lCyc) then Left emsgCyc else
    -- as far as we can tell, dictionary is in good shape
    Right st

-- | Find a cycle, if possible, halting at the first cycle found. 
-- If no cycle is found, will return the empty list. This checks
-- each input word only once, so maximum cost is O(N) with the
-- dictionary size, and roughly proportional to compiling words
-- (albeit, minus the space costs).
findCycle :: DefinitionMap -> [Word] -> [Word]
findCycle dict = L.reverse . snd . cc WS.empty [] where
    cc okw _stack [] = (okw,[])
    cc okw stack (w:ws) =
        if (L.elem w stack) then (okw,stack) else
        if (WS.member w okw) then cc okw stack ws else
        let ans@(okw',cyc) = cc okw (w:stack) (childWords w) in
        if not (L.null cyc) then ans else
        cc (WS.insert w okw') stack ws
    dlu = flip WM.lookup dict
    childWords (dlu -> Just ao) = L.nub $ aoWords ao
    childWords _w = assertImpossible []


-- | Validate the Annotations.
--
-- Annotation words must be defined in the dictionary, either before
-- or after the transaction. We'll test for annotation words by their
-- membership either in the dictionary or in the update set. The test
-- is applied after renaming, which is important because rename has 
-- already been applied to annotation words.
--
-- If a transaction deletes an annotation word, that's also the last
-- opportunity for that annotation word to appear.
validAnno :: DictTX -> DictST -> Either String DictST
validAnno dtx st =
    let okAnno w = WM.member w (_defs st) || WM.member w (dtx_update dtx) in
    let annoWords = WS.toList $ dtx_anno $ dtx_meta $ dtx in
    let badWords = L.filter (not . okAnno) annoWords in
    let emsgBadAnno = "undefined annotation words: " ++ show badWords in
    if not (L.null badWords) then Left emsgBadAnno else
    Right st 


-- for cases that shouldn't happen (but are well defined anyway)...
assertImpossible :: a -> a
assertImpossible = assert impossible where
    impossible = False

