{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}

-- | A dictionary contains words and definitions.
--
-- In Wikilon, a definition is a function that returns a value and a
-- function to compile this value. The result of compilation is also
-- a function, the meaning of the definition.
--
--      type Def a b = ∃v.∀e. e→([v→[a→b]]*(v*e))
-- 
-- Each word has a definition. A definition may access other words by
-- use of special {%word} tokens, e.g. {%dupd}{%swap}. Each token may
-- be substituted by the associated definition followed by `$vr$c` to
-- compile and apply the block. But this process is optimized easily
-- by pre-compiling each definition then inlining its meaning.
--
-- The intermediate structured value `v` corresponds to DSL or syntax.
-- This enables developers to manipulate definitions through structure
-- editors, potentially at a higher level than words and bytecode, and
-- also readily supports staged programming.
--
-- This dictionary module enforces a few invariants:
--
--   1. dependencies are acyclic
--   2. every word has a definition
--   3. constraints on tokens 
--
-- Allowed tokens are word dependencies, annotations, and discretionary
-- sealers or unsealers. These limitations ensure the AO dictionary is
-- pure, portable. There is no entanglement with specific machines.
--
-- Ideally, Wikilon shall further enforce that definitions compile and
-- check that there are no obvious errors via automatic type checking,
-- testing, linting, analysis, etc..
--
-- TODO: support many more operations:
--   batch rename
--   efficient diffs
-- 
module Wikilon.Dict
    ( Dict, dict_space
    , empty, null, size
    , lookup, toList
    , lookupBytes, toListBytes
    
    , wordsInDict
    , wordsWithPrefix

    , tokenUsedBy
    , usedBy, usedByTransitive
    , deps, abcWords
    , member

    , insert, InsertionError(..), InsertionErrors
    , delete


    , rename1

    , module Wikilon.Dict.Word
    , unsafeInsert, unsafeDelete

    , unsafeDictAddr
    ) where

import Prelude hiding (null, lookup, words)
import Control.Applicative ((<$>))
import Control.Arrow (second, (***))
import qualified Control.Monad.State as State
import Control.Monad
import Data.Monoid
import Data.Maybe 
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.VCache.Trie as Trie
import Data.Word (Word64)
import Data.VCache.Trie (Trie)
import Database.VCache

import Awelon.ABC (ABC)
import qualified Awelon.ABC as ABC

import Wikilon.Dict.Type
import Wikilon.Dict.Word
import Wikilon.Dict.Token
import Wikilon.Dict.Text

-- | a dictionary is hosted in a vcache address space
dict_space :: Dict -> VSpace
dict_space = Trie.trie_space . dict_defs

-- | provide simple information about current value
unsafeDictAddr :: Dict -> Word64
unsafeDictAddr = Trie.unsafeTrieAddr . dict_defs


-- Preferred cache mode for dictionary lookups. In this case, I don't
-- mind quickly dropping nodes from the cache, since the parse is fast
-- and operations on a dictionary are bursty anyway.
_cm :: CacheMode
_cm = CacheMode0

-- | O(1). Create a new, empty dictionary
empty :: VSpace -> Dict
empty vs = Dict1
    { dict_defs = Trie.empty vs
    , dict_deps = Trie.empty vs
    }

-- | O(1). Test whether a dictionary is empty.
null :: Dict -> Bool
null = Trie.null . dict_defs

-- | O(N). Return number of words in dictionary.
size :: Dict -> Int
size = Trie.size . dict_defs

-- | O(WordSize). Lookup the definition for a word.
lookup :: Dict -> Word -> Maybe ABC
lookup d w = _decode <$> lookupBytes d w

_decode :: LBS.ByteString -> ABC
_decode b = case ABC.decode b of
    Left _dcs -> _impossible "invalid ABC in dictionary"
    Right abc -> abc

_impossible :: String -> a
_impossible = error . dictErr

-- | List all the words in the dictionary.
wordsInDict :: Dict -> [Word]
wordsInDict = fmap Word . Trie.keys . dict_defs

-- | List all words with a common prefix. This has an optimized
-- implementation based on the underlying Trie.
wordsWithPrefix :: Dict -> BS.ByteString -> [Word]
wordsWithPrefix d pre =
    let tk = Trie.lookupPrefix pre (dict_defs d) in
    fmap (Word . (pre <>)) (Trie.keys tk)

-- | Obtain a list of all (Word, ABC) pairs.
toList :: Dict -> [(Word, ABC)]
toList = fmap (second _decode) . toListBytes

-- | Lookup raw bytestring for a word. 
lookupBytes :: Dict -> Word -> Maybe LBS.ByteString
lookupBytes d (Word w) = _bytes <$> Trie.lookupc _cm w (dict_defs d)

_bytes :: Def -> LBS.ByteString
_bytes (Def1 v) = deref' v

-- | Obtain a list of all (Word, ByteString) pairs. Words are sorted by suffix.
toListBytes :: Dict -> [(Word, LBS.ByteString)]
toListBytes = Trie.toListBy f . dict_defs where
    f w def = (Word w , _bytes def)

-- | Test whether a given word is defined in this dictionary.
member :: Dict -> Word -> Bool
member d (Word w) = isJust $ Trie.lookup' w (dict_defs d) 

-- | Find all words that use a given token. All tokens in the
-- dictionary are recorded in the dependency, so this lookup
-- is always precise.
tokenUsedBy :: Dict -> Token -> [Word]
tokenUsedBy d tok = maybe [] _depWords $ Trie.lookupc _cm tok (dict_deps d)

_depWords :: Deps -> [Word]
_depWords (Deps1 t) = fmap Word $ Trie.keys t

-- | Find direct clients of a word.
usedBy :: Dict -> Word -> [Word]
usedBy d (Word w) = tokenUsedBy d (BS.cons 37 w) -- %word

-- | Find all words depended upon by a given word. Filters ABC for just
-- the {%word} tokens, and returns each word. Returns the empty list if
-- the requested word is undefined. Doesn't eliminate duplicates.
deps :: Dict -> Word -> [Word]
deps d w = maybe [] abcWords $ lookup d w

-- | Each word is expressed as a {%word} token in the original ABC.
abcWords :: ABC -> [Word]
abcWords = mapMaybe wordTok . ABC.tokens where
    wordTok tok = case UTF8.uncons tok of
        Just ('%', w) -> Just (Word w)
        _ -> Nothing

newtype DepsMap = DepsMap { _inDepsMap :: Map Word (Set Token) }
newtype ReverseDepsMap = ReverseDepsMap 
    { _inReverseDepsMap :: Map Token (Set Word) }
newtype ReverseDepsMapDiff = ReverseDepsMapDiff 
    { _inReverseDepsMapDiff :: Map Token ReverseDepsDiff }
type ReverseDepsDiff = (Set Word, Set Word) -- (del,add)

-- build a dependencies map for a set of words in the dictionary
_dictDepsMap :: Dict -> [Word] -> DepsMap
_dictDepsMap d = DepsMap . Map.fromList . fmap _withDeps where
    _withDeps w = (w, _tokens w)
    _tokens = maybe Set.empty (Set.fromList . ABC.tokens) . lookup d

-- dependencies for inserted words. These are accessible directly.
_insertDepsMap :: [(Word, ABC)] -> DepsMap
_insertDepsMap = DepsMap . Map.fromList . fmap (second (Set.fromList . ABC.tokens))


-- compute an incomplete set of reverse dependencies, limited to the
-- words in the original DepsMap. 
_revDeps :: DepsMap -> ReverseDepsMap 
_revDeps = ReverseDepsMap . L.foldl' insw Map.empty . Map.toList . _inDepsMap where
    insw m (w,toks) = L.foldl' (insd w) m (Set.toList toks)  -- insert word under every token
    insd w m tok = Map.alter (inss w) tok m -- insert word under a single token
    inss w = Just . maybe (Set.singleton w) (Set.insert w) -- add word to the token's dependency set

-- compute the difference between reversed dependencies
_revDepsDiff :: ReverseDepsMap -> ReverseDepsMap -> ReverseDepsMapDiff
_revDepsDiff old new = ReverseDepsMapDiff $ mergeDeps old' new' where
    old' = _inReverseDepsMap old
    new' = _inReverseDepsMap new
    mergeDeps = Map.mergeWithKey mergeOldAndNew mergeOld mergeNew
    mergeNew = fmap addDeps -- just in new, so add
    mergeOld = fmap delDeps -- just in old, so delete
    mergeOldAndNew _ sOld sNew = 
        let sAdd = sNew `Set.difference` sOld in
        let sDel = sOld `Set.difference` sNew in
        let bNoChange = Set.null sAdd && Set.null sDel in
        if bNoChange then Nothing else
        Just $ addDeps sAdd <> delDeps sDel
    addDeps sAdd = (mempty, sAdd)
    delDeps sDel = (sDel, mempty)
{-# NOINLINE _revDepsDiff #-}

-- tune the reverse lookup map by the given delta map
_updateDeps :: ReverseDepsMapDiff -> Trie Deps -> Trie Deps
_updateDeps dd t = L.foldl' _updOneDep t (Map.toList (_inReverseDepsMapDiff dd)) 

-- adjust dependencies for just one token
_updOneDep :: Trie Deps -> (Token, ReverseDepsDiff) -> Trie Deps
_updOneDep t (tok,(wsDel,wsAdd)) = Trie.adjust adj tok t where
    adj Nothing = adj' $ Trie.empty (Trie.trie_space t)
    adj (Just (Deps1 td)) = adj' td
    adj' td = 
        let td' = upd td in 
        if Trie.null td' then Nothing else 
        Just (Deps1 td')
    upd = Trie.insertList lIns . Trie.deleteList lDel
    lIns = fmap insw $ Set.toList wsAdd
    lDel = fmap delw $ Set.toList wsDel
    insw (Word w) = (w, U)
    delw (Word w) = w


-- | The Wikilon.Dict module guards against a few kinds of errors:
--
-- * cyclic dependencies
-- * missing definitions
-- * malformed words
-- * unrecognized tokens
--
-- Other errors, such as badly typed code, divergent computations,
-- or incorrect documentation must be addressed by clients.
--
-- I will report invalid input requests, too. Mostly, an input set
-- must have a single entry for each word added.
--
type InsertionErrors = [InsertionError]
data InsertionError 
    = Undef    !Word !Word   -- undefined {%word} used by word
    | BadWord  !Word         -- word is not valid according to heuristics
    | BadToken !Token !Word  -- invalid {token} used by word
    | BadText  !Text !Word   -- rejecting text on heuristic constraints
    | Cycle    ![Word]
    | DupWord  !Word         -- word appears multiple times in set

instance Show InsertionError where
    show (Undef uw w)   = "word " ++ show w ++ " uses undefined {%" ++ show uw ++ "}"
    show (BadWord w)    = "malformed word: " ++ show w
    show (BadToken t w) = "rejecting token " ++ show (ABC.ABC_Tok t) ++ " in " ++ show w
    show (Cycle ws)     = "cyclic dependencies: " ++ show ws
    show (BadText t w)  = "in word" ++ show w ++ " malformed text: " ++ show (ABC.ABC_Text t)
    show (DupWord w)    = "word " ++ show w ++ " assigned twice"

-- | Insert or Update a list of words. Any existing definition for 
-- an inserted word will be replaced. Errors are possible if a word
-- introduces a cycle or uses an undefined word, or simply has some
-- malformed content.
insert :: Dict -> [(Word, ABC)] -> Either InsertionErrors Dict
insert d l = 
    -- sanitize input for internal per-word errors
    let lDupWords = DupWord <$> findDups (fst <$> l) in 
    let lMalformed = L.foldr (uncurry _testBadDef) [] l in
    let lBad = lDupWords ++ lMalformed in
    if not (L.null lBad) then Left lBad else
    -- compute the insertion
    let d' = unsafeInsert d l in
    -- filter for cycles or undefined words
    let lInsErr = _insertionErrors d' (fmap fst l) in
    if not (L.null lInsErr) then Left lInsErr else
    Right $! d'

findDups :: Eq a => [a] -> [a]
findDups = f [] where
    f ds (x:xs) = 
        let bNewDup = L.elem x xs && L.notElem x ds in
        if bNewDup then f (x:ds) xs else f ds xs
    f ds [] = L.reverse ds

-- | Insert without trying to detect any errors. 
unsafeInsert :: Dict -> [(Word, ABC)] -> Dict
unsafeInsert d l = Dict1 { dict_defs = d_defs', dict_deps = d_deps' } where
    oldDepsMap = _revDeps $ _dictDepsMap d (fmap fst l)
    newDepsMap = _revDeps $ _insertDepsMap l
    depsMapDiff = _revDepsDiff oldDepsMap newDepsMap
    d_deps' = _updateDeps depsMapDiff (dict_deps d)
    vc = dict_space d
    lIns = fmap (_wkey *** _encodeDef vc) l
    d_defs' = Trie.insertList lIns (dict_defs d)
    _wkey (Word w) = w

_testBadDef :: Word -> ABC -> InsertionErrors -> InsertionErrors
_testBadDef w abc = badWord . badToks . badTexts where
    badWord = if isValidWord w then id else (:) (BadWord w) 
    badToks = (++) (fmap (`BadToken` w) lBadToks)
    lBadToks = L.nub $ L.filter (not . isValidToken) $ ABC.tokens abc
    badTexts = (++) (fmap (`BadText` w) lBadTexts)
    lBadTexts = L.filter (not . isValidText) $ ABC.abcTexts abc

-- find two kinds of errors: undefined dependencies, cycles
-- Note: this will load content out of the VCache representation
--  which will help further validate that content parses.
_insertionErrors :: Dict -> [Word] -> InsertionErrors
_insertionErrors d ws = cycleErrors ++ undefErrors where
    undefErrors = L.concatMap (_testUndef d) ws
    cycleErrors = case findCycle d ws of
        Nothing -> []
        Just cyc -> [Cycle cyc]

_testUndef :: Dict -> Word -> InsertionErrors
_testUndef d w = fmap (`Undef` w) lUndef where
    lUndef = L.filter (not . member d) (L.nub $ deps d w)

-- encode bytecode; heuristically choose separate node vs internal
_encodeDef :: VSpace -> ABC -> Def
_encodeDef vc = Def1 . vref' vc . ABC.encode

-- Search a list of words for cyclic dependencies. Returns first cycle
-- found if one exists, or returns Nothing. Does not search any word
-- more than once.
findCycle :: Dict -> [Word] -> Maybe [Word]
findCycle d = flip State.evalState Set.empty . _fc d []

-- _fc dict    stack     frontier           known acyclic
_fc :: Dict -> [Word] -> [Word] -> State.State (Set Word) (Maybe [Word])
_fc _ _ [] = return Nothing
_fc d stack (w:ws) = 
    State.gets (Set.member w) >>= \ bKnownAcyclic ->
    if bKnownAcyclic then _fc d stack ws else
    if L.elem w stack then return (Just (_cyc w stack)) else
    _fc d (w:stack) (deps d w) >>= \ rStack ->
    case rStack of
        Nothing -> State.modify (Set.insert w) >> _fc d stack ws
        cycleFound -> return cycleFound

-- return just the cycle found (implicitly closed)
_cyc :: (Eq a) => a -> [a] -> [a]
_cyc a stack = L.dropWhile (/= a) $ L.reverse stack

-- | Delete a list of words. This may fail if deletion would leave any
-- word in the dictionary undefined. If this fails, it returns a list
-- with a transitive list of words that would become undefined if you
-- delete the requested words. Otherwise, the updated dictionary is
-- returned.
delete :: Dict -> [Word] -> Either [Word] Dict
delete d lDel = 
    let lClients = usedByTransitive d lDel in
    let lUndef = L.filter (`L.notElem` lDel) lClients in
    if not (L.null lUndef) then Left lUndef else
    Right $! unsafeDelete d lDel

-- | delete a list of words without validating.
-- May leave some words undefined.
unsafeDelete :: Dict -> [Word] -> Dict
unsafeDelete d lDel = Dict1 { dict_defs = d_defs', dict_deps = d_deps' } where
    d_defs' = Trie.deleteList (fmap _wkey lDel) (dict_defs d)
    _wkey (Word w) = w
    oldDepsMap = _revDeps $ _dictDepsMap d lDel
    newDepsMap = ReverseDepsMap Map.empty
    depsMapDiff = _revDepsDiff oldDepsMap newDepsMap
    d_deps' = _updateDeps depsMapDiff (dict_deps d)

-- | Find all clients, recursively, for a list of words. This is 
-- useful for computing invalidation after inserting a list of updates.
usedByTransitive :: Dict -> [Word] -> [Word]
usedByTransitive d ws = _usedByTransitive d Set.empty Set.empty ws

-- find all transitive clients of a list of word
-- _usedByTransitive dict    searched    found      toSearch
_usedByTransitive :: Dict -> Set Word -> Set Word -> [Word] -> [Word]
_usedByTransitive _ _  s [] = Set.toList s
_usedByTransitive d sw su (w:ws) = 
    if Set.member w sw then _usedByTransitive d sw su ws else
    let sw' = Set.insert w sw in
    let lU = usedBy d w in
    let su' = L.foldl' (flip Set.insert) su lU in
    _usedByTransitive d sw' su' (lU ++ ws)

-- | Rename a single word in the dictionary.
--
-- > rename1 dict orignalWord newWord
--
-- Renaming fails if the new word already has a definition unless
-- said definition is identical, in which case we merge and collapse
-- the two names. Renaming may also fail if the original word is not
-- defined or the new word is malformed. In case of failure, Nothing
-- is returned.
--
-- Rename is intended to support refactoring.
--
rename1 :: Dict -> Word -> Word -> Maybe Dict
rename1 d wo wt = do
    unless (isValidWord wt) mzero
    abc0 <- lookup d wo
    let bMergeOK = maybe True (== abc0) (lookup d wt)
    unless bMergeOK mzero
    let fnUpd wc = lookup d wc >>= \ abc -> return (wc, _renameInABC wo wt abc)
    let lUpdC = catMaybes $ fmap fnUpd $ usedBy d wo
    let lUpd = (wt, abc0) : lUpdC
    return $! unsafeDelete (unsafeInsert d lUpd) [wo] 

_renameInABC :: Word -> Word -> ABC -> ABC
_renameInABC wo wt = ABC.rewriteTokens rwTok where
    t0 = BS.cons 37 $ wordToUTF8 wo
    tf = BS.cons 37 $ wordToUTF8 wt
    rnTok t = if (t == t0) then tf else t
    rwTok t = ABC.ABC [ABC.ABC_Tok (rnTok t)] 1

