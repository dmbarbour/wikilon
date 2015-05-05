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
-- This dictionary module enforces three invariants:
--
--   1. dependencies are acyclic
--   2. every word has a definition
--   3. constraints on token types
--
-- Tokens are word dependencies, annotations, and discretionary sealers
-- or unsealers. This keeps the dictionary pure and portable, avoiding
-- entanglement with specific machines.
--
-- Ideally, Wikilon shall further enforce that definitions compile, that
-- words evaluate to blocks, that words are type-safe and have no obvious
-- errors (automatic linters, partial evaluation, testing, etc.).
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

    , usedBy, usedByTransitive
    , deps, member

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
import qualified Data.ByteString.Char8 as Char8
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

-- | a dictionary is hosted in a vcache address space
dict_space :: Dict -> VSpace
dict_space = Trie.trie_space . dict_data

-- | provide simple information about current value
unsafeDictAddr :: Dict -> Word64
unsafeDictAddr = Trie.unsafeTrieAddr . dict_data


-- Preferred cache mode for dictionary lookups. In this case, I don't
-- mind quickly dropping nodes from the cache, since the parse is fast
-- and operations on a dictionary are bursty anyway.
_cm :: CacheMode
_cm = CacheMode0

_fromSz :: Sz a -> a
_fromSz = either (derefc _cm) id 

_wordToKey :: Word -> BS.ByteString
_wordToKey (Word k) = k

-- | O(1). Create a new, empty dictionary
empty :: VSpace -> Dict
empty vs = Dict (Trie.empty vs) (Trie.empty vs)

-- | O(1). Test whether a dictionary is empty.
null :: Dict -> Bool
null = Trie.null . dict_data

-- | O(N). Return number of words in dictionary.
size :: Dict -> Int
size = Trie.size . dict_data

-- | O(WordSize). Lookup the definition for a word.
lookup :: Dict -> Word -> Maybe ABC
lookup d w = _decode <$> lookupBytes d w

-- we'll do our little sanity check every time.
_decode :: LBS.ByteString -> ABC
_decode b = 
    let (abc, b') = ABC.decode b in
    if LBS.null b' then abc else
    _impossible "invalid ABC"

_impossible :: String -> a
_impossible = error . dictErr

-- | List all the words in the dictionary.
wordsInDict :: Dict -> [Word]
wordsInDict = fmap Word . Trie.keys . dict_data

-- | List all words with a common prefix. This has an optimized
-- implementation based on the underlying Trie.
wordsWithPrefix :: Dict -> BS.ByteString -> [Word]
wordsWithPrefix d pre =
    let tk = Trie.lookupPrefix pre (dict_data d) in
    fmap (Word . (pre <>)) (Trie.keys tk)

-- | Obtain a list of all (Word, ABC) pairs.
toList :: Dict -> [(Word, ABC)]
toList = fmap (second _decode) . toListBytes

-- | Lookup raw bytestring for a word. 
lookupBytes :: Dict -> Word -> Maybe LBS.ByteString
lookupBytes d w = _fromSz <$> Trie.lookupc _cm (_wordToKey w) (dict_data d)

-- | Obtain a list of all (Word, ByteString) pairs. Words are sorted by suffix.
toListBytes :: Dict -> [(Word, LBS.ByteString)]
toListBytes = Trie.toListBy f . dict_data where
    f k bytes = (Word k , _fromSz bytes)

-- | Test whether a given word is defined in this dictionary.
member :: Dict -> Word -> Bool
member d w = isJust $ Trie.lookup' (_wordToKey w) (dict_data d) 

-- | Find direct clients of a word.
usedBy :: Dict -> Word -> [Word]
usedBy d w = maybe [] (fmap Word . _fromSz) $ 
    Trie.lookupc _cm (_wordToKey w) (dict_deps d)

-- | Find all words depended upon by a given word. Filters ABC for just
-- the {%word} tokens, and returns each word. Returns the empty list if
-- the requested word is undefined.
deps :: Dict -> Word -> [Word]
deps d w = maybe [] _words $ lookup d w

-- Each word is expressed as a {%word} token in the original ABC.
_words :: ABC -> [Word]
_words = fmap toWord . L.filter isWordTok . ABC.tokens where
    isWordTok = ('%' ==) . Char8.head
    toWord = Word . Char8.tail


type DepsMap = Map Word (Set Word)
type DiffDepsMap = Map Word (Set Word -> Set Word)

_depsMap :: Dict -> [Word] -> DepsMap
_depsMap d ws = Map.fromList $ L.zip ws (fmap (Set.fromList . deps d) ws)

-- compute a *partial* reverse dependencies. In this case, if word
-- foo uses bar, baz, qux then we'll have entries for bar, baz, qux
-- each containing the word foo. This isn't necessarily all dependencies
-- for bar, baz, and qux of course.
_revDeps :: DepsMap -> DepsMap
_revDeps = L.foldl' insw Map.empty . Map.toList where
    insw m (w,ds) = L.foldl' (insd w) m (Set.toList ds) -- insert w as client for each d
    insd w m d = Map.alter (inss w) d m  -- insert w as client for one d
    inss w = Just . maybe (Set.singleton w) (Set.insert w) -- set containing w

-- compute a difference in the dependencies maps for a subset of words.
-- In this case, both the old and new dependencies maps must be complete
-- for a subset of words. E.g. if `foo` appears under `bar` on one side
-- but not the other, we'll accordingly prepare it to add or subtract foo
-- from the underlying dependencies.
--
-- This operation includes some sanity checks. It will error out if the
-- index was not already in a good condition according to the changes to
-- be made.
--
-- _diffDeps oldDeps newDeps
_diffDeps :: DepsMap -> DepsMap -> DiffDepsMap
_diffDeps = Map.mergeWithKey joinDeps oldDeps newDeps where
    joinDeps _ sOld sNew = 
        let sAdd = sNew `Set.difference` sOld in -- new dependencies not in old
        let sSub = sOld `Set.difference` sNew in -- old dependencies not in new
        let bNoChange = Set.null sAdd && Set.null sSub in
        if bNoChange then Nothing else 
        Just $ addDeps sAdd . subDeps sSub
    newDeps = fmap addDeps
    oldDeps = fmap subDeps
    addDeps sAdd sW =
        let sOver = Set.intersection sAdd sW in
        let bSane = Set.null sOver in
        let eMsg = "overlap adding deps: " ++ show sOver in
        if not bSane then _impossible eMsg else
        Set.union sW sAdd
    subDeps sSub sW =
        let sMiss = Set.difference sSub sW in
        let bSane = Set.null sMiss in
        let eMsg = "missing expected deps: " ++ show sMiss in
        if not bSane then _impossible eMsg else
        Set.difference sW sSub

-- tune the reverse lookup map by the given delta map
_updateDeps :: DiffDepsMap -> Trie Deps -> Trie Deps
_updateDeps dd t = L.foldl' _updOneDep t (Map.toList dd) 

-- adjusting dependencies one at a time...
_updOneDep :: Trie Deps -> (Word, Set Word -> Set Word) -> Trie Deps
_updOneDep t (w,fn) = Trie.adjust adj (_wordToKey w) t where
    _toSet = Set.fromList . fmap Word . _fromSz
    _fromSet = _toSz . fmap unWord . Set.toList
    adj orig = 
        let s0 = maybe Set.empty _toSet orig in
        let sf = fn s0 in
        if Set.null sf then Nothing 
                       else Just (_fromSet sf)
    _toSz lDeps =
        if (_bigDeps lDeps) 
            then Left (vref' (Trie.trie_space t) lDeps)
            else Right lDeps

-- heuristic estimate for size
_bigDeps :: [BS.ByteString] -> Bool
_bigDeps lDeps =
    let sz = L.foldl' (+) 0 $ fmap ((4 +) . BS.length) lDeps in
    sz >= depSizeThresh


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
type InsertionErrors = [InsertionError]
data InsertionError 
    = Undef    Word Word   -- undefined {%word} used by word
    | BadWord  Word        -- word is not valid according to heuristics
    | BadToken Token Word  -- invalid {token} used by word
    | Cycle    [Word]

instance Show InsertionError where
    show (Undef uw w)   = "undefined {%" ++ show uw ++ "} in " ++ show w
    show (BadWord w)    = "malformed word: " ++ show w
    show (BadToken t w) = "rejecting token {" ++ UTF8.toString t ++ "} in " ++ show w
    show (Cycle ws)     = "cyclic dependencies: " ++ show ws

-- | Insert or Update a list of words. Any existing definition for 
-- an inserted word will be replaced. Errors are possible if a word
-- introduces a cycle or uses an undefined word, or simply has some
-- malformed content.
insert :: Dict -> [(Word, ABC)] -> Either InsertionErrors Dict
insert d l = 
    -- sanitize input for internal per-word errors
    let lBad = L.foldr (uncurry _testBadDef) [] l in
    if not (L.null lBad) then Left lBad else
    -- compute the insertion
    let d' = unsafeInsert d l in
    -- filter for cycles or undefined words
    let lInsErr = _insertionErrors d' (fmap fst l) in
    if not (L.null lInsErr) then Left lInsErr else
    Right $! d'

-- | Insert without trying to detect any errors. 
unsafeInsert :: Dict -> [(Word, ABC)] -> Dict
unsafeInsert d l = Dict { dict_data = d_data', dict_deps = d_deps' } where
    oldDepsMap = _revDeps $ _depsMap d (fmap fst l)
    newDepsMap = _revDeps $ _insertDepsMap l
    depsMapDiff = _diffDeps oldDepsMap newDepsMap
    d_deps' = _updateDeps depsMapDiff (dict_deps d)
    vc = dict_space d
    lIns = fmap (_wordToKey *** _encodeDef vc) l
    d_data' = Trie.insertList lIns (dict_data d)

_testBadDef :: Word -> ABC -> InsertionErrors -> InsertionErrors
_testBadDef w abc = badWord . badToks where
    badWord = if isValidWord w then id else (:) (BadWord w) 
    badToks = (++) (fmap (`BadToken` w) lBadToks)
    lBadToks = L.nub $ L.filter (not . isValidToken) $ ABC.tokens abc

-- dependencies for each inserted word
_insertDepsMap :: [(Word, ABC)] -> DepsMap
_insertDepsMap = Map.fromList . fmap (second (Set.fromList . _words))

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
_encodeDef vc abc = 
    let bytes = ABC.encode abc in
    let bLarge = LBS.length bytes >= fromIntegral defSizeThresh in
    if bLarge then Left (vrefc _cm vc bytes)
              else Right bytes

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
unsafeDelete d lDel = Dict { dict_data = d_data', dict_deps = d_deps' } where
    d_data' = Trie.deleteList (fmap _wordToKey lDel) (dict_data d)
    oldDepsMap = _revDeps $ _depsMap d lDel
    newDepsMap = Map.empty
    depsMapDiff = _diffDeps oldDepsMap newDepsMap
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
    rwTok t = ABC.ABC [ABC.ABC_Tok (rnTok t)]

