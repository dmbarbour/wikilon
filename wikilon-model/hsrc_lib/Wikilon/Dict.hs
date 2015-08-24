{-# LANGUAGE OverloadedStrings #-}

-- | Awelon Object (AO) Dictionary Model
--
-- A dictionary is simply an association from words to definitions.
--
-- In Wikilon, a definition is a function that returns a value and a
-- function to compile this value. The result of compilation is also
-- a function, the meaning of the definition.
--
--      type Def a b = ∃v.∀e. e→([v→[a→b]]*(v*e))
--
-- Within a definition, value `v` serves a role as a syntax, perhaps
-- a DSL or content for a structured editor. The function `[v→[a→b]]` 
-- is the compiler for the value `v`. Applying compiler to value will
-- return function `[a→b]`, which is the meaning of the definition.
-- Usefully, the identity function is a valid compiler for low level
-- code, where [a→b] is expressed directly as an opaque function.
--
-- Dependencies between words use tokens of form {%foo} and {%bar}.
-- Dependencies must be acyclic and are contained to the dictionary.
-- Static compilation and linking can be achieved by inlining the 
-- compiled [a→b] meaning for each word. The resulting ABC stream
-- may be interpreted or further compiled for performance.
--
-- In a healthy dictionary:
--
--   1. every word is defined
--   2. dependencies are acyclic
--   3. all definitions compile
--   4. every meaning typechecks
--
-- During development, it is not unusual for words to be undefined.
-- This is common for top-down programming, and to represent 'holes'
-- in a dictionary that may be filled with support of type inference,
-- tests analysis, simulations or machine learning, etc.. 
--
-- Typechecking isn't precisely defined for AO or ABC. In general, 
-- it includes all sorts of linting and abstract interpretation to
-- find errors. A goal of Awelon project is to be very open to adding
-- new automatic, continuous background analyses such that developers
-- can have near real-time information about dictionary health. 
--
-- Wikilon ensures that dictionaries are acyclic. Undefined words are
-- permitted because they're useful for development. Wikilon requires
-- dictionaries obey some strict constraints on the ABC. Only four
-- token types are permitted (words, annotations, discretionary sealer,
-- discretionary unsealer). Tokens and texts are further constrained to
-- ensure easy interaction with HTML forms, URLs, etc.. Typechecking
-- and other higher validation is left to background processes.
--
module Wikilon.Dict
    ( Dictionary
    , DictView(..)
    , wordsInDict
    , wordDeps, abcWords
    , transitiveDepsList
    , ABC
    , SecureHash
    , defaultVersionHash

    , WordPrefix
    , DictSplitPrefix(..)
    , splitOnPrefixWords
    , splitOnPrefixChars
    , DictRLU(..)
    , wordClients
    , transitiveClientsList
    , DictUpdate(..)
    , deleteWord
    , deleteWords
    , updateWord
    , updateWords
    , renameWord
    , InsertionError(..)
    , Cycle
    , module Wikilon.Dict.Word
    , module Wikilon.Dict.Token
    ) where

import Prelude hiding (lookup)
import Control.Monad
import Data.Maybe (mapMaybe, maybeToList)
import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Control.Monad.State as State
import Data.Char (chr)
import Data.Word (Word8)

import Awelon.ABC (ABC)
import qualified Awelon.ABC as ABC
import qualified Awelon.Base16 as B16

import Wikilon.SecureHash
import Wikilon.Dict.Word 
import Wikilon.Dict.Text
import Wikilon.Dict.Token

-- | All of Wikilon's dictionary requirements
class ( DictView dict
      , DictSplitPrefix dict
      , DictRLU dict
      , DictUpdate dict
      ) => Dictionary dict


-- | Basic view of a dictionary. Note that an undefined word will
-- return the empty ABC string (mempty). 
--
-- Note that an empty definition is distinct from an identity definition
-- due to the staging. An identity function has a form such as `[][]`.
class DictView dict where
    -- | Lookup the ABC definition for a word in a dictionary.
    lookup :: dict -> Word -> ABC

    -- | Lookup the raw bytestring for a definition. This has a default
    -- implementation, but may be optimized for a model.
    lookupBytes :: dict -> Word -> LazyUTF8.ByteString
    lookupBytes d = ABC.encode . lookup d

    -- | Obtain complete listing for dictionary.
    toList :: dict -> [(Word, ABC)]

    -- | Find a list of words that are distinct between two dictionaries.
    --
    -- The default implementation is exhaustive and unsuitable for large
    -- dictionaries. Ideally, this function is replaced by something much
    -- more efficient because fast diffs will be a very important feature.
    dictDiff :: dict -> dict -> [Word]
    dictDiff a b = Map.keys $ mapDiff (==) (m a) (m b) where
        m = Map.fromList . toList

    -- | Return a secure hash associated with the current version of a
    -- word's structure and meaning including transitive dependencies.
    -- This is used as an index for cached computations on a definition.
    -- The default implementation recomputes the hash each time.
    lookupVersionHash :: dict -> Word -> SecureHash
    lookupVersionHash = defaultVersionHash . lookup

-- Thoughts: Persistent cache of a version hash is expensive enough to be
-- noticeable when updating a dictionary in a widely used word. Is there 
-- a better way to do this? Also, I'll need some better ways to cache whole
-- dictionary computations, such as maintaining lists of words that do not
-- compile or typecheck.

-- | A default implementation for lookupVersionHash. Computes an ABC
-- resource identifier based on a naive conversion.
defaultVersionHash :: (Word -> ABC) -> Word -> SecureHash
defaultVersionHash lu wRoot = fromCache c where
    fromCache = maybe mempty id . Map.lookup wRoot
    c = _addToCache getABC mempty wRoot
    getABC w = annoAtDef w <> lu w
    annoAtDef w = ABC.mkABC [ABC.ABC_Tok ("&@" <> wordToUTF8 w)]

-- accumulate hashes in a temporary cache
type HashCache = Map Word SecureHash
_addToCache :: (Word -> ABC) -> HashCache -> Word -> HashCache
_addToCache fn c w = case Map.lookup w c of
    Just _ -> c
    Nothing -> 
        let abc = fn w in
        let c' = L.foldl' (_addToCache fn) c (abcWords abc) in
        let h = secureHashLazy $ ABC.encode $ _rwHashWords c' abc in
        Map.insert w h c'

_rwHashWords :: HashCache -> ABC -> ABC
_rwHashWords cache = ABC.rewriteTokens rw where
    rw t = [ABC.ABC_Tok (hashWordTok t)]
    h2t = BS.pack . (35 :) . B16.encode . BS.unpack
    hashWordTok t = maybe t h2t $ hashWord t
    hashWord t = 
        BS.uncons t >>= \ (c,w) ->
        guard (37 == c) >>
        Map.lookup (Word w) cache

-- generic 2-way diff element
data MapDiff a b = LeftOnly a | RightOnly b | FoundDiff a b
-- map diff given equality function
mapDiff :: (Ord k) => (a -> b -> Bool) -> Map k a -> Map k b -> Map k (MapDiff a b)
mapDiff eq = Map.mergeWithKey jf lf rf where
    lf = fmap LeftOnly
    rf = fmap RightOnly
    jf _ a b | eq a b = Nothing
             | otherwise = Just (FoundDiff a b)

-- | Obtain a list of words defined within from a dictionary.
wordsInDict :: (DictView dict) => dict -> [Word]
wordsInDict = fmap fst . toList

-- | Find all words depended upon by a given word. Filters ABC for just
-- the {%word} tokens, and returns each word. Word dependencies may occur
-- anywhere within a definition.
wordDeps :: (DictView dict) => dict -> Word -> [Word]
wordDeps d = abcWords . lookup d

-- | Each word is expressed as a {%word} token in the original ABC.
abcWords :: ABC -> [Word]
abcWords = mapMaybe wordTok . ABC.tokens where
    wordTok tok = case UTF8.uncons tok of
        Just ('%', w) -> Just (Word w)
        _ -> Nothing

-- | Given a list of words, compute all the transitive dependencies 
-- of those words, ordered such that a word appears in the list only
-- after all of its dependencies, and no more than once. This is 
-- useful for exporting just a minimal subset of a dictionary for 
-- a given set of root words.
transitiveDepsList :: (DictView dict) => dict -> [Word] -> [Word]
transitiveDepsList d = accum mempty where
    accum _ [] = []
    accum v ws@(w:ws') = 
        if Set.member w v then accum v ws' else
        let lDeps = L.filter (`Set.notMember` v) $ wordDeps d w in
        if L.null lDeps 
            then (w : accum (Set.insert w v) ws')
            else accum v (lDeps ++ ws)

-- | A word prefix is a bytestring. It is *not* necessarily a full
-- UTF-8 string, i.e. because we might split within a UTF-8 character.
type WordPrefix = BS.ByteString

-- | for browsing a dictionary in a breadth-first manner
class (DictView dict) => DictSplitPrefix dict where
    -- | Given a prefix, provide a list of associated words and larger
    -- prefixes that begin with the requested prefix. Any given step
    -- must be relatively shallow. For aesthetic reasons, a word should
    -- not be contained redundantly with a similar prefix.
    splitOnPrefix ::  dict -> WordPrefix -> [Either WordPrefix Word]

    -- | Obtain a complete list of words with a given prefix. The default
    -- implementation exhaustively repeats splitOnPrefix until only words
    -- remain.
    wordsWithPrefix :: dict -> WordPrefix -> [Word]
    wordsWithPrefix dict = L.concatMap deeply . splitOnPrefix dict where
        deeply = either (wordsWithPrefix dict) return

-- | splitOnPrefixWords is a variation of Dict.splitOnPrefix that ensures
-- every prefix in the output is also a valid Word. This is convenient 
-- if we must somehow render our prefixes, e.g. for a user to browse.
-- (However, it is a little less efficient than splitOnPrefix.)
splitOnPrefixWords :: DictSplitPrefix dict => dict -> WordPrefix -> [Either WordPrefix Word]
splitOnPrefixWords dict = L.concatMap repair . splitOnPrefix dict where
    repair = either repairPrefix (return . Right)
    okAsWord p = (BS.last p < 0x80) || (isValidWord (Word p))
    repairPrefix p = 
        if okAsWord p then return (Left p) else
        splitOnPrefixWords dict p

-- | splitOnPrefixChars will break a word into prefixes based on a
-- given set of ASCII-range characters, e.g. :.!$. This is useful
-- for aesthetic purposes. Note: we only split on characters in the
-- ASCII range at this time. These prefix characters should probably
-- be followed by more than one element, normally.
--
-- This might trade performance for aesthetics, e.g. because the 
-- prefixes returned aren't fully aligned with the underlying tree
-- structure of the dictionary.
splitOnPrefixChars :: DictSplitPrefix dict => (Char -> Bool) -> dict -> WordPrefix -> [Either WordPrefix Word]
splitOnPrefixChars = splitOnPrefixW8 . fw8 where
    fw8 f w8 = (w8 < 0x80) && (f $ chr $ fromIntegral w8) 

splitOnPrefixW8 :: DictSplitPrefix dict => (Word8 -> Bool) -> dict -> WordPrefix -> [Either WordPrefix Word]
splitOnPrefixW8 fb dict p0 = L.nub $ L.concatMap repair $ splitOnPrefix dict p0 where
    repair = either repairPrefix (return . Right) -- keep words, adjust prefixes
    repairPrefix fullPrefix =
        -- fullPrefix might be too large, e.g. 'foo:ba' as prefix for
        -- 'foo:bar' and 'foo:baz' when we could stop at 'foo:'.
        let p = fst $ BS.breakEnd fb fullPrefix in
        let bValidP = (BS.length p > BS.length p0) in
        if bValidP then return (Left p) else
        splitOnPrefixW8 fb dict fullPrefix

-- | It's very useful to know who uses what. We'll do this at the
-- granularity of individual tokens, since it's also very useful
-- to quickly discover all clients of a discretionary sealer or
-- annotation.
class (DictView dict) => DictRLU dict where
    -- | Find clients of an arbitrary token.
    tokenClients :: dict -> Token -> [Word]

-- | Find direct clients of a word.
wordClients :: (DictRLU dict) => dict -> Word -> [Word]
wordClients d = tokenClients d . BS.cons 37 . unWord

-- | Given a list of words, compute the transitive clients for those
-- words ordered such that a word appears before any of its clients.
-- This clients list corresponds, for example, to the set of words
-- whose behavior, type, version hash, compiled objects, etc. might
-- have become invalid after an update to words in the input list.
transitiveClientsList :: (DictRLU dict) => dict -> [Word] -> [Word]
transitiveClientsList d = L.reverse . accum mempty where
    accum _ [] = []
    accum v ws@(w:ws') = 
        if Set.member w v then accum v ws' else
        let lClients = L.filter (`Set.notMember` v) $ wordClients d w in
        if L.null lClients
            then (w : accum (Set.insert w v) ws')
            else accum v (lClients ++ ws)

-- | Update definitions in a dictionary. Note that no validation
-- logic is performed by these operations, and the update may diverge
-- or fail silently in case of cycles or other problem cases.
--
-- Clients should instead use `updateWord(s)`, which adds the basic
-- safety checks, or specialized update functions like delete, rename.
class (DictView dict) => DictUpdate dict where
    unsafeUpdateWord :: Word -> ABC -> dict -> dict

    unsafeUpdateWords :: Map Word ABC -> dict -> dict
    unsafeUpdateWords = flip (L.foldl' upd) . Map.toList where
        upd d (w,abc) = unsafeUpdateWord w abc d 

-- | update a word's definition to the empty ABC program
deleteWord :: (DictUpdate dict) => Word -> dict -> dict
deleteWord = deleteWords . (:[])

-- | delete a list of words.
deleteWords :: (DictUpdate dict) => [Word] -> dict -> dict
deleteWords = unsafeUpdateWords . Map.fromList . withEmpties where
    withEmpties = flip L.zip $ L.repeat mempty


-- | Match bytecode of form `{%foo}`, return word foo
matchWordLink :: ABC -> Maybe Word
matchWordLink abc = case ABC.abcOps abc of
    [ABC.ABC_Tok t] -> case BS.uncons t of
        Just (37,w) -> return (Word w)
        _ -> Nothing
    _ -> Nothing

-- | Match bytecode of the form `[{%foo}][]`; return word foo. This
-- corresponds to a simple redirect to word foo, and has the same
-- functional behavior as foo. (Some non-functional behaviors may 
-- differ, e.g. secure hash, redirects at presentation layer.)
matchSimpleRedirect :: ABC -> Maybe Word
matchSimpleRedirect abc = case ABC.abcOps abc of
    [ABC.ABC_Block cmd, ABC.ABC_Block cc] | (cc == mempty) -> matchWordLink cmd 
    _ -> Nothing

-- | Rename a word in a dictionary without modifying its behavior.
-- This will succeed only under one of the following conditions:
--   
--   1. if the target word is fresh (undefined, unused)
--   2. if target and origin have the same definition (merge)
--   3. if one of the words is a simple redirect to the other
--
-- A simple redirect is a definition of the form `[{%foo}][]`, which
-- conveniently also corresponds to claw code containing a single word.
--
-- If a rename condition isn't met, Nothing is returned. On success,
-- the origin word is undefined, the target word has the origin's 
-- definition (unless the origin was a redirect to the target), and 
-- all clients of origin are modified to use target instead.
-- 
renameWord :: (DictRLU dict, DictUpdate dict) => Word -> Word -> dict -> Maybe dict
renameWord wOrigin wTarget d =
    if (wOrigin == wTarget) then return d else -- trivial rename

    -- test safe rename conditions
    let abcOrigin = lookup d wOrigin in
    let abcTarget = lookup d wTarget in
    let bNewTarget = (mempty == abcTarget) && (L.null (wordClients d wTarget)) in
    let bMatchDefs = (abcOrigin == abcTarget) in
    let bOriginRedirectsToTarget = maybe False (== wTarget) (matchSimpleRedirect abcOrigin) in
    let bTargetRedirectsToOrigin = maybe False (== wOrigin) (matchSimpleRedirect abcTarget) in
    let bSafeUpdate = bNewTarget || bMatchDefs || bTargetRedirectsToOrigin || bOriginRedirectsToTarget in
    if not bSafeUpdate then Nothing else

    -- compute the update (which we know by construction to be safe)
    let updateOrigin = Map.insert wOrigin mempty in
    let updateTarget = if bOriginRedirectsToTarget then id else Map.insert wTarget abcOrigin in
    let fnUpd wClient = (,) wClient $ _renameInABC wOrigin wTarget (lookup d wClient) in
    let updateClients = Map.fromList $ fmap fnUpd (wordClients d wOrigin) in
    let allUpdates = updateOrigin $ updateTarget $ updateClients in

    -- apply the update
    return (unsafeUpdateWords allUpdates d)

-- rename a word within context of ABC
_renameInABC :: Word -> Word -> ABC -> ABC
_renameInABC wo wt = ABC.rewriteTokens rwTok where
    t0 = BS.cons 37 $ wordToUTF8 wo
    tf = BS.cons 37 $ wordToUTF8 wt
    rnTok t = if (t == t0) then tf else t
    rwTok t = [ABC.ABC_Tok (rnTok t)]

-- | Errors recognized by safeUpdateWords
data InsertionError 
    = BadWord  !Word         -- word is not valid according to heuristics
    | BadToken !Token !Word  -- invalid {token} used within word's definition
    | BadText  !Text !Word   -- rejecting text on heuristic constraints
    | Cycle    !(Cycle Word) -- a cycle was discovered
    | DupWord  !Word         -- word appears multiple times in request
    deriving (Eq, Ord)

instance Show InsertionError where
    show (BadWord w)    = "malformed word: " ++ show w
    show (BadToken t w) = "rejecting token " ++ show (ABC.ABC_Tok t) ++ " in " ++ show w
    show (Cycle c)     = "cyclic dependencies: " ++ show c
    show (BadText t w)  = "in word" ++ show w ++ " malformed text: " ++ show (ABC.ABC_Text t)
    show (DupWord w)    = "word " ++ show w ++ " is assigned more than once"

-- | Update words after testing for the most obvious, cheaply discovered
-- errors (cf InsertionError). Normal updates to a dictionary should be
-- performed via this function to guard against cycles and so on. In case
-- of errors, this tries to return many errors at once.
--
-- Note that leaving words undefined is not considered an error at this
-- level. Undefined words are 'holes' in the dictionary, potentially useful
-- for assisted, type-driven, or top-down development.
updateWords :: (DictUpdate dict) => [(Word, ABC)] -> dict -> Either [InsertionError] dict
updateWords [] d = Right d
updateWords l d =
    let lWords = fmap fst l in
    let lDupErrors = fmap DupWord $ findDups $ lWords in
    let lMalformed = L.concatMap (uncurry testForMalformedDef) l in

    let updMap = Map.fromList l in
    let adj w = maybe (wordDeps d w) abcWords (Map.lookup w updMap) in
    let mbCycle = State.evalState (fc adj [] lWords) mempty in
    let lCycleErrors = maybeToList $ fmap Cycle $ mbCycle in
    let lErrors = lDupErrors <> lMalformed <> lCycleErrors in
    let bHasErrors = not $ L.null lErrors in
    if bHasErrors then Left lErrors else
    Right (unsafeUpdateWords updMap d)

updateWord :: (DictUpdate dict) => Word -> ABC -> dict -> Either [InsertionError] dict
updateWord w abc = updateWords [(w,abc)]

findDups :: (Eq a) => [a] -> [a]
findDups = f [] where
    f r (x:xs) =
        let bNewDup = L.elem x xs && L.notElem x r in
        if bNewDup then f (x:r) xs else f r xs
    f r [] = L.reverse r

-- | Validate constraints on internal tokens and words.
testForMalformedDef :: Word -> ABC -> [InsertionError]
testForMalformedDef w = (malformedWord ++) . abcErrors where
    malformedWord = if isValidWord w then [] else [BadWord w]
    abcErrors = L.concatMap opError . ABC.abcOps
    opError (ABC.ABC_Tok t) | not (isValidToken t) = [BadToken t w]
    opError (ABC.ABC_Text t) | not (isValidText t) = [BadText t w]
    opError (ABC.ABC_Block b) = abcErrors b
    opError _ = []

-- | A cycle is expressed as a chain of dependencies that is implicitly
-- closed (i.e. no words are repeated). Cycles are non-empty. 
type Cycle a = [a]

-- | generic cycle discovery given an adjacency list and an initial
-- frontier.
fc :: (Ord a) => (a -> [a]) -> [a] -> [a] -> State.State (Set a) (Maybe (Cycle a))
fc _ _ [] = return Nothing
fc adj stack (x:xs) =
    State.gets (Set.member x) >>= \ bSafe ->
    if bSafe then fc adj stack xs else
    if L.elem x stack then return $ Just (_cyc x stack) else
    fc adj (x:stack) (adj x) >>= \ cycleUnderX ->
    case cycleUnderX of
        Nothing -> State.modify (Set.insert x) >> fc adj stack xs
        cycleFound -> return cycleFound 
    
-- return just the cycle found (implicitly closed)
_cyc :: (Eq a) => a -> [a] -> Cycle a
_cyc a stack = L.dropWhile (/= a) $ L.reverse stack

