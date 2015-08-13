{-# LANGUAGE TypeFamilies, ViewPatterns, OverloadedStrings #-}

-- | The Wikilon Model is an abstract interface for Wikilon. This
-- API supports atomic groups of confined queries and updates with
-- purely functional glue, i.e. a monadic interface with the idea
-- that the monad may be serialized and sent to the model directly.
-- Eventually, I may need to add support for streaming interfaces
-- and reactive continuous queries, e.g. as extensions.
--
-- Conceptually, Wikilon is hosted by a separate abstract machine,
-- which is why queries and udpates are confined to computations
-- that may occur on just that machine. The long term goal is to
-- model Wikilon within Wikilon, as an abstract virtual machine,
-- to compile the AVM and remove the Haskell layer entirely.
--
-- TODO: figure out authorization concerns, perhaps at the level
-- of whole monadic operations.
--
-- NOTE: A lot of this replicates the Wikilon.Dict interface,
-- except that the monadic context allows a more natural use of
-- cache, logging, change tracking, side-effects, etc.. at the
-- cost of implicit laziness or streaming. I'll eventually 
-- deprecate the pure interface on the dict type.
--
module Wikilon.Model
    ( BranchName, Branch, Dict
    , BranchingDictionary(..), branchSnapshot
    , ABC, Token, Word, Bytes
    , DictView(..)
    , dictWordDeps, dictWordDepsT, abcWords
    , SecureHash
    , DictRLU(..), dictWordClients, dictWordClientsT
    , WordPrefix, DictSplitPrefix(..), dictSplitPrefixW
    , DictUpdate(..)
    , dictUpdateWord, dictUpdateWords
    , dictDeleteWord, dictDeleteWords
    , dictRenameWord

    
    , module Wikilon.Time
    , matchWord, matchSimpleRedirect
    , testForMalformedDef, testForCycle
    ) where

import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as State
import Control.Monad.Trans (lift)
import Data.Monoid
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as L

import Awelon.ABC (ABC, Token)
import qualified Awelon.ABC as ABC
import Awelon.Word
import Awelon.Text
import qualified Awelon.Base16 as B16
import Wikilon.Time
import Wikilon.SecureHash
import Wikilon.Dict.Text (isValidText)
import Wikilon.Dict.Token (isValidToken)

type BranchName = Word
type Bytes = LazyUTF8.ByteString


-- | Wikilon has some opaque object types, which might be understood
-- similarly to file handles or ADTs. In context of Awelon Bytecode,
-- these could be modeled as sealed values specific to the Wikilon
-- instance. In general, they shouldn't actually be returned from
-- a query or update on Wikilon.
data family Dict (m :: * -> *)
data family Branch (m :: * -> *)


-- | Wikilon has a branching dictionary model. Branches are named under
-- the same restrictions as dictionary words (to easily fit HTML). If a
-- branch has not been defined, we can treat that as an empty dictionary.
class (DictView m) => BranchingDictionary m where
    -- | Load a branch given its name. Authentication might be performed here.
    loadBranch :: BranchName -> m (Branch m)

    -- | Return the most recent dictionary for a branch.
    branchHead :: Branch m -> m (Dict m)

    -- | Update the dictionary associated with the branch.
    branchUpdate :: Branch m -> Dict m -> m ()

    -- | Obtain all available snapshots for a branch between two time values.
    -- There is no strong requirement that a branch keeps more than the head
    -- value, but if we do keep more we must be able to access them.
    --
    -- Our history is accessed in terms of:
    --
    --   (snapshot, [(tmUpdate, previousSnapshot)])
    --
    -- The tmUpdate values should fall between the requested times.
    branchHistory :: Branch m -> T -> T -> m (Dict m, [(T,Dict m)])

-- | Obtain the snapshot of a dictionary at a specific time. 
branchSnapshot :: (BranchingDictionary m) => Branch m -> T -> m (Dict m) 
branchSnapshot b t = fst <$> branchHistory b t t

-- | Given a snapshot of a dictionary, we can at least query it. While
-- query operations should be pure, they may require manipulations on a
-- mutable cache for performance reasons. More sophisticated queries
-- will also be necessary. This is very similar to Wikilon.Dict, which
-- will become a bit deprecated because it lacked good support for the
-- cache.
class (Functor m, Applicative m, Monad m) => DictView m where
    -- | Obtain a finite list that includes all words with non-empty 
    -- definitions, including incomplete or bad definitions that don't
    -- compile. 
    dictWords :: Dict m -> m [Word]

    -- | Lookup raw definition for a specific word.
    dictLookup :: Dict m -> Word -> m ABC

    -- | Raw bytes for a specific word for optimization.
    dictLookupBytes :: Dict m -> Word -> m Bytes
    dictLookupBytes d w = ABC.encode <$> dictLookup d w

    -- | Find a subset of words whose shallow definitions are different
    -- different, e.g. based on raw structure, and ignoring updates to
    -- dependencies. The default implementation performs a complete scan
    -- on both dictionaries, so is probably much less efficient than 
    -- so is probably much less efficient than desirable.
    dictDiff :: Dict m -> Dict m -> m [Word]
    dictDiff a b = 
        let hasDiff w = (/=) <$> dictLookupBytes a w <*> dictLookupBytes b w in
        let onlyInB w = LBS.null <$> dictLookupBytes a w in
        dictWords a >>= filterM hasDiff >>= \ lDiffAB ->
        dictWords b >>= filterM onlyInB >>= \ lUniqueToB ->
        return (lDiffAB <> lUniqueToB)

    -- | Obtain a secure hash that includes the word, bytecode, and
    -- transitive dependencies. This allows us to cache computations
    -- at the level of individual words, including any views of the
    -- source code. 
    --
    -- The default implementation will compute this hash on request,
    -- which does require lookup of every transitive dependency.
    dictLookupVersionHash :: Dict m -> Word -> m SecureHash
    dictLookupVersionHash d = vh mempty where
        vh c w = snd <$> vch c w
        vc c w = fst <$> vch c w
        vch c w = case Map.lookup w c of
            Just h -> return (c,h) -- do not recompute
            Nothing ->
                dictLookup d w >>= \ abc ->
                foldM vc c (abcWords abc) >>= \ c' ->
                let abc' = annoAtDef w <> rwHashWords c' abc in
                let h = secureHashLazy $ ABC.encode abc' in
                return (Map.insert w h c', h)

type HashCache = Map Word SecureHash

annoAtDef :: Word -> ABC
annoAtDef w = ABC.mkABC [ABC.ABC_Tok ("&@" <> wordToUTF8 w)]

-- rewrite words if their hash is included
rwHashWords :: HashCache -> ABC -> ABC
rwHashWords cache = ABC.rewriteTokens rw where
    rw t = [ABC.ABC_Tok (tryWordHash t)]
    tryWordHash t = maybe t h2t (fromCache t)
    h2t = BS.pack . (35 :) . B16.encode . BS.unpack
    fromCache t = 
        UTF8.uncons t >>= \ (c,w) ->
        guard ('%' == c) >>
        Map.lookup (Word w) cache
       

-- | Each word is expressed as a {%word} token in the original ABC.
abcWords :: ABC -> [Word]
abcWords = mapMaybe wordTok . ABC.tokens where
    wordTok tok = case UTF8.uncons tok of
        Just ('%', w) -> Just (Word w)
        _ -> Nothing

-- | Find all words depended upon by a given word. Filters ABC for just
-- the {%word} tokens, and returns each word. Word dependencies may occur
-- anywhere within a definition.
dictWordDeps :: (DictView m) => Dict m -> Word -> m [Word]
dictWordDeps d w = abcWords <$> dictLookup d w

-- | Find transitive dependencies for a list of root words, ordered such
-- that dependencies of a word appear before the same word in the list.
-- Each word in the input list will appear in the output, and words are
-- in the output list at most once.
dictWordDepsT :: (DictView m) => Dict m -> [Word] -> m [Word]
dictWordDepsT d = accum [] mempty where
    accum r _ [] = return (L.reverse r)
    accum r v ws@(w:ws') =
        if Set.member w v then accum r v ws' else
        dictWordDeps d w >>= \ lWords ->
        let lNewDeps = L.filter (`Set.notMember` v) lWords in
        if L.null lNewDeps 
            then accum (w:r) (Set.insert w v) ws'
            else accum r v (lNewDeps <> ws)

-- | Wikilon needs a reverse-lookup index to easily locate all
-- words that use a specific token. This might be computed per
-- dictionary. 
class (DictView m) => DictRLU m where
    -- | Find all words that directly use a specific token.
    dictTokenClients :: Dict m -> Token -> m [Word]

-- TODO: support for full-text index, type and value indices 

-- | Find all words that directly use a specific word.
dictWordClients :: DictRLU m => Dict m -> Word -> m [Word]
dictWordClients d = dictTokenClients d . BS.cons 37 . unWord

-- | Find transitive clients for a list of root words, ordered such
-- that all clients of a word appear in the list after the word. This
-- list corresponds roughly to an update schedule, e.g. an order for
-- typechecking and compiling after an update to the initial list of
-- words. 
dictWordClientsT :: DictRLU m => Dict m -> [Word] -> m [Word]
dictWordClientsT d = accum [] mempty where
    accum r _ [] = return r
    accum r v ws@(w:ws') = 
        if Set.member w v then accum r v ws' else
        dictWordClients d w >>= \ lClients ->
        let lNewClients = L.filter (`Set.notMember` v) lClients in
        if L.null lNewClients
            then accum (w:r) (Set.insert w v) ws'
            else accum r v (lNewClients <> ws)

-- | A word prefix is a bytestring. It is *not* necessarily a full
-- UTF-8 string, i.e. because we might split within a UTF-8 character.
type WordPrefix = BS.ByteString

-- | Ultimately, I'll need many effective means to browse dictionaries:
-- by type, by full text search, by common suffix or prefix. But a useful
-- option for the short term is to browse words by prefix, which is useful
-- for both tab-completion and directory-style search. 
class (DictView m) => DictSplitPrefix m where
    -- | Given a prefix, provide a list of associated words and larger
    -- prefixes that begin with the requested prefix. Any given step
    -- must be relatively shallow. A word must not be redundant with any
    -- returned prefix.
    dictSplitPrefix :: Dict m -> WordPrefix -> m [Either WordPrefix Word]

    -- | Obtain a complete list of words with a given prefix. The default
    -- implementation exhaustively repeats splitOnPrefix until only words
    -- remain.
    dictPrefixWords :: Dict m -> WordPrefix -> m [Word]
    dictPrefixWords d p = 
        dictSplitPrefix d p >>= 
        mapM (either (dictPrefixWords d) (return . return)) >>=
        return . L.concat

-- | dictSplitPrefixW is a variation of dictSplitPrefix that prevents
-- splitting of a prefix on anything but a complete character.
dictSplitPrefixW :: DictSplitPrefix m => Dict m -> WordPrefix -> m [Either WordPrefix Word]
dictSplitPrefixW d = dictSplitPrefix d >=> mapM repair >=> fin where
    repair = either repairP keepW
    keepW = return . return . Right
    keepP = return . return . Left
    fin = return . L.concat
    okAsWord p = (BS.last p < 0x80) 
             || (L.last (UTF8.toString p) /= UTF8.replacement_char)
    repairP p = if okAsWord p then keepP p else dictSplitPrefixW d p 


-- | Modify a dictionary. These modifications aren't automatically 
-- committed to any branch. Wikilon should guard against introducing
-- cycles
class (DictView m) => DictUpdate m where
    dictUnsafeUpdateWord :: Word -> ABC -> Dict m -> m (Dict m)

    dictUnsafeUpdateWords :: Map Word ABC -> Dict m -> m (Dict m)
    dictUnsafeUpdateWords = flip (foldM accum) . Map.toList where
        accum d (w,abc) = dictUnsafeUpdateWord w abc d

-- | remove a word's definition (if any)
dictDeleteWord :: (DictUpdate m) => Word -> Dict m -> m (Dict m)
dictDeleteWord = dictDeleteWords . (:[])

-- | delete a list of words. Deleting words can break types and such,
-- but doesn't break any of the basic properties that Wikilon requires.
-- (E.g. deletion cannot introduce a cycle, add a bad word or text, etc..)
-- So this is a lot faster than a safe word update.
dictDeleteWords :: (DictUpdate m) => [Word] -> Dict m -> m (Dict m)
dictDeleteWords = dictUnsafeUpdateWords . Map.fromList . withEmpties where
    withEmpties = flip L.zip (L.repeat mempty)

-- | Match bytecode of form `{%foo}`, return word foo
matchWord :: ABC -> Maybe Word
matchWord abc = case ABC.abcOps abc of
    [ABC.ABC_Tok t] -> case BS.uncons t of
        Just (37,w) -> return (Word w)
        _ -> Nothing
    _ -> Nothing

-- | Match bytecode of the form `[{%foo}][]`; return word foo.
-- This corresponds to a simple redirect to word foo, and has
-- the same observable behavior as foo.
matchSimpleRedirect :: ABC -> Maybe Word
matchSimpleRedirect abc = case ABC.abcOps abc of
    [ABC.ABC_Block cmd, ABC.ABC_Block cc] | (cc == mempty) -> matchWord cmd 
    _ -> Nothing

-- | Rename a word in a dictionary without modifying its behavior.
-- This will succeed only under one of the following conditions:
--   
--   1. if the target word is undefined and unused (proper rename)
--   2. if the target and origin words have the same definition (merge)
--   3. if one of the words is a simple redirect to the other
--
-- A simple redirect is a definition of the form `[{%foo}][]`. In case
-- of redirects, we'll favor the redirect target's definition. If the
-- rename isn't a 'proper' one by the above conditions, rename may be 
-- lossy.
--
-- If a rename condition isn't met, Nothing is returned. On success,
-- the origin word is undefined, the target word has the origin's 
-- definition (unless the origin was a redirect to the target, in which
-- case the target's definition is not modified), and all references
-- to the origin word now directly reference the target.
-- 
dictRenameWord :: (DictRLU m, DictUpdate m) => Word -> Word -> Dict m -> m (Maybe (Dict m))
dictRenameWord wOrigin wTarget d =
    if (wOrigin == wTarget) then return (Just d) else -- trivial rename
    -- stuff I might need to know
    dictLookup d wOrigin >>= \ abcOrigin ->
    dictLookup d wTarget >>= \ abcTarget ->
    dictWordClients d wTarget >>= \ lClientsTarget ->
    dictWordClients d wOrigin >>= \ lClientsOrigin ->

    -- test safe rename conditions
    let bNewTarget = (mempty == abcTarget) && (L.null lClientsTarget) in
    let bMatchDefs = (abcOrigin == abcTarget) in
    let bOriginRedirectsToTarget = maybe False (== wTarget) (matchSimpleRedirect abcOrigin) in
    let bTargetRedirectsToOrigin = maybe False (== wOrigin) (matchSimpleRedirect abcTarget) in
    let bSafeRename = bNewTarget || bMatchDefs || bOriginRedirectsToTarget || bTargetRedirectsToOrigin in
    if not bSafeRename then return Nothing else

    -- compute the update, including references to the origin word
    let updC w = dictLookup d w >>= return . (,) w . _renameInABC wOrigin wTarget in
    mapM updC lClientsOrigin >>= \ lOriginClientUpdates ->
    let updateOriginDef = Map.insert wOrigin mempty in
    let updateTargetDef = if bOriginRedirectsToTarget then id else Map.insert wTarget abcOrigin in
    let allUpdates = updateOriginDef $ updateTargetDef $ Map.fromList lOriginClientUpdates in

    -- apply update and return
    dictUnsafeUpdateWords allUpdates d >>= \ d' ->
    return (Just d')
    
-- rename a word within context of ABC
_renameInABC :: Word -> Word -> ABC -> ABC
_renameInABC wo wt = ABC.rewriteTokens rwTok where
    t0 = BS.cons 37 $ wordToUTF8 wo
    tf = BS.cons 37 $ wordToUTF8 wt
    rnTok t = if (t == t0) then tf else t
    rwTok t = [ABC.ABC_Tok (rnTok t)]

data InsertionError
    = BadWord !Word         -- doesn't meet heuristic constraints
    | BadToken !Token !Word -- Wikilon permits annotations, links, value sealers
    | BadText !Text !Word   -- limited against control characters, U+FFFD
    | Cycle !(Cycle Word)   -- no cycles allowed!
    | DupWord !Word         -- you tried to define a word twice
    deriving (Eq, Ord)

instance Show InsertionError where
    show (BadWord w)    = "malformed word: " ++ show w
    show (BadToken t w) = "rejecting token " ++ show (ABC.ABC_Tok t) ++ " in " ++ show w
    show (Cycle c)      = "cyclic dependencies: " ++ show c
    show (BadText t w)  = "in word" ++ show w ++ " malformed text: " ++ show (ABC.ABC_Text t)
    show (DupWord w)    = "word " ++ show w ++ " is assigned more than once"

-- | Safely update a single word.
dictUpdateWord :: DictUpdate m => Word -> ABC -> Dict m -> m (Either [InsertionError] (Dict m))
dictUpdateWord w abc = dictUpdateWords [(w,abc)]

-- | Safely update a collection of words. This is 'safe' in the sense that
-- it prevents a certain subset of errors: cycles, malformed words, limits
-- on texts and tokens. As many errors are reported as feasible. 
--
-- Higher levels of automatic curation are feasible, but will probably require
-- a long-running background computation (e.g. to automate compilation, tests,
-- typechecking). So those will probably be handled by having two branches: 
-- the curated branch only updated after the maintenance branch has validated.
--
dictUpdateWords :: (DictUpdate m) => [(Word, ABC)] -> Dict m -> m (Either [InsertionError] (Dict m))
dictUpdateWords [] d = return (Right d) -- trivially no changes
dictUpdateWords l d =
    let lWords = fmap fst l in
    let lDupErrors = fmap DupWord $ findDups $ lWords in
    let lMalformed = L.concatMap (uncurry testForMalformedDef) l in
    dictUnsafeUpdateWords (Map.fromList l) d >>= \ d' ->
    testForCycle lWords d' >>= \ mbCycle ->
    let lCycleErrors = maybeToList (fmap Cycle mbCycle) in
    let lErrors = lDupErrors ++ lMalformed ++ lCycleErrors in
    return $ if L.null lErrors then Right d' else Left lErrors
 
findDups :: (Eq a) => [a] -> [a]
findDups = f [] where
    f r (x:xs) =
        let bNewDup = L.elem x xs && L.notElem x r in
        if bNewDup then f (x:r) xs else f r xs
    f r [] = L.reverse r

-- | Validate heuristic constraints on words and bytecode.
testForMalformedDef :: Word -> ABC -> [InsertionError]
testForMalformedDef w = (malformedWord ++) . abcErrors where
    malformedWord = if isValidWord w then [] else [BadWord w]
    abcErrors = L.concatMap opError . ABC.abcOps
    opError (ABC.ABC_Tok t) | not (isValidToken t) = [BadToken t w]
    opError (ABC.ABC_Text t) | not (isValidText t) = [BadText t w]
    opError (ABC.ABC_Block b) = abcErrors b
    opError _ = []

-- | A cycle is expressed as a chain of dependencies that is implicitly
-- closed (i.e. no words are repeated). Cycles must be non-empty. 
type Cycle a = [a]

-- | Search under a given list of words for cycles. If such a cycle
-- exists, this function certainly finds it. However, I won't attempt 
-- to return an exhaustive set of cycles. 
testForCycle :: (DictView m) => [Word] -> Dict m -> m (Maybe (Cycle Word))
testForCycle ws d = flip State.evalStateT mempty $ fc (dictWordDeps d) [] ws

-- generic, monadic, directed cycle detection from a list of initial points
fc :: (Ord a, Monad m) => (a -> m [a]) -> [a] -> [a] -> State.StateT (Set a) m (Maybe (Cycle a))
fc _ _ [] = return Nothing
fc readAdj stack (x:xs) =
    let continue = fc readAdj stack xs in
    State.gets (Set.member x) >>= \ bAlreadyTested ->
    if bAlreadyTested then continue else
    if L.elem x stack then return (Just (_cyc x stack)) else
    lift (readAdj x) >>= fc readAdj (x:stack) >>= \ cycleUnderX ->
    case cycleUnderX of
        Nothing -> State.modify (Set.insert x) >> continue
        _ -> return cycleUnderX
    
-- return just the cycle found (implicitly closed)
_cyc :: (Eq a) => a -> [a] -> Cycle a
_cyc a stack = L.dropWhile (/= a) $ L.reverse stack

