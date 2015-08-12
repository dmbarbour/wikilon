{-# LANGUAGE TypeFamilies, ViewPatterns #-}

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
    , BranchingDictionary(..)
    , BranchHistory(..)
    , ABC, Token, Word, Bytes
    , DictView(..)
    , dictWordDeps, dictWordDepsT, abcWords
    , SecureHash
    , DictRLU(..)
    , WordPrefix
    , DictSplitPrefix(..)
    
    , module Wikilon.Time
    ) where

import Control.Monad
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Awelon.ABC (ABC, Token)
import qualified Awelon.ABC as Awelon
import Dict.Word
import Wikilon.Time
import Wikilon.SecureHash

type BranchName = Word
type Bytes = LazyUTF8.ByteString

-- | Wikilon has some existential or sealed value types that cannot 
-- readily be exported from the machine. One might consider them to
-- be a lot like file handles, except that there is no particular 
-- need to open or close them. We can read the `m` here as monad or
-- machine.
data family Dict m
data family Branch m

-- anything else? 
--  maybe handles for capabilities or RDP features? 
--  though, any capabilities should be explicitly serializable (e.g. to an ABC value) 
--  maybe access to a 'value' type? though I could build on ABC here.
--  thoug

-- | Wikilon has a branching dictionary model. Branches are named under
-- the same restrictions as dictionary words (to easily fit HTML). If a
-- branch has not been defined, we can treat that as an empty dictionary.
class BranchingDictionary m where
    loadBranch :: BranchName -> m (Branch m)
    branchHead :: Branch m -> m (Dict m)
    branchUpdate :: Branch m -> Dict m -> m ()

-- | Each branch will also support an incomplete history, e.g. leveraging
-- a simple exponential decay model.
class BranchHistory m where
    -- | Obtain all available snapshots between two time values. The
    -- minBound and maxBound times can be used as effective infinities.
    branchSnapshots :: Branch m -> T -> T -> m [(T,Dict m)]

    -- | Obtain the snapshot of a dictionary at a specific time. If the
    -- branch is not defined at the requested time, we'll 
    branchSnapshot :: Branch m -> T -> m (Dict m) 

-- | Given a snapshot of a dictionary, we can at least query it. While
-- query operations should be pure, they may require manipulations on a
-- mutable cache for performance reasons. More sophisticated queries
-- will also be necessary. This is very similar to Wikilon.Dict, which
-- will become a bit deprecated because it lacked good support for the
-- cache.
class (Monad m) => DictView m where
    -- | Obtain a finite list that includes all words with non-empty 
    -- definitions, including incomplete or bad definitions that don't
    -- compile. 
    dictWords :: Dict m -> m [Word]

    -- | Lookup raw definition for a specific word.
    dictLookup :: Dict m -> Word -> m Awelon.ABC

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
        dictWords a >>= filterM hasDiff >>= \ lDiff ->
        dictWords b >>= filterM onlyInB >>= \ lUniqueToB ->
        lDiff <> lUniqueToB

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
        vch c w = case Map.lookup w cached of
            Just hash -> return (cached,hash) -- do not recompute
            Nothing ->
                dictLookup d w >>= \ abc ->
                foldM vc c (abcWords abc) >>= \ c' ->
                let abc' = annoAtDef w <> rwHashWords c' abc in
                let h = secureHashLazy $ ABC.encode abc' in
                (Map.insert w h c', h)

annoAtDef :: Word -> ABC
annoAtDef w = ABC.mkABC [ABC.ABC_Tok ("&@" <> wordToUTF8 w)]

-- rewrite words if their hash is included
rwHashWords :: Map Word SecureHash -> ABC -> ABC
rwHashWords cache = ABC.rewriteTokens rw where
    rw t = [ABC.ABC_Tok (tryWordHash t)]
    tryWordHash tok = maybe t h2t (cachedWord t)
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
        dictWordDeps w >>= \ lWords ->
        let lNewDeps = L.filter (`Set.notMember` v) lWords in
        if L.null lDeps 
            then accum (w:r) (Set.insert w v) ws'
            else accum r v (lDeps <> ws)

-- | Wikilon needs a reverse-lookup index to easily locate all
-- words that use a specific token. This might be computed per
-- dictionary. 
class (DictView dict) => DictRLU dict where
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
dictSplitPrefixW :: DictSplitPrefix dict => Dict m -> WordPrefix -> m [Either WordPrefix Word]
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
class (DictView dict) => DictUpdate dict where
    unsafeUpdateWord :: Word -> ABC -> Dict m -> m (Dict m)

    unsafeUpdateWords :: Map Word ABC -> Dict m -> m (Dict m)
    unsafeUpdateWords (Map.toList -> l) d = 
        let lUpd = Map.toList

flip (L.foldl' upd) . Map.toList where
        upd d (w,abc) = unsafeUpdateWord w abc d 

-- | update a word's definition to the empty ABC program
deleteWord :: (DictUpdate dict) => Word -> dict -> dict
deleteWord = deleteWords . (:[])

-- | delete a list of words.
deleteWords :: (DictUpdate dict) => [Word] -> dict -> dict
deleteWords = unsafeUpdateWords . Map.fromList . withEmpties where
    withEmpties = flip L.zip $ L.repeat mempty

-- | Rename a word in a dictionary, affecting not just that word
-- but also all the words that reference it. Note: no validation
-- is performed, and this will overwrite the second word. In general,
-- it is safest to rename under one of two conditions:
--
--   1. the target word is undefined and unused (cf. renameWord)
--   2. target and origin share same definition (cf. mergeWords)
--
-- In these conditions, you can be sure that clients of a word are not
-- impacted, do not need to be retested or recompiled, no cycles are
-- introduced, etc.. Otherwise, you must treat the rename as an update
-- to both the origin and target words.
-- 
unsafeRenameWord :: (DictUpdate dict, DictRLU dict) => Word -> Word -> dict -> dict
unsafeRenameWord wo wt d =
    if (wo == wt) then d else -- trivially, no change
    let lC = wordClients d wo in -- client of wo, for rewrite wo→wt
    let fnUpdClient = _renameInABC wo wt . lookup d in -- rename wo to wt
    flip unsafeUpdateWords d $ 
        Map.insert wt (lookup d wo) $                 -- overwrite wt
        Map.insert wo mempty $                        -- delete wo
        Map.fromList $ L.zip lC (fmap fnUpdClient lC) -- update clients of wo

-- rename a word within context of ABC
_renameInABC :: Word -> Word -> ABC -> ABC
_renameInABC wo wt = ABC.rewriteTokens rwTok where
    t0 = BS.cons 37 $ wordToUTF8 wo
    tf = BS.cons 37 $ wordToUTF8 wt
    rnTok t = if (t == t0) then tf else t
    rwTok t = [ABC.ABC_Tok (rnTok t)]


-- | Rename a word only if the target word is undefined and has no
-- clients (i.e. cannot rename into a hole). Otherwise returns Nothing.
renameWord :: (DictUpdate dict, DictRLU dict) => Word -> Word -> dict -> Maybe dict
renameWord wo wt d =
    let bUndefinedTarget = L.null $ ABC.abcOps $ lookup d wt in
    let bTargetHasNoClients = L.null $ wordClients d wt in
    let bOkRename = bUndefinedTarget && bTargetHasNoClients in
    if not bOkRename then Nothing else
    Just $ unsafeRenameWord wo wt d

-- | Merge a word only if origin and target words have the same
-- definitions. Otherwise return Nothing.
mergeWords :: (DictUpdate dict, DictRLU dict) => Word -> Word -> dict -> Maybe dict
mergeWords wo wt d =
    let bOkMerge = (lookup d wo) == (lookup d wt) in
    if not bOkMerge then Nothing else
    Just $ unsafeRenameWord wo wt d

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
-- errors. Normal updates to a dictionary should be performed via this
-- function to guard against cycles and so on. In case of errors, this
-- tries to return many errors at once.
--
-- Note that leaving words undefined is not considered an error at this
-- layer. (And we'd have difficulty distinguishing a valid definition
-- anyway.) Undefined words shall be treated as holes in later stage.
updateWords :: (DictUpdate dict) => [(Word, ABC)] -> dict -> Either [InsertionError] dict
updateWords [] d = Right d
updateWords l d =
    let lWords = fmap fst l in
    let lDupErrors = fmap DupWord $ findDups $ lWords in
    let lMalformed = L.concatMap (uncurry testForMalformedDef) l in
    let d' = unsafeUpdateWords (Map.fromList l) d in
    let lCycleErrors = maybeToList $ fmap Cycle $ testForCycle lWords d' in
    let lErrors = lDupErrors ++ lMalformed ++ lCycleErrors in
    if L.null lErrors then Right d' else Left lErrors

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
-- closed (i.e. no words are repeated). Cycles must be non-empty. 
type Cycle a = [a]

-- | Search under a given list of words for cycles. If such a cycle
-- exists, this function certainly finds it. However, I won't attempt 
-- to return an exhaustive set of cycles. 
testForCycle :: (DictView dict) => [Word] -> dict -> Maybe (Cycle Word)
testForCycle ws d = flip State.evalState mempty $ fc (wordDeps d) [] ws

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


    
    

