{-# LANGUAGE DeriveDataTypeable #-}

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
-- editors, potentially at a higher level than words and bytecode. 
--
-- This dictionary module enforces two useful invariants:
--
--   1. dependencies are acyclic
--   2. every word has a definition
--
-- Additionally, tokens are limited to word dependencies, annotations, 
-- and discretionary sealers or unsealers to keep it pure and portable.
--
-- Ideally, the system should further enforce that definitions compile,
-- that words evaluate to blocks, and that words are type-safe and have
-- no obvious errors (automatic linters, testing, etc.).
-- 
module Wikilon.Dict
    ( Dict, dict_space
    , empty, null, size
    , lookup, toList
    , lookupBytes, toListBytes
    , usedBy, uses, words
    , isValidToken

    , insert
    , delete
    -- , rename, renameSuffix
    , Error(..)
    ) where

import Prelude hiding (null, lookup, words)
import Control.Applicative ((<$>),(<*>))
import Control.Arrow (second)
import Control.Exception (assert)
import qualified Control.Monad.State as State
import Data.Char (ord)
import Data.Typeable (Typeable)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import Data.Set (Set)
import qualified Data.Set as Set

import Data.VCache.Trie (Trie)
import qualified Data.VCache.Trie as Trie
import Database.VCache

import ABC.Basic (ABC, Op(..), Token)
import qualified ABC.Basic as ABC
import Wikilon.Word

-- | A dictionary contains a set of definitions
data Dict = Dict
    { dict_data :: Trie Def
    , dict_deps :: Trie Deps
    } deriving (Eq, Typeable)
type Def  = Sz LBS.ByteString
type Deps = Sz [BS.ByteString]
type Sz a = Either (VRef a) a
-- Currently, definitions and dependencies are inlined into our Trie
-- nodes up to a heurstic threshold in size, maybe ~600 bytes. Beyond
-- this size, I'll use a separate VRef to encode content.

dict_space :: Dict -> VSpace
dict_space = Trie.trie_space . dict_data

-- Preferred cache mode for dictionary lookups. In this case, I don't
-- mind quickly dropping nodes from the cache, since the parse is fast
-- and operations on a dictionary are bursty anyway.
_cm :: CacheMode
_cm = CacheMode0

_fromSz :: Sz a -> a
_fromSz = either (derefc _cm) id 

-- Words in this dictionary are reverse encoded, i.e. such that words
-- sharing a suffix tend to be organized into the same subtrees. This
-- makes it feasible to efficiently rename an entire suffix.
_wordToKey :: Word -> BS.ByteString
_wordToKey = BS.reverse . wordToUTF8

-- | Wikilon dictionaries accept three token types: words, discretionary
-- sealers and unsealers, and annotations. For now, these tokens have
-- several constraints on them:
--
--   Words may not contain C0, C1, SP, DEL, {}(|)[]", U+FFFD
--   Annotations and sealers should be valid as words
--
-- In sealers, $ indicates what follows is a cryptographic key, so
-- those are also rejected as being non-discretionary.
--
isValidToken :: Token -> Bool
isValidToken t = case Char8.uncons t of
    Just ('%', w) -> isValidWord (Word w)
    Just ('&', a) -> isValidAnno a
    Just (':', s) -> isValidSeal s
    Just ('.', u) -> isValidSeal u
    _ -> False

isValidSeal :: Token -> Bool
isValidSeal = L.all okc . UTF8.toString where
    okc c = isValidWordChar c && ('$' /= c)

isValidAnno :: Token -> Bool
isValidAnno = isValidWord . Word

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
_impossible eMsg = error $ "Wikilon.Dict: " ++ eMsg

-- | Obtain a list of all (Word, ABC) pairs. Words are sorted by suffix.
toList :: Dict -> [(Word, ABC)]
toList = fmap (second _decode) . toListBytes where

-- | Lookup raw bytestring for a word. 
lookupBytes :: Dict -> Word -> Maybe LBS.ByteString
lookupBytes d w = _fromSz <$> Trie.lookupc _cm (_wordToKey w) (dict_data d)

-- | Obtain a list of all (Word, ByteString) pairs. Words are sorted by suffix.
toListBytes :: Dict -> [(Word, LBS.ByteString)]
toListBytes = Trie.toListBy f . dict_data where
    f k bytes = (Word (BS.reverse k), _fromSz bytes)

-- | Find direct clients of a word.
usedBy :: Dict -> Word -> [Word]
usedBy d w = maybe [] (fmap Word . _fromSz) $ 
    Trie.lookupc _cm (_wordToKey w) (dict_deps d)

-- find all transitive clients of a word
_ubt :: Dict -> Set Word -> Set Word -> [Word] -> [Word]
_ubt _ _  s [] = Set.toList s
_ubt d sw su (w:ws) = 
    if Set.member w sw then _ubt d sw su ws else
    let sw' = Set.insert w sw in
    let lU = usedBy d w in
    let su' = L.foldl' (flip Set.insert) su lU in
    _ubt d sw' su' (lU ++ ws)

-- | Find all words used by a given word. I.e. filters ABC for just
-- the {%word} tokens, and returns each word. Will return the empty
-- list if the requested word is undefined.
uses :: Dict -> Word -> [Word]
uses d w = maybe [] words $ lookup d w

-- | Filter ABC from the dictionary to just list the words it uses.
-- Each word is expressed as a {%word} token in the original ABC.
words :: ABC -> [Word]
words = fmap toWord . L.filter isWordTok . ABC.tokens where
    isWordTok = ('%' ==) . Char8.head
    toWord = Word . Char8.tail

-- Search a list of words for cyclic dependencies. Returns first cycle
-- found if one exists, or returns Nothing. Does not search any word
-- more than once.
findCycle :: Dict -> [Word] -> Maybe [Word]
findCycle d = flip State.evalState Set.empty . _fc d []

-- _fc dict    stack     frontier         acyclic
_fc :: Dict -> [Word] -> [Word] -> State.State (Set Word) (Maybe [Word])
_fc _ _ [] = return Nothing
_fc d stack (w:ws) = 
    State.gets (Set.member w) >>= \ bKnownAcyclic ->
    if bKnownAcyclic then _fc d stack ws else
    if L.elem w stack then return (Just (_cyc w stack)) else
    _fc d (w:stack) (uses d w) >>= \ rStack ->
    case rStack of
        Nothing -> State.modify (Set.insert w) >> _fc d stack ws
        cycleFound -> return cycleFound

-- return just the cycle found (implicitly closed)
_cyc :: (Eq a) => a -> [a] -> [a]
_cyc a stack = L.dropWhile (/= a) $ L.reverse stack

-- | The Wikilon.Dict module guards against three kinds of errors:
--
-- * cyclic dependencies
-- * missing definitions
-- * bad tokens or words
--
-- Other errors, such as badly typed code, divergent computations,
-- or incorrect documentation must be addressed by clients.
data Error
    = Cycle [Word] -- ^ cyclic dependency with given path
    | Undef [Word] -- ^ listed words require definitions 
    | Inval [Word] -- ^ proposed definition or word is bad 

-- | Insert or Update a list of words. Any existing definition for 
-- an inserted word will be replaced. Errors are possible if a word
-- introduces a cycle or uses an undefined word, or simply has some
-- malformed content.
insert :: Dict -> [(Word, ABC)] -> Either Error Dict
insert d l = 
    -- sanitize input
    let lBad = L.filter (uncurry _isInvalid) l in
    if not (L.null lBad) then Left (Inval (fmap fst lBad)) else
    _todo "insert"



_isInvalid :: Word -> ABC -> Bool
_isInvalid w abc = not valid where
    valid = isValidWord w 
         && L.all isValidToken (ABC.tokens abc)



-- | Delete a list of words. This may fail if deletion would leave any
-- word in the dictionary undefined. If this fails, it returns a list
-- with a transitive list of words that would become undefined if you
-- delete the requested words. Otherwise, the updated dictionary is
-- returned.
delete :: Dict -> [Word] -> Either [Word] Dict
delete d lDel = 
    let lU = L.filter (`L.notElem` lDel) $ _ubt d Set.empty Set.empty lDel in
    if not (L.null lU) then Left lU else
    _todo "delete"
    
    


_todo :: String -> a
_todo s = _impossible $ "TODO: " ++ s





instance VCacheable Dict where
    put (Dict _data _deps) = putWord8 0 >> put _data >> put _deps
    get = getWord8 >>= \ v -> case v of
        0 -> Dict <$> get <*> get
        _ -> fail $ err $ "unknown Dict version " ++ show v

err :: String -> String
err = ("Wikilon.Dict: " ++)






