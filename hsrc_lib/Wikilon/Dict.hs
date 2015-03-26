{-# LANGUAGE DeriveDataTypeable #-}

-- | A dictionary contains words and definitions.
--
-- In Wikilon, a definition is a function of type:
--
--     type Def a b = ∃ v . ∀ e . e → [v→[a→b]] * (v * e)
-- 
-- This definition is encoded in Awelon Bytecode (ABC), leveraging
-- {%word} tokens to utilize other words in the dictionary. We can
-- understand a definition as having two parts - a structured value
-- and an interpreter function that takes this structure and returns
-- a block. A single bytecode, `$`, will then compile the structure
-- into a block, which may be inlined `vr$c` as the meaning of the
-- word.
--
-- Preserving the structured value is useful for structured editing,
-- staged evaluation, and embedded DSLs. The definition structure is
-- not exposed to clients of the word, i.e. implementation hiding. 
-- But a module may voluntarily quote its structure.
--
-- This dictionary type only enforces two properties: dependencies
-- are acyclic, words are fully defined using valid Awelon Bytecode.
-- Valid bytecode, in this case, also restricts which tokens are 
-- used to just annotations, discretionary sealers, and words. 
--
-- Ideally, the system should further enforce that words evaluate to
-- blocks, that these blocks are type safe, that computations either
-- converge or explicitly fail for any input, and that all tests pass.
-- 
-- Awelon Bytecode is preserved as presented. Zero simplification.
--
-- This dictionary does provide a reverse lookup index. Given a word,
-- you can easily find all words that depend on it.
-- 
module Wikilon.Dict
    ( Dict, dict_space
    , empty, null, size
    , lookup, toList
    , lookupBytes, toListBytes
    , usedBy

{-
    , insert
    , delete
    , rename
    , renameSuffix
-}

    , module Wikilon.Word

    ) where

import Prelude hiding (null, lookup)
import Control.Applicative ((<$>),(<*>))
import Control.Exception (assert)
import Data.Char (ord)
import Data.Typeable (Typeable)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L

import Data.VCache.Trie (Trie)
import qualified Data.VCache.Trie as Trie
import Database.VCache

import ABC.Basic (ABC)
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
lookup d w = fst . ABC.decode <$> lookupBytes d w

_impossible :: String -> a
_impossible eMsg = error $ "Wikilon.Dict: " ++ eMsg

-- | Obtain a list of all (Word, ABC) pairs. Words are sorted by suffix.
toList :: Dict -> [(Word, ABC)]
toList = fmap f . toListBytes where
    f (w, bytes) = (w, fst (ABC.decode bytes))

-- | Lookup raw bytestring for a word. 
lookupBytes :: Dict -> Word -> Maybe LBS.ByteString
lookupBytes d w = _fromSz <$> Trie.lookupc _cm (_wordToKey w) (dict_data d)

-- | Obtain a list of all (Word, ByteString) pairs. Words are sorted by suffix.
toListBytes :: Dict -> [(Word, LBS.ByteString)]
toListBytes = Trie.toListBy f . dict_data where
    f k bytes = (Word (BS.reverse k), _fromSz bytes)

-- | Find direct clients of a word.
usedBy :: Dict -> Word -> [Word]
usedBy d w = fmap Word $ maybe [] _fromSz $ 
    Trie.lookupc _cm (_wordToKey w) (dict_deps d)

instance VCacheable Dict where
    put (Dict _data _deps) = putWord8 0 >> put _data >> put _deps
    get = getWord8 >>= \ v -> case v of
        0 -> Dict <$> get <*> get
        _ -> fail $ err $ "unknown Dict version " ++ show v

err :: String -> String
err = ("Wikilon.Dict: " ++)






