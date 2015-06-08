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
-- Here the intermediate value 'v' serves a role as an AST for a DSL,
-- though may contain elements opaque to the compiler, e.g. in form of
-- blocks. The `[v→[a→b]]` function is then a compiler for this value.
-- Trivial compilers such as [] or [v'c] are useful. Complex compilers
-- will typically be represented by naming another word. The output from
-- the compiler is a function [a→b], the functional meaning of a word.
--
-- A definition is modeled in Awelon Bytecode (ABC). Dependencies on
-- other words are represented using tokens of form {%foo} and {%bar}.
-- Dependencies must be acyclic graph. Static compilation and linking
-- is achieved by inlining the compiled [a→b] meaning for each word. 
-- (After which we might compile the ABC down to machine code.)
--
-- In a healthy dictionary:
--
--   1. dependencies are acyclic
--   2. every word is defined
--   3. all definitions compile
--   4. every meaning typechecks
--
-- Note that we'll generally typecheck after compilation. Dynamic
-- eval prior to compilation allows type [a→b] to depend on the
-- value `v` without a dependently typed compiler. Usually, the
-- meaning is much simpler in type.
--
-- During development, words may be undefined at least briefly. This
-- is common for top-down programming. But we can also use undefined
-- words as a 'hole' that may later be filled with support of the 
-- development environment.

 the restriction that words are defined may
-- be relaxed. An undefined or incompletely defined word can serve
-- a useful role as a 'hole' in the dictionary. By inferring types,
-- analyzing use in tests, selecting sample outputs for an inputs or
-- vice versa, leveraging simulations and machine learning, etc. an
-- interactive development environment can help fill each hole.
--
-- AO words are purely functional. Tokens are limited to dependencies,
-- annotations, and discretionary sealers or unsealers. Tokens and 
-- words are also constrained to simplify use with URIs and English
-- documentation texts. AO dictionaries aim to be very portable.
--
-- In general, we should constrain our dictionaries such that:
--
-- * words and tokens are well formed
-- * text will work well with 

These limitations ensure the AO dictionary is
-- pure, portable. There is no entanglement with specific machines.
--
-- Ideally, Wikilon shall further enforce that definitions compile and
-- check that there are no obvious errors via automatic type checking,
-- testing, linting, analysis, etc..
--
-- TODO: support many more operations:
--   batch rename
--   efficient diffs

module Wikilon.Dict
    ( DictView(..), wordsInDict, abcWords
    , Prefix, DictSplitPrefix(..)
    , DictReverseLookup(..), wordUsedBy
    , 
    , module Wikilon.Dict.Word
    ) where

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as BS

import Awelon.ABC (ABC)
import qualified Awelon.ABC as ABC

import Wikilon.Dict.Word 
import Wikilon.Dict.Text
import Wikilon.Dict.Token

-- | Basic view of a dictionary.
class DictView dict where
    -- | Lookup the ABC definition for a word in a dictionary.
    lookup :: dict -> Word -> Maybe ABC

    -- | Obtain a complete listing of the dictionary in no particular order.
    toList :: dict -> [(Word, ABC)]

-- | Obtain a list of words defined within from a dictionary.
wordsInDict :: (DictView dict) => dict -> [Word]
wordsInDict = fmap fst . toList

-- | Find all words depended upon by a given word. Filters ABC for just
-- the {%word} tokens, and returns each word. Returns the empty list if
-- the requested word is undefined. Doesn't eliminate duplicates.
wordDeps :: (DictView dict) => Dict -> Word -> [Word]
wordDeps d w = maybe [] abcWords $ lookup d w

-- | Each word is expressed as a {%word} token in the original ABC.
abcWords :: ABC -> [Word]
abcWords = mapMaybe wordTok . ABC.tokens where
    wordTok tok = case UTF8.uncons tok of
        Just ('%', w) -> Just (Word w)
        _ -> Nothing

-- | A prefix for a word should be a complete UTF8 string (no
-- partial characters at the end).
type WordPrefix = UTF8.ByteString

-- | For browsing a dictionary, it can be useful to access it 
-- incrementally via prefix.
class (DictView dict) => DictSplitPrefix dict where
    -- | Given a prefix, provide a list of larger prefixes.
    splitOnPrefix :: WordPrefix -> dict -> [WordPrefix]

    -- | Obtain a complete list of words with a given prefix.
    wordsWithPrefix :: WordPrefix -> dict -> [Word]

-- | It's very useful to know who uses what.
class DictReverseLookup dict where
    -- | Reverse lookup, find words that directly use a given token
    tokenUsedBy :: dict -> Token -> [Word]

-- | Find direct clients of a word.
wordUsedBy :: (DictReverseLookup dict) => dict -> Word -> [Word]
wordUsedBy d (Word w) = tokenUsedBy d (BS.cons 37 w) -- %word






