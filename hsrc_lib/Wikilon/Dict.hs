
-- | A dictionary contains words and definitions.
--
-- In Wikilon, a definition is a function of type:
--
--     type Def a b = ∃ v . ∀ e . e → [v→[a→b]] * (v * e)
-- 
-- This definition is encoded in Awelon Bytecode (ABC), leveraging
-- {%word} tokens to reference other words in the dictionary. We can
-- understand a definition as having two parts - a structure v and a
-- compiler function that takes this structure and returns a block.
--
-- The `$` bytecode would then apply the compiler function to the
-- structure v, returning [a→b]*e. The block is the word's meaning.
-- This meaning may be further inlined by applying `vr$c`. Each word 
-- token is compiled by inlining its meaning, i.e. inline definition
-- then apply `$vr$c`. Definitions are always acyclic.
--
-- This module enforces that dictionaries are acyclic and that words
-- are defined. It does not validate whether the definitions are well
-- typed or convergent.
-- 
module Wikilon.Dict
    ( Dict
    ) where


import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.VCache.Trie (Trie)
import qualified Data.VCache.Trie as Trie


type Def = () -- TODO
newtype Dict = Dict (Trie Def)


