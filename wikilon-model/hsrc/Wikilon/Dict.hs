
-- | A Wikilon 'Dictionary' is a finite, indexed collection of
-- (Word,Def) pairs, such that the definitions are non-empty 
-- binaries. There are many constraints for a healthy dictionary 
-- (e.g. well-formed bytecode, acyclic definitions), but they 
-- are not enforced at this layer. 
--
-- Dictionaries support efficient lookup, list, update, and
-- diff. However, there are a lot of reverse-lookup indexes
-- that dictionaries do not directly support. Those indices
-- must be supported at a separate layer.
module Wikilon.Dict
    ( Dict
    , dictCreate
    , dictLookup
    , dictList
    , dictUpdate
    , dictDelete
    , dictDiff
    , module Wikilon.Word
    , module Wikilon.AODef
    ) where

import Control.Applicative
import Data.Monoid
import qualified Data.ByteString.Lazy as LBS
import Data.VCache.Trie (Trie)
import qualified Data.VCache.Trie as Trie
import Database.VCache
import Wikilon.Word
import Wikilon.AODef

newtype Dict = Dict (Trie Def)
newtype Def = Def (VRef AODef) 

aodef :: Def -> AODef
aodef (Def r) = deref' r

-- | Create a new, empty dictionary.
dictCreate :: VSpace -> Dict
dictCreate = Dict . Trie.empty

-- | lookup a word in the dictionary. If a word is undefined,
-- an empty bytestring is returned. (A minimal valid definition
-- is `[][]`, which encodes an identity function.)
dictLookup :: Dict -> Word -> AODef
dictLookup (Dict t) (Word w) = maybe mempty aodef $ Trie.lookup w t

-- | list all non-empty definitions in the dictionary.
dictList :: Dict -> [(Word, AODef)]
dictList (Dict t) = Trie.toListBy fn t where
    fn w d = (Word w, aodef d)

-- NOTE: it might be useful to also list words relative to a given prefix
-- or suffix. However, I'm inclined to leave this feature to a separate
-- index, rather than rely on the underlying Trie structure.

-- | update a word in a dictionary. Note that this does not enforce
-- that the update is sensible or valid.
dictUpdate :: Dict -> Word -> AODef -> Dict
dictUpdate (Dict t) (Word w) def = Dict $ 
    if LBS.null def then Trie.delete w t else
    let vc = Trie.trie_space t in
    let def' = Def (vref' vc def) in
    Trie.insert w def' t

-- | Delete a word from a dictionary. Same as defining a word
-- to an empty string. Results in an undefined word, if that
-- word is in use.
dictDelete :: Dict -> Word -> Dict
dictDelete d w = dictUpdate d w mempty

-- | Quickly compute differences between two dictionaries.
dictDiff :: Dict -> Dict -> [(Word, (AODef, AODef))]
dictDiff (Dict a) (Dict b) = fmap toDiff $ Trie.diff a b where 
    toDiff (w, elemDiff) = (Word w, dd elemDiff) 
    elemDiff (Trie.InL l) = (aodef l, mempty)
    elemDiff (Trie.InR r) = (mempty, aodef r)
    elemDiff (Trie.Diff l r) = (aodef l, aodef r)  

instance VCacheable Dict where
    put (Dict d) = putWord8 1 >> put d
    get = getWord8 >>= \ v -> case v of
        1 -> Dict <$> get
        _ -> fail $ dictErr $ "unrecognized Dict version " ++ show v
instance VCacheable Def where
    put (Def str) = putWord8 1 >> put str
    get = getWord8 >>= \ v -> case v of
        1 -> Def <$> get
        _ -> fail $ dictErr $ "unrecognized Def version " ++ show v

dictErr :: String -> String
dictErr = ("Wikilon.Dict" ++)
