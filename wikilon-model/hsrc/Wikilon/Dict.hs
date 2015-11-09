{-# LANGUAGE DeriveDataTypeable #-}
-- | A dictionary is a persistent, lazily loaded map from words to
-- definitions. Given dictionary applications modeling forums and
-- wikis and similar, I anticipate very large dictionaries (millions
-- of words) that cannot be kept in memory all at once.
--
-- Wikilon will further keep thousands of dictionaries - mostly the
-- histories, but also forks and branches. So structure sharing is
-- also very valuable.
-- 
-- To meet these goals, I'm currently using a VCache trie to model
-- the dictionary. Tries support structure sharing and fast diffs.
-- But separate indexes will be kept per branch for reverse lookup,
-- fuzzy find, and other features.
-- 
module Wikilon.Dict
    ( Dict
    , dictCreate
    , dictLookup
    , dictList
    , dictInsert
    , dictDelete
    , dictDiff, DictDiff, Diff(..)
    , dictTransitiveDepsList
    , module Wikilon.Word
    , module Wikilon.AODef
    ) where

import Control.Exception (assert)
import Control.Applicative
import Data.Typeable (Typeable)
import Data.Monoid
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as LBS
import Data.VCache.Trie (Trie, Diff(..))
import qualified Data.VCache.Trie as Trie
import Database.VCache
import Wikilon.Word
import Wikilon.AODef

-- | An AO dictionary is a finite collection of (word, definition) 
-- pairs. Words and definitions should be valid by local constraints
-- (e.g. on tokens, texts). 
--
-- In a healthy dictionary, definitions should be acyclic, complete,
-- and well typed. But these properties are not checked prior to 
-- construction of the dictionary. Detecting and reporting issues is
-- left to higher layers.
newtype Dict = Dict (Trie Def)
    deriving (Eq, Typeable)

-- NOTE: I want to separate large definitions from the Trie nodes to avoid
-- copying them too often. But small definitions may be kept with the trie
-- nodes to reduce indirection. Here, 'small' will be up to 254 bytes. 
data Def 
    = DefS AODef            -- for small definitions, up to 254 bytes
    | DefL (VRef BigAODef)  -- for large definitions, maybe compressed
    deriving (Eq, Typeable)

-- I want to compress larger definitions, mostly when we're looking at space
-- savings of multiple kilobytes. Definitions containing embedded texts or
-- tables are potentially very large, so we could see savings of many kilobytes.
-- However, it isn't a critical feature. BigAODef just reserves the possibility.
data BigAODef
    = UDef AODef
    -- todo: zipped variant(s)
    deriving (Eq, Typeable)

fromBigAODef :: BigAODef -> AODef
fromBigAODef (UDef d) = d

toBigAODef :: AODef -> BigAODef
toBigAODef = UDef -- todo: heuristic compression

isSmall :: AODef -> Bool
isSmall = (< 255) . LBS.length

aodef :: Def -> AODef
aodef (DefS def) = def
aodef (DefL ref) = fromBigAODef $ deref' ref

toDef :: VSpace -> AODef -> Def
toDef vc def 
    | isSmall def = DefS def
    | otherwise   = DefL (vref' vc (toBigAODef def))

-- | Create a new, empty dictionary.
dictCreate :: VSpace -> Dict
dictCreate = Dict . Trie.empty

-- | lookup a word in the dictionary. If a word is undefined,
-- this will return Nothing. Otherwise, it returns the bytecode
-- for the definition.
dictLookup :: Dict -> Word -> Maybe AODef
dictLookup (Dict t) (Word w) = aodef <$> Trie.lookup w t

-- | list all non-empty definitions in the dictionary.
dictList :: Dict -> [(Word, AODef)]
dictList (Dict t) = Trie.toListBy fn t where
    fn w d = (Word w, aodef d)

-- | List transitive dependencies for a list of root words. Each word
-- in the input list appears in the output list after all of its
-- dependencies. A word is listed in the output at most once.
dictTransitiveDepsList :: Dict -> [Word] -> [(Word, Maybe AODef)]
dictTransitiveDepsList dict = accum mempty mempty where
    -- accum (visited) (cycle prevention) (roots) 
    accum _ _ [] = []
    accum v c ws@(w:ws') =
        if Set.member w v then accum v c ws' else -- already listed w
        case dictLookup dict w of
            Nothing -> (w, Nothing) : accum (Set.insert w v) c ws'
            Just def -> 
                let lDeps = L.filter (`Set.notMember` v) (aodefWords def) in
                let bAddWord = L.null lDeps || Set.member w c in
                if bAddWord then (w, Just def) : accum (Set.insert w v) (Set.delete w c) ws'
                            else accum v (Set.insert w c) (lDeps ++ ws)

-- | insert a word into a dictionary. Note that this does not check
-- that the definition is sensible or that the resulting dictionary
-- is valid. That property should be checked separately.
dictInsert :: Dict -> Word -> AODef -> Dict
dictInsert (Dict t) (Word w) def = Dict $ Trie.insert w d t where
    d = toDef (Trie.trie_space t) def

-- | Delete a word from a dictionary. 
dictDelete :: Dict -> Word -> Dict
dictDelete (Dict t) (Word w) = Dict $ Trie.delete w t

-- | Quickly compute differences between two dictionaries.
dictDiff :: Dict -> Dict -> DictDiff
dictDiff (Dict a) (Dict b) = fmap toDictDiff $ Trie.diff a b where 
    toDictDiff (w, d) = (Word w, fmap aodef d) 

type DictDiff = [(Word, Diff AODef)]

instance VCacheable Dict where
    put (Dict d) = putWord8 1 >> put d
    get = getWord8 >>= \ v -> case v of
        1 -> Dict <$> get
        _ -> fail $ dictErr $ "unrecognized Dict version " ++ show v
instance VCacheable Def where
    put (DefL ref) = putWord8 maxBound >> put ref
    put (DefS def) = assert (isSmall def) $
        let sz = fromIntegral (LBS.length def) in
        putWord8 sz >> putByteStringLazy def
    get = getWord8 >>= \ sz -> 
        if (maxBound == sz) then DefL <$> getVRef else
        DefS <$> getByteStringLazy (fromIntegral sz) 

-- for now, just reserving a byte for compression options
-- later, I might wish to compress large definitions, esp.
-- those representing large binary text values.
instance VCacheable BigAODef where
    put (UDef def) = putWord8 1 >> put def
    get = getWord8 >>= \ ty -> case ty of
        1 -> UDef <$> get
        _ -> fail $ dictErr $ "unrecognized compression model for large def " ++ show ty

dictErr :: String -> String
dictErr = ("Wikilon.Dict " ++)
