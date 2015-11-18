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
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Compression.Snappy.Lazy as Snappy
import System.IO.Unsafe (unsafeDupablePerformIO)
import Foreign.ForeignPtr (newForeignPtr_) 
import Data.VCache.Trie (Trie, Diff(..))
import qualified Data.VCache.Trie as Trie
import Database.VCache
import Wikilon.Word
import qualified Wikilon.Base16 as B16
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

-- I want to compress the larger, more compressible definitions. 
-- BigAODef gives me an opportunity to do so (at VCacheable)
newtype BigAODef = BigAODef { fromBigAODef :: AODef } 
    deriving (Eq, Typeable)

isSmall :: AODef -> Bool
isSmall = (< 255) . LBS.length

aodef :: Def -> AODef
aodef (DefS def) = def
aodef (DefL ref) = fromBigAODef (deref' ref)

toDef :: VSpace -> AODef -> Def
toDef vc def 
    | isSmall def = DefS def
    | otherwise   = DefL (vref' vc (BigAODef def))

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


compress :: LBS.ByteString -> LBS.ByteString
compress = Snappy.compress . B16.compress 

decompress :: LBS.ByteString -> LBS.ByteString
decompress = B16.decompress . Snappy.decompress

-- we'll perform compression, heuristically, with zero-copy decompress.
-- I require a pretty large definition before I bother with compression.
-- It's mostly intended for larger texts, binaries, etc.. Also, we'll
-- skip compression if we aren't getting at least a 33% reduction.
instance VCacheable BigAODef where
    put (BigAODef bytes) =
        let nBytes = LBS.length bytes in
        let zBytes = compress bytes in
        let nzBytes = LBS.length zBytes in
        
        let bSufficientlyLarge = (nBytes >= 2000) in
        let bAcceptableRatio = ((nzBytes * 3) <= (nBytes * 2)) in
        let bUseCompression = bSufficientlyLarge && bAcceptableRatio in

        if not bUseCompression
            then putWord8 0 >> put bytes
            else putWord8 1 >> 
                 putVarNat (fromIntegral nBytes) >>
                 putVarNat (fromIntegral nzBytes) >> 
                 putByteStringLazy zBytes

    get = getWord8 >>= \ vn -> case vn of
        0 -> BigAODef <$> get
        1 -> fmap fromIntegral getVarNat >>= \ nBytes ->
             fmap fromIntegral getVarNat >>= \ nzBytes ->
             withBytes nzBytes $ \ pzBytes -> -- zero-copy decompression
                let fpzBytes = unsafeDupablePerformIO (newForeignPtr_ pzBytes) in
                let zBytes = LBS.fromStrict $ BSI.PS fpzBytes 0 nzBytes in
                let bytes = decompress zBytes in
                if (LBS.length bytes /= nBytes) -- also forces strict bytes
                    then fail (dictErr $ "byte count mismatch on decompress")
                    else return (BigAODef bytes)
        _ -> fail (dictErr $ "unrecognized BigAODef encoding")

dictErr :: String -> String
dictErr = ("Wikilon.Dict " ++)
