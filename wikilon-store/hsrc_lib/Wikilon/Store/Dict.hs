{-# LANGUAGE DeriveDataTypeable, PatternGuards, TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | a concrete implementation of Wikilon.Dict.
--
-- Currently, dictionaries are stored using VCache.
module Wikilon.Store.Dict
    ( Dict
    , dict_space
    , empty
    , null
    , size
    , member
    , unsafeDictAddr
    , module Wikilon.Dict
    , module Wikilon.Dict.Word
    ) where

import Prelude hiding (null, lookup, words)
import Control.Arrow (second, (***))
import Control.Monad
import Data.Monoid
import Data.Maybe 
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.VCache.Trie as Trie
import qualified Data.VCache.Trie.Type as Trie
import Data.Word (Word64)
import Data.VCache.Trie (Trie)
import Database.VCache
import qualified Data.Array.IArray as A
import qualified Awelon.ABC as ABC
import qualified Awelon.Base16 as B16

import Wikilon.SecureHash
import Wikilon.Dict
import Wikilon.Dict.Word

import Wikilon.Store.Dict.Type

-- | a dictionary is hosted in a vcache address space
dict_space :: Dict -> VSpace
dict_space = Trie.trie_space . dict_defs

-- | address for a dictionary, wholistically. This might be
-- used for caching at the whole-dictionary level, but caching
-- at the word level is more viable via lookupVersionHash.
unsafeDictAddr :: Dict -> Word64
unsafeDictAddr = Trie.unsafeTrieAddr . dict_defs

-- | O(1). Create a new, empty dictionary
empty :: VSpace -> Dict
empty vs = Dict1
    { dict_defs = Trie.empty vs
    , dict_deps = Trie.empty vs
    , dict_hash = Trie.empty vs
    }

-- | O(1). Test whether a dictionary is empty.
null :: Dict -> Bool
null = Trie.null . dict_defs

-- | O(N). Return number of words in dictionary.
size :: Dict -> Int
size = Trie.size . dict_defs

_decode :: LBS.ByteString -> ABC
_decode b = case ABC.decode b of
    Left _dcs -> _impossible "invalid ABC in dictionary"
    Right abc -> abc

_impossible :: String -> a
_impossible = error . dictErr

instance Dictionary Dict

instance DictView Dict where
    lookup d = _decode . lookupBytes d
    lookupBytes d (Word w) = 
        case Trie.lookup w (dict_defs d) of
            Just (Def1 v) -> deref' v
            Nothing -> mempty
    toList = fmap (second _decode) . toListBytes

    dictDiff a b = fmap (Word . fst) $ Trie.diff (dict_defs a) (dict_defs b)

    lookupVersionHash d w = h where
        h = maybe u id $ Trie.lookup (unWord w) (dict_hash d)
        u = secureHashLazy $ ABC.encode (_annoWord w)   

-- prefix each definition when computing version hashes
_annoWord :: Word -> ABC
_annoWord (Word w) = ABC.mkABC [ABC.ABC_Tok ("&@" <> w)]

_bytes :: Def -> LBS.ByteString
_bytes (Def1 v) = deref' v

-- | Obtain a list of all (Word, ByteString) pairs. Words are sorted by suffix.
toListBytes :: Dict -> [(Word, LBS.ByteString)]
toListBytes = Trie.toListBy f . dict_defs where
    f w def = (Word w , _bytes def)

instance DictSplitPrefix Dict where
    splitOnPrefix d p = 
        case Trie.lookupPrefixNode p (dict_defs d) of
            Nothing -> [] -- no content under prefix
            Just tn | BS.null (Trie.trie_prefix tn) ->
                let bAccept = isJust (Trie.trie_accept tn) in
                let lAccept = if bAccept then [Right (Word p)] else [] in
                let lChildBytes = fmap fst $ L.filter (isJust . snd) $
                        A.assocs $ Trie.trie_branch tn
                in
                let lChildren = fmap (Left . (p `BS.snoc`)) lChildBytes in
                lAccept ++ lChildren
            Just tn ->
                let fullPrefix = (p <> Trie.trie_prefix tn) in
                let lChildren = L.filter isJust $ A.elems $ Trie.trie_branch tn in
                if L.null lChildren then return (Right (Word fullPrefix)) else
                return (Left fullPrefix)
    wordsWithPrefix d p =
        let tk = Trie.lookupPrefix p (dict_defs d) in
        fmap (Word . (p <>)) (Trie.keys tk)

-- | Test whether a given word is defined in this dictionary.
member :: Dict -> Word -> Bool
member d w = (not . LBS.null) $ lookupBytes d w

instance DictRLU Dict where
    tokenClients d tok = maybe [] _rlu $ Trie.lookup tok (dict_deps d) where
        _rlu (Deps1 t) = fmap Word $ Trie.keys t

newtype DepsMap = DepsMap { _inDepsMap :: Map Word (Set Token) }
newtype ReverseDepsMap = ReverseDepsMap 
    { _inReverseDepsMap :: Map Token (Set Word) }
newtype ReverseDepsMapDiff = ReverseDepsMapDiff 
    { _inReverseDepsMapDiff :: Map Token ReverseDepsDiff }
type ReverseDepsDiff = (Set Word, Set Word) -- (del,add)

-- build a dependencies map for a set of words in the dictionary
_dictDepsMap :: Dict -> [Word] -> DepsMap
_dictDepsMap d = DepsMap . Map.fromList . fmap _withDeps where
    _withDeps w = (w, _tokens w)
    _tokens = Set.fromList . ABC.tokens . lookup d

-- dependencies for inserted words. These are accessible directly.
_insertDepsMap :: [(Word, ABC)] -> DepsMap
_insertDepsMap = DepsMap . Map.fromList . fmap (second (Set.fromList . ABC.tokens))


-- compute an incomplete set of reverse dependencies, limited to the
-- words in the original DepsMap. 
_revDeps :: DepsMap -> ReverseDepsMap 
_revDeps = ReverseDepsMap . L.foldl' insw Map.empty . Map.toList . _inDepsMap where
    insw m (w,toks) = L.foldl' (insd w) m (Set.toList toks)  -- insert word under every token
    insd w m tok = Map.alter (inss w) tok m -- insert word under a single token
    inss w = Just . maybe (Set.singleton w) (Set.insert w) -- add word to the token's dependency set

-- compute the difference between reversed dependencies
_revDepsDiff :: ReverseDepsMap -> ReverseDepsMap -> ReverseDepsMapDiff
_revDepsDiff old new = ReverseDepsMapDiff $ mergeDeps old' new' where
    old' = _inReverseDepsMap old
    new' = _inReverseDepsMap new
    mergeDeps = Map.mergeWithKey mergeOldAndNew mergeOld mergeNew
    mergeNew = fmap addDeps -- just in new, so add
    mergeOld = fmap delDeps -- just in old, so delete
    mergeOldAndNew _ sOld sNew = 
        let sAdd = sNew `Set.difference` sOld in
        let sDel = sOld `Set.difference` sNew in
        let bNoChange = Set.null sAdd && Set.null sDel in
        if bNoChange then Nothing else
        Just $ addDeps sAdd <> delDeps sDel
    addDeps sAdd = (mempty, sAdd)
    delDeps sDel = (sDel, mempty)
{-# NOINLINE _revDepsDiff #-}

-- tune the reverse lookup map by the given delta map
_updateDeps :: ReverseDepsMapDiff -> Trie Deps -> Trie Deps
_updateDeps dd t = L.foldl' _updOneDep t (Map.toList (_inReverseDepsMapDiff dd)) 

-- adjust dependencies for just one token
_updOneDep :: Trie Deps -> (Token, ReverseDepsDiff) -> Trie Deps
_updOneDep t (tok,(wsDel,wsAdd)) = Trie.adjust adj tok t where
    adj Nothing = adj' $ Trie.empty (Trie.trie_space t)
    adj (Just (Deps1 td)) = adj' td
    adj' td = 
        let td' = upd td in 
        if Trie.null td' then Nothing else 
        Just (Deps1 td')
    upd = Trie.insertList lIns . Trie.deleteList lDel
    lIns = fmap insw $ Set.toList wsAdd
    lDel = fmap delw $ Set.toList wsDel
    insw (Word w) = (w, U)
    delw (Word w) = w

-- recompute version hash for a specific word
_rehashWord :: Dict -> Word -> Dict
_rehashWord d w = 
    let abc = lookup d w in
    let bNull = L.null (ABC.abcOps abc) in
    let dhDel = Trie.delete (unWord w) (dict_hash d) in
    if bNull then d { dict_hash = dhDel } else
    let abc' = _annoWord w <> _rwHashWords (lookupVersionHash d) abc in
    let h' = secureHashLazy $ ABC.encode abc' in
    let dhUpd = Trie.insert (unWord w) h' (dict_hash d) in
    d { dict_hash = dhUpd } 

-- write ABC definition word tokens into versioned resource tokens
_rwHashWords :: (Word -> SecureHash) -> ABC -> ABC
_rwHashWords hf = ABC.rewriteTokens rw where
    rw t = [ABC.ABC_Tok (hashWordTok t)]
    h2t = BS.pack . (35 :) . B16.encode . BS.unpack
    hashWordTok t = maybe t h2t $ hashWord t
    hashWord t = 
        BS.uncons t >>= \ (c,w) ->
        guard (37 == c) >>
        return (hf (Word w))

-- update is one of the more sophisticated operations
-- mostly to maintain the reverse lookup index
instance DictUpdate Dict where
    unsafeUpdateWord w abc = unsafeUpdateWords (Map.singleton w abc)
    unsafeUpdateWords m d = d' where
        l = Map.toList m
        d' = L.foldl' _rehashWord du $ transitiveClientsList du (fmap fst l)
        du = d { dict_defs = defs', dict_deps = deps' } 

        defs' = updateDefs (dict_defs d)
        updateDefs = Trie.insertList lUpdated . Trie.deleteList lDeleted
        lDeleted = filterDeleted l
        lUpdated = filterUpdated l
        filterDeleted = fmap (unWord . fst) . L.filter isDeleted
        filterUpdated = fmap (unWord *** encDef) . L.filter (not . isDeleted)
        isDeleted = L.null . ABC.abcOps . snd
        encDef = Def1 . vref' (dict_space d) . ABC.encode

        deps' = _updateDeps depsMapDiff (dict_deps d)
        depsMapDiff = _revDepsDiff oldDepsMap newDepsMap
        oldDepsMap = _revDeps $ _dictDepsMap d (fmap fst l)
        newDepsMap = _revDeps $ _insertDepsMap l

