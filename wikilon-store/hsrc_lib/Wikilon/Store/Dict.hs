{-# LANGUAGE DeriveDataTypeable, PatternGuards, TypeSynonymInstances #-}
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

import Awelon.ABC (ABC)
import qualified Awelon.ABC as ABC

import Wikilon.Dict
import Wikilon.Dict.Word
import Wikilon.Dict.Token

import Wikilon.Store.Dict.Type

_cm :: CacheMode
_cm = CacheMode0

-- | a dictionary is hosted in a vcache address space
dict_space :: Dict -> VSpace
dict_space = Trie.trie_space . dict_defs

-- | provide simple information about current value
unsafeDictAddr :: Dict -> Word64
unsafeDictAddr = Trie.unsafeTrieAddr . dict_defs

-- | O(1). Create a new, empty dictionary
empty :: VSpace -> Dict
empty vs = Dict1
    { dict_defs = Trie.empty vs
    , dict_deps = Trie.empty vs
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


instance DictView Dict where
    lookup d = _decode . lookupBytes d
    lookupBytes d (Word w) = 
        case Trie.lookupc _cm w (dict_defs d) of
            Just (Def1 v) -> deref' v
            Nothing -> mempty
    toList = fmap (second _decode) . toListBytes

    -- dictDiff :: dict -> dict -> [Word]
    -- TODO: support efficient Trie diff-list and merge functions

_bytes :: Def -> LBS.ByteString
_bytes (Def1 v) = deref' v

-- | Obtain a list of all (Word, ByteString) pairs. Words are sorted by suffix.
toListBytes :: Dict -> [(Word, LBS.ByteString)]
toListBytes = Trie.toListBy f . dict_defs where
    f w def = (Word w , _bytes def)

instance DictSplitPrefix Dict where
    splitOnPrefix d p = 
        let t = Trie.lookupPrefix p (dict_defs d) in
        case Trie.trie_root t of
            Nothing -> [] -- no content under prefix
            Just v -> 
                let tn = derefc _cm v in
                let fullPrefix = p <> Trie.trie_prefix tn in
                let bAccept = isJust (Trie.trie_accept tn) in
                let lAccept = if bAccept then [Right (Word fullPrefix)] else [] in
                let lChildBytes = fmap fst $ L.filter (isJust . snd) $
                        A.assocs $ Trie.trie_branch tn
                in
                let lChildren = fmap (Left . (fullPrefix `BS.snoc`)) lChildBytes in
                lAccept ++ lChildren
    wordsWithPrefix d p =
        let tk = Trie.lookupPrefix p (dict_defs d) in
        fmap (Word . (p <>)) (Trie.keys tk)

-- | Test whether a given word is defined in this dictionary.
member :: Dict -> Word -> Bool
member d w = (not . LBS.null) $ lookupBytes d w

instance DictRLU Dict where
    tokenClients d tok = maybe [] _rlu $ Trie.lookupc _cm tok (dict_deps d) where
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

-- update is one of the more sophisticated operations
-- mostly to maintain the reverse lookup index
instance DictUpdate Dict where
    updateDictWord w abc = updateDictWords (Map.singleton w abc)
    updateDictWords m d = d' where
        l = Map.toList m
        d' = Dict1 { dict_defs = defs', dict_deps = deps' } 
        isDeleted = L.null . ABC.abcOps . snd
        filterDeleted = fmap (unWord . fst) . L.filter isDeleted
        filterUpdated = fmap (unWord *** encDef) . L.filter (not . isDeleted) 
        encDef = Def1 . vref' (dict_space d) . ABC.encode
        lDeleted = filterDeleted l
        lUpdated = filterUpdated l
        updateDefs = Trie.insertList lUpdated . Trie.deleteList lDeleted
        defs' = updateDefs (dict_defs d)
        oldDepsMap = _revDeps $ _dictDepsMap d (fmap fst l)
        newDepsMap = _revDeps $ _insertDepsMap l
        depsMapDiff = _revDepsDiff oldDepsMap newDepsMap
        deps' = _updateDeps depsMapDiff (dict_deps d)

