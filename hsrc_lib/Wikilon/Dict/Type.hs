{-# LANGUAGE DeriveDataTypeable #-}

-- | Internal structure for Wikilon dictionary.
module Wikilon.Dict.Type
    ( Dict1(..)
    , Def1(..)
    , Deps1(..)
    , U(..)
    , Dict, Def, Deps
    , dictErr
    ) where

import Control.Applicative
import Data.Typeable
import qualified Data.ByteString.Lazy as LBS
import Data.VCache.Trie (Trie)
import qualified Data.VCache.Trie as Trie
import Database.VCache

-- | A dictionary contains two indices:
--
-- * a defs trie from words to definitions
-- * a deps trie from tokens to client words
--
-- All tokens are tracked in the dependencies. Thus, words are listed
-- as `%foo` but we can also find any annotation used in the dictionary
-- (and by whom it is used). Locating all words that use a given token
-- is potentially useful for debugging, typechecking, etc..
--
-- This basic dictionary performs no compression. I think it probably
-- isn't essential, since the dictionary itself can be used as a basis
-- for compression.
-- 
type Dict = Dict1
type Def  = Def1
type Deps = Deps1

-- version 1 of the dictionary
data Dict1 = Dict1
    { dict_defs :: !(Trie Def1)
    , dict_deps :: !(Trie Deps1)
    } deriving (Eq, Typeable)
newtype Def1 = Def1 (VRef LBS.ByteString) deriving (Eq, Typeable)
newtype Deps1 = Deps1 (Trie U) deriving (Eq, Typeable)
data U = U deriving (Eq, Typeable) -- a Unit that is upgradeable in VCache
 -- i.e. stores as a '0' placeholder value (like empty list or Nothing).

-- Note: I've decided not to include a suffix-tree or full text lookup at
-- this layer, in part because they're too difficult to easily maintain.
-- I might later try to maintain these using background batch mechanisms
-- in another layer.

-- THOUGHTS: Should I add a suffix-tree? or generalized suffix tree?
--  Probably not, unless I can maintain it easily across updates. Otherwise,
--  it might be better to treat this as a full index problem, which might
--  be tooled externally (export all the different dictionaries and versions,
--  index them). 
--

-- Note: if an old version of a dictionary isn't supported, you may
-- always export to file then import again. However, I'll try to support
-- updateable types here.

-- includes version number to support updates to representation
-- (e.g. in case I add compression later, or switch to a sized
-- trie for incremental views).
instance VCacheable Dict1 where
    put (Dict1 _defs _deps) = putWord8 1 >> put _defs >> put _deps
    get = getWord8 >>= \ v -> case v of
        1 -> Dict1 <$> get <*> get
        _ -> fail $ dictErr $ "unrecognized Dict version " ++ show v
instance VCacheable Def1 where
    put (Def1 ref) = putWord8 1 >> put ref
    get = getWord8 >>= \ v -> case v of
        1 -> Def1 <$> get
        _ -> fail $ dictErr $ "unrecognized Def version " ++ show v
instance VCacheable Deps1 where
    put (Deps1 t) = putWord8 1 >> put t
    get = getWord8 >>= \ v -> case v of
        1 -> Deps1 <$> get
        _ -> fail $ dictErr $ "unrecognized Deps version " ++ show v
instance VCacheable U where
    put U = putWord8 0
    get = getWord8 >>= \ v -> case v of
        0 -> return U
        _ -> fail $ dictErr $ "unrecognized Unit value " ++ show v

dictErr :: String -> String
dictErr = ("Wikilon.Dict: " ++)

