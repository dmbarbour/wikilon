{-# LANGUAGE DeriveDataTypeable #-}

-- | Internal structure for Wikilon dictionary.
module Wikilon.Dict.Type
    ( Dict(..), Def, Deps, Sz
    , Error
    , dictErr
    ) where

import Control.Applicative
import Data.Typeable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.VCache.Trie (Trie)
import Database.VCache

-- | A dictionary contains a set of definitions
data Dict = Dict
    { dict_data :: Trie Def
    , dict_deps :: Trie Deps
    } deriving (Eq, Typeable)
type Def  = Sz LBS.ByteString
type Deps = Sz [BS.ByteString]
type Sz a = Either (VRef a) a
-- Currently, definitions and dependencies are inlined into our Trie
-- nodes up to a heurstic threshold in size, maybe ~700 bytes. Very 
-- large elements thus use their own nodes to help control deep lookup
-- costs.
--
-- At this time, no compression is performed.

-- | The Wikilon.Dict module guards against a few kinds of errors:
--
-- * cyclic dependencies
-- * missing definitions
-- * unrecognized tokens
--
-- Other errors, such as badly typed code, divergent computations,
-- or incorrect documentation must be addressed by clients.
type Error = String 

-- include a version number in case I need to update the dictionary
-- type in the future.
instance VCacheable Dict where
    put (Dict _data _deps) = putWord8 0 >> put _data >> put _deps
    get = getWord8 >>= \ v -> case v of
        0 -> Dict <$> get <*> get
        _ -> fail $ dictErr $ "unrecognized version " ++ show v

dictErr :: String -> String
dictErr = ("Wikilon.Dict: " ++)

