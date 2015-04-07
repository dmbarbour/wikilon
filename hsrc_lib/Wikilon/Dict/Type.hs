{-# LANGUAGE DeriveDataTypeable #-}

-- | Internal structure for Wikilon dictionary.
module Wikilon.Dict.Type
    ( Dict(..), Def, Deps, Sz
    , dictErr
    , depSizeThresh
    , defSizeThresh
    ) where

import Control.Applicative
import Data.Typeable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.VCache.Trie (Trie)
import Database.VCache

-- | A dictionary contains two indices:
--
-- * a trie from words to definitions
-- * another trie from words to clients
--
-- Here a word 'foo' is a client of 'bar' if bar uses foo. Having the
-- client index is essential for efficient deletion or rename while 
-- guarding dictionary invariants. It's also very convenient for other
-- roles, such as incremental compilation or live coding.
--
-- At this time, the dictionary performs no compression within the
-- definitions. Compression is achieved by refactoring code to control
-- the lookup costs (i.e. so we don't have trouble if a very large
-- value has a key in the middle of the trie).
-- 
data Dict = Dict
    { dict_data :: !(Trie Def)
    , dict_deps :: !(Trie Deps)
    } deriving (Eq, Typeable)
type Def  = Sz LBS.ByteString
type Deps = Sz [BS.ByteString]
type Sz a = Either (VRef a) a
-- Currently, definitions and dependencies are inlined into our Trie
-- nodes up to a heurstic threshold in size, a few hundred bytes. Very 
-- large elements thus use their own nodes to help control deep lookup
-- costs.
--
-- At the moment, I'm not compressing the dictionary entries. I'll 
-- review this option later, if I start seeing very large entries.
depSizeThresh, defSizeThresh :: Int
depSizeThresh = 400
defSizeThresh = 400

-- includes version number to support updates to representation
-- (e.g. in case I add compression later).
instance VCacheable Dict where
    put (Dict _data _deps) = putWord8 0 >> put _data >> put _deps
    get = getWord8 >>= \ v -> case v of
        0 -> Dict <$> get <*> get
        _ -> fail $ dictErr $ "unrecognized version " ++ show v

dictErr :: String -> String
dictErr = ("Wikilon.Dict: " ++)

