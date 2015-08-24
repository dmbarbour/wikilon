
-- | An OO interface for a dictionary, i.e. to avoid depending
-- on global nominative types and typeclasses. This provides a
-- typeclass instance, but shifts all the real labor into an 
-- explicit record of functions carried with the dictionary
-- representation.
module Wikilon.Dict.Object 
    ( DictInterface(..), DictObj(..)
    , wrapDictObj
    , module Wikilon.Dict
    ) where

import Prelude hiding (lookup)
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Wikilon.Dict

-- | A 'DictObj' pairs an abstract representation with a concrete
-- interface. The DictInterface value should be constant for a given
-- representation type. The 'rep' type could feasibly be hidden as
-- an existential type, but for dictDiff it's important to keep it
-- constant.
data DictObj rep = DictObj rep (DictInterface rep)

dr :: DictObj rep -> rep
dr (DictObj rep _) = rep

di :: DictObj rep -> DictInterface rep
di (DictObj _ dictInterface) = dictInterface

-- | A record of functions for the full Dictionary instance.
data DictInterface rep = DictInterface
    { di_lookup :: rep -> Word -> ABC
    , di_lookupBytes :: rep -> Word -> LazyUTF8.ByteString
    , di_toList :: rep -> [(Word, ABC)]
    , di_diff :: rep -> rep -> [Word]
    , di_lookupVersionHash :: rep -> Word -> SecureHash
    , di_splitOnPrefix :: rep -> WordPrefix -> [Either WordPrefix Word]
    , di_wordsWithPrefix :: rep -> WordPrefix -> [Word]
    , di_tokenClients :: rep -> Token -> [Word]
    , di_unsafeUpdateWords :: Map Word ABC -> rep -> rep
    }

-- | Given any object of class Dictionary, we can trivially wrap it
-- into a DictObj.
wrapDictObj :: Dictionary dict => dict -> DictObj dict
wrapDictObj d = DictObj d $ DictInterface 
    { di_lookup = lookup
    , di_lookupBytes = lookupBytes
    , di_toList = toList
    , di_diff = dictDiff
    , di_lookupVersionHash = lookupVersionHash
    , di_splitOnPrefix = splitOnPrefix
    , di_wordsWithPrefix = wordsWithPrefix
    , di_tokenClients = tokenClients
    , di_unsafeUpdateWords = unsafeUpdateWords
    }

instance DictView (DictObj rep) where
    lookup d = di_lookup (di d) (dr d)
    lookupBytes d = di_lookupBytes (di d) (dr d)
    toList d = di_toList (di d) (dr d)
    dictDiff a b = di_diff (di a) (dr a) (dr b)
    lookupVersionHash d = di_lookupVersionHash (di d) (dr d)
instance DictSplitPrefix (DictObj rep) where
    splitOnPrefix d = di_splitOnPrefix (di d) (dr d)
    wordsWithPrefix d = di_wordsWithPrefix (di d) (dr d)
instance DictRLU (DictObj rep) where
    tokenClients d = di_tokenClients (di d) (dr d)
instance DictUpdate (DictObj rep) where
    unsafeUpdateWord w abc = unsafeUpdateWords (Map.singleton w abc)
    unsafeUpdateWords update (DictObj rep iface) = 
        let rep' = di_unsafeUpdateWords iface update rep in
        DictObj rep' iface 
instance Dictionary (DictObj rep)
