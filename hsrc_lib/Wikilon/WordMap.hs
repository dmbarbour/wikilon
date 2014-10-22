-- | Efficient word map based on IntMap, leveraging that words are
-- interned and have a unique hash identity.
module Wikilon.WordMap
    ( WordMap, empty, null, size
    , lookup, member, insert, delete, filter
    , union, difference, intersection
    , toList, fromList, keys, elems
    , toSortedList, sortedKeys
    ) where

import Prelude hiding (null, lookup, filter)
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as L
import Data.Hashable
import Wikilon.Word


newtype WordMap a = WordMap { _imap :: IntMap (W a) }
data W a = W { _word :: !Word, _val :: !a }

empty :: WordMap a 
empty = WordMap IntMap.empty

null :: WordMap a -> Bool
null = IntMap.null . _imap

size :: WordMap a -> Int
size = IntMap.size . _imap

member :: Word -> WordMap a -> Bool
member w = IntMap.member (hash w) . _imap

lookup :: Word -> WordMap a -> Maybe a
lookup w m = fmap _val $ IntMap.lookup (hash w) (_imap m)

insert :: Word -> a -> WordMap a -> WordMap a
insert w v = WordMap . IntMap.insert (hash w) (W w v) . _imap 

delete :: Word -> WordMap a -> WordMap a
delete w = WordMap . IntMap.delete (hash w) . _imap

filter :: (a -> Bool) -> WordMap a -> WordMap a
filter p = WordMap . IntMap.filter (p . _val) . _imap

union :: WordMap a -> WordMap a -> WordMap a
union a b = WordMap (IntMap.union (_imap a) (_imap b))

difference :: WordMap a -> WordMap a -> WordMap a
difference a b = WordMap (IntMap.difference (_imap a) (_imap b))

intersection :: WordMap a -> WordMap a -> WordMap a
intersection a b = WordMap (IntMap.intersection (_imap a) (_imap b))

toList :: WordMap a -> [(Word,a)]
toList = fmap wp . IntMap.elems . _imap where
    wp (W w v) = (w,v)

-- | obtain a deterministically, lexicographically ordered list
toSortedList :: WordMap a -> [(Word,a)]
toSortedList = L.sortBy (compare `on` (wordToUTF8 . fst)) . toList

fromList :: [(Word,a)] -> WordMap a
fromList = L.foldl' ins empty where
    ins m (w,v) = insert w v m

keys :: WordMap a -> [Word]
keys = fmap _word . IntMap.elems . _imap

sortedKeys :: WordMap a -> [Word]
sortedKeys = L.sortBy (compare `on` wordToUTF8) . keys

elems :: WordMap a -> [a]
elems = fmap _val . IntMap.elems . _imap

instance Functor WordMap where
    fmap f = WordMap . fmap f' . _imap where
        f' (W w v) = (W w (f v))

instance (Show a) => Show (WordMap a) where
    showsPrec _ m = showString "fromList "  . shows (toList m)

