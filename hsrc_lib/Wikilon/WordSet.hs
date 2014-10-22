
-- | Efficient set of words based on IntMap, leveraging interned
-- structure of words to moderately improve performance.
module Wikilon.WordSet
    ( WordSet, empty, null, size
    , member, insert, delete, filter
    , union, difference, intersection
    , toList, fromList
    , toSortedList
    , map
    ) where

import Prelude hiding (null, lookup, map, filter)
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as L
import Data.Hashable
import Wikilon.Word

newtype WordSet = WordSet { _imap :: IntMap Word }

empty :: WordSet 
empty = WordSet IntMap.empty

null :: WordSet -> Bool
null = IntMap.null . _imap

size :: WordSet -> Int
size = IntMap.size . _imap

member :: Word -> WordSet -> Bool
member w = IntMap.member (hash w) . _imap

insert :: Word -> WordSet -> WordSet
insert w = WordSet . IntMap.insert (hash w) w . _imap

delete :: Word -> WordSet -> WordSet
delete w = WordSet . IntMap.delete (hash w) . _imap

filter :: (Word -> Bool) -> WordSet -> WordSet
filter f = WordSet . IntMap.filter f . _imap

toList :: WordSet -> [Word]
toList = IntMap.elems . _imap

fromList :: [Word] -> WordSet
fromList = L.foldl' (flip insert) empty

-- | Obtain a *lexicographically* sorted list of words.
-- The output from toList has a non-deterministic order 
-- based on interning. toSortedList will be deterministic.
toSortedList :: WordSet -> [Word]
toSortedList = L.sortBy (compare `on` wordToUTF8) . toList

union :: WordSet -> WordSet -> WordSet
union a b = WordSet (IntMap.union (_imap a) (_imap b))

difference :: WordSet -> WordSet -> WordSet
difference a b = WordSet (IntMap.difference (_imap a) (_imap b))

intersection :: WordSet -> WordSet -> WordSet
intersection a b = WordSet (IntMap.intersection (_imap a) (_imap b))

map :: (Word -> Word) -> WordSet -> WordSet
map f = fromList . fmap f . toList

instance Show WordSet where
    showsPrec _ s = showString "fromList "  . shows (toList s)


