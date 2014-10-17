-- | Words are currently implemented as an interned UTF8 bytestring.
--
-- This has the following advantages:
--
--  * tighter memory requirements
--  * fast comparison and hashing
--
-- There are also a few disadvantages:
--
--  * cannot GC old words, e.g. from a failed parse
--  * non-deterministic ordering for sets or maps keyed by words
-- 
-- I can adapt for the latter issue, e.g. by explicitly translating
-- word maps or sets to bytestrings when serializing transactions.
-- GC is perhaps a non-issue anyway, since I'm keeping histories.
-- 
module Wikilon.Word
    ( Word, textToWord, wordToText, wordToUTF8
    ) where

import Data.String (IsString(..))
import qualified Data.ByteString as B
import qualified Codec.Binary.UTF8.Generic as UTF8
import Data.Interned
import Data.Interned.ByteString
import Data.Hashable

-- | A word is represented as a bytestring with UTF8 encoding.
newtype Word = Word { unWord :: InternedByteString } deriving (Ord,Eq)

wordToText :: Word -> String
wordToText = UTF8.toString . B.unpack . wordToUTF8

textToWord :: String -> Word
textToWord = Word . intern . B.pack . UTF8.fromString 

wordToUTF8 :: Word -> B.ByteString
wordToUTF8 = unintern . unWord

-- Thoughts: I'm not going to provide the word's intern ID, since I
-- want the ability to back out of using interning. 

-- Show a Word
instance Show Word where 
    showsPrec _ = showString . wordToText

instance IsString Word where
    fromString = textToWord

instance Hashable Word where 
    hashWithSalt s = hashWithSalt s . hash
    hash = internedByteStringId . unWord

