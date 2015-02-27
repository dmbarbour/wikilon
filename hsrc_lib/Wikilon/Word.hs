-- | Words are represented as a reverse-ordered UTF-8 bytestring.
--
-- The normal convention for words is that prefixes indicate role
-- such as testing or documentation, while suffixes indicate context
-- such as a lists, maps, or a particular project. The word's bytes
-- are reversed with the hypothesis that it should improve clustering
-- of updates to dictionaries and other data structures. (But this is
-- not verified.)
--
module Wikilon.Word
    ( Word(..), textToWord, wordToText, wordToUTF8
    ) where

import Data.String (IsString(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8

-- | A word is represented as a bytestring with UTF8 encoding.
newtype Word = Word { unWord :: B.ByteString } deriving (Ord,Eq)

wordToText :: Word -> String
wordToText = UTF8.toString . wordToUTF8 

textToWord :: String -> Word
textToWord = Word . B.reverse . UTF8.fromString 

wordToUTF8 :: Word -> B.ByteString
wordToUTF8 = B.reverse . unWord

-- Show a Word
instance Show Word where 
    showsPrec _ = showString . wordToText

instance IsString Word where
    fromString = textToWord

