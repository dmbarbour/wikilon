
module Wikilon.Word
    ( Word, textToWord, wordToText
    , wordToUTF8
    ) where

import Data.String (IsString(..))
import qualified Data.ByteString as B
import qualified Codec.Binary.UTF8.Generic as UTF8

-- | A word is represented as a bytestring with UTF8 encoding.
newtype Word = Word { unWord :: B.ByteString } deriving (Ord,Eq)

wordToText :: Word -> String
wordToText = UTF8.toString . B.unpack . unWord

textToWord :: String -> Word
textToWord = Word . B.pack . UTF8.fromString 

wordToUTF8 :: Word -> B.ByteString
wordToUTF8 = unWord

-- Show a Word
instance Show Word where 
    showsPrec _ = showString . wordToText

instance IsString Word where
    fromString = textToWord
