-- | Words are represented as a simple UTF-8 bytestring.
--
-- Because I'm no longer using Awelon Object directly, I don't have any
-- special concerns about the starting characters for a word. However, 
-- words must use characters valid in tokens (no LF, {}). And I'll apply
-- a few extra constraints to support display, import, export, metadata,
-- editing, listing, etc..
--
-- Forbidden characters: C0, SP, DEL, C1, U+FFFD, {}(|)[]"
-- Other forbidden words: the empty string
-- 
module Wikilon.Word
    ( Word(..), textToWord, wordToText, wordToUTF8
    , isValidWord, isValidWordChar
    ) where

import Data.Maybe (isNothing)
import Data.Char (ord)
import Data.String (IsString(..))
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8

-- | A word is represented as a bytestring with UTF8 encoding.
newtype Word = Word { unWord :: B.ByteString } deriving (Ord,Eq)

wordToText :: Word -> String
wordToText = UTF8.toString . wordToUTF8 

textToWord :: String -> Word
textToWord = Word . UTF8.fromString 

wordToUTF8 :: Word -> B.ByteString
wordToUTF8 = unWord

isValidWordChar :: Char -> Bool
isValidWordChar c = 
    let n = ord c in
    (n > 32) && ((n < 127) || (n >= 160)) &&
    L.notElem c "{}(|)[]\"\xfffd"

isValidWord :: Word -> Bool
isValidWord (Word w) = not (B.null w) && noForbiddenChars where
    noForbiddenChars = L.null $ L.dropWhile isValidWordChar $ UTF8.toString w

-- Show a Word
instance Show Word where 
    showsPrec _ = showString . wordToText

instance IsString Word where
    fromString = textToWord

