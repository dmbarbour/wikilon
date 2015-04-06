-- | Words are represented as a simple UTF-8 bytestring.
--
-- While I'm no longer using Awelon Object directly, I might still want
-- something like AO for purpose of rendering end editing code. So, I
-- continue to constrain word structure for a few nice properties:
--
-- Forbidden characters: C0, SP, DEL, C1, U+FFFD, {}(|)[]"`
-- Other forbidden words: 
--   the empty string
--   words starting with a digit 0-9
--   words starting with +-. followed by a digit
--
-- The latter two constraints avoid most words that would be visually
-- confusable with numerals. 
--
-- In original AO, I reversed context such that role, type, or purpose
-- appears first in a word and context appears last. However, for Wikilon,
-- I think I'll stick with the conventional order that users are familiar
-- with from filesystems and URLs: context first, type last. 
-- 
module Wikilon.Dict.Word
    ( Word(..), textToWord, wordToText, wordToUTF8
    , isValidWord, isValidWordChar
    ) where

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
isValidWord (Word w) = validStart && noForbiddenChars where
    noForbiddenChars = L.all isValidWordChar $ UTF8.toString w
    validStart = case UTF8.uncons w of
        Nothing -> False
        Just (c, w') | _isDigit c -> False
                     | not (_isPMD c) -> True
                     | otherwise -> maybe True (not . _isDigit . fst) (UTF8.uncons w')

_isPMD :: Char -> Bool
_isPMD = flip L.elem "+-."

_isDigit :: Char -> Bool
_isDigit c = ('0' <= c) && (c <= '9')

-- Show a Word
instance Show Word where 
    showsPrec _ = showString . wordToText

instance IsString Word where
    fromString = textToWord

