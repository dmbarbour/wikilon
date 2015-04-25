{-# LANGUAGE OverloadedStrings #-}

-- | Words in Wikilon should be URL-friendly, and also friendly in
-- other contexts. Sadly, this is quite limiting. Why don't URLs 
-- allow UTF8 yet? I might relax these constraints later, if I'm
-- later convinced that using more UTF8 is safe. (My Google-fu
-- returns mixed answers.)
--
-- Constraints:
--
--  URL friendly: alpha | num | -._~ | !$&'()*+,;= | :@ | pct-encoded
--    (pchars <http://tools.ietf.org/html/rfc3986#section-3.3>)
--  Eyeball friendly: 
--    forbid C0 SP DEL C1 U+FFFD 
--    forbid empty string
--    forbid pct-encoded (e.g. %3C%20%25%2A)
--  Text and Delimiter friendly: 
--    forbid ,;{}(|)[]"`
--    don't end with a .
--  Not confusable with numbers:
--    forbid words starting with digit
--    forbid words starting with +-. followed by digit
--  Rel-path friendly: forbid . and ..
--
-- Some of these constraints are redundant.
--
-- A few sub-delims remain available for Wikilon's use "(),;". It should
-- be easy to pick words out of English text, though I might need to mark
-- them in such a context.
--
-- Conventions:
--
-- In older AO code, I used role.word.context as an organization
-- within words. I think in Wikilon I'll flip this around closer to
-- how URLs typically encode things: context.word.role. This is also
-- more convenient for my use of tries to group words from a given
-- context.
-- 
module Wikilon.Dict.Word
    ( Word(..), textToWord, wordToText, wordToUTF8
    , isValidWord, isValidWordChar
    ) where

import Data.Char (ord)
import Data.Word (Word8)
import Data.String (IsString(..))
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Array.Unboxed as UA

-- | A word is represented as a bytestring with UTF8 encoding.
newtype Word = Word { unWord :: UTF8.ByteString } deriving (Ord,Eq)

wordToText :: Word -> String
wordToText = UTF8.toString . wordToUTF8 

textToWord :: String -> Word
textToWord = Word . UTF8.fromString 

wordToUTF8 :: Word -> UTF8.ByteString
wordToUTF8 = unWord

-- valid bytes in a word string
wcArray :: UA.UArray Word8 Bool
wcArray = UA.accumArray (flip const) False (minBound,maxBound) lst where
    toE c = (fromIntegral (ord c), True)
    lst = fmap toE okChars
    okChars = alpha ++ num ++ "-._~!$&'*+=:@"
    alpha = ['a'..'z']++['A'..'Z']
    num = ['0'..'9']

isValidWordByte :: Word8 -> Bool
isValidWordByte = (wcArray UA.!)
{-# INLINE isValidWordByte #-}

isValidWordChar :: Char -> Bool
isValidWordChar c = (n <= 255) && isValidWordByte (fromIntegral n)
    where n = ord c
{-# INLINE isValidWordChar #-}

isValidWord :: Word -> Bool
isValidWord (Word w) = okStart && okMiddle && okEnd where
    okEnd = not $ "." `BS.isSuffixOf` w
    okMiddle = BS.all isValidWordByte w
    okStart = case UTF8.uncons w of
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

