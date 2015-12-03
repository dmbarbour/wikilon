-- | AO dictionary words reference other code within a closed, acyclic
-- dictionary. In AO code, this is represented by {%word} tokens. But
-- words should also be friendly for other contexts such as use within
-- URLs, English text, and command language.
--
-- Constraints:
--
--  URL friendly: alpha | num | -._~ | !$&'()*+,;= | :@ | pct-encoded
--    (pchars <http://tools.ietf.org/html/rfc3986#section-3.3>)
--    must exclude '.' and '..' which have special URL rules
--  Eyeball friendly: 
--    forbid C0 SP DEL C1 U+FFFD 
--    forbid empty string
--  Text, HTML, Delimiter, and Extension friendly: 
--    forbid ,;{}(|)[]<>"`&=
--    don't end with a . (easily mistaken for end of sentence)
--  Not confusable with numbers:
--    forbid words starting with digit
--    forbid words starting with +-. followed by digit
--
-- Some of these constraints are redundant.
--
-- A few sub-delims remain available for use in URLs "(),;=". It should
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
module Wikilon.Word
    ( Word(..), textToWord, wordToText, wordToUTF8
    , isValidWord, isValidWordChar
    , listWordConstraintsForHumans
    , wordSizeMax
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
wcArray = UA.accumArray (flip const) False (0,127) lst where
    toE c = (fromIntegral (ord c), True)
    lst = fmap toE okChars
    okChars = alpha ++ num ++ "-._~!$'*+:"
    alpha = ['a'..'z']++['A'..'Z']
    num = ['0'..'9']
{-# NOINLINE wcArray #-}

isValidWordChar :: Char -> Bool
isValidWordChar c = okASCII || okUnicode where
    n = ord c
    okASCII = ((n >= 0) && (n <= 127)) && (wcArray UA.! fromIntegral n)
    okUnicode = (n >= 160) && not badUnicode
    badUnicode = isReplacementChar || isSurrogateCodepoint
    isReplacementChar = (n == 0xfffd)
    isSurrogateCodepoint = (0xd800 <= n) && (n <= 0xdfff)

isValidWord :: Word -> Bool
isValidWord (Word w) = okSize && okStart && okEnd && okMiddle where
    okSize = (BS.length w <= wordSizeMax)
    okStart = case UTF8.uncons w of
        Nothing -> False
        Just (c, w') | _isDigit c -> False
                     | not (_isPMD c) -> True
                     | otherwise -> maybe True (not . _isDigit . fst) (UTF8.uncons w')
    okEnd = not . badEnd . fromIntegral $ BS.last w where
        badEnd n = (n == ord '.') || (n == ord ':')
    okMiddle = L.all isValidWordChar (UTF8.toString w)

-- | maximum size for a word, in bytes
wordSizeMax :: Int
wordSizeMax = 64

_isPMD :: Char -> Bool
_isPMD = flip L.elem "+-."

_isDigit :: Char -> Bool
_isDigit c = ('0' <= c) && (c <= '9')

-- | heuristic constraints on words, written for humans
listWordConstraintsForHumans :: [String]
listWordConstraintsForHumans =
    ["ASCII if alphabetical, numeral, or in -._~!$'*+:"
    ,"UTF8 except C1, surrogates, and replacement char"
    ,"must not start with digit or +-. followed by digit"
    ,"must not terminate with . or : (period or colon)"
    ,"UTF8 encoding of word between 1 and " ++ show wordSizeMax ++ " bytes"
    ]

-- Show a Word
instance Show Word where 
    showsPrec _ = showString . wordToText

instance IsString Word where
    fromString = textToWord
