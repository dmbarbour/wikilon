{-# LANGUAGE ViewPatterns #-}

-- | Utilities for URL encoding and decoding. 
--
-- The functions in Network.HTTP.Base aren't doing exactly what I 
-- need wrgt. Utf8 encode and decode.
module Wikilon.WAI.URL
    ( isOKPathByte
    , encodePathBytes
    , decodePathBytes
    ) where

import Control.Exception (assert)
import Data.Word
import Data.Bits
import Data.Char
import qualified Data.Array.Unboxed as UA

-- all characters that don't need to be pct-encoded
okPathBytes :: UA.UArray Word8 Bool
okPathBytes = UA.accumArray (flip const) False (minBound,maxBound) lst where
    lst = fmap toE okChars
    toE c = (fromIntegral (ord c), True)
    okChars = "/" ++ okPathSeg
    okPathSeg = unreserved ++ subdelims ++ ":@"
    unreserved = alpha ++ digit ++ "-._~"
    alpha = ['a'..'z'] ++ ['A'..'Z']
    digit = ['0'..'9']
    subdelims = "!$&'()*+,;="

-- | Test whether a Path byte must be pct-encoded
isOKPathByte :: Word8 -> Bool
isOKPathByte = (UA.!) okPathBytes
{-# INLINE isOKPathByte #-}

-- | Encode a list of bytes with pct-encodings. This will favor
-- capital letters in the pct-encoding, e.g. %CC. Always succeeds.
encodePathBytes :: [Word8] -> [Word8]
encodePathBytes (x:xs) = 
    if isOKPathByte x then x : encodePathBytes xs else
    let (xHi,xLo) = x `divMod` 16 in
    let hi = toHexDigit xHi in
    let lo = toHexDigit xLo in
    hi `seq` lo `seq`
    37 : hi : lo : encodePathBytes xs
encodePathBytes [] = []

-- | Decode a list of bytes with pct-encodings. This accepts both   
-- upper and lower case pct-encoded characters. Also, invalid chars
-- will be preceded by 0xFE to guard against invalid encodings.
-- (0xFE does not appear in valid UTF-8.)
decodePathBytes :: [Word8] -> [Word8]
decodePathBytes (37 : -- pct-encoded
                 (fromHexDigit -> Just hi) : 
                 (fromHexDigit -> Just lo) : more) =
    let n = (hi * 16) + lo in
    n `seq` (n : decodePathBytes more)
decodePathBytes (x : xs) =
    let pathBytes = x : decodePathBytes xs in
    if isOKPathByte x then pathBytes else 0xFE : pathBytes
decodePathBytes [] = []

toHexDigit :: Word8 -> Word8
toHexDigit n = assert (n == (n .&. 0xf)) $
    if (n < 10) then n + 48 else n + 55

fromHexDigit :: Word8 -> Maybe Word8
fromHexDigit n | (48 <= n) && (n <= 57) = Just $! (n - 48)
               | (65 <= n) && (n <= 70) = Just $! (n - 55)
               | (97 <= n) && (n <= 102) = Just $! (n - 87)
               | otherwise = Nothing

