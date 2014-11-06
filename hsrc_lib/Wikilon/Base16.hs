{-# LANGUAGE ViewPatterns, PatternGuards #-}
-- | Awelon project uses a non-conventional approach for encoding
-- binary data: simply embed binaries in text or tokens using a 
-- base16 alphabet, then apply a specialized compression pass on
-- large sequences of this alphabet. For large binaries, overhead 
-- is less than 1% compared to a raw binary encoding.
--
-- The alphabet is `bdfghjkmnpqstxyz`. And the compression form adds
-- a two byte header 0xF8+L. L encodes length 3..256 (0..253 +3). So
-- we can encode 256 bytes in 258 bytes for 0.8% overhead. The 0xF8
-- character does not normally appear in UTF-8, but will be escaped
-- by following with 0xFE as needed.
--
-- The alphabet (a-z minus vowels and `vrwlc` data plumbing) resists
-- interference with human meaningful text and numbers.
--
-- This is much better than base64. The encoding breaks even at six
-- bytes. And this base16 approach does not interfere much with more 
-- byte-level compression.
--
-- TODO: switch encoder/decoder/compression to operate on lazy byte
-- strings or byte string builders, for extra performance.
-- 
module Wikilon.Base16 
    ( alphabet
    , encode
    , decode
    , compress
    , decompress
    ) where

import Control.Exception (assert)
import Data.Word
import qualified Data.List as L
import qualified Data.Array.Unboxed as A


-- | The alphabet for embedding Base16 in ABC is not conventional.
-- Instead of 0-9 A-F, we use the lower case alphabet minus vowels
-- and most ABC data plumbing operators (vrwlc). This mitigates risk
-- of spelling offensive words, and limits interference with other
-- compression for the non-binary elements.
alphabet :: String
alphabet = "bdfghjkmnpqstxyz"

alph8 :: [Word8]
alph8 = fmap (fromIntegral . fromEnum) alphabet


-- array from numeric value (0..15) to ASCII character
n16tob16 :: A.UArray Word8 Word8
n16tob16 = A.listArray (0,15) alph8 -- alphabet is values

-- array from ASCII character to numeric value (or sentinel)
b16ton16 :: A.UArray Word8 Word8
b16ton16 = A.accumArray upd maxBound (minBound,maxBound) lst where
    lst = L.zip alph8 [0..] -- alphabet is index this time
    upd _ = id -- simply assign the given value.

-- given a character that should be in the alphabet, 
-- translate it to a number in range 0..15.
h2n :: Word8 -> Maybe Word8
h2n c | (n < 16)  = Just n
      | otherwise = Nothing
    where n = b16ton16 A.! c

-- | Given raw binary, translate into alphabet.
encode :: [Word8] -> [Word8]
encode (b:bs) = h1 : h2 : encode bs where
    (n1,n2) = b `divMod` 16
    h1 = n16tob16 A.! n1
    h2 = n16tob16 A.! n2
encode [] = []

-- | Given a binary consisting of ABC base16 alphabet
-- elements, translate to binary. The second bytestring
-- returns the first data that could not be decoded.
decode :: [Word8] -> ([Word8],[Word8])
decode = decode' []

decode' :: [Word8] -> [Word8] -> ([Word8],[Word8])
decode' dbs ((h2n -> Just h1) : (h2n -> Just h2) : bs) = decode' dbs' bs where
    dbs'    = newByte : dbs 
    newByte = h1*16 + h2
decode' dbs bs = (L.reverse dbs, bs)


-- | Compression always works. Given a binary that may contain some
-- *embedded* base16, it will compress those embedded base16 sequences.
--
-- Existing header bytes (0xF8), however, will be expanded to two bytes. 
-- (But this won't happen for compressing UTF-8 encoded text.)
compress :: [Word8] -> [Word8]
compress (0xF8 : bs) = (0xF8 : 0xFE : compress bs)
compress (tkb16 -> (n,hbs,bs)) | (n >= 3) = 
    assert (n <= 256) $ (0xF8 : fromIntegral (n - 3) : hbs) ++ compress bs
compress (b:bs) = (b : compress bs)
compress [] = []

-- try to take up to 256 base16 characters from the stream
tkb16 :: [Word8] -> (Int, [Word8], [Word8])
tkb16 = tkb16' 0 []

tkb16' :: Int -> [Word8] -> [Word8] -> (Int, [Word8], [Word8])
tkb16' n hbs bs | (n >= 256) = (n, L.reverse hbs, bs) -- maximum extraction
tkb16' n hbs ((h2n -> Just h1) : (h2n -> Just h2) : bs') = tkb16' n' hbs' bs' where
    hbs' = hb:hbs
    hb = h1*16 + h2
    n' = n + 1
tkb16' n hbs bs = (n, L.reverse hbs, bs) -- could not extract a byte

-- | Decompress a byte stream, expanding embedded base16. 
--
-- Note: this implementation will succeed for all inputs, even the
-- invalid ones (e.g. 0xF8 0xFF, or not enough bytes in list). 
decompress :: [Word8] -> [Word8]
decompress (0xF8 : n : bs) | (n > 253) = (0xF8 : decompress bs)
                           | otherwise = 
    let (hbs,bs') = L.splitAt ((fromIntegral n)+3) bs in
    encode hbs ++ decompress bs'
decompress (b : bs) = b : decompress bs
decompress [] = []

