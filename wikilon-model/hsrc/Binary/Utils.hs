
-- | utilities for encoding and decoding binary strings
module Binary.Utils
    ( bbVarInt, bbVarNat, rdVarInt, rdVarNat
    , bbSizedSlice, rdSizedSlice
    ) where

import Control.Arrow (first)
import Data.Monoid
import Data.Bits
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS

bbVarInt :: Integer -> BB.Builder
bbVarInt = bbVarNat . zigZag

zigZag :: Integer -> Integer
zigZag n | (n < 0)   = (negate n * 2) - 1 
         | otherwise = (n * 2)

bbVarNat :: Integer -> BB.Builder
bbVarNat nat | (nat < 0) = error "no varnat encoding for negative number"
             | otherwise = hib (nat `shiftR` 7) <> BB.word8 (lob nat)
    where hib 0 = mempty
          hib n = hib (n `shiftR` 7) <> BB.word8 (0x80 .|. lob n) 
          lob n = 0x7f .&. fromIntegral n

rdVarInt :: LBS.ByteString -> (Integer, LBS.ByteString)
rdVarInt = first unZigZag . rdVarNat

unZigZag :: Integer -> Integer
unZigZag n | (0x1 == (n .&. 0x1)) = negate ((n + 1) `div` 2)
           | otherwise = n `div` 2

-- | read a base128 varnat encoded number
rdVarNat :: LBS.ByteString -> (Integer, LBS.ByteString)
rdVarNat = go 0 where
    go n s = case LBS.uncons s of
        Nothing -> error "invalid varnat encoding"
        Just (d,s') | done -> (n', s')
                    | otherwise -> go n' s' 
            where n' = (n `shiftL` 7) .|. digit
                  digit = fromIntegral (d .&. 0x7f) 
                  done = (0x80 == (d .&. 0x80))

-- | record a bytestring prefixed by its size as a varnat
bbSizedSlice :: LBS.ByteString -> BB.Builder
bbSizedSlice s = bbVarNat len <> BB.lazyByteString s where
    len = fromIntegral (LBS.length s)

-- | read a bytestring that is prefixed by its size as a varnat
rdSizedSlice :: LBS.ByteString -> (LBS.ByteString, LBS.ByteString)
rdSizedSlice s = 
    let (len, s') = rdVarNat s in 
    LBS.splitAt (fromIntegral len) s'
