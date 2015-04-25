{-# LANGUAGE BangPatterns #-}
module Wikilon.ABC.Util
    ( encSizedSlice, encVarNat
    , readSizedSlice, readVarNat
    , sizedSliceFby
    ) where

import Data.Monoid
import Data.Bits
import Data.Int
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8

type Bytes = LBS.ByteString

-- argument must be positive; low byte is 0..127, 
encVarNat :: Int64 -> BB.Builder
encVarNat n | (n < 0) = error $ "negative varNat " ++ show n
            | otherwise = _encVarNat q <> BB.word8 lo
  where q = n `shiftR` 7
        lo = 0x7f .&. fromIntegral n

-- all bytes except the low byte tagged by 0x80
_encVarNat :: Int64 -> BB.Builder
_encVarNat 0 = mempty
_encVarNat n = _encVarNat q <> BB.word8 b where
    q = n `shiftR` 7
    b = 0x80 .|. (0x7f .&. fromIntegral n)

encSizedSlice :: Bytes -> BB.Builder
encSizedSlice s = encVarNat (LBS.length s) <> BB.lazyByteString s
{-# INLINE encSizedSlice #-}

readSizedSlice ::  Bytes -> Maybe (Bytes, Bytes)
readSizedSlice b = 
    readVarNat b >>= \ (n, b') ->
    let (lhs,rhs) = LBS.splitAt n b' in
    let bOK = (n == LBS.length lhs) in
    if not bOK then Nothing else
    return (lhs,rhs)
{-# INLINE readSizedSlice #-}

readVarNat :: Bytes -> Maybe (Int64, Bytes)
readVarNat = r 0 where
    r !n !t =
        LBS.uncons t >>= \ (byte, t') -> 
        let n' = n `shiftL` 7 .|. (fromIntegral (byte .&. 0x7f)) in
        let bDone = (0 == (byte .&. 0x80)) in
        if bDone then return (n', t') else r n' t'

sizedSliceFby :: Char -> Bytes -> Maybe (Bytes, Bytes)
sizedSliceFby cExpect bs = 
    readSizedSlice bs >>= \ (slice, cont) ->
    LazyUTF8.uncons cont >>= \ (c, bs') ->
    let bOK = cExpect == c in
    if not bOK then Nothing else
    return (slice, bs')
{-# INLINE sizedSliceFby #-}


