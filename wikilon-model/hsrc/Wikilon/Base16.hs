{-# LANGUAGE BangPatterns #-}
-- | Support for the Awelon project's favored "bdfghjkmnpqstxyz" base16
-- alphabet with compression optimized for ABC bytestrings and binaries
-- embedded within texts.
module Wikilon.Base16
    ( alphabet, compress, decompress
    ) where

import Data.Monoid
import Data.Word
import Data.Bits
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Array.Unboxed as UA
import Data.Char (ord)

-- | The Base16 alphabet primarily used within ABC resources.
alphabet :: String
alphabet = "bdfghjkmnpqstxyz"

alph8 :: [Word8]
alph8 = fmap (fromIntegral . ord) alphabet

h2cA :: UA.UArray Word8 Word8
h2cA = UA.listArray (0,15) alph8

h2c :: Word8 -> Word8
h2c = (UA.!) h2cA

c2hA :: UA.UArray Word8 Word8
c2hA = UA.accumArray ins maxBound (minBound,maxBound) lst where
    lst = L.zip alph8 [0..]
    ins = const id

c2h :: Word8 -> Word8
c2h = (UA.!) c2hA

isB16c :: Word8 -> Bool
isB16c = (/= maxBound) . c2h

-- | Compress ABC that may contain large Base16 sequences. 
--
--  * header byte 0xF8
--  * second byte encodes length:
--    * 0..127: 4..512 bytes on one line (multiples of four)
--    * 128..254: 2..128 lines of 32 bytes (separated by LF SP)
--    * 255: escape prior 0xF8 for a safety on arbitrary bytestrings
--
-- Thus, the optimal way to encode base16 is in 4096-byte embedded
-- text chunks. But we can use ad-hoc strings, e.g. of 128 bytes,
-- and still encode with acceptable overheads.
-- 
compress :: LBS.ByteString -> LBS.ByteString
compress = skipping where
    -- skip a literal
    skipping bytes = go 0 0 bytes where
        escF8 = LBS.pack [0xF8, 0xFF]
        go !ct !run s = case LBS.uncons s of
            Nothing -> bytes
            Just (c,s') 
                | (0xF8 == c) -> LBS.take (run+ct) bytes <> escF8 <> compress s'
                | (not (isB16c c)) -> go (1 + run + ct) 0 s'
                | (run < 7) -> go ct (1 + run) s'
                | otherwise -> LBS.take ct bytes <> scanning (LBS.drop ct bytes) run

    -- scan for end of binary (already at least 8 chars)
    -- does not scan more than we can process at once
    scanning bytes = flip ini bytes where
        ini run = line1 run . LBS.drop run
        line1 !run _ | (run == maxrun) = encode maxrun bytes
        line1 !run s = case LBS.uncons s of
            Nothing -> encode run bytes
            Just (c,s') 
                | (isB16c c) -> line1 (1+run) s'
                | (64 == run) -> scanLines 1 s 
                | otherwise -> encode run bytes
        lineSep = LBS.pack [10,32] -- LF SP
        scanLines !ct s = -- lines of 32 bytes with LF SP separators
            let bLineSep = (lineSep == LBS.take 2 s) in
            let ln = LBS.take 64 (LBS.drop 2 s) in
            let bOkLine = (64 == LBS.length ln) && (LBS.all isB16c ln) in
            let bAddLine = (ct < maxL) && bLineSep && bOkLine in
            if bAddLine then scanLines (1+ct) (LBS.drop 66 s) 
                        else encodeLines ct bytes

    encodeBytes (c1:c2:cs) = b:bs where
        b = (h1 `shiftL` 4) .|. h2
        h1 = c2h c1
        h2 = c2h c2
        bs = encodeBytes cs
    encodeBytes _ = []

    -- encoding a run of 4..512 bytes (8..1024 chars)
    maxWords = 128 
    maxrun = maxWords * 8
    encode run bytes =
        let nW = run `div` 8 in
        let nC = nW * 8 in
        let (sb,bytes') = LBS.splitAt nC bytes in
        let hdr = [0xF8, fromIntegral (nW - 1)] in
        let bdy = encodeBytes (LBS.unpack sb) in
        LBS.pack (hdr <> bdy) <> compress bytes'

    -- encode 2..128 lines each of 32 bytes (64 chars) 
    -- each separated by LF SP (up to 4kB).
    maxL = 128
    lineBytes s | LBS.null s = mempty
                | otherwise  = LBS.take 64 s <> lineBytes (LBS.drop 66 s)
    encodeLines 1 bytes = encode 64 bytes
    encodeLines ct bytes =
        let nC = (66 * ct) - 2 in
        let (sb,bytes') = LBS.splitAt nC bytes in
        let hdr = [0xF8, fromIntegral (ct + 126)] in
        let bdy = encodeBytes (LBS.unpack (lineBytes sb)) in
        LBS.pack (hdr <> bdy) <> compress bytes'

decompress :: LBS.ByteString -> LBS.ByteString
decompress = seeking where
    seeking s = case LBS.elemIndex 0xF8 s of
        Nothing -> s
        Just ix -> case LBS.uncons (LBS.drop (ix + 1) s) of
            Nothing -> s
            Just (c, s') 
                | (c == 0xFF) -> LBS.take (ix + 1) s <> decompress s'
                | (c >= 0x80) -> LBS.take ix s <> loadLines (fromIntegral (c - 126)) s'
                | otherwise -> LBS.take ix s <> loadChunk (fromIntegral c) s'

    loadChunk nW s =
        let nB = 4 * nW in
        let (sb,s') = LBS.splitAt nB s in
        let bdy = decodeBytes (LBS.unpack sb) in
        LBS.pack bdy <> decompress s'

    loadLines ct s = 
        let nB = 32 * ct in
        let (sb,s') = LBS.splitAt nB s in
        LBS.pack (lineBytes sb []) <> decompress s' 

    lineBytes s =
        let (bytes,s') = LBS.splitAt 32 s in
        let chars = mappend $ decodeBytes (LBS.unpack bytes) in
        let sep = if (LBS.null s') then id else (++) [10,32] in
        chars . sep . lineBytes s'

    decodeBytes (b:bs) = (c1:c2:cs) where
        h1 = (b `shiftR` 4)
        h2 = (b .&. 0xF)
        c1 = h2c h1
        c2 = h2c h2
        cs = decodeBytes bs
    decodeBytes [] = []


-- \n gbgdgfggghgkgjgmgngpfbhdhfhghhhjhkhmhnhphqhshthxhyhzjbjdjfjgjhfb

