{-# LANGUAGE BangPatterns #-}
-- | Support for the Awelon project's favored "bdfghjkmnpqstxyz" base16
-- alphabet with compression optimized for ABC bytestrings and binaries
-- embedded within texts.
module Wikilon.Base16
    ( alphabet, compress, decompress
    ) where

import Control.Exception (assert)
import Data.Monoid
import Data.Word
import Data.Bits
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBSI
import qualified Data.ByteString as BS
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
-- text chunks at 32 bytes (64 chars) per line. But we can use ad-hoc
-- strings and at least have some savings over base64 encodings.
-- 
compress :: LBS.ByteString -> LBS.ByteString
compress = skipping where
    escF8 = LBS.pack [0xF8, 0xFF]
    blockSize = 32 * 1024
    minRun = 8
    -- skip a literal
    skipping bytes = go 0 0 bytes where
        go !ct !run s = case LBS.uncons s of
            Nothing -> bytes
            Just (c,s') 
                | (0xF8 == c) -> LBS.take (run+ct) bytes <> escF8 <> compress s'
                | (not (isB16c c)) -> 
                        let ct' = 1 + run + ct in
                        if (ct' < blockSize) then go ct' 0 s' else
                        let (literal, bytes') = LBS.splitAt ct' bytes in
                        literal <> compress bytes' 
                | otherwise -> 
                        let run' = 1 + run in
                        if (run' < minRun) then go ct run' s' else
                        let (literal, bytes') = LBS.splitAt ct bytes in
                        literal <> scanning bytes' run'

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
    seeking s@(LBSI.Chunk chunk s') = case BS.elemIndex 0xF8 chunk of
        Nothing -> LBSI.Chunk chunk (decompress s') -- for streaming decompress
        Just ix -> foundF8 (fromIntegral ix) s -- element to decompress
    seeking LBSI.Empty = LBSI.Empty

    foundF8 ix s = case LBS.uncons (LBS.drop (ix + 1) s) of
        Nothing -> s -- invalid input, treat as escape
        Just (c, s') 
                | (c == 0xFF) -> LBS.take (ix + 1) s <> decompress s'
                | (c >= 0x80) -> LBS.take ix s <> loadLines (fromIntegral (c - 126)) s'
                | otherwise   -> LBS.take ix s <> loadChunk (fromIntegral (c + 1)) s'

    -- 4..512 bytes (1..128 words of 4 bytes)
    loadChunk nW s =
        assert ((nW >= 1) && (nW <= 128)) $
        let nB = 4 * nW in
        let (sb,s') = LBS.splitAt nB s in
        let bdy = decodeBytes (LBS.unpack sb) in
        LBS.pack bdy <> decompress s'

    -- 64..4096 bytes (2..128 lines of 32 bytes)
    loadLines ct s = 
        assert ((ct >= 2) && (ct <= 128)) $
        let nB = 32 * ct in
        let (sb,s') = LBS.splitAt nB s in
        LBS.pack (lineBytes sb) <> decompress s' 

    lineBytes = line where
        moreLines s = if LBS.null s then mempty else [10,32] <> line s
        line s = decodeBytes (LBS.unpack l) <> moreLines s' where
            (l, s') = LBS.splitAt 32 s

    decodeBytes (b:bs) = (c1:c2:cs) where
        h1 = (b `shiftR` 4)
        h2 = (b .&. 0xF)
        c1 = h2c h1
        c2 = h2c h2
        cs = decodeBytes bs
    decodeBytes [] = []

-- abc base16 compression test text
-- kdkfkgfbkfkdmgkjgdgkfbkgkzkxmbmfkjmgmgkpkzkyfbmhkjmgmhfbmhkjmnmh

