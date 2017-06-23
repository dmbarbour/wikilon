module Awelon.Hash 
    ( Hash
    , hash, hashL
    , hashAlphabet
    , validHashByte, validHashLen, validHash
    ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.UTF8 as U8
import qualified Crypto.Hash.BLAKE2.BLAKE2b as B2b
import qualified Data.List as L
import qualified Data.Array.Unboxed as A
import Data.Word (Word8, Word32)
import Data.Char (chr, ord)
import Data.Bits
import Data.String
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafeDupablePerformIO) 
import Control.Exception (assert)

-- | A Hash is just a bytestring, for now. But it should be valid.
--
-- Note: In context of lookups for secure hash resources, we may
-- need to ensure constant-time comparisons.
type Hash = BS.ByteString

-- | Awelon language makes widespread use of secure hashes to name
-- various resources. In particular, we use a 280-bit Blake2b hash
-- and we encode this using a specilized base32 alphabet. 
--
-- > :set -XOverloadedStrings
-- > hash "test"
-- "HSjFNGRnqHpFFbPhlThmqCbqkmDSHCBlJNnmDPnDtnCpKHqtNgqhRMJG"
--
hash :: BS.ByteString -> Hash
hash = hashL . LBS.fromStrict

hashL :: LBS.ByteString -> Hash
hashL = encodeHash . b2b_digest 35

b2b_digest :: Int -> LBS.ByteString -> BS.ByteString
b2b_digest z = B2b.finalize z 
             . L.foldl (flip B2b.update) (B2b.initialize z)
             . LBS.toChunks

-- | Awelon's unusual Base32 alphabet for hashes.
--
-- This alphabet does not conflict with human meaningful words or
-- numbers, nor structural conventions that use punctuation. And it
-- is unlikely to appear by accident in context of conservative GC.
hashAlphabet :: String
hashAlphabet = assert (32 == L.length alphabet) $ alphabet where
    alphabet = "bcdfghjklmnpqrstBCDFGHJKLMNPQRST"

-- | The length (in characters) of a valid hash string.
validHashLen :: (Num a) => a
validHashLen = 56

hashBytesArray :: A.UArray Word8 Bool
hashBytesArray = A.listArray (0,255) $ fmap accept [0..255] where
    accept = flip L.elem hashAlphabet . chr

-- | Really tests whether a byte is a valid base64url character.
validHashByte :: Word8 -> Bool
validHashByte = (A.!) hashBytesArray

validHash :: BS.ByteString -> Bool
validHash s = (validHashLen == BS.length s) 
            && (BS.all validHashByte s)

-- encoding
type Word5 = Word8

eW5A :: A.UArray Word5 Word8
eW5A = A.listArray (0,31) $ fmap (fromIntegral . ord) hashAlphabet

eW5 :: Word5 -> Word8
eW5 = (A.!) eW5A

encodeHash :: BS.ByteString -> BS.ByteString
encodeHash (BS.PS fptr off sz) = 
    unsafeDupablePerformIO $ 
     BS.create validHashLen $ \ dst ->
      withForeignPtr fptr $ \ src -> 
       packHash (src `plusPtr` off) dst

-- pack hash assuming sufficient data in source and space in destination.
-- Note that this is highly specialized for the validHashlen. It does not
-- try for a generic hash encoding.
--
-- We want to output 8 bytes for every 5 bytes of input. Following normal
-- conventions for base32 (RFC4648) the high order bit of the first byte
-- will be the first bit in the stream.
--
-- We never actually 'decode' this base32 string. It's more convenient and
-- debuggable to stick with the ASCII encoding after the hash is formed. 
packHash :: Ptr Word8 -> Ptr Word8 -> IO ()
packHash s d = p280 where
    p280  = do { p40 0; p40 1; p40 2; p40 3; p40 4; p40 5; p40 6 }
    p40 n = do
        let r ix   = peekElemOff (s `plusPtr` (5 * n)) ix
        let w ix v = pokeElemOff (d `plusPtr` (8 * n)) ix (eW5 v)
        -- read five bytes
        i0 <- r 0 
        i1 <- r 1
        i2 <- r 2
        i3 <- r 3
        i4 <- r 4
        -- write eight bytes
        w 0 $ ((i0 .&. 0xF8) `unsafeShiftR` 3)
        w 1 $ ((i0 .&. 0x07) `unsafeShiftL` 2) .|.
              ((i1 .&. 0xC0) `unsafeShiftR` 6)
        w 2 $ ((i1 .&. 0x3E) `unsafeShiftR` 1)
        w 3 $ ((i1 .&. 0x01) `unsafeShiftL` 4) .|.
              ((i2 .&. 0xF0) `unsafeShiftR` 4)
        w 4 $ ((i2 .&. 0x0F) `unsafeShiftL` 1) .|.
              ((i3 .&. 0x80) `unsafeShiftR` 7)
        w 5 $ ((i3 .&. 0x7C) `unsafeShiftR` 2)
        w 6 $ ((i3 .&. 0x03) `unsafeShiftL` 3) .|.
              ((i4 .&. 0xE0) `unsafeShiftR` 5)
        w 7 $ ((i4 .&. 0x1F)                 )

