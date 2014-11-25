
-- | Wikilon DB needs a good, fast, consistent hash. I don't want to
-- use Data.Hash because it might change from version to version.
--
-- For simplicity, I'm just using Murmur3. It's a pretty good hash 
-- function developed by Austin Appleby around 2010-2011, and is
-- generally in the public domain. It has a Haskell implementation. 
--
-- The main gripes I have about it are inability to efficiently hash
-- a lazy bytestring, and inability to directly access hash results 
-- as a number.
-- 
module Wikilon.DB.Hash 
    ( hash
    ) where

import Data.Word (Word64)
import Data.Bits (shiftL, (.|.))
import qualified Data.Digest.Murmur3 as M3
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

hash :: B.ByteString -> Word64
hash = unsafeWord64be . M3.asByteString . M3.hash

unsafeWord64be :: B.ByteString -> Word64
unsafeWord64be = \ s ->
    (fromIntegral (s `B.unsafeIndex` 0) `shiftL` 56) .|.
    (fromIntegral (s `B.unsafeIndex` 1) `shiftL` 48) .|.
    (fromIntegral (s `B.unsafeIndex` 2) `shiftL` 40) .|.
    (fromIntegral (s `B.unsafeIndex` 3) `shiftL` 32) .|.
    (fromIntegral (s `B.unsafeIndex` 4) `shiftL` 24) .|.
    (fromIntegral (s `B.unsafeIndex` 5) `shiftL` 16) .|.
    (fromIntegral (s `B.unsafeIndex` 6) `shiftL` 8)  .|.
    (fromIntegral (s `B.unsafeIndex` 7))

