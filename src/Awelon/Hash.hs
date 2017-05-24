
module Awelon.Hash 
    ( hash, hashL
    , validHash, validHashByte, validHashLen
    ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64
import qualified Crypto.Hash.BLAKE2.BLAKE2b as B2b
import qualified Data.List as L
import qualified Data.Array.Unboxed as A
import Data.Word (Word8)
import Data.Char (chr)

-- | Awelon language makes widespread use of secure hashes to name
-- various resources. In particular, we use a 360-bit Blake2b hash
-- and we encode this with base64url. 
--
-- > :set -XOverloadedStrings
-- > hash "test"
-- "J7URBffnfK_NVVcQNQ6D21k5A7J8Zhhwb2Ry3WLYfFc7Vy1TiE01Q4H7duKE"
--
hash :: BS.ByteString -> BS.ByteString
hash = hashL . LBS.fromStrict

hashL :: LBS.ByteString -> BS.ByteString
hashL = B64.encode . b2b_digest 45

b2b_digest :: Int -> LBS.ByteString -> BS.ByteString
b2b_digest z = B2b.finalize z 
             . L.foldl (flip B2b.update) (B2b.initialize z)
             . LBS.toChunks

b64s :: String
b64s = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['-', '_']

b64bytesArray :: A.UArray Word8 Bool
b64bytesArray = A.listArray (0,255) $ fmap accept [0..255] where
    accept = flip L.elem b64s . chr

-- | Really tests whether a byte is a valid base64url character.
validHashByte :: Word8 -> Bool
validHashByte = (A.!) b64bytesArray

-- | The length of a valid hash string.
validHashLen :: (Num a) => a
validHashLen = 60

-- | Tests whether a bytestring matches the structure expected
-- from a hash output (60 characters, each a validHashByte)
validHash :: BS.ByteString -> Bool
validHash s = (validHashLen == BS.length s) 
            && (BS.all validHashByte s)



