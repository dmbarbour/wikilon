
module Wikilon.Hash ( hash, hashL ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64
import qualified Crypto.Hash.BLAKE2.BLAKE2b as B2b
import qualified Data.List as L

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


