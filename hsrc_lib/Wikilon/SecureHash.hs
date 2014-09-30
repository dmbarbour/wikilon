{-# LANGUAGE ViewPatterns #-}

-- | Awelon project uses secure hashes for lots of purposes. The main
-- choice of secure hash is SHA3-384, selected for its simplicity and
-- its useful divisibility into thirds.
module Wikilon.SecureHash 
    ( secureHash
    , hmac
    ) where

import Data.ByteString (ByteString)
import qualified Data.Byteable as B
import qualified Crypto.Hash as CH

-- for type inference
using_sha3_384 :: CH.Digest CH.SHA3_384 -> CH.Digest CH.SHA3_384
using_sha3_384 = id

-- | generate a secure hash from any bytestring.
secureHash :: ByteString -> ByteString
secureHash = B.toBytes . using_sha3_384 . CH.hash

-- | hmac secret message
--
-- generate hash based message authentication code
-- may truncate if using less security is okay
hmac :: ByteString -> ByteString -> ByteString
hmac secret = B.toBytes . using_sha3_384 . CH.hmacGetDigest . CH.hmac secret
