{-# LANGUAGE ViewPatterns #-}

-- | Awelon project uses secure hashes for lots of purposes. The main
-- choice of secure hash is SHA3-384, selected for its simplicity and
-- its useful divisibility into thirds.
--
-- Note: I'll need to be aware of timing attacks on HMAC comparisons.
-- This may be a non-issue for the resource model I'm developing, but
-- naive techniques like taking {foo!proposed hmac} and comparing the
-- proposed hmac to hmac(foo) would be vulnerable. 
-- 
--
module Wikilon.SecureHash 
    ( SecureHash, secureHash, secureHashLazy
    , hmac, hmacValidate, hmacBlockSize
    ) where

import Data.ByteString (ByteString)
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Byteable
import qualified Crypto.Hash as CH

-- | a Wikilon SecureHash is a bytestring of length 48.
type SecureHash = ByteString

-- for type inference
toSecureHash :: CH.Digest CH.SHA3_384 -> SecureHash
toSecureHash = Data.Byteable.toBytes

hashContext :: CH.Context CH.SHA3_384
hashContext = CH.hashInit

-- | generate a secure hash from any bytestring.
secureHash :: ByteString -> SecureHash
secureHash = toSecureHash . CH.hash

-- | generate secure hash from lazy bytestring
-- (avoids an intermediate allocation)
secureHashLazy :: BL.ByteString -> SecureHash
secureHashLazy = toSecureHash . CH.hashlazy

-- | hmac secret message
--
-- generate hash based message authentication code
-- may truncate if using less security is okay
hmac :: ByteString -> ByteString -> SecureHash
hmac secret = toSecureHash . CH.hmacGetDigest . CH.hmac secret

-- | validation of hmac (timing attack resistant)
--
--    hmacValidate secret message suspiciousSignature -> Bool
--
-- Returns True if the given signature passes muster.
--
-- At the moment, I'm just using an extra secureHash step to 
-- randomize the bytes before comparison. Given that applying
-- hmac uses two secureHashes already, this will very roughly 
-- double the cost compared to naive comparisons.
--
-- It's very important to use this function if validating
-- signed capabilities.
--
hmacValidate :: ByteString -> ByteString -> ByteString -> Bool
hmacValidate secret message suspiciousSignature = matched where
    matched = (h validSignature) == (h suspiciousSignature)
    validSignature = hmac secret message
    h = secureHash


-- | maximum effective secret size (in bytes) for HMAC
hmacBlockSize :: Int
hmacBlockSize = CH.hashBlockSize hashContext

