{-# LANGUAGE ViewPatterns #-}

-- | Awelon project uses secure hashes for lots of purposes. The main
-- choice of secure hash is SHA3-384, selected for its simplicity and
-- its useful divisibility into thirds.
module Wikilon.SecureHash 
    ( SecureHash, secureHash, secureHashLazy
    , hmac, hmacBlockSize
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Byteable
import qualified Crypto.Hash as CH

-- | a Wikilon SecureHash is a bytestring of length 48.
type SecureHash = B.ByteString

-- for type inference
toSecureHash :: CH.Digest CH.SHA3_384 -> SecureHash
toSecureHash = Data.Byteable.toBytes

hashContext :: CH.Context CH.SHA3_384
hashContext = CH.hashInit

-- | generate a secure hash from any bytestring.
secureHash :: B.ByteString -> SecureHash
secureHash = toSecureHash . CH.hash

-- | generate secure hash from lazy bytestring
-- (avoids an intermediate allocation)
secureHashLazy :: BL.ByteString -> SecureHash
secureHashLazy = toSecureHash . CH.hashlazy

-- | hmac secret message
--
-- generate hash based message authentication code
-- may truncate if using less security is okay
hmac :: B.ByteString -> B.ByteString -> SecureHash
hmac secret = toSecureHash . CH.hmacGetDigest . CH.hmac secret

-- | maximum effective secret size (in bytes) for HMAC
hmacBlockSize :: Int
hmacBlockSize = CH.hashBlockSize hashContext

