{-# LANGUAGE ViewPatterns #-}

-- | Awelon project uses secure hashes for lots of purposes. 
-- The favored secure hash is SHA3-256.
module Wikilon.Hash 
    ( SecureHash, secureHash, secureHashLazy
    , Signature(..), hmac, hmacLazy
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Byteable
import qualified Crypto.Hash as CH
-- import Data.Function (on)
import qualified Data.List as L

-- | a Wikilon SecureHash is a bytestring of length 32.
type SecureHash = ByteString

-- for type inference
toSecureHash :: CH.Digest CH.SHA3_256 -> SecureHash
toSecureHash = Data.Byteable.toBytes

hashContext :: CH.Context CH.SHA3_256
hashContext = CH.hashInit

-- | generate a secure hash from any bytestring.
secureHash :: ByteString -> SecureHash
secureHash = secureHashL . (:[])

-- | generate secure hash from lazy bytestring
-- (avoids an intermediate allocation)
secureHashLazy :: BL.ByteString -> SecureHash
secureHashLazy = secureHashL . BL.toChunks

secureHashL :: [ByteString] -> SecureHash
secureHashL = toSecureHash . CH.hashFinalize . CH.hashUpdates hashContext

type Secret = ByteString

-- | Signature supports constant time comparison.
newtype Signature = Signature { sigBytes :: SecureHash }
instance Eq Signature where (==) = constTimeEqSigs

-- | hmac secret message; generate hash-based signature for message
hmac :: Secret -> ByteString -> Signature
hmac secret message = Signature $ secureHashL [secret, message]

-- Note: According to the developers of SHA3, the SHA3 algorithm is
-- not vulnerable to the attacks that require the more sophisticated
-- HMAC algorithm. I can simply prepend the key. But this is specific
-- to SHA3. Most hash algorithms need the double-hashing treatment.

-- | hmac secret message, with lazy message string
hmacLazy :: Secret -> BL.ByteString -> Signature
hmacLazy secret = Signature . secureHashL . (secret :) . BL.toChunks

-- | compare signatures in a manner resistant to timing attacks.  
constTimeEqSigs :: Signature -> Signature -> Bool
constTimeEqSigs (Signature sigA) (Signature sigB) = 
    if (B.length sigA /= B.length sigB) then False else
    let iMatch a b = if (a == b) then 1 else 0 in
    let matchList = B.zipWith iMatch sigA sigB in
    let matchCount = L.foldl' (+) 0 matchList in
    (B.length sigA == matchCount)
    -- a robust alternative: (==) `on` secureHash
    -- but that is also much more expensive.