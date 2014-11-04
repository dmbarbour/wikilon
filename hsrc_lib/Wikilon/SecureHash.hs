{-# LANGUAGE ViewPatterns #-}

-- | Awelon project uses secure hashes for lots of purposes. The main
-- choice of secure hash is SHA3-384, selected for its simplicity and
-- its useful divisibility into thirds.
--
-- Note: I'll need to be aware of timing attacks on HMAC comparisons.
--
module Wikilon.SecureHash 
    ( SecureHash, secureHash, secureHashLazy
    , Signature, hmac, hmacLazy, hmacBlockSize
    , constTimeEqSigs
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Byteable
import qualified Crypto.Hash as CH
-- import Data.Function (on)
import qualified Data.List as L

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

type Secret = ByteString
type Signature = SecureHash

-- | hmac secret message; generate hash-based signature for message
--
-- COMPARE SIGNATURES WITH `constTimeEqSigs` TO RESIST TIMING ATTACKS.
hmac :: Secret -> ByteString -> Signature
hmac secret = hmacLazy secret . BL.fromStrict

-- | hmac secret message, with lazy message string
--
-- COMPARE SIGNATURES WITH `constTimeEqSigs` TO RESIST TIMING ATTACKS.
hmacLazy :: Secret -> BL.ByteString -> Signature
hmacLazy secret = secureHashLazy . BL.append (BL.fromStrict secret)
    -- Note: According to the developers of SHA3, the algorithm is
    -- not vulnerable to the attacks that require a more sophisticated
    -- HMAC algorithm.

-- | effective secret size (in bytes) for HMAC
hmacBlockSize :: Int
hmacBlockSize = CH.hashBlockSize hashContext

-- | compare signatures in a manner resistant to timing attacks.  
constTimeEqSigs :: Signature -> Signature -> Bool
constTimeEqSigs sigA sigB = 
    if (B.length sigA /= B.length sigB) then False else
    let iMatch a b = if (a == b) then 1 else 0 in
    let matchList = B.zipWith iMatch sigA sigB in
    let matchCount = L.foldl' (+) 0 matchList in
    (B.length sigA == matchCount)
    -- a more robust option: (==) `on` secureHash
    -- but that is also relatively expensive.
