
-- | Every Wikilon instance has a large secret value.
--
-- This value is used for lots of purposes: HMACs, generation of
-- unique identifiers, keys, and securely pseudo-random numbers.
-- These other values shall generally be *deterministic*, i.e.
-- such that the same values are recomputed after a restart, yet
-- secure and specific to each Wikilon instance.
--
-- At no point is the secret to be directly shared; rather, this
-- secret shall be applied primarily through use of HMAC. 
-- 
-- Related: 
--   http://awelonblue.wordpress.com/2013/08/26/source-stable-uniqueness/
--
module Wikilon.Secret 
    ( Secret, newSecret
    ) where

import Data.ByteString (ByteString)
import System.Entropy (getEntropy)
import Wikilon.SecureHash (hmacBlockSize)

-- | A secret is a high-entropy bytestring. 
type Secret = ByteString

-- | generate a new secret
newSecret :: IO Secret
newSecret = getEntropy hmacBlockSize
    -- choice of hmacBlockSize is a pretty good option for most
    -- secure hashes. For SHA3, it doesn't matter as much, but
    -- might later allow optimization by preprocessing the secret
    -- into the hash context.
