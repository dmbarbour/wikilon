
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

-- | A secret is a high-entropy bytestring. 
type Secret = ByteString

-- How big should our secrets be? Large enough that they are essentially
-- unforgeable. In this case, I'm using 512 bytes, or 4096 bits, which is
-- large enough that I don't expect any trouble.
secretSize :: Int
secretSize = 512 {- bytes -}

-- | generate a new secret
newSecret :: IO Secret
newSecret = getEntropy secretSize
