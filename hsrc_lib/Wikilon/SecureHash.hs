{-# LANGUAGE ViewPatterns #-}

-- | Awelon project uses secure hashes for lots of purposes. The main
-- choice of secure hash is SHA3-384, selected for its simplicity.
-- 
module Wikilon.SecureHash 
    ( secureHash
    ) where

import Data.ByteString (ByteString)
-- import qualified Data.ByteString as B
import qualified Data.Byteable as B
import qualified Crypto.Hash as CH

-- | generate a secure hash from any bytestring.
secureHash :: ByteString -> ByteString
secureHash = B.toBytes . sha3_384

sha3_384 :: ByteString -> CH.Digest CH.SHA3_384
sha3_384 = CH.hash
