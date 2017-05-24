
module Wikilon.DB.HashDeps
    ( hashDeps
    ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Awelon.Hash as H

-- | Find substrings that look like hashes.
-- 
-- The motivation here is to identify potential dependencies in context
-- of conservative GC. This search assumes hashes are encoded in base64url
-- format, with the exact length (sixty bytes), and separated from other
-- hashes with a non-hash character (e.g. an Awelon word separator). 
--
-- This is guaranteed for Awelon code, and is easy enough to enforce for
-- other structures.
hashDeps :: LBS.ByteString -> [BS.ByteString]
hashDeps s = 
    if LBS.null s then [] else
    let hs' = LBS.dropWhile (not . H.validHashByte) s in
    let (h, s') = LBS.span H.validHashByte hs' in
    let consHash = if (H.validHashLen == LBS.length h) 
                    then (:) (LBS.toStrict h) 
                    else id {- not a valid hash -} 
    in
    consHash $ hashDeps s'

