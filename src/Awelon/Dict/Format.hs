{-# LANGUAGE BangPatterns #-}
-- | This module is concerned with parsing or serializing the Awelon
-- dictionary patch representation, which essentially consists of:
--
--   secureHashOfPatch1
--   secureHashOfPatch2
--   @word1 def1
--   @word2 def2
--   @@ns secureHashOfDict
--   ...
-- 
-- An assumption is that a single dictionary file isn't too large,
-- usually a few tens of kilobytes up to a couple megabytes. The 
-- large scale dictionary requires leveraging those secure hashes
-- via load and stow operations from Awelon.CX.
-- 
module Awelon.Dict.Format
    ( findLLSep, splitDict
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Control.Exception (assert)

-- | An Awelon dictionary separates logical lines with the `LF @`
-- sequence. This function returns the index to the next `LF @`
-- line separator (returning the index for the LF). 
findLLSep :: LBS.ByteString -> Maybe Int64
findLLSep = f 0 where
    f !k s = case LBS.elemIndex 10 s of
        Nothing -> Nothing
        Just ix -> 
            let skip = (1 + ix) in
            let s' = LBS.drop skip s in
            case LBS.uncons s' of
                Just (64, _) -> Just (k + ix)
                _ -> f (k + skip) s'

-- | break a dictionary into a header and entries, without loss of
-- any information. This function doesn't parse the elements, but
-- may simplify isolation of errors.
splitDict :: LBS.ByteString -> (LBS.ByteString, [LBS.ByteString])
splitDict s = case LBS.uncons s of
    Just (64, _) -> (mempty, splitEnt s)
    _ -> case findLLSep s of
        Just ix -> (hd, splitEnt bdy) 
            where (hd,bdy) = LBS.splitAt (ix + 1) s
        _ -> (s, mempty)

-- | split entries on logical lines. Invariants:
--     s == mconcat (splitEnt s)
--     modulo terminals, entries end with LF and start with @.
splitEnt :: LBS.ByteString -> [LBS.ByteString]
splitEnt s = case findLLSep s of
    Nothing -> [s]
    Just ix -> e : splitEnt s' where
        (e, s') = LBS.splitAt (ix + 1) s

-- note: we might wish to annotate line numbers in case of a parse error.

-- parse hashes
-- parse entries
-- map entries


