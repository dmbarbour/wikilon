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
    ( DictHead, DictNS, DictDefs, Dict(..)
    , parseHead, findLLSep, splitDict
    ) where

import Prelude hiding (Word)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import Control.Exception (assert)
import Data.Int (Int64)
import qualified Data.Map as M
import Awelon.Syntax
import Awelon.Hash

-- | A parsed dictionary header - a list of secure hashes. Logically,
-- each secure hash resource is inlined into the dictionary.
type DictHead = [Hash]

-- | Hierarchical dictionary namespaces - each uses a hash to specify
-- the dictionary used, or nothing to indicate deletion of namespace.
-- Namespaces are used for words like `foo@ns`. Each namespace is fully
-- confined and self-contained, with no external dependencies.
type DictNS = M.Map NS (Maybe Hash)

-- | Words with definitions. A word is logically equivalent to its
-- definition, semantically we just rewrite from word to definition
-- when doing so allows progress. If words are primitive or implicit
-- they may not be directly defined at this layer.
type DictDefs = M.Map Word Prog

-- | The Dict type consisting of a head, namespaces, and definitions.
--
-- This is a lossy representation of a parsed dictionary. Whitespace
-- formatting is lost. Order of definitions on separate words within
-- code is lost (last definition 'wins', per Awelon semantics). 
--
-- Constructing the Dict will detect a subset of errors - especially
-- parse errors, but would not include detection of cyclic definitions
-- or type errors or totality errors, etc..
data Dict = Dict { dictHead :: DictHead
                 , dictNS   :: DictNS
                 , dictDefs :: DictDefs 
                 } deriving (Eq, Ord)


-- | Parse the dictionary header. This assumes hashes are separated
-- by whitespace (LF or SP, usually LF) and the header consists just
-- of valid hashes. I'd rather be quite picky on what I accept for
-- now - no comments, etc. in the header. (Developers may always use
-- `@meta.comment` if they need to comment.)
parseHead :: LBS.ByteString -> Either ([Hash],LBS.ByteString) DictHead
parseHead = p [] . skipWS where
    p r s = if LBS.null s then Right (L.reverse r) else
            let (h, s') = LBS.span validHashByte s in
            if validHashLen /= LBS.length h then Left (r, s) else

            let (h, s') = LBS.splitAt validHashLen s in
            if not (LBS.all validHashByte h) then Left (r, s) else
            let r' = (LBS.toStrict h) : r in
            p r' (skipWS s')

-- utility
skipWS :: LBS.ByteString -> LBS.ByteString
skipWS = LBS.dropWhile isWS where
    isWS c = (10 == c) || (32 == c)

-- | Parse a 

-- | Parse


            

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


