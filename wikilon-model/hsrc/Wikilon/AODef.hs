{-# LANGUAGE BangPatterns #-}

-- | ABCDef includes the basic type and some validation and processing
-- functions for raw bytecode defintions in context of an AO functions.
module Wikilon.AODef
    ( AODef
    , aodefTokens
    , aodefWords
    , isValidAODef
    ) where

import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Wikilon.Token
import Wikilon.Word
import Wikilon.Text
import Wikilon.ABC.Pure (abcTokens, isValidABC)

-- | Definitions in an AO dictionary should be valid Awelon Bytecode
-- (ABC), constrained to use a subset of tokens and texts to ensure
-- purity and portability. In particular, the tokens permitted are:
--
-- * {%word} - inline dictionary definition for identified word 
-- * {:seal} - discretionary seal for values; similar to newtype
-- * {.seal} - discretionary accessor (un-seal) for sealed values
-- * {&anno} - annotations for performance, type safety, structure
--
-- Tokens and texts are further constrained to be friendly in other
-- contexts, e.g. in HTML or URLs or documentation. 
--
-- Ideally, definitions should not be too large. While there are no
-- strict size limits, it seems wise to raise warnings for anything
-- larger than a few megabytes. Mostly, large definitions may raise 
-- issues for incremental loading, computation, and GC.
--
type AODef = BS.ByteString

-- | Obtain list of tokens (assuming isValidAODef)
aodefTokens :: AODef -> [Token]
aodefTokens = abcTokens . LBS.fromStrict

-- | Filter aodefTokens for {%word} dependencies.
aodefWords :: AODef -> [Word]
aodefWords = mapMaybe ff . aodefTokens where
    ff (Token s) = case BS.uncons s of
        Just ('%', w) -> Just (Word w)
        _ -> Nothing

-- | Validate an AODef without constructing a parse result. This test
-- validates tokens, texts, bytecodes, and balanced block structure.
isValidAODef :: AODef -> Bool
isValidAODef = isValidABC isvTok isvTxt . LBS.fromStrict where
    isvTok = isValidDictToken 
    isvTxt = isValidText

