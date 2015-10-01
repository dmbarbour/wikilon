
-- | ABCDef includes the basic type and some validation and processing
-- functions for raw bytecode defintions in context of an AO functions.
module Wikilon.AODef
    ( AODef
    , aodefTokens
    , aodefWords
    ) where

import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Char8 as BS
import Wikilon.Token
import Wikilon.Word

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
-- strict size limits, very big definitions (e.g. greater than one
-- megabyte) should probably raise warnings.
--
-- In a Wikilon dictionary, definitions are recorded as bytestrings.
type AODef = BS.ByteString

-- | Assuming valid bytecode, return the list of tokens from an AO
-- definition without requiring a full parse. Effectively, this 
-- returns all {token} substrings that are not within an embedded
-- text.
aodefTokens :: AODef -> [Token]
aodefTokens s = case BS.uncons s of 
        Just ('"', s') -> aodefTokens (skipText s')
        Just ('{', s') -> case BS.elemIndex '}' s' of
            Just ix -> 
                let (tok, more) = BS.splitAt ix s' in
                (Token tok) : aodefTokens (BS.drop 1 more)
            _ -> [Token s']
        Just (_, s') -> aodefTokens s'
        Nothing -> []

-- drop to end of line, repeat while end-of-line is escaped
skipText :: AODef -> AODef
skipText s = case BS.elemIndex '\n' s of
    Just ix -> 
        let s' = BS.drop (ix + 1) s in
        case BS.uncons s' of
            Just (' ', more) -> skipText more
            _ -> s' -- ideally, should start with `~`.
    _ -> BS.empty

-- | Filter aodefTokens for word dependencies.
aodefWords :: AODef -> [Word]
aodefWords = mapMaybe ff . aodefTokens where
    ff (Token s) = case BS.uncons s of
        Just ('%', w) -> Just (Word w)
        _ -> Nothing
