{-# LANGUAGE BangPatterns #-}

-- | ABCDef includes the basic type and some validation and processing
-- functions for raw bytecode defintions in context of an AO functions.
module Wikilon.AODef
    ( AODef
    , aodefTokens
    , aodefWords
    , isValidAODef
    ) where

import Data.Maybe (mapMaybe, isJust)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Wikilon.Token
import Wikilon.Word
import Wikilon.Text
import Wikilon.ABC.Pure (abcCharToOp)

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
        Just (c, s') -> case c of
            '{' -> case BS.elemIndex '}' s' of
                Just ix -> 
                    let tok = Token (BS.take ix s') in
                    let afterTok = BS.drop (ix + 1) s' in
                    tok : aodefTokens afterTok
                Nothing -> [Token s'] -- invalid ABC
            '"' -> aodefTokens $ dropText s'
            _   -> aodefTokens s'
        Nothing -> []

-- drop text, result should start with '~' in valid bytecode
dropText :: AODef -> AODef
dropText = snd . takeText

-- drop to end of line, repeat while end-of-line is escaped
--
-- Note: does not remove escapes from text, and in valid 
-- ABC the next character in AODef should be '~'.
takeText :: AODef -> (Text,AODef)
takeText def = t 0 def where
    t n s = case BS.elemIndex '\n' s of
        Just ix -> 
            let s' = BS.drop (ix + 1) s in
            case BS.uncons s' of
                Just (' ', more) -> t (n + ix + 1) more
                _ -> let txt = LBS.fromStrict (BS.take (n + ix) def) in
                     (txt, s')
        Nothing -> (LBS.fromStrict def, BS.empty) -- (invalid ABC)

-- | Filter aodefTokens for word dependencies.
aodefWords :: AODef -> [Word]
aodefWords = mapMaybe ff . aodefTokens where
    ff (Token s) = case BS.uncons s of
        Just ('%', w) -> Just (Word w)
        _ -> Nothing

-- | Validate an AODef without constructing a parse result. This test
-- validates tokens, texts, bytecodes, and balanced block structure.
isValidAODef :: AODef -> Bool
isValidAODef = v (0 :: Int) where
    v !b !s = case BS.uncons s of
        Nothing -> (b == 0) -- neutral block count
        Just (c, afterChar) -> case c of
            '[' -> v (b + 1) afterChar
            ']' -> (b > 0) && (v (b - 1) afterChar)
            '{' -> case BS.elemIndex '}' afterChar of
                Nothing -> False
                Just ix -> 
                    let tok = Token (BS.take ix afterChar) in
                    let afterTok = BS.drop (ix + 1) afterChar in
                    (isValidDictToken tok) && (v b afterTok)
            '"' -> let (txt, txtEnd) = takeText afterChar in
                   case BS.uncons txtEnd of
                        Just ('~', afterTxt) -> (isValidText txt) && (v b afterTxt) 
                        _ -> False
            _ -> (isValidABC c) && (v b afterChar)

-- test for valid ABC character... (todo: move to an ABC module)
isValidABC :: Char -> Bool
isValidABC = isJust . abcCharToOp
