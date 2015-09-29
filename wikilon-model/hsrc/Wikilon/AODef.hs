
-- | ABCDef includes the basic type and some validation and processing
-- functions for raw bytecode defintions in context of an AO functions.
module Wikilon.AODef
    ( AODef
    , aodefWords
    , aodefTokens
    ) where

import Data.Either (lefts, rights)
import Data.Maybe (mapMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Wikilon.RawABC
import Wikilon.Token
import Wikilon.Text

-- | Definitions in an AO dictionary should be valid Awelon Bytecode,
-- and further should be valid definitions. A definition requires a 
-- specific type of bytecode: ∀e.∃v.(e→([v→[a→b]]*(v*e)). This holds
-- a staged computation that generates a value v and a compiler for
-- [v→[a→b]], which is applied to generate a word's meaning [a→b].
-- Linking replaces each word token (e.g. {%foo}) with the meaning.
--
-- Wikilon will store definitions as raw bytecode represented in simple
-- bytestrings.
--
type AODef = LBS.ByteString
-- type AODef = RawABC

-- | Return a list of tokens without requiring a full parse. However,
-- this does assume that the bytecode is valid, e.g. that all texts
-- and tokens are properly terminated. This basically searches for
-- all {token} strings that are not within an embedded text.
aodefTokens :: AODef -> [Token]
aodefTokens s = case LBS.uncons s of 
        Just (34, s') -> aodefTokens (skipText s')
        Just (123, s') -> case LBS.elemIndex 125 s' of
            Just ix -> 
                let (sTok, more) = LBS.splitAt ix s' in
                let tok = Token (LBS.toStrict sTok) in
                tok : aodefTokens (LBS.drop 1 more)
            _ -> []
        Just (_, s') -> aodefTokens s'
        Nothing -> []

-- drop to end of line, repeat if end-of-line is escaped
skipText :: AODef -> AODef
skipText s = case LBS.elemIndex 10 s of
    Just ix -> -- found end of line
        let s' = LBS.drop (ix + 1) s in
        case LBS.uncons s' of
            Just (32, more) -> skipText more
            _ -> s' 
    _ -> LBS.empty

-- | Filter aodefTokens for just the words dependencies.
aodefWords :: AODef -> [Word]
aodefWords = mapMaybe ff . aodefTokens where
    ff (Token s) = case BS.uncons s of
        Just (37, w) -> Just (Word w)
        _ -> Nothing

-- | Assuming valid bytecode, return the set of available texts.
-- These texts will still contain the LFSP escape sequence. 
aodefTexts :: AODef -> [Text]
aodefTexts = rights . tt

-- | Assuming valid bytecode (e.g. that all tokens and texts are
-- properly terminated) return a simple listing of tokens and texts
-- used within the bytecode. The intention 
tt :: AODef -> [Either Token Text]
tt s = case LBS.uncons s of
    Just (c, s') -> case c of
        '{' -> let (tok,more) = takeTok s' in (Left tok : tt more)
        '"' -> let (txt,more) = takeTxt s' in (Right txt : tt more)
        _ -> tt s'
    



-- TODO:
--  validation or parsing of AODef
--  efficient extraction of tokens and texts

{-
-- | extract tokens and texts from an AODef. Returned texts will
-- contain any `LF SP` escape sequences at this point. 
tt :: AODef -> [Either Token Text]
tt s = case LBS.uncons s of
    Just (c, s') -> case c of
        '{' -> let (tok,more) = takeToken s' in (Left tok : tt more)
        '"' -> let (txt,more) = takeText s' in (Right txt : tt more)
        _ -> tt s'
    Nothing -> []

takeToken :: AODef -> (Token, AODef)
takeToken s = case LBS.elemIndex 

takeText :: AODef -> (Text, AODef)
takeText def = try 0 def where
    try n s = case LBS.elemIndex '\n' s of
        Nothing -> (def, mempty)

    Just ('"', s') -> 
        let (sText, sAfterText) = takeText s' in
       (Right sText : tt sAfterText)
    Just ('{', s') ->
        let (sToken, sAfterTok) = takeToken s' in
        (Left sToken : tt sAfterToken)
    Just (_, s') 
        '{' -> let (sTok, sAfterTok) = takeTok s' in
               (Left sTok, tt sAfterTok)
        _ -> tt s'
    


-- | List tokens found within AODef.
aodefTokens :: AODef -> [Token]
aodefTokens = lefts . tt

-- | List texts found within AODef
aodefTexts :: AODef -> [Text]
aodefTexts = rights . tt

-- | List word dependencies of an AODef.
aodefWords :: AODef -> [Word]
aodefWords = mapMaybe f . aodefTokens
    f (Token s) = case BS.uncons s of
        Just ('%', w) -> Just (Word w)
        _ -> Nothing

-}
