{-# LANGUAGE BangPatterns #-}

-- | ABCDef includes the basic type and some validation and processing
-- functions for raw bytecode defintions in context of an AO functions.
module Wikilon.AODef
    ( AODef
    , aodefTokens
    , aodefWords
    , renameInAODef
    , wordToToken
    , isValidAODef

    , aodefToABC, ABC
    , aodefToClaw, ClawCode
    ) where

import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Wikilon.Token
import Wikilon.Word
import Wikilon.Text
import Wikilon.ABC.Pure (ABC(..), abcTokens, isValidABC, Op(..))
import qualified Wikilon.ABC.Pure as ABC
import Wikilon.Claw (ClawCode)
import qualified Wikilon.Claw as Claw

-- | Definitions in an AO dictionary should be valid Awelon Bytecode
-- (ABC), constrained to use a subset of tokens and texts to ensure
-- purity and portability. In particular, the tokens permitted are:
--
-- * {%word} - inline dictionary definition for identified word 
-- * {:seal} - discretionary seal for values; similar to newtype
-- * {.seal} - discretionary accessor (unseal) for sealed values
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

-- | Rename a word (from origin to target) within an AODef.
renameInAODef :: Word -> Word -> AODef -> AODef
renameInAODef origin target = rnAODef where
    tokOrigin = wordToToken origin
    tokTarget = wordToToken target
    rnAODef = LBS.toStrict . ABC.encode . rnABC . aodefToABC
    rnABC = ABC . fmap rnOp . abcOps
    rnOp (ABC_Tok tok) | (tok == tokOrigin) = ABC_Tok tokTarget
    rnOp (ABC_Block abc) = ABC_Block (rnABC abc)
    rnOp op = op

-- | Validate an AODef without constructing a parse result. This test
-- validates tokens, texts, bytecodes, and block structure. Types are
-- not computed.
isValidAODef :: AODef -> Bool
isValidAODef = isValidABC aoTok aoTxt . LBS.fromStrict where
    aoTok = isValidDictToken 
    aoTxt = isValidText

-- | Assuming a valid AODef, convert it to bytecode.
aodefToABC :: AODef -> ABC
aodefToABC = fin . ABC.decode . LBS.fromStrict where
    fin (Left _stuck) = error $ aodefErr $ "aodefToABC received invalid AODef"
    fin (Right abc) = abc

-- | token associated with a word
wordToToken :: Word -> Token
wordToToken = Token . BS.cons '%' . unWord

-- | Assuming a valid AODef, convert it to a Claw view.
aodefToClaw :: AODef -> ClawCode
aodefToClaw = Claw.clawFromABC . aodefToABC

aodefErr :: String -> String
aodefErr = (++) "Wikilon.AODef: "

