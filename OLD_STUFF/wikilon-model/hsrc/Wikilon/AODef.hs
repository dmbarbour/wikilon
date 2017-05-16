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
    , aodefRedirect

    , aodefToABC, ABC
    , aodefToClaw, ClawCode
    ) where

import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Char8 as BS
import Wikilon.Token
import Wikilon.Word
import Wikilon.Text
import Wikilon.ABC.Pure (ABC(..), abcTokens, isValidABC, Op(..), PrimOp(..))
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
-- larger than a few megabytes. Very large definitions raise issues 
-- for incremental loading, computation, and GC.
--
type AODef = Text

-- | Obtain list of tokens (assuming isValidAODef)
aodefTokens :: AODef -> [Token]
aodefTokens = abcTokens 

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
    rnAODef = ABC.encode . rnABC . aodefToABC
    rnABC = ABC . fmap rnOp . abcOps
    rnOp (ABC_Tok tok) | (tok == tokOrigin) = ABC_Tok tokTarget
    rnOp (ABC_Block abc) = ABC_Block (rnABC abc)
    rnOp op = op

-- | Validate an AODef without constructing a parse result. This test
-- validates tokens, texts, bytecodes, and block structure. Types are
-- not computed.
isValidAODef :: AODef -> Bool
isValidAODef = isValidABC aoTok aoTxt where
    aoTok = isValidDictToken 
    aoTxt = isValidText

-- | Assuming a valid AODef, convert it to bytecode.
aodefToABC :: AODef -> ABC
aodefToABC = fin . ABC.decode where
    fin (Left _stuck) = error $ aodefErr $ "aodefToABC received invalid AODef"
    fin (Right abc) = abc

-- | token associated with a word
wordToToken :: Word -> Token
wordToToken = Token . BS.cons '%' . unWord

-- | Modulo whitespace and attributes, a definition `{%foo}` is a
-- redirect to word `foo`. This function filters whitespace and
-- attributes to return a possible redirect target.
aodefRedirect :: AODef -> Maybe Word
aodefRedirect = cmp . cln . abc where
    abc = ABC.abcOps . aodefToABC
    abcWS ABC_SP = True
    abcWS ABC_LF = True
    abcWS _ = False
    cln (ABC_Block _ : ABC_Prim ABC_drop : ops) = cln ops -- filter attributes
    cln (ABC_Prim op : ops) | abcWS op = cln ops -- filter whitespace
    cln (op : ops) = op : cln ops
    cln [] = []
    cmp [ABC_Tok (Token t)] = case BS.uncons t of
        Just ('%', w) -> Just (Word w)
        _ -> Nothing
    cmp _ = Nothing

-- | Assuming a valid AODef, convert it to a Claw view.
aodefToClaw :: AODef -> ClawCode
aodefToClaw = Claw.clawFromABC . aodefToABC

aodefErr :: String -> String
aodefErr = (++) "Wikilon.AODef: "

