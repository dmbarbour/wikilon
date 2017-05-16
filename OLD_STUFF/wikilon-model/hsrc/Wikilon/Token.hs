{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Awelon Bytecode (ABC) uses {tokens} as ad-hoc extensions to the
-- bytecode, supporting annotations, type safety, security, linking,
-- and potentially side-effects. Though, Wikilon doesn't use tokens
-- for side-effects, instead modeling effects directly with abstract
-- virtual machines and a network model.
--
-- Within a dictionary, tokens are constrained to resist entanglement
-- and ensure portability and reusability of dictionary code. Thus,
-- only a few tokens are permitted:
--
--  * {%word} link functional behavior from another word
--  * {&anno} annotate a value for performance or safety
--  * {:seal} {.seal} discretionary sealers or unsealers 
--
-- This module validates tokens for use in a dictionary, to ensure
-- they have nice properties for linking, listing, documenting, etc.
--
module Wikilon.Token
    ( Token(..)
    , isValidDictToken
    ) where

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Char8 as BS
import Data.String (IsString(..))
import Wikilon.Word

-- | A token has the form {token} in ABC, and represents an escape
-- to an environment-layer feature or procedure. Potentially, this
-- could be used as a basic FFI. However, Awelon project tends to
-- use tokens in a more limited manner: annotations for performance,
-- a sealed values concept for cryptography and type safety, and
-- linking of code (allowing for separate compilation, etc.). 
--
-- Tokens are generally distinguished based on a prefix character.
--
--   & - annotations, identity semantics but impact performance,
--       safety analyses, quotas for computation, debugging, etc.
--   : - value sealers, discretionary unless contains '$' in which
--       case sealer is cryptographic (e.g. {:aes$key}). Type is
--       (v*e)→((sealed v) * e). 
--   . - value unsealer corresponding to a sealer
--   % - linking at a local scale within an implicit AO dictionary
--   # - resource linking at a global scale, using secure hashes
--   $ - might be used for cryptographic value sealing
newtype Token = Token UTF8.ByteString
    deriving (Eq, Ord)

instance IsString Token where 
    fromString = Token . UTF8.fromString
instance Show Token where 
    showsPrec _ (Token s) = 
        showChar '{' . showString (UTF8.toString s) . showChar '}'

-- | Wikilon accepts only a subset of tokens within a dictionary: 
-- annotations, discretionary sealers or unsealers, and word links.
-- Further, these must be valid as words (modulo prefix).
isValidDictToken :: Token -> Bool
isValidDictToken (Token tok) = case BS.uncons tok of
    Just (c, s) -> case c of
        '%' -> isValidWord (Word s)
        '&' -> isValidAnno s
        ':' -> isValidSeal s
        '.' -> isValidSeal s
        _ -> False
    Nothing -> False

isValidSeal :: UTF8.ByteString -> Bool
isValidSeal = isValidWord . Word 

isValidAnno :: UTF8.ByteString -> Bool
isValidAnno = isValidWord . Word

