
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

import qualified Data.ByteString.Char8 as BS
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
--   $ - wraps a cryptographically sealed value, e.g. {$aes:fingerprint}
--   # - resource linking at a global scale, using secure hashes
--   % - linking at a local scale within an implicit AO dictionary
newtype Token = Token BS.ByteString

-- | Wikilon accepts only a subset of tokens: annotations, discretionary
-- sealers or unsealers, and local linking within the dictionary. Further,
-- annotations, sealers, and unsealers are all constrained to be valid as
-- words, i.e. to have the same HTTP-friendly properties.
isValidDictToken :: Token -> Bool
isValidDictToken (Token t) = case BS.uncons t of
    Just ('%', w) -> isValidWord (Word w)
    Just ('&', a) -> isValidAnno a
    Just (':', s) -> isValidSeal s
    Just ('.', u) -> isValidSeal u
    _ -> False

isValidSeal :: BS.ByteString -> Bool
isValidSeal s = isValidWord (Word s) && BS.notElem '$' s

isValidAnno :: BS.ByteString -> Bool
isValidAnno a = isValidWord (Word a)

