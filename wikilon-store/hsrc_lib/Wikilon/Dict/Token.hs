
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
module Wikilon.Dict.Token
    ( Token
    , isValidToken
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
-- import qualified Data.List as L

import Awelon.ABC (Token)
import Wikilon.Dict.Word

-- | Wikilon dictionaries accept three token types: words, discretionary
-- sealers and unsealers, and annotations. For now, these tokens have
-- several constraints on them:
--
--   Words may not contain C0, C1, SP, DEL, {}(|)[]", U+FFFD
--   Annotations, sealers, unsealers should also be valid as words
--   Discretionary sealers and unsealers mustn't contain `$`
--
-- The 'valid as words' constraint makes it easier to attach documentation
-- and rendering functions and such for the sealers or annotations through
-- naming conventions. Also, for URIs. Wikilon will simply reject dictionaries
-- that use invalid tokens.
--
isValidToken :: Token -> Bool
isValidToken t = case UTF8.uncons t of
    Just ('%', w) -> isValidWord (Word w)
    Just ('&', a) -> isValidAnno a
    Just (':', s) -> isValidSeal s
    Just ('.', u) -> isValidSeal u
    _ -> False

isValidSeal :: Token -> Bool
isValidSeal s = isValidWord (Word s) && BS.notElem 36 s

isValidAnno :: Token -> Bool
isValidAnno = isValidWord . Word
