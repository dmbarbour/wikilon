
-- | Wikilon needs to constrain texts just a little to simplify 
-- interactions with HTML. In particular, I'd prefer to avoid a
-- scenario where CRLF or surrogate conversions lose information.
--
-- Also, I'd like to simply forbid most control chars and the
-- replacement character.
--
module Wikilon.Dict.Text
    ( Text
    , isValidText
    , isValidTextChar
    -- , sanitizeText
    , listTextConstraintsForHumans
    ) where

import Data.Char (ord)
import Awelon.ABC (Text)
import qualified Data.List as L
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8

-- | test whether a text is valid by Wikilon's heuristics
isValidText :: Text -> Bool
isValidText = L.all isValidTextChar . LazyUTF8.toString

-- | test whether a character is allowed in the Wikilon dictionaries
isValidTextChar :: Char -> Bool
isValidTextChar c = not badChar where
    n = ord c
    badChar = isControl || isSurrogate || isReplacementChar
    isControl = isC0exceptLF || isC1orDel
    isC0exceptLF = (n <= 0x1F) && (n /= 10)
    isC1orDel = (0x7F <= n) && (n <= 0x9F)
    isSurrogate = (0xD800 <= n) && (n <= 0xDFFF)
    isReplacementChar = (n == 0xFFFD)

listTextConstraintsForHumans :: [String]
listTextConstraintsForHumans =
    ["rejects C0 or C1 control codes except LF"
    ,"rejects DEL (U+007F) and Replacement Char (U+FFFD)"
    ,"rejects surrogate codepoints (U+D800 - U+DFFF)"
    ,"weakly, text content should encode valid characters"
    ]

-- alternative idea:
-- 
-- sanitize texts if they are almost good, e.g. combine surrogate
-- pairs and translate CRLF or CR to just LF. But I'll address this
-- if the need arises. Pushing the task to the provider seems wiser
-- in general than attempting to guess what was intended.


