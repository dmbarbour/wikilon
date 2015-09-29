
-- | Awelon Bytecode allows embedded UTF-8 texts. Wikilon allows a 
-- subset of these texts, constraining use of control characters
-- and problematic codepoints to simplify interaction with HTML 
-- (e.g. automatic conversion of line endings to CRLF).
--
-- This module contains basic utilities for validating and processing
-- texts.
--
-- NOTE: Wikilon shall generally preserve the `LF SP` escape sequences
-- when working with texts internally, mostly because it's easier than
-- repeatedly injecting and removing the escapes.
module Wikilon.Text
    ( Text
    , isValidText
    , isValidTextChar
    -- , sanitizeText
    , listTextConstraintsForHumans
    , textLines
    ) where

import Data.Char (ord)
import qualified Data.List as L
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.Lazy as LBS

type Text = LazyUTF8.ByteString

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

-- | A lossless 'lines' function. The original structure may be
-- recovered by simply adding a '\n' before each segment in the
-- list result, then concatenating.
textLines :: Text -> (Text, [Text])
textLines txt =
    case LBS.elemIndex 10 txt of
        Nothing -> (txt, [])
        Just idx ->
            let (ln1,txt') = LBS.splitAt idx txt in
            let p = textLines (LBS.drop 1 txt') in
            (ln1, (fst p : snd p))

-- alternative idea:
-- 
-- sanitize texts if they are almost good, e.g. combine surrogate
-- pairs and translate CRLF or CR to just LF. But I'll address this
-- if the need arises. Pushing the task to the provider seems wiser
-- in general than attempting to guess what was intended.

