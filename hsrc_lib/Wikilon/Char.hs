-- just a few classifiers for characters
-- (I use this instead of Data.Char for stability and control of AO)
module Wikilon.Char
    ( isWordSep, isWordStart, isWordCont
    , isTokenChar, isInlineTextChar
    , isSpace, isControl, isDigit, isNZDigit
    , isPMD
    ) where

import qualified Data.List as L

-- | most word separators are spaces, but [] and (|) are also okay
-- because it's convenient to write code of form [foo[bar]].
--
-- The current AO parser doesn't actually support ambiguous (foo|bar)
-- code, but I reserve the relevant characters and preserve them as
-- separators. 
isWordSep :: Char -> Bool
isWordSep = flip L.elem " \n[](|)"

-- | characters restricted from use within words
-- to this list is added C0 and C1 chars, and DEL
wcBlacklist :: [Char]
wcBlacklist = " []{}\"(|)⦃⦄⦅⦆〚〛"

isWordStart, isWordCont :: Char -> Bool
isWordCont c = not (bl || ctl) where
    bl = c `L.elem` wcBlacklist
    ctl = isControl c
isWordStart c = isWordCont c && not (isDigit c || '%' == c || '@' == c)

-- plus, minus, dot need special handling for word start.
-- mostly, I forbid a digit after '+' or '.', and negative
-- numbers need a little special care. Also need '/' since
-- for example the symbol `/4` isn't very clear.
isPMD :: Char -> Bool
isPMD c = ('+' == c) || ('-' == c) || ('.' == c) || ('/' == c)

-- in token {foo} the token text 'foo' cannot
-- contain newlines or curly braces
isTokenChar :: Char -> Bool
isTokenChar c = not (lf || cb) where
    lf = ('\n' == c)
    cb = ('{' == c) || ('}' == c)

-- inline text may not contain '"' or '\n'
isInlineTextChar :: Char -> Bool
isInlineTextChar c = not (lf || qu) where
    lf = ('\n' == c)
    qu = ('"' == c)

isSpace, isControl, isDigit, isNZDigit :: Char -> Bool
isSpace c = (' ' == c) || ('\n' == c) -- the only spaces recognized by AO & ABC
isControl c = isC0 || isC1orDEL where
    n = fromEnum c
    isC0 = (n < 32)
    isC1orDEL = (127 <= n) && (n < 160)
isDigit c = ('0' <= c) && (c <= '9')
isNZDigit c = isDigit c && not ('0' == c)
