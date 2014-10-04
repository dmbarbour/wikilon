{-# LANGUAGE ViewPatterns, PatternGuards #-}
-- | The Awelon Object (AO) language is essentially a macro language
-- for Awelon Bytecode (ABC). AO will trivially expand and compile
-- into bytecode. AO makes ABC accessible to humans, without being
-- opaque - i.e. one can gain a good feel for the bytecode via use
-- of AO.
--
-- Wikilon will use a simplified parser for AO. I'm not going to worry
-- about good parse errors, since (a) AO definitions are too small for
-- detailed location informaiton to matter, and (b) I'll likely have a
-- structured editor before long. 
--
-- I shall still forbid words to start with `@` for escapes, control
-- control, export; or `%` for inline ABC. I'll reserve the unicode 
-- white brackets, braces, parens. 
--
-- At the moment, I haven't decided quite how I want to implement
-- embedded literal objects. I still like the idea, but it might be
-- better to treat them as special objects or words on the wiki.
--
-- I still plan to eventually support Ambiguous AO, where we can
-- define a whole search-space of programs very quickly by use of
-- phrases like: `(0|1|2)(foo|bar)(baz|qux)`. Likely, this will be
-- a special IDE-supported search rather than a runtime search.
--
-- Wikilon will still limit which capabilities may be embedded in
-- source code to just sealers and annotations. Anything else
-- must be provided via powerblock (or whatever).
-- 
module Wikilon.AO
    ( Word, AO_Code, AO_Action(..), AOp(..)
    ) where

import Control.Exception (assert)
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Array.Unboxed as A
import qualified Data.Decimal as Dec
import Data.Ratio (numerator,denominator)
import Data.Word (Word16)

type Word = Text

-- | AO code is simply a (usually very short) list of actions
type AO_Code = [AO_Action]

-- | basic AO actions
data AO_Action
    = AO_Word  Word
    | AO_Block [AO_Action]
    | AO_Num   Rational -- simple numbers
    | AO_Text  String   -- literal text (inline or multi-line)
    | AO_ABC   [AOp]    -- inline ABC (e.g. %vrwlc; `%` prefix for ABC)
    | AO_Tok   String   -- {token}
    deriving (Eq, Ord)

-- | AOp encodes just the subset of ABC that AO may inline.
data AOp
    = AOp_l | AOp_r | AOp_w | AOp_z | AOp_v | AOp_c -- basic data plumbing
    | AOp_L | AOp_R | AOp_W | AOp_Z | AOp_V | AOp_C -- sum-type data plumbing
    | AOp_copy | AOp_drop -- '^' and '%'
    | AOp_add | AOp_neg | AOp_mul | AOp_inv | AOp_divMod -- basic math
    | AOp_ap | AOp_cond | AOp_quote | AOp_comp -- higher order programming
    | AOp_rel | AOp_aff -- substructural types
    | AOp_distrib | AOp_factor | AOp_merge | AOp_assert -- working with sums
    | AOp_gt -- value observations
    deriving (Eq, Ord, Enum, A.Ix, Bounded)

-- | table of associations between inline ABC and characters.
aopCharList :: [(AOp, Char)]
aopCharList =
    [(AOp_l,'l'),(AOp_r,'r'),(AOp_w,'w'),(AOp_z,'z'),(AOp_v,'v'),(AOp_c,'c')
    ,(AOp_L,'L'),(AOp_R,'R'),(AOp_W,'W'),(AOp_Z,'Z'),(AOp_V,'V'),(AOp_C,'C')
    ,(AOp_copy,'^'),(AOp_drop,'%')
    ,(AOp_add,'+'),(AOp_neg,'-'),(AOp_mul,'*'),(AOp_inv,'/'),(AOp_divMod,'Q')
    ,(AOp_ap,'$'),(AOp_cond,'?'),(AOp_quote,'\''),(AOp_comp,'o')
    ,(AOp_rel,'k'),(AOp_aff,'f')
    ,(AOp_distrib,'D'),(AOp_factor,'F'),(AOp_merge,'M'),(AOp_assert,'K')
    ,(AOp_gt,'>')
    ]

aoOpCharArray :: A.UArray AOp Char
aoOpCharArray = A.accumArray skip maxBound (minBound,maxBound) aopCharList where

skip :: a -> b -> b
skip = flip const

aoCharAOpArray :: A.UArray Char Word16
aoCharAOpArray = A.accumArray skip maxBound (lb,ub) lst where
    lst = fmap swix aopCharList
    swix (op,c) = (c, fromIntegral (fromEnum op))
    lb = L.minimum $ fmap snd aopCharList
    ub = L.maximum $ fmap snd aopCharList

aoOpToChar :: AOp -> Char
aoOpToChar op = assert (maxBound /= c) c where
    c = aoOpCharArray A.! op

aoCharToAOp :: Char -> Maybe AOp
aoCharToAOp c | okay = Just $! toEnum (fromIntegral w)
              | otherwise = Nothing
    where okay = inBounds && (maxBound /= w)
          inBounds = ((lb <= c) && (c <= ub))
          (lb,ub) = A.bounds aoCharAOpArray
          w = aoCharAOpArray A.! c

-------------------------------------
-- Showing or Serializing AO code  --
-------------------------------------

-- Show has a special case for AO_Text to prevent inline text
-- from starting just after a newline. We'll just add a space
-- then run with it.
instance Show AO_Action where
    showsPrec _ op = showList [op]
    showList ops@(AO_Text _ : _) = showChar ' ' . sa ops
    showList ops = sa ops 

instance Show AOp where
    showsPrec _ = showChar . aoOpToChar 
    showList (op:ops) = shows op . showList ops
    showList [] = id

-- sa assumes that the previous character is a space or other valid
-- separator, but not necessarily a newline. We're careful with spaces
-- mostly for aesthetic reasons.
sa :: [AO_Action] -> ShowS
sa [] = id -- all done
sa (AO_Word w : more) = showString (T.unpack w) . sWithSP more
sa (AO_Block ops : more) = showChar '[' . sa ops . showChar ']' . sWithSP more
sa (AO_Text s : more) | inlineableTxt s = 
    showChar '"' . showString s . showChar '"' . sWithSP more
sa (AO_Text s : more) = 
    showChar '\n' . showChar '"' . showEscaped s . 
    showChar '\n' . showChar '~' . sWithSP more
sa (AO_Num r : more) = showNumber r . sWithSP more
sa (AO_ABC aoOps : ops) = showChar '%' . showList aoOps . sWithSP ops
sa (AO_Tok s : more) = showChar '{' . showString s . showChar '}' . sWithSP more

-- add a space before showing the next action
sWithSP :: [AO_Action] -> ShowS
sWithSP [] = id -- no need to add space (end of block or code)
sWithSP ops = showChar ' ' . sa ops

-- escape a string for AO's multi-line text. Unlike most PLs,
-- only newlines need be escaped in AO's texts. 
showEscaped :: String -> ShowS
showEscaped ('\n':s) = showChar '\n' . showChar ' ' . showEscaped s
showEscaped (c:s) = showChar c . showEscaped s
showEscaped [] = id

inlineableTxt :: String -> Bool
inlineableTxt = L.all isInlineTextChar

-- show number as decimal if possible, otherwise as fraction
-- todo: consider supporting scientific notations
showNumber :: Rational -> ShowS
showNumber (toDecimal -> (Just dec)) = shows dec
showNumber r = shows (numerator r) . showChar '/' . shows (denominator r)

toDecimal :: Rational -> Maybe Dec.Decimal
toDecimal = either (const Nothing) Just . Dec.eitherFromRational

-- | Extract the words used by AO code. For example:
--     foo "hello" [42 bar baz] %vrwlc bar → [foo,bar,baz,bar]
-- Duplicates are still part of the list at this point.
aoWords :: [AO_Action] -> [Word]
aoWords = flip lw [] where
    lw (x:xs) = ew x . lw xs
    lw [] = id
    ew (AO_Word w) = (w:)
    ew (AO_Block ops) = lw ops
    ew _ = id

---------------------------
-- Reading or Parsing AO --
---------------------------
--
-- Thoughts:
--  Need a little extra state to 
--  Mostly, this is related to the use of multi-line text.
--
-- Potentially, I could read multi-line text in a more accepting manner, 
-- i.e. 

-- Thoughts on parsing AO:
-- 
--  This doesn't really fit the ReadPrec parser combinators model.
--   
--
-- An ad-hoc, linear parser should work well and be very fast.
-- (Though speed shouldn't be an issue in 99% of use cases).
--
-- Most attention must be paid to:
--
-- * whether I'm starting a new line (for multi-line text)
-- * 
--  


{-
---------------------------------
 -- Character Classifications --
---------------------------------

isWordSep :: Char -> Bool
isWordSep = flip L.elem " \n[](|)"

isWordStart, isWordCont :: Char -> Bool
isWordCont c = not (bl || ctl) where
    bl  = L.elem c " []{}\"(|)⦃⦄⦅⦆〚〛"
    ctl = isControl c
isWordStart c = not (d || bl) && isWordCont c where
    bl = ('%' == c) || ('@' == c)
    d = isDigit c

-- a token {foo} the token text 'foo' 
-- may not contain newline characters or curly braces. 
isTokenChar :: Char -> Bool
isTokenChar c = not (lf || cb) where
    lf = ('\n' == c)
    cb = ('{' == c) || ('}' == c)
-}
-- inline text may not contain '"' or '\n'
isInlineTextChar :: Char -> Bool
isInlineTextChar c = not (lf || qu) where
    lf = ('\n' == c)
    qu = ('"' == c)
{-
isSpace, isControl, isDigit, isNZDigit, isHexDigit :: Char -> Bool
isSpace c = (' ' == c) || ('\n' == c) -- spaces recognized by Awelon project
isControl c = isC0 || isC1orDEL where
    n = fromEnum c
    isC0 = n <= 0x1F
    isC1orDEL = n >= 0x7F && n <= 0x9F
isDigit c = ('0' <= c) && (c <= '9')
isNZDigit c = isDigit c && not ('0' == c)
isHexDigit c = isDigit c || smallAF || bigAF where
    smallAF = ('a' <= c) && (c <= 'f')
    bigAF = ('A' <= c) && (c <= 'F')

-}