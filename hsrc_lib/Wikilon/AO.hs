{-# LANGUAGE ViewPatterns, PatternGuards #-}
-- | The Awelon Object (AO) language is essentially a macro language
-- for Awelon Bytecode (ABC). AO will trivially expand and compile
-- into bytecode. AO makes ABC accessible to humans, without being
-- opaque - i.e. one can gain a good feel for the bytecode via use
-- of AO.
--
-- Wikilon uses parsers based on the `cereal` class. This is convenient
-- for efficient integration with serializers and acid-state. Wikilon's
-- parser for AO is slightly simplified from that used in the original
-- 'awelon' project, mostly favoring a less sophisticated number type.
-- useful for both 
-- Wikilon will use a simplified parser for AO. I'm not going to worry
-- about good parse errors, since (a) AO definitions are too small for
-- detailed location informaiton to matter, and (b) I'll likely have a
-- structured editor before long. Wikilon will be a bit more accepting,
-- allowing all inline ABC primitive operators except for SP and LF.
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
    ( Word, AO_Code(..), AO_Action(..), PrimOp(..)
    , aoWords, wordToText, textToWord
    , getAO, putAO
    ) where

import Control.Applicative
--import Control.Monad
import Control.Exception (assert)
import qualified Data.List as L
import qualified Data.Decimal as Dec
import qualified Data.Serialize as C
import qualified Codec.Binary.UTF8.Generic as UTF8
import Data.Ratio (numerator,denominator)
import Data.String
--import Data.Char (ord)

import Wikilon.ABC
import Wikilon.Word
import Wikilon.Char 

-- | AO_Code mostly exists for read, show, serialization instances.
newtype AO_Code = AO_Code { ao_code :: [AO_Action] } deriving (Eq, Ord)

-- | AO code is simply a (usually very short) list of actions.
-- Use of words refer to the larger dictionary. Tokens offer
-- access to annotations or side-effects. 
data AO_Action
    = AO_Word  {-# UNPACK #-} !Word
    | AO_Block [AO_Action]
    | AO_Num   {-# UNPACK #-} !Rational -- simple numbers
    | AO_Text  String   -- literal text (inline or multi-line)
    | AO_ABC   [PrimOp] -- inline ABC as a pseudo-word (grouping preserved)
    | AO_Tok   String   -- {token}
    deriving (Eq, Ord)

instance C.Serialize AO_Code where
    put = putAO . ao_code
    get = AO_Code <$> getAO True

instance Show AO_Action where
    showsPrec _ = showList . (:[])
    showList = shows . AO_Code

instance Show AO_Code where
    show = UTF8.toString . C.encodeLazy
    -- default showList

-- Read is mostly for convenience. It shouldn't be used directly
-- because the conversions between ByteString and String have a
-- lot of overhead.
instance Read AO_Code where
    readsPrec _ s = 
        let bytes = UTF8.fromString s in
        case C.runGetLazyState C.get bytes of
            Left _error -> []
            Right (code, brem) -> [(code, UTF8.toString brem)]
instance IsString AO_Code where fromString = read

-- | Serialize AO into binary. 
putAO :: [AO_Action] -> C.PutM ()
putAO (op:ops) = putAction op >> putSPAO ops
putAO [] = return ()

-- put a space then put the next action (if any).
--
-- Text always adds SP for inline text or LF for multiline text. To
-- avoid doubling number of spaces, text is treated as special case. 
putSPAO :: [AO_Action] -> C.PutM ()
putSPAO [] = return ()
putSPAO (AO_Text txt : ops) = putSPText txt >> putSPAO ops 
putSPAO ops = C.put ' ' >> putAO ops

putAction :: AO_Action -> C.Put
putAction (AO_Word w) = C.putByteString (wordToUTF8 w)
putAction (AO_Block ao) = C.put '[' >> putAO ao >> C.put ']'
putAction (AO_ABC abc) = C.put '%' >> mapM_ (C.put . abcOpToChar) abc
putAction (AO_Num n) = putNum n
putAction (AO_Text txt) = putSPText txt
putAction (AO_Tok tok) = putTok tok

putNum :: Rational -> C.PutM ()
putNum (toDecimal -> Just dec) = putShow dec
putNum r = putShow (numerator r) >> C.put '/' >> putShow (denominator r)

toDecimal :: Rational -> Maybe Dec.Decimal
toDecimal = either (const Nothing) Just . Dec.eitherFromRational

-- put string directly (no container/wrapper)
putShow :: (Show a) => a -> C.PutM ()
putShow = mapM_ C.put . show

-- conservatively add spaces or newlines before text literals
-- (this ensures a safe parse in a broader range of contexts).
putSPText :: String -> C.PutM ()
putSPText t | isInlineText t = C.put ' ' >> C.put '"' >> mapM_ C.put t >> C.put '"'
            | otherwise      = C.put '\n' >> C.put '"' >> putMLT t >> C.put '\n' >> C.put '~'

-- inline text if it's relatively short and contains only characters
-- that may be part of inline text (i.e. no LF or double quote).
isInlineText :: String -> Bool
isInlineText t = 
    let maxInlineTextLen = 64 in
    let (hd,tl) = L.splitAt maxInlineTextLen t in
    (null tl) && (L.all isInlineTextChar hd) 

putMLT :: String -> C.PutM ()
putMLT ('\n':cs) = C.put '\n' >> C.put ' ' >> putMLT cs -- escape LF
putMLT (c:cs) = C.put c >> putMLT cs
putMLT [] = return ()

putTok :: String -> C.PutM ()
putTok tok = assert (validTok tok) $ C.put '{' >> mapM_ C.put tok >> C.put '}'

validTok :: String -> Bool
validTok = L.all isTokenChar

getAO :: Bool -> C.Get [AO_Action]
getAO = error "undefined"




{-
ops@(AO_Text _ : _) = showChar ' ' . sa ops
    showList ops = sa ops 

-- sa assumes that the previous character is a space or other valid
-- separator, but not necessarily a newline. We're careful with spaces
-- mostly for aesthetic reasons.
sa :: [AO_Action] -> ShowS
sa [] = id -- all done
sa (AO_Word w : more) = shows w . sWithSP more
sa (AO_Block ops : more) = showChar '[' . sa ops . showChar ']' . sWithSP more
sa (AO_Text s : more) | inlineableTxt s = 
    showChar '"' . showString s . showChar '"' . sWithSP more
sa (AO_Text s : more) = 
    showChar '\n' . showChar '"' . showEscaped s . 
    showChar '\n' . showChar '~' . sWithSP more
sa (AO_Num r : more) = showNumber r . sWithSP more
sa (AO_ABC abcOps : ops) = showChar '%' . shows abcOps . sWithSP ops
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


-- show number as decimal if possible, otherwise as fraction
-- todo: consider supporting scientific notations
showNumber :: Rational -> ShowS
showNumber (toDecimal -> (Just dec)) = shows dec
showNumber r = shows (numerator r) . showChar '/' . shows (denominator r)

toDecimal :: Rational -> Maybe Dec.Decimal
toDecimal = either (const Nothing) Just . Dec.eitherFromRational
-}

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


{-
---------------------------
-- Reading or Parsing AO --
---------------------------


-- | Parse a string as AO.
--
--      readAO bNewLine inputString -> (code,unparsedInput)
--
-- The `bNewLine` value should be True if we're starting in a context
-- at the start of a new line, i.e. such that text at that point would
-- begin a multi-line text.
--
-- Other than multi-line text, AO is mostly easy to parse. The other 
-- big challenge is parsing numbers, but this simplified parser will
-- simply require decimals or rationals (no hexadecimal etc.)
-- 
readAO :: Bool -> [Char] -> ([AO_Action],[Char])
readAO = rdAO []

--       reversed      LF      input       forward   unparsed
rdAO :: [AO_Action] -> Bool -> [Char] -> ([AO_Action],[Char]) 
rdAO ops _ (' ' :s) = rdAO ops False s
rdAO ops _ ('\n':s) = rdAO ops True s
rdAO ops _     (rdNUM -> Just (num,s)) = rdAO (AO_Num   num : ops) False s
rdAO ops _     (rdWRD -> Just (wrd,s)) = rdAO (AO_Word  wrd : ops) False s 
rdAO ops _     (rdBLK -> Just (blk,s)) = rdAO (AO_Block blk : ops) False s
rdAO ops _     (rdABC -> Just (abc,s)) = rdAO (AO_ABC   abc : ops) False s
rdAO ops _     (rdTOK -> Just (tok,s)) = rdAO (AO_Tok   tok : ops) False s
rdAO ops True  (rdMLT -> Just (txt,s)) = rdAO (AO_Text  txt : ops) False s
rdAO ops False (rdILT -> Just (txt,s)) = rdAO (AO_Text  txt : ops) False s
rdAO ops _ unparsed = (L.reverse ops, unparsed)

rdNUM :: String -> Maybe (Rational, String)
rdWRD :: String -> Maybe (Word, String)
rdBLK :: String -> Maybe ([AO_Action], String)
rdABC :: String -> Maybe ([PrimOp], String)
rdTOK :: String -> Maybe (String, String)
rdMLT :: String -> Maybe (String, String)
rdILT :: String -> Maybe (String, String)

-- For reading words, we have just a few special cases:
--   a word starting with +,.,- may NOT follow that with a digit
--   
rdWRD (confusingWord -> True) = Nothing
rdWRD (c:cs) | isWordStart c = 
    let (cw,cs') = L.span isWordCont cs in
    let wrd = T.pack (c:cw) in
    let bValid = hasWordSep cs' in
    if bValid then Just (wrd, cs') else Nothing
rdWRD _ = Nothing

-- read `%vrwlc` and the like.
rdABC ('%':cs) =
    let (ops,cs') = gatherABC [] cs in
    let bValid = not (L.null ops) && hasWordSep cs' in
    if bValid then Just (ops, cs') else Nothing
rdABC _ = Nothing

gatherABC :: [PrimOp] -> [Char] -> ([PrimOp],[Char])
gatherABC ops s@(c:_) | isSpace c = (L.reverse ops, s)
gatherABC ops ((abcCharToOp -> Just op) : s) = gatherABC (op:ops) s
gatherABC ops s = (L.reverse ops, s)

-- read e.g. [foo %vrwlc bar 11.4 baz]
rdBLK ('[':s) = 
    let (blk, sBlockEnd) = readAO False s in
    case sBlockEnd of
        (']':s') -> Just (blk,s')
        _ -> Nothing
rdBLK _ = Nothing


rdNUM = error "TODO"
rdTOK = error "TODO"
rdMLT = error "TODO"
rdILT = error "TODO"

hasWordSep :: [Char] -> Bool
hasWordSep (c:_) = isWordSep c
hasWordSep [] = True


---------------------------------
 -- Character Classifications --
---------------------------------

-}