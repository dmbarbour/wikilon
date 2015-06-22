{-# LANGUAGE OverloadedStrings, ViewPatterns, PatternGuards, BangPatterns #-}
-- | Command Language (or Command Line) for Awelon, 'claw'
--
-- Claw is an editable view of Awelon Bytecode (ABC), essentially a
-- syntactic sugar with a reversible, context-free expansion process.
-- Claw is very simple, and its main purpose is to amplify the user's
-- abilility to write REPL or shell commands via keyboard. 
--
-- Example codes and expansions:
--
--      2\/3        2 3 ratio
--      4\/10       4 10 ratio
--      3.141       3141 3 decimal
--      -1.20       -120 2 decimal
--      6.02e23     6.02 23 exp10
--      42          \\#42 integer
--      -7          \\#7- integer
--      "foo"       \\"foo
--                  ~ literal
--      [foo]       \\[foo] block
--
-- Currently the focus is on numbers and inline texts, sufficient for
-- a Forth-like experience. However, claw code is very extensible. An
-- extension might enable vectors or association lists and so on. Or
-- even graphs and diagrams for a structure editor. This code for Claw
-- doesn't focus on support for extensibility, but I would like to try
-- modeling claw within an AO dictionary.
--
-- Words are the exception to Claw's context free encodings. A word is
-- context sensitive to the current namespace
--
--      #NS         \{&ns:NS}      (set current namespace to NS)
--      integer     \{%NSinteger}  (context dependent on namespace)
--
-- At the bottom level are just a few primary escape forms:
--
--      \\vrwlc
--      \\{token}
--      \\[mul]
--      \\"escaped text 
--       possibly having
--       multiple lines
--      ~
--
-- These expand directly into bytecode, though the escaped blocks
-- must be expanded recursively for the inner content. Escaped forms
-- are generally not 
--
-- The reverse path, from bytecode back into claw code, is equally
-- important. Claw code is stored in the expanded form, which is 
-- easy to process and preserves meaning regardless of which 
-- extensions a given editor understands.
-- 
module Awelon.ClawCode
    ( ClawCode(..)
    , ClawOp(..)
    , Namespace
    , ClawInt
    , ClawRatio
    , ClawDecimal
    , ClawExp10
    , clawToABC
    , clawFromABC
    , isInlineableText

{-
    , encode
    , encode'
    , encodeNum
    , decode, decoder
    , DecoderStuck(..), DecoderCont(..)
-}
    -- , fromABC
    -- , toABC
    , module Awelon.Word
    ) where

import Control.Monad
import Control.Arrow (first)
import Data.Monoid
import Data.Char
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.Builder as BB
import qualified Data.List as L
import qualified Data.Decimal as Decimal
import Data.String (IsString(..))
import Awelon.ABC 
import qualified Awelon.ABC as ABC
import Awelon.Word

-- | Command Language for Awelon (claw)
data ClawCode = ClawCode 
    { clawNS :: !Namespace
    , clawOps :: [ClawOp]
    } deriving (Eq, Ord)

data ClawOp 
    = NS !Namespace     -- #bar:
    | CW !Word          -- mul inc
    | P0 !ABC.PrimOp    -- escaped ABC ops
    | T0 !ABC.Text      -- escaped texts
    | K0 !ABC.Token     -- escaped token
    | B0 ![ClawOp]      -- escaped blocks 
    | NI !ClawInt       -- integer
    | NR !ClawRatio     -- ratio
    | ND !ClawDecimal   -- decimal
    | NE !ClawExp10     -- exponential
    | TL !ABC.Text      -- text literal
    | BC ![ClawOp]      -- block of code
    deriving (Ord, Eq)
    
type ClawInt = Integer
data ClawRatio = ClawRatio !ClawInt !ClawInt deriving (Eq, Ord)
data ClawDecimal = ClawDecimal !ClawInt !ClawInt deriving (Eq, Ord)
data ClawExp10 = ClawExp10 !ClawDecimal !ClawInt deriving (Eq, Ord)

-- | A region of claw code has exactly one namespace. This serves as
-- a prefix for all words within that region of code. Claw namespace
-- is the only source of context sensitivity in claw code.
type Namespace = UTF8.ByteString

wInteger, wLiteral, wBlock :: Word
wRatio, wDecimal, wExp10 :: Word

wInteger = "integer"
wLiteral = "literal"
wBlock = "block"
wRatio = "ratio"
wDecimal = "decimal"
wExp10 = "exp10"

nsTokPrefix :: UTF8.ByteString
nsTokPrefix = "&ns:"

-- | Translate claw code into ABC code
clawToABC :: ClawCode -> ABC
clawToABC cc = mkABC $ opsToABC (clawNS cc) (clawOps cc)

-- | convert Claw operations into Awelon bytecode;
-- requires current namespace to convert words
opsToABC :: Namespace -> [ClawOp] -> [ABC.Op]
opsToABC _ [] = []
opsToABC _ (NS ns : ops) = ABC_Tok nstok : opsToABC ns ops where 
    nstok = nsTokPrefix <> ns
opsToABC ns (CW (Word w) : ops) = ABC_Tok wtok : opsToABC ns ops where
    wtok = mconcat ["%", ns, w]
opsToABC ns (P0 prim : ops) = (ABC_Prim prim : opsToABC ns ops)
opsToABC ns (T0 txt : ops) = (ABC_Text txt : opsToABC ns ops)
opsToABC ns (K0 tok : ops) = (ABC_Tok tok : opsToABC ns ops)
opsToABC ns (B0 cc : ops) = (ABC_Block abc : opsToABC ns ops) where
    abc = mkABC $ opsToABC ns cc
opsToABC ns (NI i : ops) = ABC.quote i ++ opsToABC ns (CW wInteger : ops)
opsToABC ns (NR (ClawRatio num den) : ops) = 
    opsToABC ns (NI num : NI den : CW wRatio : ops)
opsToABC ns (ND (ClawDecimal body places) : ops) = 
    opsToABC ns (NI body : NI places : CW wDecimal : ops)
opsToABC ns (NE (ClawExp10 dec exp10) : ops) = 
    opsToABC ns (ND dec : NI exp10 : CW wExp10 : ops)
opsToABC ns (TL lit : ops) = opsToABC ns (T0 lit : CW wLiteral : ops)
opsToABC ns (BC cc : ops) = opsToABC ns (B0 cc : CW wBlock : ops)

-- | parse Claw code from bytecode, given the initial namespace.
clawFromABC :: Namespace -> ABC -> ClawCode
clawFromABC ns = ClawCode ns . reduceOps . escABC ns . ABC.abcOps

-- | recognize claw words from bytecode 
-- {%foo:word} → word 
--
-- if namespace is `foo:` 
--   and `word` doesn't start with digits, etc.
escWord :: Namespace -> ABC.Token -> Maybe Word
escWord ns tok = case BS.uncons tok of
    Just ('%', fullWord) ->
        let bOkPrefix = ns `BS.isPrefixOf` fullWord in
        let w = Word $ BS.drop (BS.length ns) fullWord in
        let bOK = bOkPrefix && isValidWord w in
        if bOK then Just w else Nothing
    _ -> Nothing

-- | recognize claw namespace tokens {&ns:NS} → #NS
escNSTok :: ABC.Token -> Maybe Namespace
escNSTok tok =
    let bNS = nsTokPrefix `BS.isPrefixOf` tok in
    let ns = BS.drop 4 tok in
    let bOK = bNS && isValidWord (Word ns) in
    if bOK then Just ns else Nothing

-- | recognize basic claw operations and handle the namespace contexts.
-- Also, removes any SP and LF identity operators, whose main role is
-- ABC-layer formatting. 
escABC :: Namespace -> [ABC.Op] -> [ClawOp]
escABC ns (ABC_Tok tok : ops) | Just w <- escWord ns tok = CW w : escABC ns ops
escABC _ (ABC_Tok tok : ops) | Just ns <- escNSTok tok = NS ns : escABC ns ops
escABC ns (ABC_Tok tok : ops) = K0 tok : escABC ns ops
escABC ns (ABC_Prim op : ops) 
    | bWhitespace = escABC ns ops -- skip SP and LF (identity prims)
    | otherwise = P0 op : escABC ns ops -- keep all other primitives
    where bWhitespace = (ABC_SP == op) || (ABC_LF == op)
escABC ns (ABC_Text txt : ops) = T0 txt : escABC ns ops
escABC ns (ABC_Block abc : ops) = B0 cc : escABC ns ops where
    cc = escABC ns (ABC.abcOps abc)
escABC _ [] = []

-- | parse structured values from lower level claw code...
reduceOps :: [ClawOp] -> [ClawOp]
reduceOps = reduce [] where
    reduce lhs [] = L.reverse lhs
    reduce lhs rhs
        | Just (n, rhs') <- parseInt rhs
        = reduce (NI n : lhs) rhs'
    reduce (NI d : NI n : lhs) (CW w : rhs) 
        | (w == wRatio) && (d > 0)
        = reduce (NR (ClawRatio n d) : lhs) rhs
    reduce (NI dp : NI body : lhs) (CW w : rhs)
        | (w == wDecimal) && (dp > 0)
        = reduce (ND (ClawDecimal body dp) : lhs) rhs
    reduce (NI exp10 : ND dec : lhs) (CW w : rhs)
        | (w == wExp10)
        = reduce (NE (ClawExp10 dec exp10) : lhs) rhs
    reduce (T0 txt : lhs) (CW w : rhs) 
        | (w == wLiteral) && isInlineableText txt
        = reduce (TL txt : lhs) rhs
    reduce (B0 cc : lhs) (CW w : rhs) | (w == wBlock) 
        = reduce (BC cc : lhs) rhs
    reduce lhs (op:rhs) = reduce (op:lhs) rhs

-- | parse integer e.g. from form `\\#42 integer`. 
parseInt :: [ClawOp] -> Maybe (ClawInt, [ClawOp]) 
parseInt = p0 where
    p0 (P0 ABC_newZero : ops) = pn 0 ops
    p0 _ = Nothing
    pn !n (P0 (digitOp -> Just d) : ops) = pn ((10*n)+d) ops
    pn !n (P0 ABC_negate : ops) = pn (negate n) ops 
    pn !n (CW w : ops) | (w == wInteger) = Just (n, ops) -- done
    pn _ _ = Nothing

digitOp :: ABC.PrimOp -> Maybe ClawInt
digitOp op =
    let c = ABC.abcOpToChar op in
    let bOK = ('0' <= c) && (c <= '9') in
    if not bOK then Nothing else
    Just (fromIntegral $ ord c - ord '0')

-- | Test whether the text is valid for inline representation.
-- This minimally requires the text does not use `"` or LF.
isInlineableText :: ABC.Text -> Bool
isInlineableText s = LBS.notElem '"' s && LBS.notElem '\n' s


{-



-- | Encode Claw into a Lazy UTF8 Bytestring. Assumes valid input.
encode :: ClawCode -> LazyUTF8.ByteString
encode = BB.toLazyByteString . encode'

encode' :: ClawCode -> BB.Builder
encode' = mconcat . injectSpaces . fmap _encodeOp . clawOps where
    injectSpaces = L.intersperse (BB.char8 ' ')

_encodeOp :: Op -> BB.Builder
_encodeOp (Word w) = BB.byteString w
_encodeOp (Num r) = encodeNum r
_encodeOp (Text t) = BB.char8 '"' <> BB.lazyByteString t <> BB.char8 '"'
_encodeOp (Block b) = BB.char8 '[' <> encode' b <> BB.char8 ']'
_encodeOp (EscPrim abc) = BB.char8 '\\' <> _encodeABC abc
_encodeOp (EscTok tok)  = BB.char8 '\\' <> ABC.encodeTokenBB tok
_encodeOp (EscText txt) = BB.char8 '\\' <> ABC.encodeTextBB txt

_encodeABC :: [ABC.PrimOp] -> BB.Builder
_encodeABC = mconcat . fmap (BB.char8 . ABC.abcOpToChar)

-- | Encode as exact decimal if possible, otherwise as fractional
-- (currently Claw does not support Scientific notation)
encodeNum :: Rational -> BB.Builder
encodeNum (toDecimal -> Just d) = BB.string8 (show d)
encodeNum r = num <> BB.char8 '/' <> den where
    num = BB.string8 $ show $ numerator r
    den = BB.string8 $ show $ denominator r

toDecimal :: Rational -> Maybe Decimal.Decimal
toDecimal (Decimal.eitherFromRational -> Right d) = Just d
toDecimal _ = Nothing

data DecoderCont
    = DecoderDone
    | DecodeBlock [Op] DecoderCont
    deriving (Show, Eq)
data DecoderStuck = DecoderStuck
    { dcs_text :: Text
    , dcs_cont :: DecoderCont
    , dcs_ws   :: Bool -- preceded by SP, LF, or '[' (word separators)
    , dcs_data :: [Op] -- reverse order
    } deriving (Show, Eq)

-- | Decode Claw code from a Lazy UTF8 Bytestring. This will parse
-- as far as it can then return if stuck. The precision for errors
-- is a location within a block after some complete operations.
decode :: Text -> Either DecoderStuck ClawCode
decode txt = decode' $ DecoderStuck
    { dcs_text = txt
    , dcs_cont = DecoderDone
    , dcs_ws = True
    , dcs_data = []
    }

-- | Decode from an initial Stuck position, i.e. assuming it has
-- been tweaked so we should no longer be Stuck. This could allow
-- lenient decoding or other simple extensions to the decoder.
decode' :: DecoderStuck -> Either DecoderStuck ClawCode
decode' s = decoder (dcs_cont s) (dcs_ws s) (dcs_data s) (dcs_text s)

-- skip whitespace then decode from the next character
decoder :: DecoderCont -> Bool -> [Op] -> Text -> Either DecoderStuck ClawCode
decoder cc bWordSep r txt0 = 
    let decoderIsStuck = Left (DecoderStuck txt0 cc bWordSep r) in
    case LBS.uncons txt0 of
        Nothing -> case cc of
            DecoderDone -> Right (ClawCode (L.reverse r))
            _ -> decoderIsStuck
        Just (c, txt) -> case c of
            ' '  -> decoder cc True r txt    -- skip whitespace
            '\n' -> decoder cc True r txt    -- skip whitespace
            '[' -> if not bWordSep then decoderIsStuck else
                   decoder (DecodeBlock r cc) True [] txt
            ']' -> case cc of
                DecodeBlock ops cc' -> decoder cc' False (block:ops) txt where
                    block = Block (ClawCode (L.reverse r))
                _ -> decoderIsStuck
            '"' -> case LBS.elemIndex '"' txt of
                Nothing -> decoderIsStuck
                Just idx ->
                    let (lit, litEnd) = LBS.splitAt idx txt in
                    let bOK = bWordSep && isInlinableText lit in
                    if not bOK then decoderIsStuck else
                    decoder cc False (Text lit : r) (LBS.drop 1 litEnd)
            '\\' -> if not bWordSep then decoderIsStuck else
                case decodeEscOp txt of
                    Nothing -> decoderIsStuck
                    Just (op, txt') -> decoder cc False (op:r) txt'
            _ -> if not bWordSep then decoderIsStuck else
                case decodeWordOrNumber txt0 of
                    Nothing -> decoderIsStuck
                    Just (op, txt') -> decoder cc False (op:r) txt'

decodeWordOrNumber :: Text -> Maybe (Op, Text)
decodeWordOrNumber = error "TODO"

-- just after the \ escape
decodeEscOp :: Text -> Maybe (Op, Text)
decodeEscOp txt0 = 
    LBS.uncons txt0 >>= \ (c, txt) -> case c of
        (charToEscPrim -> Just primOp) -> 
            let (moreOps, txt') = takePrims txt in
            let ops = primOp : moreOps in
            return (EscPrim ops, txt')
        '{' -> -- escaped token, may contain SP (but not LF)
            LBS.elemIndex '}' txt >>= \ idx ->
            let (lzt, tokEnd) = LBS.splitAt idx txt in
            let bOK = not $ LBS.elem '{' lzt || LBS.elem '\n' lzt in
            if not bOK then fail "illegal token" else
            let tok = LBS.toStrict lzt in
            let txt' = LBS.drop 1 tokEnd in
            tok `seq` return (EscTok tok, txt') 
        '"' -> -- escaped ABC text, multi-line required
            ABC.decodeLiteral txt >>= \ (lit, litEnd) ->
            LBS.uncons litEnd >>= \ (litEndChar, txt') ->
            let bOK = ('~' == litEndChar) in
            if not bOK then fail "illegal literal" else
            return (EscText lit, txt')
        _ -> fail "unrecognized escape sequence"

-- escPrim is any ABC PrimOp except SP and LF
charToEscPrim :: Char -> Maybe ABC.PrimOp
charToEscPrim c = if space then Nothing else ABC.abcCharToOp c where
    space = ((' ' == c) || ('\n' == c))

takePrims :: Text -> ([ABC.PrimOp], Text)
takePrims = tkps [] where
    tkps r txt = case tkop txt of
        Nothing -> (L.reverse r, txt)
        Just (op, txt') -> tkps (op:r) txt'
    tkop txt =
        LBS.uncons txt >>= \ (c, txt') ->
        charToEscPrim c >>= \ op ->
        return (op, txt')

instance IsString ClawCode where
    fromString s =
        case decode (LazyUTF8.fromString s) of
            Right clawCode -> clawCode
            Left dcs ->
                let sLoc = L.take 40 $ LazyUTF8.toString $ dcs_text dcs in
                error $ clawCodeErr $ "parse failure @ " ++ sLoc

instance Show Op where
    showsPrec _ = showList . (:[])
    showList = shows . ClawCode
instance Show ClawCode where
    showsPrec _ = showString . LazyUTF8.toString . encode


clawCodeErr :: String -> String
clawCodeErr = (++) "Awelon.ClawCode: " 
-}
