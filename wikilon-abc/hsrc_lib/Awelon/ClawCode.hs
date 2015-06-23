{-# LANGUAGE OverloadedStrings, ViewPatterns, PatternGuards, BangPatterns #-}
-- | Command Language (or Command Line) for Awelon, 'claw'
--
-- Claw is an editable view of Awelon Bytecode (ABC), essentially a
-- syntactic sugar with a reversible, context-free expansion process.
-- Claw is very simple, and its main purpose is to amplify the human
-- user's abilility to write REPL or shell commands via keyboard.
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
-- Decimal numbers are currently limited to 255 decimal places or less
-- (via Data.Decimal).
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
    , ClawRatio(..)
    , ClawExp10(..)
    , ClawDecimal
    , clawToABC
    , clawFromABC
    , isInlineableText

    , encode
    , encode'

{-
    , decode, decoder
    , DecoderStuck(..), DecoderCont(..)
-}
    -- , fromABC
    -- , toABC
    , module Awelon.Word
    ) where

import Data.Monoid
import Data.Char
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.Builder as BB
import qualified Data.List as L
import qualified Data.Decimal as D
import Data.String (IsString(..))
import Awelon.ABC (ABC,Op(..),PrimOp(..))
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
    | T0 !ABC.Text      -- escaped text
    | K0 !ABC.Token     -- escaped token
    | P0 !ABC.PrimOp    -- escaped ABC ops
    | B0 ![ClawOp]      -- escaped blocks
    | NI !ClawInt       -- integer
    | NR !ClawRatio     -- ratio
    | ND !ClawDecimal   -- decimal
    | NE !ClawExp10     -- exponential
    | TL !ABC.Text      -- text literal
    | BC ![ClawOp]      -- block of code
    deriving (Ord, Eq)
    
type ClawInt = Integer
type ClawDecimal = D.Decimal -- current limit 255 decimal places
data ClawRatio = ClawRatio !ClawInt !ClawInt deriving (Eq, Ord)
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

-- | Convert claw code to ABC for interpretation or storage. This is
-- essentially a 'compiler' for Claw code, though resulting bytecode 
-- will usually need linking and processing for performance.
clawToABC :: ClawCode -> ABC
clawToABC = mconcat . zns opToABC

zns :: (Namespace -> ClawOp -> a) -> ClawCode -> [a]
zns f = \ cc -> jfn (clawNS cc) (clawOps cc) where
    jfn _ (op@(NS ns) : ops) = f ns op : jfn ns ops
    jfn ns (op : ops) = f ns op : jfn ns ops
    jfn _ [] = []

oneOp :: ABC.Op -> ABC
oneOp = ABC.mkABC . return 

expand :: Namespace -> [ClawOp] -> ABC
expand ns = clawToABC . ClawCode ns

opToABC :: Namespace -> ClawOp -> ABC
-- low level
opToABC _ (NS ns) = oneOp $ ABC_Tok $ nsTokPrefix <> ns
opToABC ns (CW (Word w)) = oneOp $ ABC_Tok $ mconcat ["%", ns, w]
opToABC _ (P0 op) = oneOp $ ABC_Prim op
opToABC _ (T0 txt) = oneOp $ ABC_Text txt
opToABC _ (K0 tok) = oneOp $ ABC_Tok tok
opToABC ns (B0 cc) = oneOp $ ABC_Block $ expand ns cc
-- expansions
opToABC ns (NI i) = expand ns (escInt ++ [CW wInteger]) where
    escInt = fmap P0 $ ABC.primQuoteInteger i []
opToABC ns (NR (ClawRatio n d)) = expand ns [NI n, NI d, CW wRatio]
opToABC ns (ND (D.Decimal dp m)) = 
    expand ns [NI m, NI (fromIntegral dp), CW wDecimal]
opToABC ns (NE (ClawExp10 d x)) = expand ns [ND d, NI x, CW wExp10]
opToABC ns (TL lit) = expand ns [T0 lit, CW wLiteral]
opToABC ns (BC cc) = expand ns [B0 cc, CW wBlock]

-- | parse Claw code from bytecode. This requires the current
-- namespace in order to provide some useful context.
clawFromABC :: Namespace -> ABC -> ClawCode
clawFromABC ns = ClawCode ns . reduceClaw . escABC ns . ABC.abcOps

-- | recognize claw words from bytecode 
--
--   {%foo:word} → word 
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
-- namespace must also be valid word (or empty string)
escNSTok :: ABC.Token -> Maybe Namespace
escNSTok tok =
    let bMatchPrefix = nsTokPrefix `BS.isPrefixOf` tok in
    let ns = BS.drop 4 tok in
    let bValidNS = BS.null ns || isValidWord (Word ns) in
    let bOK = bMatchPrefix && bValidNS in
    if bOK then Just ns else Nothing

-- | Identify SP and LF identity operators from ABC-layer formatting.
abcWS :: ABC.PrimOp -> Bool
abcWS ABC_SP = True
abcWS ABC_LF = True
abcWS _ = False

-- | recognize basic claw operations and handle namespace context.
-- This will also filter all ABC_SP and ABC_LF elements from the
-- input (which simplifies further processing)
escABC :: Namespace -> [ABC.Op] -> [ClawOp]
escABC ns (ABC_Tok tok : ops) 
  | Just w <- escWord ns tok = CW w : escABC ns ops
  | Just ns' <- escNSTok tok = NS ns' : escABC ns' ops
  | otherwise = K0 tok : escABC ns ops
escABC ns (ABC_Prim op : ops)  
  | abcWS op = escABC ns ops
  | otherwise = P0 op : escABC ns ops
escABC ns (ABC_Text txt : ops) = T0 txt : escABC ns ops
escABC ns (ABC_Block abc : ops) = B0 cc : escABC ns ops where
    cc = escABC ns (ABC.abcOps abc)
escABC _ [] = []

-- | collapse structured values from lower level claw code.
--
-- Targets:
--  full blocks
--  texts
--  integers
--  ratios
--  decimals
--  e-notation
--
-- At the moment, this is not a streaming reducer but rather a
-- zipper-based implementation. 
--
reduceClaw :: [ClawOp] -> [ClawOp]
reduceClaw = rdz []

rdz :: [ClawOp] -> [ClawOp] -> [ClawOp]
rdz lhs (CW w : rhs)
    | (w == wInteger)
    , Just (lhs', n) <- parseIntR lhs
    = rdz (NI n : lhs') rhs
rdz (NI d : NI n : lhs) (CW w : rhs)
    | (w == wRatio) && (d > 0)
    = rdz (NR r : lhs) rhs
    where r = ClawRatio n d
rdz (NI dp : NI m : lhs) (CW w : rhs) 
    | (w == wDecimal) && (dp > 0) && (dp <= 255)
    = rdz (ND dec : lhs) rhs
    where dec = D.Decimal (fromIntegral dp) m
rdz (NI e : ND d : lhs) (CW w : rhs)
    | (w == wExp10)
    = rdz (NE ne : lhs) rhs
    where ne = ClawExp10 d e
rdz (T0 txt : lhs) (CW w : rhs)
    | (w == wLiteral) && (isInlineableText txt)
    = rdz (TL txt : lhs) rhs
rdz (B0 cc : lhs) (CW w : rhs)
    | (w == wBlock)
    = rdz (BC cc : lhs) rhs
rdz lhs (op : rhs) = rdz (op : lhs) rhs -- step
rdz lhs [] = L.reverse lhs -- all done

-- | parse raw integer (e.g. #42) from lhs in the zipper-based
-- reduction, i.e. we'll see #42 from the left hand side, parse
-- back to the '#', and accumulate the operations on the way.
--
-- This uses a simple strategy. We obtain a list of numeric operations
-- for building the integer up to '#', then we process it.
parseIntR :: [ClawOp] -> Maybe ([ClawOp], ClawInt)
parseIntR = run where
  run ops = 
    collectR [] ops >>= \(fs, ops') ->
    return (ops', composeList fs 0)
  collectR fs (P0 op : ops)
    | Just f <- intOp op = collectR (f:fs) ops -- include value
    | (op == ABC_newZero) = Just (fs, ops) -- done
    | abcWS op = collectR fs ops
    | otherwise = Nothing
  collectR _ _ = Nothing -- 

composeList :: [a -> a] -> a -> a
composeList = L.foldr (flip (.)) id

intOp :: ABC.PrimOp -> Maybe (ClawInt -> ClawInt)
intOp (digitOp -> Just d) = Just step where
    step !n = ((10*n)+d)
intOp ABC_negate = Just negate
intOp _ = Nothing

digitOp :: ABC.PrimOp -> Maybe ClawInt
digitOp op =
    let c = ABC.abcOpToChar op in
    let bOK = ('0' <= c) && (c <= '9') in
    if not bOK then Nothing else
    Just $! fromIntegral $ ord c - ord '0'

-- | Test whether the text is valid for inline representation.
-- This minimally requires the text does not use `"` or LF.
isInlineableText :: ABC.Text -> Bool
isInlineableText s = LBS.notElem '"' s && LBS.notElem '\n' s

-- | Render claw code as a lazy utf-8 bytestring for human use and
-- editing. Note that the current implementation isn't optimal for
-- large 
encode :: ClawCode -> LazyUTF8.ByteString
encode = BB.toLazyByteString . encode'

encode' :: ClawCode -> BB.Builder
encode' = encodeOps . clawOps

-- collect non-whitespace operators for display together
joinP0 :: [ABC.PrimOp] -> [ClawOp] -> ([ABC.PrimOp],[ClawOp])
joinP0 abc (P0 op : ops) | not (abcWS op) = joinP0 (op:abc) ops
joinP0 abc ops = (L.reverse abc, ops)

-- encode a list of operations, with special case to encode long
-- strings of primitive operations into a single escape string.
encodeOps :: [ClawOp] -> BB.Builder
encodeOps (P0 op : ops) | not (abcWS op) = output where
    output = BB.char8 '\\' <> encPrims abc <> moreOps ops'
    encPrims = BB.string8 . fmap ABC.abcOpToChar
    (abc,ops') = joinP0 [op] ops
encodeOps (op:ops) = encodeOp op <> moreOps ops
encodeOps [] = mempty

-- encode operators after adding a space character
moreOps :: [ClawOp] -> BB.Builder
moreOps [] = mempty
moreOps ops = BB.char8 ' ' <> encodeOps ops

-- encode a singular operation.
encodeOp :: ClawOp -> BB.Builder
encodeOp (NS ns) = BB.char8 '#' <> BB.byteString ns
encodeOp (CW (Word w)) = BB.byteString w
encodeOp (NI i) = BB.string8 (show i)
encodeOp (TL txt) = BB.char8 '"' <> BB.lazyByteString txt <> BB.char8 '"'
encodeOp (BC cc) = encBlock cc
encodeOp (NR (ClawRatio n d)) = 
    BB.string8 (show n) <> BB.char8 '/' <> BB.string8 (show d)
encodeOp (ND d) = BB.string8 (show d)
encodeOp (NE (ClawExp10 d e)) = 
    BB.string8 (show d) <> BB.char8 'e' <> BB.string8 (show e)
encodeOp (P0 op) 
    | abcWS op = mempty
    | otherwise = BB.char8 '\\' <> c
    where c = BB.char8 $ ABC.abcOpToChar op
encodeOp (T0 txt) = BB.char8 '\n' <> BB.char8 '\\' <> ABC.encodeTextBB txt
encodeOp (K0 tok) = BB.char8 '\\' <> ABC.encodeTokenBB tok
encodeOp (B0 cc) = BB.char8 '\\' <> encBlock cc

encBlock :: [ClawOp] -> BB.Builder
encBlock cc = BB.char8 '[' <> encodeOps cc <> BB.char8 ']'

instance Show ClawOp where
    showsPrec _ = showList . (:[])
    showList = (++) . LazyUTF8.toString . BB.toLazyByteString . encodeOps
instance Show ClawCode where
    showsPrec _ = showList . clawOps


{-

-- | Encode Claw into a Lazy UTF8 Bytestring. Assumes valid input.
encode :: ClawCode -> LazyUTF8.ByteString
encode = BB.toLazyByteString . encode'

encode' :: ClawCode -> BB.Builder
encode' = mconcat . injectSpaces . fmap _encodeOp . clawOps where

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


impossible :: String -> a
impossible = error . clawCodeErr

clawCodeErr :: String -> String
clawCodeErr = (++) "Awelon.ClawCode: " 

-}
