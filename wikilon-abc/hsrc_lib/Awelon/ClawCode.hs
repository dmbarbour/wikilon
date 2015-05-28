{-# LANGUAGE ViewPatterns #-}
-- | Command Language (or Command Line) for Awelon, 'claw'
--
-- Claw is a very thin layer above Awelon Bytecode intended to make
-- ABC more suitable for command line or REPL activities by humans.
-- Claw assumes that most logic will already be specified externally
-- using a dictionary, such that the command line mostly glues a few
-- words and arguments together for a Forth-like experience.
--
-- Words translate to {%word} tokens, also used within Awelon Object
-- dictionaries. Values are just inline texts and numbers. The main
-- contribution of Claw is to make values and words more accessible.
-- The cost is that other tokens and multi-line texts need escapes.
--
-- * words: inc over mul
-- * numbers: -7 2\/3 1.2345
-- * inline texts: "foo"
-- * blocks: [2 mul] 
-- * escape ABC \\vrwlc
-- * escape a token \\{&par}
-- * escape a multi-line text \\"foo\n bar...\n~
--
-- Word separators are required before all elements except SP, LF,
-- and ']'. Word separators are just SP, LF, and '['. The asymmetry
-- on [] is intentional, allowing contained elements to sit adjacent
-- to the brackets but not surrounding elements. E.g. we may write
-- `[[2 mul] 3 repeat]` but not `foo[ bar ]baz`.
--
-- Each value is implicitly followed by \\l, to push the value onto
-- the stack in a (stack * environment) pair. The stable environment
-- is valuable for both access by the user and integration with the
-- outside world. This can be canceled by an explicit \\r after the
-- value.
--
-- The three escape forms are not mixed, i.e. if a token or text is
-- escaped this is easily determined by the first character after the
-- backslash, otherwise it's a list of ABC primitives ending upon any
-- word separator. Multi-line texts are included for completeness if
-- round-tripping ABC to and from Claw, and aren't very suitable 
-- for use on a command line.
--
-- Aside: I am interested in support for lists and richly structured
-- information in text, e.g. in the style of YAML or JSON. I will save
-- use of brackets (){} for such extensions. This might not become part
-- of claw per se (big data doesn't fit on a command line anyway), but
-- rather a variant that extends claw.
-- 
module Awelon.ClawCode
    ( ClawCode(..)
    , Op(..)
    , isInlinableText
    , isUnambiguousWord
    , encode
    , encode'
    , encodeNum
    , decode, decoder
    , DecoderStuck(..), DecoderCont(..)
    -- , fromABC
    -- , toABC
    ) where

import Data.Monoid
import Data.Ratio
import Data.Char
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.Builder as BB
import qualified Data.List as L
import qualified Data.Decimal as Decimal
import Data.String (IsString(..))
import qualified Awelon.ABC as ABC
import qualified Awelon.Word as W
import Awelon.Text

-- | Command Language for Awelon (claw)
-- parses to a plain old sequence of operations
newtype ClawCode = ClawCode { clawOps :: [Op] }
    deriving (Eq, Ord)

data Op 
    = Word      !UTF8.ByteString    -- unambiguous words
    | Num       !Rational           -- any number (decimal encoding favored)
    | Text      !ABC.Text           -- inlinable texts only!
    | Block     !ClawCode           -- first class function in Claw
    | EscPrim   ![ABC.PrimOp]       -- escaped ABC, e.g. \vrwlc; no SP or LF
    | EscTok    !ABC.Token
    | EscText   !ABC.Text
    deriving (Eq, Ord)

-- | inline texts must avoid C0, DEL, C1, ".
isInlinableText :: Text -> Bool
isInlinableText = L.all okc . LazyUTF8.toString where
    okc c = not ((isCtl c) || ('"' == c))
    isCtl c = (n <= 0x1F) || ((0x7F <= n) && (n <= 0x9f)) where
        n = ord c

-- | test whether a proposed word is unambiguous if represented 
-- directly in Claw.
isUnambiguousWord :: UTF8.ByteString -> Bool
isUnambiguousWord = W.isValidWord . W.Word

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
_encodeOp (EscTok tok)  = BB.char8 '\\' <> ABC.encodeToken' tok
_encodeOp (EscText txt) = BB.char8 '\\' <> ABC.encodeText' txt

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

