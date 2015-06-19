{-# LANGUAGE ViewPatterns, EmptyDataDecls #-}
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
-- even graphs and diagrams for a structure editor.
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
-- must be expanded recursively for the inner content. 
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
    , clawToAO
    , clawFromAO
    , isInlinableText
    , isUnambiguousWord
    , encode
    , encode'
    , encodeNum
    , decode, decoder
    , DecoderStuck(..), DecoderCont(..)
    -- , fromABC
    -- , toABC
    , module Awelon.Word
    ) where

import Control.Monad
import Control.Arrow (first)
import Data.Monoid
import Data.Ratio
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
-- expands into a plain old sequence of operations
data ClawCode ext = ClawCode 
    { clawNS  :: !Namespace
    , clawOps :: [ClawBase ext]
    } deriving (Eq, Ord)

data ClawBase ext
    = ClawWord      !Word               -- unambiguous words
    | ClawEscPrim   !ABC.PrimOp
    | ClawEscText   !ABC.
    | ClawInteger   !Integer            -- e.g. -7         \#7- integral
    | ClawRatio     !Integer !Integer   -- e.g. 2/3        \#2#3 rational
    | ClawDecimal   !Integer !Integer   -- e.g. 1.20       \#120#2 decimal
    | ClawLiteral   !ABC.Text           -- inlinable texts only!
    | ClawBlock     ![ClawOp]           -- first class Claw function
    | ClawEscPrim   !ABC.PrimOp         -- escaped ABC primitive
    | ClawEscTok    !ABC.Token          -- escaped token
    | ClawEscText   !ABC.Text           -- escaped text
    | ClawEscBlock  ![ClawOp]           -- escaped block
    deriving (Eq, Ord)

-- | A region of claw code has exactly one namespace. A namespace is
-- a prefix for all the words in the namespace, and should be a valid
-- Utf8 string.
type Namespace = UTF8.ByteString

-- 



wIntegral, wRational, wDecimal, wLiteral :: Word
wIntegral = "integral"
wRational = "rational"
wDecimal = "decimal"
wLiteral = "literal"

-- | convert claw code to ABC code assuming all words are valid and 
-- words expand into simple tokens. This is the normal use case and
-- is simple and efficient.
clawToABC :: (Word -> ABC.Token) -> ClawCode -> ABC
clawToABC wtok = ABC.mkABC . L.concatMap opToABC . clawOps where
    wt = ABC_Tok . wtok
    opToABC (ClawWord w) = [wt w]
    opToABC (ClawIntegral n) = ABC.quotes n [wt wIntegral]
    opToABC (ClawRational n d) = ABC.quotes n . ABC.quotes d $ [wt wRational]
    opToABC (ClawDecimal c d) = ABC.quotes c . ABC.quotes d $ [wt wDecimal]
    opToABC (ClawLiteral txt) = [ABC_Text txt, wt wLiteral]
    opToABC (ClawBlock cc) = [ABC_Block (clawToABC wtok cc)]
    opToABC (ClawEscPrim op) = [ABC_Prim op]
    opToABC (ClawEscTok tok) = [ABC_Tok tok]
    opToABC (ClawEscText txt) = [ABC_Text txt]

-- | convert ABC to claw code assuming that all words expand into
-- tokens. This potentially performs a little backtracking, but not
-- much at the moment.
clawFromABC :: (ABC.Token -> Maybe Word) -> ABC -> ClawCode
clawFromABC t2w = ClawCode . reduceClawOps . fmap (escABC t2w) . abcOps where

escABC :: (ABC.Token -> Maybe Word) -> ABC.Op -> ClawOp
escABC _   (ABC_Prim op)   = ClawEscPrim op
escABC t2w (ABC_Block abc) = ClawBlock (clawFromABC t2w abc)
escABC _   (ABC_Text txt)  = ClawEscText txt
escABC t2w (ABC_Tok tok)   = case t2w tok of
    Just w | isValidWord w -> ClawWord w
    _ -> ClawEscTok tok

-- a simplistic Claw code parser
newtype ClawParse a = ClawParse { runClawParse :: [ClawOp] -> Maybe (a, [ClawOp]) }

instance Functor ClawParse where
    fmap f = ClawParse . fmap (fmap (first f)) . runParse 
instance Applicative ClawParse where 
    pure = return
    (<*>) = ap
instance Monad ClawParse where
    return x = ClawParse (\ ops -> return (x, ops))
    (>>=) p1 fp2 = ClawParse $ \ ops ->
        runParse p1 ops >>= \ (x, ops') ->
        runParse (fp2 x) ops'
    fail = const mzero
instance Alternative ClawParse where
    empty = ClawParse (const Nothing)
    (<|>) p1 p2 = ClawParse $ \ ops -> 
        runParse p1 ops <|> runParse p2 ops
instance MonadPlus ClawParse where
    mzero = empty
    mplus = (<|>)

pWord :: ClawParse Word
pWord = ClawParse $ \ ops -> case ops of
    (ClawWord w : ops') -> Just (w, ops')
    _ -> Nothing

pExactWord :: Word -> ClawParse ()
pExactWord w = pWord >>= guard . (== w)

pPrimOp :: ClawParse ABC.PrimOp
pPrimOp = ClawParse $ \ ops -> case ops of
    (ClawEscPrim op : ops') -> Just (op, ops')
    _ -> Nothing

pExactPrim :: ABC.PrimOp -> ClawParse ()
pExactPrim op = pPrimOp >>= guard . (== op)

-- parse a single ABC op
pDigit :: ClawParse Integer
pDigit = 
    primOp >>= \ op ->
    let c = ABC.abcOpToChar op in
    let bOK = ('0' <= c) && (c <= '9') in
    if not bOK then mzero else
    fromIntegral (ord c - ord '0') 

-- parse just the content of a `#42...` sequence
pRawInt :: ClawParse Integer
pRawInt = newIntOp >> (nonZero <|> return 0) where
    newIntOp = pExactPrim ABC_newZero
    nonZero = posInt >>= tryNegate
    posInt =
        pDigit >>= \ n1 ->
        guard (n1 /= 0) >>
        many pDigit >>= \ ns ->
        let accum acc x = (10*acc)+x in
        return $! L.foldl' accum n1 ns
    getNegateFn = (negOp >> return negate) <|> return id
    negOp = pExactPrim ABC_negate
    tryNegate n = getNegateFn >>= \ f -> return $! f n
        

reduceClawCode :: ClawCode -> ClawCode
reduceClawCode = ClawCode . reduceClawOps . clawOps

-- Simplistic parser function that recognizes numbers and literals.
-- This operation always succeeds.
reduceClawOps :: [ClawOp] -> [ClawOp]
reduceClawOps (ClawBlock cc : ops) = ClawBlock (reduceClawCode cc) : reduceClawOps ops
reduceClawOps (rNum -> Just (num, ops)) = num : reduceClawOps ops
reduceClawOps (rLit -> Just (txt, ops)) = ClawLit txt : reduceClawOps ops
reduceClawOps (op : ops) = op : reduceClaw ops
reduceClawOps [] = []

-- | inline texts must at least exclude LF and double quote.
isInlinableText :: Text -> Bool
isInlinableText s = BS.notElem '\n' s && BS.notElem '"' s

-- extract ClawLit
rLit :: [ClawOp] -> Maybe (ABC.Text, [ClawOp])
rLit (ClawEscText txt : ClawWord w : ops) 
    | (w == wLiteral) && isInlineableText txt
    = Just (txt, ops)
rLit _ = Nothing

-- extract ClawInt, ClawRat, or ClawDec
-- maybe later add three-part numbers for scientific
--   e.g. 1.20e3 becomes #120#2#3 decimalExp
rNum :: [ClawOp] -> Maybe (ClawOp, [ClawOp])
rNum = runClawParse pNum where
    pNum = pRawInt >>= pNum1
    pNum1 n1 = (pInt n1 <|> pRawInt >>= pNum2 n1)
    pNum2 n1 n2 = (pRat n1 n2 <|> pDec n1 n2)
    pInt n = pExactWord wIntegral >> return (ClawInt n)
    pRat n d = pExactWord wRational >> return (ClawRat n d)
    pDec m p = 
        guard (p > 0) >>    -- at least one decimal place
        pExactWord wDecimal >>  -- followed by word 'decimal'
        return (ClawDecimal m p) 


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

