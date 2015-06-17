{-# LANGUAGE ViewPatterns, EmptyDataDecls #-}
-- | Command Language (or Command Line) for Awelon, 'claw'
--
-- Claw is an editor-layer syntactic sugar for Awelon Bytecode (ABC),
-- oriented towards the dictionary bindings of Awelon Object (AO).
-- Claw is aimed towards a Forth-like programmer experience suitable
-- for command line shells and REPLs, e.g. sentences of at most ten
-- to twenty tokens
--
-- Claw optimizes access to words, numbers, blocks, and small texts:
--
-- * words: inc over mul
-- * numbers: 42 -2\/3 3.141
-- * small inline texts: "foo"
-- * blocks: [2 mul]
-- * escaped ABC primitive(s): \\vrwlc
-- * escaped ABC token: \\{&foo}
-- * escaped ABC text: \\"foo\\n~
--
-- Claw semantics is simply the expansion of Claw code into ABC. 
-- Numerals and literals expand in a straightforward manner:
--
--   42             \\#42 integral
--   "foo"          \\"foo
--                  ~ literal
--   2\/3           \\#2#3 rational      
--   3.141          \\#3141#3 decimal  
--   -1.20          \\#120-#2 decimal
--
-- Note that numbers are not simplified. Numbers 2\/3 and 4\/6 have 
-- distinct representations in ABC and this is preserved such that
-- a programmer revisiting the code will see numbers as entered. 
--
-- Words usually expand into tokens, e.g. `{%inc}{%over}{%mul}`, that
-- bind to an implicit Awelon Object dictionary. This module assumes
-- expansion into tokens.
--
-- In addition to trivial expansion, it is easy to parse the expanded
-- bytecode back into Claw code. This allows us to store Claw code in
-- raw bytecode form. Further, it simplifies introducing new features
-- at the editor layer. One might transparently introduce support for
-- vectors, matrices, math formulae, XML, etc..
-- 
-- Ultimately, Claw code is a very simplistic structuring of ABC. It
-- is the simplest thing that could possibly work, and is orthogonal
-- to semantics-layer structuring found in AO dictionary definitions.
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
-- parses to a plain old sequence of operations
newtype ClawCode = ClawCode { clawOps :: [ClawOp] }
    deriving (Eq, Ord)

data ClawOp
    = ClawWord  !Word                -- unambiguous words
    | ClawInt   !Integer             -- e.g. -7         \#7- integral
    | ClawRat   !Integer !Integer    -- e.g. 2/3        \#2#3 rational
    | ClawDec   !Integer !Integer    -- e.g. 1.20       \#120#2 decimal
    | ClawLit   !ABC.Text            -- inlinable texts only!
    | ClawBlock !ClawCode            -- first class function in Claw
    | ClawEscPrim   !ABC.PrimOp      -- escaped ABC primitive, e.g. \v
    | ClawEscTok    !ABC.Token       -- a single escaped token
    | ClawEscText   !ABC.Text        -- a single escaped text
    deriving (Eq, Ord)

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
    opToABC (ClawInt n) = ABC.quotes n [wt wIntegral]
    opToABC (ClawRat n d) = ABC.quotes n . ABC.quotes d $ [wt wRational]
    opToABC (ClawDec c d) = ABC.quotes c . ABC.quotes d $ [wt wDecimal]
    opToABC (ClawLit txt) = [ABC_Text txt, wt wLiteral]
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
rNum :: [ClawOp] -> Maybe (ClawOp, [ClawOp])
rNum = runClawParse pNum where
    pNum = pRawInt >>= pNum1
    pNum1 n1 = (pInt n1 <|> pRawInt >>= pNum2 n1)
    pNum2 n1 n2 = (pRat n1 n2 <|> pDec n1 n2)
    pInt n =
        pWord >>= \ w ->
        guard (w == wIntegral) >>
        return (ClawInt n)
    pRat num den = 
        pWord >>= \ w ->
        guard (w == wRational) >>
        return (ClawRat num den)
    pDec m p = 
        pWord >>= \ w ->
        guard (w == wDecimal) >>
        return (ClawDec m p)


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

