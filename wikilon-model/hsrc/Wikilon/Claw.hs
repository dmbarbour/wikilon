{-# LANGUAGE OverloadedStrings, ViewPatterns, PatternGuards, BangPatterns, GeneralizedNewtypeDeriving #-}
-- | 'Command Language for Awelon' (or Claw) 
--
-- Claw is a syntactic sugar or lense for Awelon Object (AO) code.
-- AO code is a subset of pure Awelon Bytecode (ABC) constrained to
-- use a pure, portable subset of tokens. (cf. Wikilon.AODef).
--
-- Claw aims at a Forth-like programmer experience, suitable for one
-- liner programs, command lines, and REPLs. Claw primarily optimizes
-- representation of words, numbers, small texts, short lists. Claw 
-- operates by simple, reversible expansion rules.
--
-- Examples:
--
--      2\/3        2 3 ratio
--      4\/10       4 10 ratio
--      3.141       3141 3 decimal
--      -1.20       -120 2 decimal
--      6.02e23     6.02 23 exp10
--      42          \\#42 integer
--      -7          \\#7- integer
--      ratio       \\{%ratio}
--      "foo"       \\"foo
--                  ~ literal
--      [foo]       \\[foo] block
--      \\[foo,bar,baz]  \\[foo \\[bar \\[baz] seq] seq]
-- 
-- Claw is easily extensible by adding new expansion rules. Wikilon
-- will simply hard-code a particular set of extensions or features
-- that will work well enough for most use cases. If a few specific
-- variants are needed, I'll model them in separate modules.
--
-- Claw includes an experimental namespace mechanism. Currently, we
-- support only one global namespace via `#foo:`, which prefixes all
-- following words. I'm contemplating an extension for qualified 
-- namespaces to provide concise access to words with large prefixes.
-- 
module Wikilon.Claw
    ( ClawCode(..)
    , ClawOp(..)
    , Namespace
    , ClawInt
    , ClawRatio(..)
    , ClawExp10(..)
    , ClawDecimal
    , ClawCmdSeq, IsEscCmd
    , clawToABC, clawToABC'
    , clawFromABC, clawFromABC'
    , isInlineableText

    , encode
    , encode'

    , decode
    , runDecoder
    , DecoderState(..)
    , DecoderCont(..)

    , PrimOp(..)
    , module Wikilon.Word
    , module Wikilon.Token
    , module Wikilon.Text
    ) where

import Control.Exception (assert)
import Control.Arrow (first)
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Char
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.Builder as BB
import qualified Data.List as L
import qualified Data.Decimal as D
import Data.String (IsString(..))
import Wikilon.Word
import Wikilon.Text
import Wikilon.Token
import Wikilon.ABC.Pure (ABC(..), Op(..), PrimOp(..))
import qualified Wikilon.ABC.Pure as ABC

-- | Command Language for Awelon (claw)
newtype ClawCode = ClawCode { clawOps :: [ClawOp] } 
    deriving (Eq, Ord, Monoid)

-- | A region of claw code has exactly one namespace. This serves as
-- a prefix for all words within that region of code. Claw namespace
-- is the only source of context sensitivity in claw code.
--
-- A namespace #foo: expands to `[{&_foo:_}]%`.
type Namespace = UTF8.ByteString

-- | Representation for a single Claw operation.
data ClawOp 
    = NS !Namespace     -- #bar:
    | CW !Word          -- mul inc
    | T0 !Text          -- escaped text
    | K0 !Token         -- escaped token
    | P0 !PrimOp        -- escaped ABC ops
    | B0 !ClawCmdSeq    -- escaped block 
    | NI !ClawInt       -- integer
    | NR !ClawRatio     -- ratio
    | ND !ClawDecimal   -- decimal
    | NE !ClawExp10     -- exponential
    | TL !ABC.Text      -- text literal
    | BC !ClawCmdSeq    -- block
    | AT ![Attrib]      -- attributes list 
    deriving (Ord, Eq)

type ClawCmdSeq = [ClawCode] -- non-empty
type IsEscCmd = Bool
type ClawInt = Integer
type ClawDecimal = D.Decimal -- current limit 255 decimal places
data ClawRatio = ClawRatio !ClawInt !ClawInt deriving (Eq, Ord)
data ClawExp10 = ClawExp10 !ClawDecimal !ClawInt deriving (Eq, Ord)
type Attrib = Word

instance Show ClawRatio where 
    showsPrec _ (ClawRatio n d) = shows n . showChar '/' . shows d
instance Show ClawExp10 where 
    showsPrec _ (ClawExp10 c e) = shows c . showChar 'e' . shows e    

-- TODO: maybe develop a fast Claw parse from a raw 'AODef' bytestring?

wLiteral, wBlock, wSeq :: Word
wInteger, wRatio, wDecimal, wExp10 :: Word

wLiteral = "lit"
wBlock = "block"
wSeq  = "seq"
wInteger = "int"
wRatio = "ratio"
wDecimal = "decimal"
wExp10 = "exp10"

-- under consideration: a simple expansion rule from `,` to `comma`
-- treating commas as whitespace.

-- | Convert claw code to ABC for interpretation or storage. This is
-- essentially a 'compiler' for Claw code, though resulting bytecode 
-- will usually need linking and processing for performance.
clawToABC :: ClawCode -> ABC
clawToABC = clawToABC' BS.empty

clawToABC' :: Namespace -> ClawCode -> ABC
clawToABC' ns = expand ns . clawOps

expand :: Namespace -> [ClawOp] -> ABC
expand ns = mconcat . zns opToABC ns 

zns :: (Namespace -> ClawOp -> a) -> Namespace -> [ClawOp] -> [a]
zns f _ (op@(NS ns) : ops) = f ns op : zns f ns ops
zns f ns (op : ops) = f ns op : zns f ns ops
zns _ _ [] = []

oneOp :: ABC.Op -> ABC
oneOp = ABC . return

opToABC :: Namespace -> ClawOp -> ABC
-- low level
opToABC _ (NS ns) = atToABC [nsAttr] where
    nsAttr = Word $ mconcat ["_",ns,"_"]
opToABC ns (CW (Word w)) = oneOp . ABC_Tok $ wordTok where
    wordTok = Token $ mconcat ["%", ns, w]
opToABC _ (P0 op) = oneOp $ ABC_Prim op
opToABC _ (T0 txt) = oneOp $ ABC_Text txt
opToABC _ (K0 tok) = oneOp $ ABC_Tok tok
opToABC ns (B0 bs) = oneOp $ ABC_Block abc where
    abc = clawToABC' ns (compactSeq bs) 

-- expansions
opToABC ns (NI i) = expand ns (escInt ++ [CW wInteger]) where
    escInt = fmap P0 $ ABC.itoabc' i 
opToABC ns (NR (ClawRatio n d)) = expand ns [NI n, NI d, CW wRatio]
opToABC ns (ND (D.Decimal dp m)) = 
    expand ns [NI m, NI (fromIntegral dp), CW wDecimal]
opToABC ns (NE (ClawExp10 d x)) = expand ns [ND d, NI x, CW wExp10]
opToABC ns (TL lit) = expand ns [T0 lit, CW wLiteral]
opToABC ns (BC cc) = expand ns [B0 cc, CW wBlock]
opToABC _ (AT lst) = atToABC lst

atToABC :: [Attrib] -> ABC
atToABC lWords = ABC [attrBlock, ABC_Prim ABC_drop] where 
    lToks = fmap (Token . mappend "&" . unWord) lWords
    attrBlock = ABC_Block $ ABC $ fmap ABC_Tok lToks 

-- | Translate a sequence back to a single command.
compactSeq :: ClawCmdSeq -> ClawCode
compactSeq [cc] = cc
compactSeq (c0:cs) = c0 <> ClawCode [B0 cs, CW wSeq]
compactSeq [] = assert False mempty

-- | Parse Claw structure from bytecode.
clawFromABC :: ABC -> ClawCode
clawFromABC = clawFromABC' BS.empty

-- | parse Claw code from bytecode. This requires the current
-- namespace in order to provide some useful context.
clawFromABC' :: Namespace -> ABC -> ClawCode
clawFromABC' ns = ClawCode . reduceClaw . escABC ns . ABC.abcOps

-- | recognize claw words from bytecode 
--
--   {%foo:word} → word 
--
-- if namespace is `foo:` 
--   and `word` doesn't start with digits, etc.
escWord :: Namespace -> Token -> Maybe Word
escWord ns (Token tok) = case BS.uncons tok of
    Just ('%', fullWord) ->
        let bOkPrefix = ns `BS.isPrefixOf` fullWord in
        let w = Word $ BS.drop (BS.length ns) fullWord in
        guard (bOkPrefix && isValidWord w) >> return w
    _ -> Nothing

-- | recognize claw namespace from attribute list.
-- Our namespace attribute has the form (_foo:_).
escNS :: [Attrib] -> Maybe Namespace
escNS [Word s] = case BS.split '_' s of
    ["", expansion, prefix] ->
        guard (BS.null prefix) >> 
        return expansion
    _ -> mzero
escNS _ = mzero

-- | Identify SP and LF identity operators from ABC-layer formatting.
abcWS :: ABC.PrimOp -> Bool
abcWS ABC_SP = True
abcWS ABC_LF = True
abcWS _ = False

-- | recognize basic claw operations and handle namespace context.
-- This will also filter all ABC_SP and ABC_LF elements from the
-- input to simplify processing and automate formatting.
escABC :: Namespace -> [ABC.Op] -> [ClawOp]
escABC ns (ABC_Tok tok : ops) 
  | Just w <- escWord ns tok = CW w : escABC ns ops
  | otherwise = K0 tok : escABC ns ops
escABC ns (ABC_Prim op : ops)  
  | abcWS op = escABC ns ops -- ignore whitespace
  | otherwise = P0 op : escABC ns ops
escABC ns (ABC_Text txt : ops) = T0 txt : escABC ns ops
escABC ns (ABC_Block b : ABC_Prim ABC_drop : ops)
    | Just lst <- escAttribs (abcOps b)
    = case escNS lst of
        Just ns' -> NS ns' : escABC ns' ops
        Nothing -> AT lst : escABC ns ops
escABC ns (ABC_Block abc : ops) = B0 [cc] : escABC ns ops where
    cc = ClawCode $ escABC ns (ABC.abcOps abc)
escABC _ [] = []

-- | Extract annotations from a block consisting only of the
-- annotations. Assume context where block is immediately dropped.
escAttribs :: [ABC.Op] -> Maybe [Attrib]
escAttribs = go [] where
    go r (ABC_Tok (aw -> Just w) : ops) = go (w:r) ops
    go r (ABC_Prim op : ops) | abcWS op = go r ops
    go r [] = return (L.reverse r)
    go _ _ = mzero
    aw (Token t) = case BS.uncons t of
        Just ('&',w) -> return (Word w)
        _ -> mzero


-- | collapse structured values from lower level claw code.
--
-- Targets:
--  blocks & cmd sequences.
--  inline literals
--  integers
--  ratios 
--  decimals
--  exponential notation (base 10)
--
-- At the moment, this is not a streaming reducer but rather a
-- zipper-based implementation. 
--
reduceClaw :: [ClawOp] -> [ClawOp]
reduceClaw = L.reverse . rdz []

-- note: returned claw ops are in reverse order.
--
-- This is convenient for recognizing command sequences
-- without performing extra list reversals.
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

-- recursion on blocks & seq recognition
rdz lhs (B0 [cmd] : rhs) = rdz (B0 cseq : lhs) rhs where 
    cseq = case rdz [] (clawOps cmd) of
        (CW w : B0 cs : rc) | (w == wSeq) -> (c:cs) where
            c = ClawCode (L.reverse rc) 
        rc -> [ClawCode (L.reverse rc)] -- full block
rdz lhs (op : rhs) = rdz (op : lhs) rhs -- progress
rdz lhs [] = lhs -- termination

-- | Parse raw integer (e.g. #42) from a reverse-ordered list (24#).
-- Return remaining operations and the integer so parsed, assuming
-- one could be parsed. 
parseIntR :: [ClawOp] -> Maybe ([ClawOp], ClawInt)
parseIntR = run where
  run ops = 
    collectR id ops >>= \(fn, ops') ->
    return (ops', fn 0)
  collectR fn (P0 op : ops)
    | Just f <- intOp op = collectR (fn . f) ops -- include value
    | ABC_newZero <- op = Just (fn, ops) -- done
    | abcWS op = collectR fn ops
    | otherwise = Nothing
  collectR _ _ = Nothing -- 

intOp :: ABC.PrimOp -> Maybe (ClawInt -> ClawInt)
intOp (digitOp -> Just d) = Just step where
    step !n = ((10*n)+d)
intOp ABC_negate = Just negate
intOp _ = Nothing

digitOp :: ABC.PrimOp -> Maybe ClawInt
digitOp = digitFromChar . ABC.abcOpToChar

digitFromChar :: Char -> Maybe ClawInt
digitFromChar !c =
    let bOK = ('0' <= c) && (c <= '9') in
    if not bOK then mzero else
    return $! fromIntegral $ ord c - ord '0'

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
encodeOp (TL txt) = BB.char8 '"' <> BB.lazyByteString txt <> BB.char8 '"'
encodeOp (BC cc) = encBlock cc
encodeOp (NI i) = BB.string8 (show i)
encodeOp (ND d) = BB.string8 (show d)
encodeOp (NR r) = BB.string8 (show r) 
encodeOp (NE e) = BB.string8 (show e)
encodeOp (P0 op) -- should not happen normally
    | abcWS op = mempty
    | otherwise = BB.char8 '\\' <> c
    where c = BB.char8 $ ABC.abcOpToChar op
encodeOp (T0 txt) = encMultiLineText txt
encodeOp (K0 tok) = BB.char8 '\\' <> encTok tok
encodeOp (B0 cc) = BB.char8 '\\' <> encBlock cc
encodeOp (AT lst) = BB.char8 '(' <> encodeOps (fmap CW lst) <> BB.char8 ')'

-- Claw blocks generalize into command sequences, i.e. allowing
-- a simple escape from a block via `,` to support monadic programs
-- and concise data lists.
--
-- I'll probably need to do some smart formatting, eventually, i.e.
-- with some awareness of appropriate spacing. This could be supported
-- by attributes.
encBlock :: ClawCmdSeq -> BB.Builder
encBlock = enc where
    enc cs = BB.char8 '[' <> sep cs <> BB.char8 ']'
    sep = mconcat . L.intersperse cmdSep . fmap encode'
    cmdSep = BB.char8 ','

encTok :: Token -> BB.Builder
encTok (Token tok) = BB.char8 '{' <> BB.byteString tok <> BB.char8 '}'

-- Claw's encoding of multi-line texts.
encMultiLineText :: Text -> BB.Builder
encMultiLineText txt =
    -- multi-line text with full padding
    BB.char8 '\\' <> BB.char8 '"' <> BB.char8 '\n' <>
    let encLn ln = BB.char8 '\n' <> BB.char8 ' ' <> BB.lazyByteString ln in
    mconcat (fmap encLn (uncurry (:) (textLines txt))) <>
    BB.char8 '\n' <> BB.char8 '\n' <> BB.char8 '~' 

instance Show ClawOp where
    showsPrec _ = showList . (:[])
    showList = (++) . LazyUTF8.toString . BB.toLazyByteString . encodeOps
instance Show ClawCode where
    showsPrec _ = showList . clawOps


-- | Decode Claw from text, e.g. from a command line. Text decodes
-- into a sequence of claw operations, so you'll additionally need
-- to know the namespace (or require one be provided with the text).
--
-- If the decoder is 'stuck' at any point, we'll return the final
-- decoder state. This allows more precise error reports to the
-- client.
decode :: LazyUTF8.ByteString -> Either DecoderState ClawCode
decode t = runDecoder $ DecoderState
    { dcs_text = t
    , dcs_cont = DecodeDone
    , dcs_ws = True
    , dcs_ops = []
    }

runDecoder :: DecoderState -> Either DecoderState ClawCode
runDecoder dcs = decode' cc bWS ops txt where
    cc = dcs_cont dcs
    bWS = dcs_ws dcs
    ops = dcs_ops dcs
    txt = dcs_text dcs 

-- | we provide parse errors up to an operation within a block.
data DecoderCont 
    = DecodeDone
    | DecodeBlock IsEscBlock ClawCmdSeq [ClawOp] DecoderCont  
    deriving (Show)
type IsEscBlock = Bool
data DecoderState = DecoderState
    { dcs_text :: LazyUTF8.ByteString   -- ^ text to parse
    , dcs_cont :: DecoderCont           -- ^ location in hierarchical blocks
    , dcs_ws   :: Bool                  -- ^ recently seen a word separator?
    , dcs_ops  :: [ClawOp]              -- ^ operators parsed, reverse order
    } deriving (Show)

decode' :: DecoderCont -> Bool -> [ClawOp] -> LazyUTF8.ByteString -> Either DecoderState ClawCode
decode' cc bWS r txt0 =
    let decoderIsStuck = Left (DecoderState txt0 cc bWS r) in
    case LBS.uncons txt0 of
        Nothing -> case cc of
            DecodeDone -> Right $ ClawCode $ L.reverse r
            _ -> decoderIsStuck
        Just (c, txt) -> case c of
            -- special case elements for word separators
            ' ' -> decode' cc True r txt
            '\n' -> decode' cc True r txt
            ',' -> case cc of
                DecodeBlock bEsc cseq ops cc' -> 
                    let cmd = ClawCode (L.reverse r) in
                    decode' (DecodeBlock bEsc (cmd:cseq) ops cc') True [] txt
                _ -> decoderIsStuck
            ']' -> case cc of
                DecodeBlock bEsc cseq ops cc' -> decode' cc' False (b:ops) txt where
                    cmd = ClawCode (L.reverse r)
                    bType = if bEsc then B0 else BC
                    b = bType (L.reverse (cmd:cseq))
                _ -> decoderIsStuck

            -- everything else requires a word separator
            _ | not bWS -> decoderIsStuck

            '[' -> decode' (DecodeBlock False [] r cc) True [] txt
            '"' -> case LBS.elemIndex '"' txt of
                Nothing -> decoderIsStuck
                Just idx ->
                    let (lit, litEnd) = LBS.splitAt idx txt in
                    let bOK = LBS.notElem '\n' lit in
                    if not bOK then decoderIsStuck else
                    decode' cc False (TL lit : r) (LBS.drop 1 litEnd)
            '#' -> 
                let (lns, txt') = LazyUTF8.span isValidWordChar txt in
                let ns = LBS.toStrict lns in
                decode' cc False (NS ns : r) txt'
            '(' -> 
                let (attrs, attrsEnd) = decodeAttribs txt in
                case LBS.uncons attrsEnd of
                    Just (')', txt') -> decode' cc False (AT attrs : r) txt'
                    _ -> decoderIsStuck

            -- escapes
            '\\' -> case LBS.uncons txt of -- escaped content
                Nothing -> decoderIsStuck
                Just (c', escTxt) -> case c' of
                    '[' -> decode' (DecodeBlock True [] r cc) True [] escTxt
                    '"' -> 
                        let (lit,litEnd) = clawTakeText escTxt in
                        case LBS.uncons litEnd of
                            Just ('~', txt') -> decode' cc False (T0 lit : r) txt'
                            _ -> decoderIsStuck
                    '{' -> case LBS.elemIndex '}' escTxt of
                        Nothing -> decoderIsStuck
                        Just idx -> 
                            let (lzt, tokEnd) = LBS.splitAt idx escTxt in
                            let tok = Token (LBS.toStrict lzt) in
                            tok `seq` decode' cc False (K0 tok : r) (LBS.drop 1 tokEnd)
                    (charToEscPrim -> Just op0) ->
                        let loop ops t = case takeEscPrim t of
                                Just (op, t') -> loop (P0 op : ops) t'
                                Nothing -> (ops, t)
                        in
                        let (r', txt') = loop (P0 op0 : r) escTxt in
                        decode' cc False r' txt'
                    _ -> decoderIsStuck -- not a recognized escape

            _ -> case decodeWordOrNumber txt0 of
                Just (op, txt') -> decode' cc False (op:r) txt'
                _ -> decoderIsStuck


takeEscPrim :: ABC.Text -> Maybe (ABC.PrimOp, ABC.Text)
takeEscPrim txt =
    LBS.uncons txt >>= \ (c, txt') ->
    charToEscPrim c >>= \ op ->
    return (op, txt')

-- any primitive except ABC_SP and ABC_LF
charToEscPrim :: Char -> Maybe ABC.PrimOp
charToEscPrim c = 
    ABC.abcCharToOp c >>= \ op ->
    guard (not (abcWS op)) >>
    return op

-- Parse multi-line Claw text after having read the \" prefix. 
--
-- Each line is `LF SP (line of text)`. `LF LF` is treated as `LF SP LF`.
-- In a correctly encoded text, the following character is `~`.
--
-- If available, we eliminate one empty line each from the start and end
-- of our texts. This allows some vertical space as padding, which I find
-- easier to read for large texts. 
--
-- I'll ignore spaces following `\"`. 
-- I'll accept `\"~` as a simple empty text.
--
clawTakeText :: LBS.ByteString -> (Text, LBS.ByteString)
clawTakeText = run where
    skipSP = LBS.dropWhile (== ' ')
    run s = fromMaybe (mempty,s) $ 
        LBS.uncons (skipSP s) >>= \ (c, s') ->
        guard ('\n' == c) >>
        return (first toText (tt [] s'))
    -- at `tt` we're just past reading a final `\n`.
    -- every line of text will include the terminal `\n`.
    tt :: [Text] -> Text -> ([Text],Text)
    tt r s = fromMaybe (r,s) $
        LBS.uncons s >>= \ (c, s') ->
        if ('\n' == c) then return (tt (el:r) s') else
        guard (' ' == c) >>
        LBS.elemIndex '\n' s' >>= \ ix ->
        let (line,more) = LBS.splitAt (ix+1) s' in
        return (tt (line:r) more)
    el = LBS.singleton '\n'
    toText = trimPadStart . mconcat . L.reverse . trimLFTerm . trimPadEnd
    -- drop one pad line above multi-line text
    trimPadStart s = case LBS.uncons s of { Just ('\n', s') -> s'; _ -> s }
    -- drop one pad line below multi-line text
    trimPadEnd (l:r) | (l == el) = r
    trimPadEnd r = r
    -- drop final LF from the `LF ~` text terminator.
    trimLFTerm (l:r) = ((LBS.init l) : r) 
    trimLFTerm [] = []

decodeWordOrNumber :: ABC.Text -> Maybe (ClawOp, ABC.Text)
decodeWordOrNumber txt = dn <|> dw where
    dn = decodeNumber txt
    dw = decodeWord txt >>= \ (w, txt') -> return (CW w, txt')
    
decodeWord :: ABC.Text -> Maybe (Word, ABC.Text)
decodeWord txt =
    let (s, txt') = LazyUTF8.span isValidWordChar txt in
    let w = Word (LBS.toStrict s) in
    guard (isValidWord w) >>
    return (w, txt')

-- decodes a sequence of whitespace separated attributes
-- I'll be lenient here for decoding commas, too.
decodeAttribs :: ABC.Text -> ([Attrib], ABC.Text)
decodeAttribs = more [] where
    more r = load r . LBS.dropWhile isWS
    isWS c = (' ' == c) || ('\n' == c) || (',' == c) 
    load r s = case decodeWord s of
        Just (w, s') -> more (w:r) s'
        Nothing -> (L.reverse r, s)

-- | decode NI, NR, ND, or NE. (Or Nothing.)
decodeNumber :: ABC.Text -> Maybe (ClawOp, ABC.Text)
decodeNumber txt = de <|> ir where 
    ir = decodeInteger txt >>= \ (n, txtAfterNum) ->
         case LBS.uncons txtAfterNum of
            Just ('/', txtDenom) -> 
                decodePosInt txtDenom >>= \ (d, txtAfterDenom) ->
                return (NR (ClawRatio n d), txtAfterDenom)
            _ -> return (NI n, txtAfterNum)
    de = 
        decodeDecimal txt >>= \ (c, txtAfterDecimal) ->
        case LBS.uncons txtAfterDecimal of
            Just ('e', txtExp10) ->
                decodeInteger txtExp10 >>= \ (e, txtAfterExp10) ->
                return (NE (ClawExp10 c e), txtAfterExp10)
            _ -> return (ND c, txtAfterDecimal)

decodeDecimal :: ABC.Text -> Maybe (ClawDecimal, ABC.Text)
decodeDecimal (LBS.uncons -> Just ('-', txt)) = 
    -- simplified handling for negative values
    -- also need to permit '-0.01' and similar
    decodeDecimal txt >>= \ (dAbs, txt') ->
    guard (dAbs > 0) >> -- forbid negative zero
    return (negate dAbs, txt')
decodeDecimal txt =
    decodeInteger txt >>= \ (m0, txtAfterIntPart) ->
    LBS.uncons txtAfterIntPart >>= \ (decimalPoint, txtDecimal) ->
    guard ('.' == decimalPoint) >>
    let (m, dp, txtAfterDecimal) = accumDecimal m0 0 txtDecimal in
    -- at least one decimal place for visual distinction (e.g. 1.0)
    -- at most 255 decimal places due to Data.Decimal limitations
    guard ((0 < dp) && (dp <= 255)) >>
    return (D.Decimal (fromIntegral dp) m, txtAfterDecimal)

-- decode content after the decimal point (a sequence of 0-9 digits)
-- while counting number of digits and accumulating the mantissa.
accumDecimal :: ClawInt -> Int -> ABC.Text -> (ClawInt, Int, ABC.Text)
accumDecimal !m !dp (takeDigit -> Just (d, txt)) =
    accumDecimal ((10*m)+d) (1+dp) txt
accumDecimal !m !dp !txt = (m,dp,txt)

takeDigit :: ABC.Text -> Maybe (ClawInt, ABC.Text)
takeDigit (LBS.uncons -> Just (c, txt)) = 
    digitFromChar c >>= \ d -> return (d, txt)
takeDigit _ = Nothing

decodeInteger :: ABC.Text -> Maybe (ClawInt, ABC.Text)
decodeInteger txt = case LBS.uncons txt of
    Nothing -> Nothing
    Just ('0', txtAfterZero) -> 
        return (0, txtAfterZero)
    Just ('-', txtAfterNeg) ->
        decodePosInt txtAfterNeg >>= \ (n, txtAfterNum) ->
        return (negate n, txtAfterNum)
    Just (c, txtAfterD0) ->
        digitFromChar c >>= \ d0 ->
        return (accumPosInt d0 txtAfterD0)

decodePosInt :: ABC.Text -> Maybe (ClawInt, ABC.Text)
decodePosInt txt =
    takeDigit txt >>= \ (d0, txtAfterD0) ->
    guard (d0 > 0) >> -- start with 1..9
    return (accumPosInt d0 txtAfterD0)

accumPosInt :: ClawInt -> ABC.Text -> (ClawInt, ABC.Text)
accumPosInt !n (takeDigit -> Just (d, txt)) = accumPosInt ((10*n)+d) txt
accumPosInt !n !txt = (n,txt)

instance IsString ClawCode where
    fromString s =
        case decode (LazyUTF8.fromString s) of
            Right cc -> cc
            Left dcs ->
                let sLoc = L.take 40 $ LazyUTF8.toString $ dcs_text dcs in
                error $ clawCodeErr $ "parse failure @ " ++ sLoc

clawCodeErr :: String -> String
clawCodeErr = (++) "Wikilon.Claw: " 

