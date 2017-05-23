{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
-- | The Awelon language, as used by Wikilon.
--
-- Awelon is semantically and syntactically simple. This module will
-- provide concrete parsers and serialization. Interpretation is left
-- to another module.
--
-- Note: for performance, Wikilon assumes a valid UTF-8 encoding and
-- all validation and parsing functions operate at the byte level.
-- But do validate UTF-8 at another layer, as needed.
module Wikilon.Lang
    ( Word(..), Anno(..), NS(..), Text(..)
    , Prog(..), Op(..)
    , encode, encodeBB
    , decode, DecoderStack, DecoderStuck
    , validWord, validWordByte, validAnno, validNS
    , validText, validTextByte
    ) where

import Prelude hiding (Word)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BB
import qualified Data.ByteString.UTF8 as U8
import qualified Data.ByteString.Lazy.UTF8 as LU8
import qualified Data.List as L
import qualified Data.Array.Unboxed as A
import Data.String
import Data.Word (Word8)
import Data.Monoid

-- | Words are the primary user-definable unit of Awelon. A word is
-- identified by an ideally small UTF-8 bytestring.
newtype Word = Word { wordBytes :: BS.ByteString } 
    deriving (Ord, Eq)

-- | Annotations are generally runtime or interpreter defined. They
-- are essentially words wrapped within parentheses, such as (par)
-- to request a parallel evaluation. At this point, the parentheses
-- have been stripped.
newtype Anno = Anno { annoWord :: Word } 
    deriving (Ord, Eq)

-- | Awelon supports namespaces for hierarchical dictionaries. The
-- form `foo@dict` would mean we're using the word `foo` as defined
-- in a child dictionary `dict`. Any operation may be qualified with
-- a namespace - words, blocks, texts, and even namespace themselves
-- may be hierarchical. The namespace must be a valid word.
newtype NS = NS { nsWord :: Word } 
    deriving (Ord, Eq)

-- | Awelon supports inline embedded texts like "hello, world!".
-- Since these texts are inline and relatively small, I'll just
-- encode them as a bytestring.
--
-- Embedded texts may not contain C0, DEL, or " (34). Semantically,
-- we treat a text as a binary list. But it should be valid UTF-8.
--
-- Note: Earlier versions of Awelon did support multi-line texts.
-- But they lack a strong use case to justify the complexity, so 
-- were removed. At this time, multi-line texts are only possible
-- as an editable view or via secure hash resources.
newtype Text = Text { textData :: BS.ByteString } 
    deriving (Ord, Eq, Monoid) 

-- | A basic operation is a word, annotation, text, or block. In
-- addition, any operation may be modified with a namespace, which
-- is potentially hierarchical.
--
-- While this representation is acceptable for parsing, it is not
-- good for efficient interpretation. We still need to bind each
-- word to its definition (or at least support efficient lookup),
-- evaluate definitions and analyze arity, recognize accelerators,
-- track reference attributes (unique, (nc), (nd)), and accelerate
-- numeric, array, sum, and record data types.
--
data Op = OpWord  !Word  -- ^ just a word
        | OpAnno  !Anno  -- ^ (annotation)
        | OpBlock !Prog  -- ^ [block of code]
        | OpText  !Text  -- ^ "embedded text"
        | OpNS !Op !NS    -- ^ Op@NS
    deriving (Ord, Eq)

-- | An Awelon program consists of a sequence of operations. These
-- operations can be understood as manipulating an implicit stack,
-- similarly to Forth. However, Awelon evaluates by rewriting, and
-- output is an equivalent representation of the input program. 
newtype Prog = Prog { progOps :: [Op] } 
    deriving (Ord, Eq, Monoid)

-- | This serializes a program for stream processing.
--
-- This encoder doesn't do anything fancy with word separators. It
-- simply adds a little whitespace between operations. I find this
-- to be aesthetically acceptable, if not optimal.
encode :: Prog -> LBS.ByteString
encode = toLBS . encodeBB where
    toLBS = BB.toLazyByteStringWith strat LBS.empty
    strat = BB.untrimmedStrategy 240 BB.smallChunkSize

-- | In case you need to integrate the program into a larger binary,
-- or desire an alternative allocation strategy.
encodeBB :: Prog -> BB.Builder
encodeBB = mconcat . addWhitespace . fmap opBB . progOps where
    addWhitespace = L.intersperse (BB.word8 32)

opBB :: Op -> BB.Builder
opBB (OpWord w) = BB.byteString (wordBytes w)
opBB (OpAnno a) = BB.word8 40 <> BB.byteString (wordBytes (annoWord a)) <> BB.word8 41
opBB (OpBlock p) = BB.word8 91 <> encodeBB p <> BB.word8 93
opBB (OpText t) = BB.word8 34 <> BB.byteString (textData t) <> BB.word8 34
opBB (OpNS op ns) = opBB op <> BB.word8 64 <> BB.byteString (wordBytes (nsWord ns))

-- | Parse a serialized Awelon program.
-- 
-- Awelon has a simple syntax, like the Forth stream of words plus
-- first class [blocks of code] akin to Lisp S-expressions. But it
-- is possible to have syntax errors, in which case we'll return the
-- final parser state in the left.
decode :: LBS.ByteString -> Either DecoderStuck Prog
decode = dstep [] []

-- | Parser state is a stack of partial programs, the last element
-- in this list corresponding to the outermost program. Each partial
-- program is a stack of operations, i.e. in reverse order.
type DecoderStack = [[Op]]

-- | When decode fails, we simply return the parse state (the stack)
-- together with remaining, unparsed text. This is sufficient for a
-- lightweight parse error diagnosis.
type DecoderStuck = (DecoderStack, LBS.ByteString)

-- | decode input in small steps, with given parser state
dstep :: DecoderStack -> [Op] -> LBS.ByteString -> Either DecoderStuck Prog
dstep cc r s =
    let decoderStuck = Left (r:cc, s) in
    case LBS.uncons s of
        Nothing -> case cc of
            [] -> Right (Prog (L.reverse r)) -- program fully parsed!
            _ -> decoderStuck -- imbalanced blocks (missing ']')
        Just (c, s') -> case c of
            32 -> dstep cc r s' -- skip spaces
            10 -> dstep cc r s' -- skip newlines
            91 {- [ -} -> dstep (r:cc) [] s' -- start of block
            93 {- ] -} -> case cc of         -- end of block
                (ops:cc') -> dstepNS cc' ops block s' where
                    block = OpBlock (Prog (L.reverse r))
                _ -> decoderStuck -- imbalanced blocks (extra ']')
            40 {- ( -} ->  -- annotations
                let mkOp = OpAnno . Anno . Word . LBS.toStrict in
                let (a, eoa) = LBS.span validWordByte s' in
                if (LBS.null a) then decoderStuck else -- empty annotation?
                case LBS.uncons eoa of
                    Just (41, eoa') -> dstepNS cc r (mkOp a) eoa'
                    _ -> decoderStuck -- could not find close parens
            34 {- " -} -> -- embedded texts
                let mkOp = OpText . Text . LBS.toStrict in
                let (t, eot) = LBS.span validTextByte s' in
                case LBS.uncons eot of
                    Just (34, eot') -> dstepNS cc r (mkOp t) eot'
                    _ -> decoderStuck
            _ -> -- otherwise should be a normal word
                let mkOp = OpWord . Word . LBS.toStrict in
                let (w, eow) = LBS.span validWordByte s in
                if LBS.null w then decoderStuck else
                dstepNS cc r (mkOp w) eow

-- | parse the namespace qualifier after any normal operation.
-- Potentially qualify an operation multiple times.
dstepNS :: DecoderStack -> [Op] -> Op -> LBS.ByteString -> Either DecoderStuck Prog
dstepNS cc r op s = 
    let decoderStuck = Left ((op:r):cc, s) in
    case LBS.uncons s of
        Just (64, s') -> -- add namespace
            let mkNS = OpNS op . NS . Word . LBS.toStrict in
            let (ns, eons) = LBS.span validWordByte s' in
            if LBS.null ns then decoderStuck else -- empty NS?
            dstepNS cc r (mkNS ns) eons
        _ -> dstep cc (op:r) s -- no NS qualifier

-- take text data, escaping characters as needed. 
takeText :: LBS.ByteString -> (LBS.ByteString, LBS.ByteString) 
takeText s = case LBS.uncons s of
    Just (10, s') -> takeTextML s'
    _ -> case LBS.elemIndex 34 s of
        Just ix -> LBS.splitAt ix s
        _ -> (s, mempty) -- unterminated text

-- obtain multi-line texts. This assumes we start at a new line
-- of text. Empty lines are accepted without escapes, otherwise
-- any non-empty line must be escaped.
--
-- Note: "", "\n", and "\n\n" each refer to the empty string.
takeTextML :: LBS.ByteString -> (LBS.ByteString, LBS.ByteString)
takeTextML = lineStart [] where
    fin = mconcat . L.intersperse lf . L.reverse
    lf = LBS.singleton 10 
    lineStart r s = case LBS.uncons s of
        Just (32, s') -> lineEsc r s' -- escaped non-empty line
        Just (10, s') -> lineStart (mempty : r) s' -- empty line
        _ -> (fin r, s) -- end of text (usually ")
    lineEsc r s = case LBS.elemIndex 10 s of
        Just ix -> lineStart ((LBS.take ix s):r) (LBS.drop (ix+1) s)
        _ -> (fin (s:r), mempty) -- unterminated text

instance Show Prog where 
    showsPrec _ = showString . LU8.toString . encode 

instance IsString Prog where
    fromString s = case decode (LU8.fromString s) of
            Right prog -> prog
            Left _ -> error $ "invalid Awelon program, received: " ++ s

-- render an operator as a singleton program. It might be a block,
-- so treating this as a large program is reasonable.
instance Show Op where
    showsPrec _ = shows . Prog . (:[])

-- in general, we might parse a [block of code]@ns, so we need the
-- full parser for a single operator.
instance IsString Op where
    fromString s = case fromString s of {- using `IsString Prog` -}
            (Prog [op]) -> op
            _    -> error $ "expecting one Awelon operation, received: " ++ s

-- we'll assume a valid word for efficient display
instance Show Word where 
    showsPrec _ = showString . U8.toString . wordBytes

-- run the proposed word through the full Awelon parser
instance IsString Word where 
    fromString s = case fromString s of {- using `IsString Op` -}
        OpWord w -> w
        _ -> error $ "expecting Awelon word, received: " ++ s

validWord :: Word -> Bool
validWord = validWordToken . wordBytes

validWordToken :: BS.ByteString -> Bool
validWordToken s = not (BS.null s) && BS.all validWordByte s

c0del, invalidUTF8Bytes :: [Word8]
c0del = [0..31] ++ [127]
invalidUTF8Bytes = [192,193] ++ [245..255]

-- Word blacklist is: @#[]()<>{}\/,;|&='"`, SP, C0 (0-31), and DEL.
-- We can also forbid a few bytes never permitted in UTF-8.
wordBytesArray :: A.UArray Word8 Bool
wordBytesArray = A.listArray (0,255) $ fmap accept [0..255] where
    accept = flip L.notElem blacklist
    blacklist = [32,34,35,38,39,40,41,44,47,59,60,61,62
               ,64,91,92,93,96,123,124,125] 
            ++ c0del ++ invalidUTF8Bytes

validWordByte :: Word8 -> Bool
validWordByte = (A.!) wordBytesArray

instance Show Anno where 
    showsPrec _ a = showChar '(' . shows (annoWord a) . showChar ')'

instance IsString Anno where
    fromString s = case fromString s of {- using `IsString Op` -}
        OpAnno a -> a
        _ -> error $ "expecting valid Awelon annotation, received: " ++ s 

validAnno :: Anno -> Bool
validAnno = validWord . annoWord

-- namespaces in Awelon are second class, so don't appear by
-- themselves. But we may simply add the `@` prefix to a word.
instance Show NS where 
    showsPrec _ ns = showChar '@' . shows (nsWord ns)

-- require `@word` format
instance IsString NS where 
    fromString s = case fromString ('_':s) of
        OpNS _ ns -> ns
        _ -> error $ "expecting valid Awelon namespace, received: " ++ s

validNS :: NS -> Bool
validNS = validWord . nsWord

-- texts are converted directly, 
instance Show Text where
    showsPrec _ = shows . U8.toString . textData

instance IsString Text where 
    fromString s =
        let t = Text (U8.fromString s) in
        if validText t then t else
        error $ "invalid Awelon text: " ++ s

-- blacklist for normal characters in a text.
textBytesArray :: A.UArray Word8 Bool
textBytesArray = A.listArray (0,255) $ fmap accept [0..255] where
    accept = flip L.notElem blacklist
    blacklist = [34] ++ c0del ++ invalidUTF8Bytes

validTextByte :: Word8 -> Bool
validTextByte = (A.!) textBytesArray

-- | This only tests for forbidden characters (C0 except LF, DEL).
-- The assumption is the text has a valid UTF-8 encoding.
validText :: Text -> Bool
validText = BS.all validTextByte . textData


