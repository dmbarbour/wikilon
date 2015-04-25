{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

-- | Wikilon's internal variation on Awelon Bytecode (ABC).
--
-- While ABC has many nice properties, it doesn't perform nicely when
-- directly interpreted. Direct interpretation of bytecode is very
-- convenient for reasons of simplicity and extensibility. Wikilon's
-- internal variation on ABC is designed with the following goals:
--
-- * can be converted trivially to or from pure ABC
-- * has better performance for direct interpretion
-- 
-- In the long term, I would like to properly compile ABC down to
-- machine code or try JIT based on Lambdachine or System.Plugins.
-- But Wikilon's ABC should be sufficient for early developments.
--
-- At the moment, the following performance techniques are applied:
--
-- * bytecode is preserved in compact, lazy bytestrings 
-- * fast slicing for texts, blocks, and tokens in code
-- * quoted values are held separately from the bytecode
-- * ABCD-like extended dictionary of accelerated ops
-- 
-- Additional optimizations are viable through the Value model, which
-- is described in a separate module Wikilon.ABC.Value.
module Wikilon.ABC.Code
    ( ABC(..)
    , Bound(..)
    , Op(..)
    , PrimOp(..)
    , ExtOp(..)
    , Text
    , Token
    , Bytes
    , decodeOp, decodeOps, encodeOps
    , extOpTable, extCharToOp, extOpToChar, extOpToABC
    ) where

import Prelude hiding (null)
import Control.Monad
import Data.Monoid
import qualified Data.Array.IArray as A
import qualified Data.List as L
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.UTF8 as UTF8

import Awelon.ABC (PrimOp(..), Quotable(..), quote, quoteList, quotesList)
import qualified Awelon.ABC as Pure

import Wikilon.ABC.Util

type Token = UTF8.ByteString
type Text = LazyUTF8.ByteString
type Bytes = LBS.ByteString

-- | Wikilon's internal representation of Awelon Bytecode (ABC).
data ABC v = ABC 
    { abc_code :: !Bytes    -- ^ Wikilon internal ABC variant 
    , abc_data :: ![Bound v] -- ^ stack of precomputed values
    } deriving (Eq)

-- | Test whether ABC is empty. Empty ABC has identity behavior.
null :: ABC v -> Bool
null abc = L.null (abc_data abc) && LBS.null (abc_code abc)

-- | Each binding corresponds to either a quoted value or a block.
-- There should be a precise correspondence between ABC code and
-- the set of bindings.
data Bound v
    = BBlock ![Bound v]
    | BQuote v
    deriving (Eq)

-- | Individual operations.
data Op v
    -- | Normal ABC
    = ABC_Prim  !PrimOp
    | ABC_Block !(ABC v)
    | ABC_Text  !Text
    | ABC_Tok   !Token
    -- | Performance Extensions
    | ABC_Ext   !ExtOp
    | ABC_Quote v
    deriving (Eq)

-- | Extended Operations are essentially a dictionary recognized by
-- Wikilon for specialized implementations, e.g. to accelerate an
-- interpreter or support tail-call optimizations. ExtOp is similar
-- to ABCD, but is entirely internal to Wikilon and thus does not
-- need careful standardization.
data ExtOp
    -- tail-call operators
    = ExtOp_Inline -- vr$c (full inline)
    | ExtOp_Apc    -- $c (tail call)

    -- favorite fixpoint function
    | ExtOp_Fixpoint -- [^'ow^'zowvr$c]^'ow^'zowvr$c

    -- mirrored v,c operations
    | ExtOp_Intro1L  -- vvrwlc
    | ExtOp_Elim1L   -- vrwlcc
    | ExtOp_Intro0L  -- VVRWLC
    | ExtOp_Elim0L   -- VRWLCC

    | ExtOp_prim_swap    -- vrwlc
    | ExtOp_prim_mirror  -- VRWLC

    -- annotations? {&fork}, {&trace}, {&≡}, etc.
    --  not sure it's worthwhile though, these aren't frequent and
    --  recognition overhead is marginal compared to the processing.

    -- stack swaps?
    -- hand manipulations?
    -- more as needed!
    deriving (Ord, Eq, Bounded, Enum, A.Ix)

-- | Decode ABC into a list of operations. This assumes the bytecode
-- is correctly encoded, though will report an error if necessary.
decodeOps :: ABC v -> [Op v]
decodeOps abc = case decodeOp abc of
    Just (op, abc') -> op : decodeOps abc'
    Nothing | null abc -> []
    _ -> impossible "invalid bytecode!"

-- | Read an operation from the ABC bytes. Returns Nothing if the
-- input is empty or contains invalid bytecode.
decodeOp :: ABC v -> Maybe (Op v, ABC v)
decodeOp abc = 
    LazyUTF8.uncons (abc_code abc) >>= \ (c, bs) ->
    case c of
        (Pure.abcCharToOp -> Just primOp) ->
            let op = ABC_Prim primOp in
            let abc' = abc { abc_code = bs } in
            return (op, abc')
        '[' ->
            sizedSliceFby ']' bs >>= \ (bcode, _code') ->
            takeBBlock (abc_data abc) >>= \ (bdata, _data') ->
            let blockABC = ABC { abc_code = bcode, abc_data = bdata } in
            let op = ABC_Block blockABC in
            let abc' = ABC { abc_code = _code', abc_data = _data' } in
            return (op, abc')
        '"' ->
            sizedSliceFby '~' bs >>= \ (txt, _code') ->
            let op = ABC_Text txt in
            let abc' = abc { abc_code = _code' } in
            return (op, abc')
        '{' ->
            sizedSliceFby '}' bs >>= \ (lzTok, _code') ->
            let op = ABC_Tok (LBS.toStrict lzTok) in
            let abc' = abc { abc_code = _code' } in
            return (op, abc')
        '»' ->
            takeBQuote (abc_data abc) >>= \ (v, vs) ->
            let op = ABC_Quote v in
            let abc' = ABC { abc_code = bs, abc_data = vs } in
            return (op, abc')
        (extCharToOp -> Just extOp) ->
            let op = ABC_Ext extOp in
            let abc' = abc { abc_code = bs } in
            return (op, abc')
        _ -> mzero

-- expecting data for a block
takeBBlock :: [Bound v] -> Maybe ([Bound v], [Bound v])
takeBBlock (BBlock x : xs) = Just (x, xs)
takeBBlock _ = Nothing

-- expecting data for a quotation
takeBQuote :: [Bound v] -> Maybe (v, [Bound v])
takeBQuote (BQuote x : xs) = Just (x, xs)
takeBQuote _ = Nothing

-- | Encode and compact a list of Wikilon's ABC operators into ABC.
-- This preserves the given code, no simplification is performed.
encodeOps :: [Op v] -> ABC v
encodeOps = extract . L.foldl' (flip encOp) mempty where
    extract (bb,rv) = 
        let _code = BB.toLazyByteString bb in
        let _data = L.reverse rv in
        ABC { abc_code = _code, abc_data = _data }

type EncSt v = (BB.Builder, [Bound v])

encOp :: Op v -> EncSt v -> EncSt v
encOp (ABC_Prim op) = encPrimOp op
encOp (ABC_Block abc) = encBlock abc
encOp (ABC_Text txt) = encText txt
encOp (ABC_Tok tok) = encTok tok
encOp (ABC_Ext op) = encExtOp op
encOp (ABC_Quote v) = encQuote v

{-# INLINE encPrimOp #-}
{-# INLINE encBlock #-}
{-# INLINE encText #-}
{-# INLINE encTok #-}
{-# INLINE encExtOp #-}
{-# INLINE encQuote #-}

encPrimOp :: PrimOp -> EncSt v -> EncSt v
encPrimOp op (bb,rv) = (bb <> opc, rv) where
    opc = BB.charUtf8 (Pure.abcOpToChar op)

encBlock :: ABC v -> EncSt v -> EncSt v
encBlock abc (bb,rv) = (bb <> s, bv:rv) where
    s = BB.char8 '[' <> encSizedSlice (abc_code abc) <> BB.char8 ']'
    bv = BBlock (abc_data abc)

encText :: Text -> EncSt v -> EncSt v
encText txt (bb, rv) = (bb <> s, rv) where
    s = BB.char8 '"' <> encSizedSlice txt <> BB.char8 '~'

encTok :: Token -> EncSt v -> EncSt v
encTok tok (bb, rv) = (bb <> s, rv) where
    lzTok = LBS.fromStrict tok
    s = BB.char8 '{' <> encSizedSlice lzTok <> BB.char8 '}'

encExtOp :: ExtOp -> EncSt v -> EncSt v
encExtOp op (bb, rv) = (bb <> opc, rv) where
    opc = BB.charUtf8 (extOpToChar op)

encQuote :: v -> EncSt v -> EncSt v
encQuote v (bb,rv) = (bb <> unQuote, bv:rv) where
    unQuote = BB.charUtf8 '»'
    bv = BQuote v

-- | Table of ExtOps and relevant semantics. 
--
-- I'll follow ABCD's mandate to leave the ASCII range to future ABC
-- expansions (though such expansions are very unlikely). ExtOps is
-- effectively a prototype for ABCD.
extOpTable :: [(ExtOp, Char, Pure.ABC)]
extOpTable =
    [(ExtOp_Inline,'£',"vr$c")
    ,(ExtOp_Apc,'¢',"$c")
    ,(ExtOp_Fixpoint,'¥',"[^'ow^'zowvr$c]^'ow^'zowvr$c")
    ,(ExtOp_Intro1L,'ń',"vvrwlc")
    ,(ExtOp_Elim1L,'ć',"vrwlcc")
    ,(ExtOp_Intro0L,'Ń',"VVRWLC")
    ,(ExtOp_Elim0L,'Ć',"VRWLCC")
    ,(ExtOp_prim_swap,'ś',"vrwlc")
    ,(ExtOp_prim_mirror,'Ś',"VRWLC")
    ]

extCharOpArray :: A.Array Char (Maybe ExtOp)
extCharOpArray = A.accumArray ins Nothing (lb,ub) lst where
    ins _ op = Just op
    lb = L.minimum (fmap fst lst)
    ub = L.maximum (fmap fst lst)
    lst = fmap (\(op,c,_d) -> (c,op)) extOpTable

extOpDataArray :: A.Array ExtOp (Char, Pure.ABC)
extOpDataArray = A.accumArray ins eUndef (minBound,maxBound) lst where
    lst = fmap (\(op,c,d)->(op,(c,d))) extOpTable
    eUndef = impossible "missing encoding for ABC ExtOp"
    ins _ c = c

extOpToChar :: ExtOp -> Char
extOpToChar = fst . (A.!) extOpDataArray

extOpToABC :: ExtOp -> Pure.ABC
extOpToABC = snd . (A.!) extOpDataArray

extCharToOp :: Char -> Maybe ExtOp
extCharToOp c | inBounds = extCharOpArray A.! c
              | otherwise = Nothing
    where inBounds = (lb <= c) && (c <= ub)
          (lb,ub) = A.bounds extCharOpArray

instance Monoid (ABC v) where
    mempty = ABC { abc_code = LBS.empty, abc_data = [] }
    mappend a b = ABC { abc_code = _code, abc_data = _data } where
        _code = abc_code a <> abc_code b
        _data = abc_data a <> abc_data b

instance (Quotable v) => Show (ABC v) where 
    showsPrec _ = shows . quote

instance (Quotable v) => Show (Op v) where
    showsPrec _ = shows . quote
    showList = shows . quoteList

instance Show ExtOp where
    showsPrec _ = shows . quote
    showList = shows . quoteList

instance Quotable ExtOp where 
    quotes = quotes . extOpToABC

instance (Quotable v) => Quotable (ABC v) where 
    quotes = quotesList . decodeOps

-- I'm assuming that our value type quotes into code that will, when
-- evaluated, regenerate the value. This would certainly be the obvious
-- implementation. :)
instance (Quotable v) => Quotable (Op v) where
    quotes (ABC_Prim op) = quotes op
    quotes (ABC_Block abc) = quotes (Pure.ABC_Block $ Pure.ABC $ Pure.quote abc)
    quotes (ABC_Text txt) = quotes (Pure.ABC_Text txt)
    quotes (ABC_Tok tok) = quotes (Pure.ABC_Tok tok)
    quotes (ABC_Ext op) = quotes op
    quotes (ABC_Quote v) = quotes v

abcErr :: String -> String
abcErr = (++) "Wikilon.ABC.Code: "

impossible :: String -> a
impossible = error . abcErr
