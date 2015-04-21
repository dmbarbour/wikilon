{-# LANGUAGE BangPatterns, ViewPatterns, OverloadedStrings, DeriveDataTypeable #-}

-- | Wikilon uses Awelon Bytecode (ABC) to model user-defined behavior.
-- ABC has many nice properties for security, distribution, streaming,
-- simplicity, parallelism, and dynamic linking. 
--
-- See <https://github.com/dmbarbour/awelon/blob/master/AboutABC.md>.
--
-- ABC has a major weakness: naive interpretation is inefficient. ABC
-- must be compiled for effective use. 
--
-- Separate compilation is viable via secure hash {#resourceId} tokens
-- to name reusable fragments. JIT is possible, though difficult in
-- Haskell (plugins or Lambdachine might be useful here). Accelerated
-- operations is possible by recognizing common subprograms and using
-- an optimized implementation.
-- 
-- Wikilon enhances performance by the following techniques.
--
--  * data stack for quotation without serialization
--  * ABCD-like dictionary of accelerated operations
--  * compact representation using lazy bytestrings
--  * interaction with VCache to load very large values
--  * efficient slicing for texts, blocks, tokens
--
-- Unlike ABCD, Wikilon's extended operations set doesn't wait on any
-- standards committee. Wikilon's use of VCache is comparable to the
-- {#resourceId'kf} value resources from ABC, but is strictly local
-- which allows easy reference-counting GC.
--
module Wikilon.ABC
    ( ABC(..)
    , Op(..)
    , Value(..)
    , PrimOp(..)
    , ExtOp(..)
    , Rsc(..)
    , Text
    , Token
    , Bytes
    , Flags

    , null
    , decodeOp, decodeOps, encodeOps
    , loadVal, encodeVal, decodeVal

    , asUnit, asPair, asSum, asNumber, asBlock, asText
    , asListOf, fromList, asStackOf, asStack, fromStack
    , copyable, droppable

    , extOpTable, extCharToOp, extOpToChar, extOpToABC

    -- * attributes for blocks and links
    , flag_affine,   f_copyable
    , flag_relevant, f_droppable
    , flag_parallel, f_parallel

    -- * 


{-
    -- * primitive ABC evaluation operators.
    , op_l, op_r, op_w, op_z, op_v, op_c
    , op_L, op_R, op_W, op_Z, op_V, op_C
    , op_copy, op_drop
    , op_add, op_negate, op_multiply, op_reciprocal, op_divMod, op_compare
    , op_apply, op_condApply, op_quote, op_compose, op_relevant, op_affine
    , op_distrib, op_factor, op_merge, op_assert
    , op_newZero, op_d0, op_d1, op_d2, op_d3, op_d4, op_d5, op_d6, op_d7, op_d8, op_d9
    , op_SP, op_LF

    -- * multi-byte operators
    , op_Block, op_Text, op_Tok

    -- * Wikilon's extended operators


    -- * 

    , EvalSt(..)
    , EvalErr(..)

-}


{-
    , encodeVal, decodeVal
    , encodeVals, decodeVals
    , encodeABC, decodeABC
-}  


    ) where

import Prelude hiding (null)
import Control.Monad
import Control.Applicative

import Data.Monoid
import Data.Typeable (Typeable)
import qualified Data.Array.IArray as A
import qualified Data.List as L
import Data.Word
import Data.Int
import Data.Bits
import Data.Ratio
import Data.Char
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.UTF8 as UTF8
import Database.VCache

import Awelon.ABC (PrimOp(..), Quotable(..))
import qualified Awelon.ABC as Pure

type Token = UTF8.ByteString
type Text = LazyUTF8.ByteString
type Bytes = LBS.ByteString

-- | Wikilon's internal representation of Awelon Bytecode (ABC). This
-- encoding has several features:
--
--  * blocks and texts are tuned for fast slicing
--  * extended op set to accelerate common functions
--  * stack of values for quotation & parallel eval
--  * support to lazily load large values in VCache
--  * compact representation compared to list of ops
--
-- Equality for bytecode is inherently structural.
data ABC = ABC 
    { abc_code :: !Bytes   -- ^ Wikilon internal ABC variant 
    , abc_data :: ![Value] -- ^ stack of precomputed values
    } deriving (Eq, Typeable)

-- | Individual operations from Wikilon encoded via ABC.
data Op
    -- Normal ABC
    = ABC_Prim  !PrimOp -- ^ common ABC operations
    | ABC_Block !ABC    -- ^ first class functions
    | ABC_Text  !Text   -- ^ embedded text literal
    | ABC_Tok   !Token  -- ^ embedded {token} for seal, unseal, etc.
    -- Extended Operations
    | ABC_Ext   !ExtOp  -- ^ accelerated operations
    | ABC_Pop   !Value  -- ^ value resource from precomp stack
    deriving (Eq, Typeable)

-- | Extended Operations are essentially a dictionary recognized by
-- Wikilon for specialized implementations, e.g. to accelerate an
-- interpreter or support tail-call optimizations. ExtOp is similar
-- to ABCD, but doesn't require careful standardization.
--
-- Additionally, these extended operations data stack manipulations.
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

    -- annotations? {&par}, {&trace}, {&≡}, etc.
    --  not sure it's worthwhile though, these aren't frequent and
    --  recognition overhead is marginal compared to the processing.

    -- stack swaps?
    -- hand manipulations?
    -- more as needed!
    deriving (Ord, Eq, Bounded, Enum, A.Ix)

-- | Read an operation from the ABC bytes. Returns Nothing if the
-- input is empty or contains invalid bytecode.
decodeOp :: ABC -> Maybe (Op, ABC)
decodeOp abc = 
    LazyUTF8.uncons (abc_code abc) >>= \ (c, bs) ->
    case c of
        (Pure.abcCharToOp -> Just primOp) ->
            let op = ABC_Prim primOp in
            let abc' = abc { abc_code = bs } in
            return (op, abc')
        '[' ->
            sizedSliceFby ']' bs >>= \ (bcode, _code') ->
            _uncons (abc_data abc) >>= \ (v, _data') ->
            let bdata = asStack v in
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
            _uncons (abc_data abc) >>= \ (v,vs) ->
            let op = ABC_Pop v in
            let abc' = ABC { abc_code = bs, abc_data = vs } in
            return (op, abc')
        (extCharToOp -> Just extOp) ->
            let op = ABC_Ext extOp in
            let abc' = abc { abc_code = bs } in
            return (op, abc')
        _ -> mzero

sizedSliceFby :: Char -> Bytes -> Maybe (Bytes, Bytes)
sizedSliceFby cExpect bs = 
    readSizedSlice bs >>= \ (slice, cont) ->
    LazyUTF8.uncons cont >>= \ (c, bs') ->
    let bOK = cExpect == c in
    if not bOK then mzero else
    return (slice, bs')
{-# INLINE sizedSliceFby #-}

readSizedSlice ::  Bytes -> Maybe (Bytes, Bytes)
readSizedSlice b = 
    readVarNat b >>= \ (n, b') ->
    let (lhs,rhs) = LBS.splitAt n b' in
    let bOK = (n == LBS.length lhs) in
    if not bOK then mzero else
    return (lhs,rhs)
{-# INLINE readSizedSlice #-}

readVarNat :: Bytes -> Maybe (Int64, Bytes)
readVarNat = r 0 where
    r !n !t =
        LBS.uncons t >>= \ (byte, t') -> 
        let n' = n `shiftL` 7 .|. (fromIntegral (byte .&. 0x7f)) in
        let bDone = (0 == (byte .&. 0x80)) in
        if bDone then return (n', t') else r n' t'
{-# INLINE readVarNat #-}

_uncons :: [a] -> Maybe (a,[a])
_uncons (x:xs) = Just (x,xs)
_uncons [] = Nothing
{-# INLINE _uncons #-}

-- | Decode ABC into a list of operations. This will error at runtime
-- if the ABC is not properly encoded.
decodeOps :: ABC -> [Op]
decodeOps abc = case decodeOp abc of
    Just (op, abc') -> op : decodeOps abc'
    Nothing | null abc -> []
    _ -> impossible $ "invalid bytecode! " ++
            show (abc_code abc) ++ " " ++
            show (abc_data abc)

-- | Test whether ABC is empty. Empty ABC has identity behavior.
null :: ABC -> Bool
null abc = L.null (abc_data abc) && LBS.null (abc_code abc)

-- | Encode a list of Wikilon's ABC operators into a collective ABC
-- structure. This involves a compact encoding for bytecode. This
-- encoding will preserve the input structure. A separate simplify
-- pass is appropriate, e.g. to translate some ABC_Pop into texts
-- or blocks.
encodeOps :: [Op] -> ABC
encodeOps = extract . writeOps where
    extract (bb,rv) = 
        let _code = BB.toLazyByteString bb in
        let _data = L.reverse rv in
        ABC { abc_code = _code, abc_data = _data }

    writeOps = L.foldl' writeOp (mempty,mempty)
    writeOp st (ABC_Prim op) = writePrim st op
    writeOp st (ABC_Block abc) = writeBlock st abc
    writeOp st (ABC_Text txt) = writeText st txt
    writeOp st (ABC_Tok tok) = writeTok st tok
    writeOp st (ABC_Ext op) = writeExtOp st op
    writeOp st (ABC_Pop v) = writeData st v

    writePrim (bb,rv) op = (bb <> opc, rv) where
        opc = BB.charUtf8 (Pure.abcOpToChar op)
    writeBlock (bb,rv) abc = (bb <> s, bv:rv) where
        s = BB.char8 '[' <> encSizedSlice (abc_code abc) <> BB.char8 ']'
        bv = fromStack (abc_data abc)
    writeText (bb,rv) txt = (bb <> s, rv) where
        s = BB.char8 '"' <> encSizedSlice txt <> BB.char8 '~'
    writeTok (bb,rv) tok = (bb <> s, rv) where
        lzTok = LBS.fromStrict tok 
        s = BB.char8 '{' <> encSizedSlice lzTok <> BB.char8 '}'
    writeExtOp (bb,rv) op = (bb <> opc, rv) where
        opc = BB.charUtf8 (extOpToChar op)
    writeData (bb,rv) v = (bb <> BB.charUtf8 '»', v:rv)

encSizedSlice :: LBS.ByteString -> BB.Builder
encSizedSlice s = encVarNat (LBS.length s) <> BB.lazyByteString s

-- argument must be positive; low byte is 0..127, 
encVarNat :: Int64 -> BB.Builder
encVarNat n | (n < 0) = impossible $ "varNat with " ++ show n
         | otherwise = _encVarNat q <> BB.word8 lo
  where q = n `shiftR` 7
        lo = 0x7f .&. fromIntegral n

-- all bytes except the low byte tagged by 0x80
_encVarNat :: Int64 -> BB.Builder
_encVarNat 0 = mempty
_encVarNat n = _encVarNat q <> BB.word8 b where
    q = n `shiftR` 7
    b = 0x80 .|. (0x7f .&. fromIntegral n)

-- | Values have a few basic forms:
--
--    (a * b) -- pairs    ~Haskell (a,b)
--    (a + b) -- sums     ~Haskell (Either a b)
--    1       -- unit     ~Haskell ()
--    0       -- void     ~Haskell EmptyDataDecls
--    N       -- numbers  ~Haskell Rational
--    [a→b]   -- blocks   ~Haskell functions or arrows
--    a{:foo} -- sealed   ~Haskell newtype
--
-- Blocks may be affine or relevant, and other substructural types are
-- possible. Sealed values may be cryptographic, using {$format}. 
--
-- Wikilon additionally supports lazily loaded values, i.e. such that
-- developers could model a whole multi-gigabyte filesystem as a value
-- if they desire to do so. This is achieved using VCache.
--
-- I would like to optimize representations and processing for common
-- data structures, especially vectors and matrices.
data Value
    = Number !Rational
    | Pair Value Value
    | SumL Value
    | SumR Value
    | Unit
    | Block !ABC {-# UNPACK #-} !Flags
    | Sealed !Token Value
    -- performance variants
    | Stowed {-# UNPACK #-} !Rsc
    | Text !Text
    deriving (Eq, Typeable)

-- | Wikilon uses VCache to model value resources, similar to the
-- {#resourceId'kf} tokens in ABC. Values are pushed to VCache via
-- the {&stow} annotation. Use of VCache simplifies GC issues.
--
-- Wikilon does not have any equivalent to the inline ABC resources
-- {#resourceId}, mostly because it isn't clear how to construct such
-- resources or reliably regenerate them through annotations. 
data Rsc = Rsc !(VRef Value) {-# UNPACK #-} !Flags
    deriving (Show, Eq, Typeable)

-- | Load a value from VCache using the default cache mode.
loadVal :: Rsc -> Value
loadVal (Rsc r _) = derefc CacheMode0 r

-- | Encode values for VCache. Uses a simplified bytecode optimized 
-- for fast reconstruction of values. This bytecode is only used 
-- internally to Wikilon.
encodeVal :: Value -> (Bytes, [Rsc])
encodeVal = extract . enc where
    extract (bb,rsc) = (BB.toLazyByteString bb, rsc)
    enc (Number r) = encNum r
    enc (Pair a b) = enc a <> enc b <> encPair
    enc (SumR b) = enc b <> encSumR
    enc (asText -> Just t) = encText t
    enc (SumL a) = enc a <> encSumL
    enc Unit = encUnit 
    enc (Block abc f) = encABC abc <> encFlags f
    enc (Sealed tok val) = enc val <> encSeal tok
    enc (Stowed rsc) = encRSC rsc
    enc (Text t) = encText t
    encNum r = (Pure.encode' abc, mempty) where
        abc = Pure.ABC $ Pure.quote r 
    encPair = (BB.char8 'P', mempty)
    encSumR = (BB.char8 'R', mempty)
    encSumL = (BB.char8 'L', mempty)
    encUnit = (BB.char8 'v', mempty)
    encABC abc = enc (fromStack (abc_data abc)) <> (bytes, mempty) where
        bytes = BB.char8 '[' <> encSizedSlice (abc_code abc) <> BB.char8 ']'
    encFlags flags = (rel <> aff <> par, mempty) where
        has prop c = if flags_include prop flags then BB.char8 c else mempty
        rel = has flag_relevant 'k'
        aff = has flag_affine 'f'
        par = has flag_parallel 'j'
    encSeal tok = (bytes, mempty) where
        lzTok = LBS.fromStrict tok
        bytes = BB.char8 '{' <> encSizedSlice lzTok <> BB.char8 '}'
    encText t = (bytes, mempty) where
        bytes = BB.char8 '"' <> encSizedSlice t <> BB.char8 '~'
    encRSC rsc = (BB.char8 '!', [rsc])

type DecoderState = (Bytes, [Rsc])
type DecoderStep = Value -> DecoderState -> Maybe (Value, DecoderState)
decOpTable :: [(Char, DecoderStep)]    
decOpTable = 
    -- encoding of numbers (same as ABC)
    [('#',decOp_intro0)
    ,('0',decOp_d 0),('1',decOp_d 1),('2',decOp_d 2),('3',decOp_d 3),('4',decOp_d 4)
    ,('5',decOp_d 5),('6',decOp_d 6),('7',decOp_d 7),('8',decOp_d 8),('9',decOp_d 9)
    ,('-',decOp_negate),('/',decOp_recip),('*',decOp_mul)
    -- encoding for pairs, sums, unit (bare minimum for plumbing)
    ,('p',decOp_pair),('R',decOp_inR),('L',decOp_inL),('v',decOp_unit)
    -- encoding blocks and attributes
    ,('[',decOp_block)
    ,('k',decOp_bf flag_relevant)
    ,('f',decOp_bf flag_affine)
    ,('j',decOp_bf flag_parallel)
    -- text, sealers, stowage 
    ,('"',decOp_text),('{',decOp_seal),('!',decOp_stow)
    ]

decCharOpArray :: A.Array Char DecoderStep
decCharOpArray = A.accumArray ins decOp_fail (lb,ub) lst where
    ins _ op = op
    lb = L.minimum (fmap fst lst)
    ub = L.maximum (fmap fst lst)
    lst = decOpTable

decCharToOp :: Char -> DecoderStep
decCharToOp c | inBounds = decCharOpArray A.! c
              | otherwise = decOp_fail
  where inBounds = (lb <= c) && (c <= ub)
        (lb,ub) = A.bounds decCharOpArray

-- | Decode a value from its VCache encoding. The bytecode used here
decodeVal :: (Bytes, [Rsc]) -> Value
decodeVal = extract . dec v0 where
    v0 = Sealed "decodeVal" (Block mempty kf)
    kf = flag_relevant .|. flag_affine
    extract (Pair a b) | (b == v0) = a
    extract v = impossible $ "expecting decoded value, received " ++ show v
    dec v st@(bs,rs) = case LazyUTF8.uncons bs of
        Just (decCharToOp -> decOp, bs') -> case decOp v (bs',rs) of
            Just (v',st') -> dec v' st'
            Nothing -> impossible $ "decodeVal: decode failure @ " ++ show (st, v)
        Nothing | L.null rs -> v -- all done!
        _ -> impossible $ "decodeVal: invalid state " ++ show st

decOp_intro0 :: DecoderStep
decOp_d :: Int -> DecoderStep
decOp_negate, decOp_recip, decOp_mul :: DecoderStep
decOp_pair, decOp_inR, decOp_inL, decOp_unit :: DecoderStep
decOp_block, decOp_text, decOp_seal, decOp_stow :: DecoderStep
decOp_bf :: Flags -> DecoderStep

decOp_fail :: DecoderStep
decOp_fail _ _ = Nothing

decOpV :: DecoderStep
{-# INLINE decOpV #-}
decOpV v st = Just (v,st)

decOp_intro0 = decOpV . Pair (Number 0) 

decOp_d digit (Pair (Number n) s) = 
    let n' = 10 * n + fromIntegral digit in
    decOpV $! n' `seq` (Pair (Number n') s)
decOp_d _ _ = const Nothing

decOp_negate (Pair (Number n) s) = 
    let n' = negate n in
    decOpV $! n' `seq` (Pair (Number n') s)
decOp_negate _ = const Nothing

decOp_recip (Pair (Number n) s) | (n /= 0) =
    let n' = recip n in
    decOpV $! n' `seq` (Pair (Number n') s)
decOp_recip _ = const Nothing

decOp_mul (Pair (Number b) (Pair (Number a) s)) =
    let n' = (a * b) in
    decOpV $! n' `seq` (Pair (Number n') s)
decOp_mul _ = const Nothing

decOp_pair (Pair b (Pair a s)) = decOpV (Pair (Pair a b) s)
decOp_pair _ = const Nothing

decOp_inR (Pair b s) = decOpV (Pair (SumR b) s)
decOp_inR _ = const Nothing

decOp_inL (Pair a s) = decOpV (Pair (SumL a) s)
decOp_inL _ = const Nothing

decOp_unit = decOpV . Pair Unit

decOp_bf f (Pair (Block abc bf) s) =
    let block' = Block abc (f .|. bf) in
    decOpV $! block' `seq` (Pair block' s)
decOp_bf _ _ = const Nothing 

decOp_block (Pair bStack s) (bs,rs) =
    sizedSliceFby ']' bs >>= \ (_code, bs') ->
    let _data = asStack bStack in
    let abc = ABC { abc_data = _data, abc_code = _code } in
    let block = Block abc 0 in
    return (Pair block s, (bs',rs))
decOp_block _ _ = Nothing

decOp_text s (bs,rs) =
    sizedSliceFby '~' bs >>= \ (txt, bs') ->
    return ((Pair (Text txt) s), (bs',rs))

decOp_seal (Pair v s) (bs,rs) =
    sizedSliceFby '}' bs >>= \ (lzTok, bs') ->
    let tok = LBS.toStrict lzTok in
    return ((Pair (Sealed tok v) s), (bs',rs))
decOp_seal _ _ = Nothing

decOp_stow s (bs, (r:rs)) = return ((Pair (Stowed r) s), (bs,rs))
decOp_stow _ _ = Nothing    

-- | Table of ExtOps and relevant semantics. 
--
-- I'll follow ABCD's mandate to leave the ASCII range to future ABC
-- expansions (though such expansions are very unlikely). ExtOps is
-- effectively a prototype for ABCD.
extOpTable :: [(ExtOp, Char, Pure.ABC)]
extOpTable =
    [(ExtOp_Inline,'£',"vr$c"),(ExtOp_Apc,'¢',"$c")
    ,(ExtOp_Fixpoint,'¥',"[^'ow^'zowvr$c]^'ow^'zowvr$c")
    ,(ExtOp_Intro1L,'ń',"vvrwlc"),(ExtOp_Elim1L,'ć',"vrwlcc")
    ,(ExtOp_Intro0L,'Ń',"VVRWLC"),(ExtOp_Elim0L,'Ć',"VRWLCC")
    ,(ExtOp_prim_swap,'ś',"vrwlc"),(ExtOp_prim_mirror,'Ś',"VRWLC")
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








-- | Flags for blocks
--   bit 0: true if relevant (no drop with %)
--   bit 1: true if affine   (no copy with ^)
--   bit 2: true for parallelism {&par}
--   bit 3..7: Reserved. Tentatively: 
--     memoization for blocks
--     local values (cannot send in message, except in context)
--     ephemeral values (cannot store in state)
--
-- Besides the basic substructural types, it might be worth using
-- annotations to enforce a few new ones.
type Flags = Word8

flag_affine, flag_relevant, flag_parallel :: Flags
flag_relevant = 0x01
flag_affine   = 0x02
flag_parallel = 0x04

flags_include :: Flags -> Flags -> Bool
flags_include f fs = f == (f .&. fs)
{-# INLINE flags_include #-}

f_copyable :: Flags -> Bool
f_copyable = not . flags_include flag_affine
{-# INLINE f_copyable #-}

f_droppable :: Flags -> Bool
f_droppable = not . flags_include flag_relevant
{-# INLINE f_droppable #-}

f_parallel :: Flags -> Bool
f_parallel = flags_include flag_parallel
{-# INLINE f_parallel #-}

-- | Is this value copyable (not affine)
copyable :: Value -> Bool
copyable (Number _) = True
copyable (Pair a b) = copyable a && copyable b
copyable (SumL a) = copyable a
copyable (SumR b) = copyable b
copyable Unit = True
copyable (Block _ f) = f_copyable f
copyable (Sealed _ v) = copyable v
copyable (Stowed (Rsc _ f)) = f_copyable f
copyable (Text _) = True

-- | Is this value droppable (not relevant)
droppable :: Value -> Bool
droppable (Number _) = True
droppable (Pair a b) = droppable a && droppable b
droppable (SumL a) = droppable a
droppable (SumR b) = droppable b
droppable Unit = True
droppable (Block _ f) = f_droppable f
droppable (Sealed _ v) = droppable v
droppable (Stowed (Rsc _ f)) = f_droppable f
droppable (Text _) = True

asUnit :: (Monad m) => Value -> m ()
{-# INLINE asUnit #-}
asUnit Unit = return ()
asUnit (Stowed rsc) = asUnit (loadVal rsc)
asUnit v = fail $ abcErr $ "expecting unit, received " ++ show v

asPair :: (Monad m) => Value -> m (Value,Value)
{-# INLINE asPair #-}
asPair (Pair a b) = return (a,b)
asPair (Stowed rsc) = asPair (loadVal rsc)
asPair v = fail $ abcErr $ "expecting pair, received " ++ show v

asSum :: (Monad m) => Value -> m (Either Value Value)
{-# INLINE asSum #-}
asSum (SumL a) = return (Left a)
asSum (SumR b) = return (Right b)
asSum (Stowed rsc) = asSum (loadVal rsc)
asSum (Text t) = case LazyUTF8.uncons t of
    Nothing -> return $ Right Unit
    Just (c,t') -> 
        let n = Number (fromIntegral (ord c)) in
        return $ Left (Pair n (Text t'))
asSum v = fail $ abcErr $ "expecting sum, received " ++ show v

asNumber :: (Monad m) => Value -> m Rational
{-# INLINE asNumber #-}
asNumber (Number r) = return r
asNumber (Stowed rsc) = asNumber (loadVal rsc)
asNumber v = fail $ abcErr $ "expecting number, received " ++ show v

asCharNum :: (Monad m) => Value -> m Char
{-# INLINE asCharNum #-}
asCharNum num = 
    asNumber num >>= \ r ->
    let n = numerator r in
    let d = denominator r in
    let ok = (1 == d) && (0 <= n) && (n <= 0x10ffff) in
    if ok then return (chr (fromInteger n)) else
    fail $ abcErr $ "expecting utf8 codepoint, received " ++ show num

asBlock :: (Monad m) => Value -> m (ABC, Flags)
{-# INLINE asBlock #-}
asBlock (Block abc f) = return (abc,f)
asBlock (Stowed rsc) = asBlock (loadVal rsc)
asBlock v = fail $ abcErr $ "expecting block, received " ++ show v

-- | asText will short-circuit if the value is obviously a text.
-- Otherwise it will prepare a bytestring builder.
asText :: (Monad m) => Value -> m Text
{-# INLINE asText #-}
asText (Text t) = return t
asText (Stowed rsc) = asText (loadVal rsc)
asText v = asText' mempty v 

asText' :: (Monad m) => BB.Builder -> Value -> m Text 
{-# INLINE asText' #-}
asText' bb (Text t) = return $! (BB.toLazyByteString bb) <> t
asText' bb s = asSum s >>= \ lr -> case lr of
    Right u -> asUnit u >> return $! BB.toLazyByteString bb
    Left p -> 
        asPair p >>= \ (n , t') -> 
        asCharNum n >>= \ c ->
        asText' (bb <> BB.charUtf8 c) t'

-- | Obtain a list from a value, with monadic failure. In this case
-- we expect an argument of form µL.((a*L)+1).
asListOf :: (Monad m) => (Value -> m a) -> Value -> m [a]
{-# INLINE asListOf #-}
asListOf fn = asListOf' [] where
    asListOf' xs = asSum >=> \ e -> case e of
        Right u -> asUnit u >> return (L.reverse xs) 
        Left p -> asPair p >>= \ (a,l) -> fn a >>= \ x -> asListOf' (x:xs) l

-- | Encode a list into a value, using ABC's underlying list structure,
-- which is λa.µL.((a*L)+1). Every item in a list is a sum.
fromList :: [Value] -> Value
{-# INLINE fromList #-}
fromList (x:xs) = SumL (Pair x (fromList xs))
fromList [] = SumR Unit


-- | Obtain a stack of values. A stack is just pairs terminated by a
-- Unit value. Within ABC, it is generally necessary that knowledge of
-- stack size be implicit in context. Values on a stack may generally
-- be of heterogeneous type.
asStackOf :: (Monad m) => (Value -> m a) -> Value -> m [a]
{-# INLINE asStackOf #-}
asStackOf fn (Pair a b) =
    fn a >>= \ x ->
    asStackOf fn b >>= \ xs ->
    return (x:xs)
asStackOf _ Unit = return []
asStackOf fn (Stowed rsc) = asStackOf fn (loadVal rsc)
asStackOf _ v = fail $ abcErr $ "expecting stack, received " ++ show v

-- | Convert to a stack of values with error-based failure.
asStack :: Value -> [Value]
asStack (Pair a b) = a : asStack b
asStack Unit = []
asStack (Stowed rsc) = asStack (loadVal rsc)
asStack v = impossible $ "expecting stack, received " ++ show v

-- | Convert a list of values to a stack of values. Trivial.
fromStack :: [Value] -> Value
fromStack (x:xs) = (Pair x (fromStack xs))
fromStack [] = Unit
















-- composition is still trivial
instance Monoid ABC where
    mempty = ABC { abc_code = LBS.empty, abc_data = [] }
    mappend a b = ABC { abc_code = _code, abc_data = _data } where
        _code = abc_code a <> abc_code b
        _data = abc_data a <> abc_data b

instance Show ABC where showsPrec _ = shows . Pure.quote
instance Show Value where showsPrec _ = shows . Pure.quote

-- Value will quote as ABC that regenerates it. This includes
-- regenerating any linked or stowed value resources.
instance Quotable Value where
    quotes (Number r) = quotes r
    quotes (Text t) = quotes (Pure.ABC_Text t)
    quotes (asText -> Just t) = quotes (Pure.ABC_Text t)
    quotes (Pair a b) = quotes a . quotes b . quotes ("wl" :: Pure.ABC)
    quotes (SumL a) = quotes a . quotes ("V" :: Pure.ABC)
    quotes (SumR b) = quotes b . quotes ("VVRWLC" :: Pure.ABC)
    quotes Unit = quotes ("vvrwlc" :: Pure.ABC)
    quotes (Block abc flags) = 
        let block = quotes (Pure.ABC_Block $ Pure.ABC $ Pure.quote abc) in
        let bAff = flags_include flag_affine flags in
        let bRel = flags_include flag_relevant flags in
        let bPar = flags_include flag_parallel flags in
        let k = if bRel then quotes ABC_relevant else id in
        let f = if bAff then quotes ABC_affine else id in
        let p = if bPar then quotes (Pure.ABC_Tok "{&par}") else id in
        block . k . f . p
    quotes (Sealed tok a) = quotes a . quotes (Pure.ABC_Tok tok)
    quotes (Stowed rsc) = quotes (loadVal rsc) . quotes (Pure.ABC_Tok "{&stow}")

instance Quotable ExtOp where quotes = quotes . extOpToABC
instance Quotable ABC where quotes = Pure.quotesList . decodeOps
instance Quotable Op where
    quotes (ABC_Prim op) = quotes op
    quotes (ABC_Block abc) = quotes (Pure.ABC_Block $ Pure.ABC $ Pure.quote abc)
    quotes (ABC_Text txt) = quotes (Pure.ABC_Text txt)
    quotes (ABC_Tok tok) = quotes (Pure.ABC_Tok tok)
    quotes (ABC_Ext op) = quotes op
    quotes (ABC_Pop val) = quotes val



-- encode ABC as a plain old Block value
instance VCacheable ABC where
    put abc = put (Block abc 0)
    get = get >>= \ v -> case v of
        (Block abc 0) -> return abc
        _ -> fail $ abcErr $ "expecting block, received " ++ show v

-- versioned encoder for values 
-- e.g. in case I want to change compression algs
newtype V0 = V0 { unV0 :: (Bytes, [Rsc]) }
    deriving (Typeable)

instance VCacheable Value where
    put = put . V0 . encodeVal
    get = (decodeVal . unV0) <$> get

instance VCacheable V0 where
    put = error $ abcErr $ "todo"
    get = error $ abcErr $ "todo"

abcErr :: String -> String
abcErr = (++) "Wikilon.ABC: "

impossible :: String -> a
impossible = error . abcErr
