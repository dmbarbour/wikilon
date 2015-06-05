{-# LANGUAGE OverloadedStrings, BangPatterns, RankNTypes #-}
-- | Utility for compact encoding of a value in VCache. 
--
-- The design of Awelon Bytecode (ABC) includes a concept for value
-- resources, which appear in bytecode as {#resourceId'kf}. A secure
-- hash function is used to generate the resource identifier, while
-- the 'kf suffix indicates: 
--
--   '    a value resource
--   k    a relevant value (no drop)
--   f    an affine value (no copy)
-- 
-- Bytecode resources enable separate compilation and linking in open
-- distributed systems. They also enable lazy loading of large values,
-- which allows Awelon systems to model databases or filesystems as
-- values (even at scales of many gigabytes).
--
-- For Wikilon, the intention is to push value resources into VCache
-- where they're off the heap and subject to reference-counted GC. 
--
-- However, we cannot simply use a token to encode the reference. The
-- reference must be encoded separately such that VCache is aware of
-- it (e.g. for GC purposes). Also, it is ideal if we can compress 
-- large values at the VCache layer.
-- 
module Wikilon.ABC.EncVal
    ( encodeVal
    , decodeVal
    , decodeValM
    ) where

import Control.Monad
import Data.Monoid
import qualified Data.Array.IArray as A
import qualified Data.List as L
import Data.Word
import Data.Bits
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LazyChar8

import qualified Awelon.ABC as Pure
import Wikilon.ABC.Util
import Wikilon.ABC.Code
import Wikilon.ABC.Value

-- | encode a value into a bytestring and some resources. The value
-- can be recovered if later given the same bytestring and resource
-- list. This encoding uses a bytecode simpler than ABC, specific to
-- incremental construction of a value.
--
-- This will encode values as they are represented. No simplification
-- is performed. A simplification pass before encoding is recommended.
encodeVal :: Value r -> (Bytes, [r])
encodeVal = encSt . enc where

encSt :: EncSt r -> (Bytes, [r])
encSt (bb,rsc) = (bytes,rs) where
    rs = fmap fst rsc
    encFlags = encSizedSlice $ LBS.pack $ fmap snd rsc
    bytes = BB.toLazyByteString $ encFlags <> bb

type EncSt r = (BB.Builder, [(r, Word8)])
enc :: Value r -> EncSt r
enc (Number n) = (encNum n, mempty)
enc (Pair a b) = enc a <> enc b <> encPair
enc (SumR b) = enc b <> encSumR
enc (SumL a) = enc a <> encSumL
enc Unit = encUnit 
enc (Block abc f) = encBlockCode abc <> encBlockFlags f
enc (Sealed tok val) = enc val <> encSeal tok
enc (Text t) = encText t
enc (Resource r f) = encResource r f

{-# INLINE encNum #-}
{-# INLINE encPair #-}
{-# INLINE encSumR #-}
{-# INLINE encSumL #-}
{-# INLINE encUnit #-}
{-# INLINE encBlockCode #-}
{-# INLINE encBlockFlags #-}
{-# INLINE encSeal #-}
{-# INLINE encText #-}
{-# INLINE encResource #-}

-- encNum will use opcodes #0123456789-
encNum :: Integer -> BB.Builder
encNum = mconcat . fmap Pure.encodeOpBB . Pure.quote

encPair, encSumR, encSumL, encUnit :: EncSt r
encPair = (BB.char8 'P', mempty)
encSumR = (BB.char8 'R', mempty)
encSumL = (BB.char8 'L', mempty)
encUnit = (BB.char8 'v', mempty)

encBlockCode :: ABC (Value r) -> EncSt r
encBlockCode abc = enc vBind <> (bytes, mempty) where
    bytes = BB.char8 '[' <> encSizedSlice (abc_code abc) <> BB.char8 ']'
    vBind = fromBindings (abc_data abc)

encBlockFlags :: Flags -> EncSt r
encBlockFlags bf = (rel <> aff, mempty) where
    has p c = if (p == (p .&. bf)) then BB.char8 c else mempty
    rel = has f_rel 'k'
    aff = has f_aff 'f'
    
encSeal :: Token -> EncSt r
encSeal tok = (bytes, mempty) where
    lzTok = LBS.fromStrict tok
    bytes = BB.char8 '{' <> encSizedSlice lzTok <> BB.char8 '}'

encText :: Text -> EncSt r 
encText txt = (bytes, mempty) where
    bytes = BB.char8 '"' <> encSizedSlice txt <> BB.char8 '~'

encResource :: r -> Flags -> EncSt r
encResource r f = (BB.char8 '!', [(r,f)])

type DecSt r = (Bytes, [(r,Flags)])
type DecoderStep r = Value r -> DecSt r -> Maybe (Value r, DecSt r)

decOpTable :: [(Char, DecoderStep r)]
decOpTable = 
    -- encoding of numbers (same as ABC)
    [('#',decOp_intro0),('0',decOp_d 0)
    ,('1',decOp_d 1),('2',decOp_d 2),('3',decOp_d 3)
    ,('4',decOp_d 4),('5',decOp_d 5),('6',decOp_d 6)
    ,('7',decOp_d 7),('8',decOp_d 8),('9',decOp_d 9)
    ,('-',decOp_negate)
    -- encoding for pairs, sums, unit (bare minimum for plumbing)
    ,('p',decOp_pair)
    ,('R',decOp_inR)
    ,('L',decOp_inL)
    ,('v',decOp_unit)
    -- encoding blocks and attributes
    ,('[',decOp_block)
    ,('k',decOp_bf f_rel)
    ,('f',decOp_bf f_aff)
    -- misc.
    ,('"',decOp_text)
    ,('{',decOp_seal)
    ,('!',decOp_resource)
    ]

decCharOpArray :: A.Array Char (DecoderStep r)
decCharOpArray = A.accumArray ins decOp_fail (lb,ub) lst where
    ins _ op = op
    lb = L.minimum (fmap fst lst)
    ub = L.maximum (fmap fst lst)
    lst = decOpTable

decCharToOp :: Char -> DecoderStep r
decCharToOp c | inBounds = decCharOpArray A.! c
              | otherwise = decOp_fail
  where inBounds = (lb <= c) && (c <= ub)
        (lb,ub) = A.bounds decCharOpArray

decOp_fail :: DecoderStep r
decOp_fail _ _ = Nothing

-- | Attempt to decode a value from the content generated when we 
-- encoded the value. If given the same 
decodeVal :: (Bytes, [r]) -> Value r
decodeVal = maybe badVal id . decodeValM where
    badVal = impossible "failed to decode value"

-- | Attempt to decode a value. Returns Nothing on failure.
decodeValM :: (Bytes, [r]) -> Maybe (Value r)
decodeValM (bs,rs) =
    readSizedSlice bs >>= \ (fs,bs') ->
    let bOkLen = LBS.length fs == fromIntegral (L.length rs) in
    if not bOkLen then mzero else
    let rcs = L.zip rs (LBS.unpack fs) in
    let st0 = (bs', rcs) in
    decode Unit st0 >>= \ val ->
    case val of
        (Pair a Unit) -> return a
        _ -> Nothing

decode :: Value r -> DecSt r -> Maybe (Value r)
decode v (bs,rsc) = case LazyChar8.uncons bs of
    Just (c,bs') -> case decCharToOp c v (bs',rsc) of
        Just (v',st') -> decode v' st' -- progress
        Nothing -> Nothing -- failure
    Nothing | L.null rsc -> Just v -- done
    _ -> Nothing -- underflow rsc
    
decOp_intro0 :: DecoderStep r
decOp_d :: Int -> DecoderStep r
decOp_negate :: DecoderStep r
decOp_pair, decOp_inR, decOp_inL, decOp_unit :: DecoderStep r
decOp_block, decOp_text, decOp_seal, decOp_resource :: DecoderStep r
decOp_bf :: Flags -> DecoderStep r


decOpV :: DecoderStep r
{-# INLINE decOpV #-}
decOpV v st = Just (v,st)

decOp_intro0 = decOpV . Pair (Number 0) 

decOp_d digit (Pair (Number n) s) = 
    let n' = Number (10 * n + fromIntegral digit) in
    n' `seq` decOpV (Pair n' s)
decOp_d _ _ = const Nothing

decOp_negate (Pair (Number n) s) = 
    let n' = Number (negate n) in
    n' `seq` decOpV (Pair n' s)
decOp_negate _ = const Nothing

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
    let _data = toBindings bStack in
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

decOp_resource v (bs, ((r,!f):rcs)) = return (v',st') where
    v' = (Pair (Resource r f) v)
    st' = (bs,rcs) 
decOp_resource _ _ = Nothing    

-- | translate bindings to values when encoding.
fromBindings :: [Bound (Value r)] -> Value r
fromBindings = L.foldr Pair Unit . fmap fromBound where

fromBound :: Bound (Value r) -> Value r
fromBound (BBlock s) = SumL (fromBindings s)
fromBound (BQuote v) = SumR v

toBound :: Value r -> Bound (Value r)
toBound (SumL s) = BBlock (toBindings s)
toBound (SumR v) = BQuote v
toBound _ = impossible $ "invalid bound structure"

-- | translate values to bindings when decoding. This
-- will error if the value is not in the expected format.
toBindings :: Value r -> [Bound (Value r)]
toBindings (Pair v vs) = (toBound v : toBindings vs) where
toBindings Unit = []
toBindings _ = impossible $ "invalid bindings structure"


encErr :: String -> String
encErr = (++) "Wikilon.ABC.EncVal: "

impossible :: String -> a
impossible = error . encErr


