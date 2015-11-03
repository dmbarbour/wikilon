{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ViewPatterns #-}
-- | While Awelon Bytecode may be naively interpreted, doing so is
-- not efficient. Wikilon must pre-process and simplify bytecode to
-- support many performance tweaks:
--
-- * bytecode held in relatively compact bytestrings
-- * embedded and quoted values are pre-processed
-- * eliminates interpreter searching of bytestring
-- * larger-than-memory values and structure sharing
-- * ABCD-like extended dictionary of accelerated ops
--
-- I'm still contemplating whether I should model lazy evaluations.
-- 
module Wikilon.ABC.Fast
    ( ABC(..)
    , Op(..)
    , V(..)
    , PrimOp(..)
    , ExtOp(..)
    , Token, Text
    , decodeOps, encodeOps
    , copyable, droppable


    , purifyABC, valueToPureABC
    , extOpTable, charToExtOp, extOpToChar
    ) where

import Data.Typeable (Typeable)
import Data.Word
import Data.Bits
import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Array.IArray as A
import Database.VCache
import Wikilon.ABC.Pure (PrimOp(..))
import qualified Wikilon.ABC.Pure as Pure
import Wikilon.Text
import Wikilon.Token

-- | Wikilon's internal representation of Awelon Bytecode is an
-- encoding of [Op]. The stack of quoted values allows rapid
-- reuse of values and quotation.
data ABC val = ABC
    { abc_code :: LBS.ByteString    -- ABC & Extensions
    , abc_data :: [val]             -- matched by '_' in code
    , abc_toks :: [Token]           -- matched by '!' in code
    } deriving (Eq, Typeable)

-- | A simple ABC operation, with potential for accelerated
-- operations corresponding to common sequences of ABC, and
-- for ad-hoc values (rather than just texts and blocks).
data Op val
    = ABC_Prim  !PrimOp     -- the basic 42 ABC operators
    | ABC_Ext   !ExtOp      -- ABCD-like performance extensions
    | ABC_Val   val         -- a quoted value, block, or text
    | ABC_Tok   !Token      -- e.g. value sealers, annotations
    deriving (Eq, Typeable)

-- consider support for: binaries, vectors, fast copy/drop, laziness, stacks 
--
-- Some of these may require dedicated accelerators before I proceed. 
--
-- I think stacks and laziness might be treated instead as special optimization
-- cases. 
, fast copyable/droppable, laziness
--
-- for simplification, at least, it seems useful to consider intermediate
-- values including 'lazy computations' and 'stacks-frames'. But I don't
-- strictly need to extend the value types to support simplification. I 
-- could wrap them instead, during simplification only. 
--
-- to represent runtime errors could be done with sealed values, easily
-- enough. This would be convenient for allowing computation with some
-- errors, though might not be something we want on a normal basis.
--
-- 
--
-- Laziness, done right, would probably need me to model a hole:
--
--  type Hole = PVar (V, Maybe ABC)
--
-- This would allow me to compute a value once and reuse it. But the
-- disadvantage would be the need to 
--  
--
-- laziness would express a value in terms of the current value and the
-- continuation (as ABC). The real question is whether or not to use a
-- PVar, which could support background computations.
--
--
--  Z V ABC
--  Z (PVar (V, ABC))
--
-- ABC can directly represent a continuation. It is feasible to partially
-- run a lazy evaluation and extract some content for further evaluation.

-- | Accelerated operations corresponding to common substrings of
-- Awelon Bytecode (and hence to common functions). These encode
-- as a single character and are processed by a dedicated function.
--
-- In general, ExtOp will use UTF8 encoding. But it may include some
-- ASCII characters for the more common extended operations.
data ExtOp
    = ExtOp_i -- i; vr$c, also for tail calls
    | ExtOp_u -- u; vvrwlc; intro unit as first in pair
    | ExtOp_U -- U; VVRWLC; intro void as left in sum
    -- I'll probably want:
    --  safe copy and drop (untested)
    --  high performance data plumbing
    --  optimized fixpoint function
    --  list processing (reverse, append, map, etc.)
    --  leverage lists as vectors or matrices
    --  functions for rational numbers
    --  functions for a floating point encoding
    deriving (Eq, Ord, A.Ix, Enum, Bounded)

-- | Table of extended operations and semantics, extOpTable.
extOpTable :: [(ExtOp, Char, Pure.ABC)]
extOpTable =
    [(ExtOp_i, 'i', "vr$c")
    ,(ExtOp_u, 'u', "vvrwlc")
    ,(ExtOp_U, 'U', "VVRWLC")
    ]

-- todo: build a proper lookup array
extOpToChar :: ExtOp -> Char
extOpToChar ExtOp_Inline = 'i'

charToExtOp :: Char -> Maybe ExtOp
charToExtOp 'i' = Just ExtOp_Inline
charToExtOp _ = Nothing

charToOp :: Char -> Maybe Op
charToOp = error "todo: combine extOp and abcOp tables"

-- | Wikilon ABC is a compact representation for [Op].
decodeOps :: ABC -> [Op]
decodeOps abc = decodeOps' (abc_toks abc) (abc_data abc) (abc_code abc)

decodeOps' :: [Token] -> [V] -> LBS.ByteString -> [Op]
decodeOps' toks vals s = case LazyUTF8.uncons s of
    Just (c, s') -> case c of
        (charToOp -> Just op) -> op : decodeOps' toks vals s'
        '_' -> case vals of
            (v:vals') -> ABC_Val v : decodeOps' toks vals' s'
            [] -> impossible "value stack underflow"
        '!' -> case toks of
            (t:toks') -> ABC_Tok t : decodeOps' toks' vals s'
            [] -> impossible "token stack underflow"
        -- tokens, blocks, and texts are captured by _ and !.
        _ -> impossible (show c ++ " not recognized as ABC or ExtOp")
    Nothing -> -- do some sanity checking
        if not (L.null vals) then impossible "value stack overflow" else
        if not (L.null toks) then impossible "token stack overflow" else
        [] -- done

-- | Obtain a compact encoding for a list of ABC operations. 
encodeOps :: [Op] -> ABC
encodeOps ops = ABC (encOpCodes ops) (encOpData ops) (encOpToks ops)

-- filter for values
encOpData :: [Op] -> [V]
encOpData ((ABC_Val v) : ops) = v : encOpData ops
encOpData (_ : ops) = encOpData ops
encOpData [] = []

-- filter for tokens
encOpToks :: [Op] -> [Token]
encOpToks ((ABC_Tok tok) : ops) = tok : encOpToks ops
encOpToks (_ : ops) = encOpToks ops
encOpToks [] = []

-- encode one character per operation. This is feasible because we
-- separate encoding for values and tokens.
encOpCodes :: [Op] -> LazyUTF8.ByteString
encOpCodes = LazyUTF8.fromString . fmap encOpChar

encOpChar :: Op -> Char
encOpChar (ABC_Prim op) = Pure.abcOpToChar op
encOpChar (ABC_Ext op)  = extOpToChar op
encOpChar (ABC_Val _)   = '_' -- value in abc_data 
encOpChar (ABC_Tok _)   = '!' -- token in abc_toks


-- Next, I must encode ABC and Values for VCache-level storage.
--
-- This is different from the compact runtime encoding, though must
-- be pretty close.
--
-- Goals:
--   (a) share structure between `VRef ABC` and `VRef V` for block values
--   (b) lazily parse bytestrings for [V] and [Token] lists in ABC
--   (c) fast slicing for texts, tokens, and block binaries
--   (d) join bytestrings for effective VCache-layer compression
-- 

toCacheABC :: ABC -> ([VRef V], LBS.ByteString)
toCacheABC = error "TODO"

fromCacheABC :: ([VRef V], LBS.ByteString) -> ABC
fromCacheABC = error "TODO"

toCacheVals :: [V] -> ([VRef V], LBS.ByteString)
toCacheVals = error "TODO"

fromCacheVals :: ([VRef V], LBS.ByteString) -> [V]
fromCacheVals = error "TODO"



-- I also need to encode ABC and values for VCache-layer storage.
-- Depending on whether I want to perform compression, I have some
-- options here. What I'd like to do is generate both a bytestring
-- and a list of [VRef V] to encode the whole ABC. This gives an
-- option to compress the bytestring before encoding it.
--
-- Use of a compression algorithm prior to VCache might avoid an
-- extra copy on read, if we decompress directly from VCache memory.
--
-- The ability to lazily process parts of the larger bytestring
-- may also be convenient, e.g. so we can parse tokens into a
-- list incrementally, and the same for the list of values.
-- This suggests leveraging 'sized' slices that we can chomp.


-- Thoughts: if I can preserve the {%word} tokens when linking, this
-- might simplify debugging or error reports. OTOH, it may complicate
-- optimization and other features.

-- | We may 'purify' bytecode to recover the original ABC without any
-- special Wikilon extensions. This purification is direct and naive.
-- Annotations will be injected to preserve value representations.
purifyABC :: ABC -> Pure.ABC
purifyABC = Pure.ABC . flip purifyOps [] . decodeOps

purifyOps :: [Op] -> [Pure.Op] -> [Pure.Op]
purifyOps (op:ops) = purifyOp op . purifyOps ops
purifyOps [] = id

purifyOp :: Op -> [Pure.Op] -> [Pure.Op]
purifyOp (ABC_Prim op) = (:) (Pure.ABC_Prim op)
purifyOp (ABC_Tok tok) = (:) (Pure.ABC_Tok tok)
purifyOp (ABC_Ext extOp) = purifyExtOp extOp
purifyOp (ABC_Val v) = valueToPureABC' v

-- todo: leverage extOp table via lookup array
purifyExtOp :: ExtOp -> [Pure.Op] -> [Pure.Op]
purifyExtOp ExtOp_Inline = (++) (Pure.abcOps "vr$c")

-- Note that an ABC resource can be modeled by using an external
-- block then inlining it. Naked ABC can be encoded as a block for
-- structure sharing purposes.


-- | Flags for substructural attributes. These are recorded for both
-- blocks (the subject of `kf` substructural type tags) and external
-- value resources (to allow copy and drop without fully loading a
-- deep structure).
--
-- I am interested in additional tags for parallelism, memoization.
type Flags = Word16

f_rel, f_aff :: Flags
f_rel = 0x01
f_aff = 0x02

f_copyable, f_droppable :: Flags -> Bool
f_copyable = not . f_includes f_aff
f_droppable = not . f_includes f_rel

f_includes :: Flags -> Flags -> Bool
f_includes f = (f ==) . (f .&.)

-- | Test whether a value is copyable by ^ (not affine)
copyable :: V -> Bool
copyable (N _) = True
copyable (P a b) = (copyable a) && (copyable b)
copyable (L a) = copyable a
copyable (R b) = copyable b
copyable (U) = True
copyable (B _ kf) = f_copyable kf
copyable (S _ v) = copyable v
copyable (T _) = True
copyable (X _ kf) = f_copyable kf

-- | Test whether a value is droppable by % (not relevant)
droppable :: V -> Bool
droppable (N _) = True
droppable (P a b) = (droppable a) && (droppable b)
droppable (L a) = droppable a
droppable (R b) = droppable b
droppable (U) = True
droppable (B _ kf) = f_droppable kf
droppable (S _ v) = droppable v
droppable (T _) = True
droppable (X _ kf) = f_droppable kf

-- | Convert a value into pure Awelon Bytecode (ABC) that will later
-- recompute the same value. External value references are annotated
-- with {&stow} to support recovery of deep structure.
valueToPureABC :: V -> Pure.ABC
valueToPureABC = Pure.ABC . flip valueToPureABC' []

abcStr :: Pure.ABC -> [Pure.Op] -> [Pure.Op]
abcStr = (++) . Pure.abcOps

-- conversion with a difference list
valueToPureABC' :: V -> [Pure.Op] -> [Pure.Op]
valueToPureABC' = vops where
    pureOp = (:)
    vops (N i) = abcStr $ Pure.itoabc i
    vops (P a b) = vops b . vops a . abcStr "l"
    vops (L a) = vops a . abcStr "V"
    vops (R b) = vops b . abcStr "VVRWLC"
    vops (U) = abcStr "vvrwlc"
    vops (B abc flags) = b . k . f where
        b = pureOp (Pure.ABC_Block (purifyABC abc))
        k = if f_copyable flags then id else abcStr "k"
        f = if f_droppable flags then id else abcStr "f"
    vops (S tok v) = vops v . pureOp (Pure.ABC_Tok tok)
    vops (T txt) = pureOp (Pure.ABC_Text txt)
    vops (X ref _) = vops (deref' ref) . pureOp (Pure.ABC_Tok "&stow")

-- monoid instance shall compose functionality, running all
-- operations (and popping all values) off the first function
-- before reaching any from the second.
instance Monoid ABC where
    mempty = ABC mempty mempty mempty
    mappend a b = ABC
        { abc_code = abc_code a <> abc_code b
        , abc_data = abc_data a <> abc_data b
        , abc_toks = abc_toks a <> abc_toks b
        }
    mconcat lst = ABC
        { abc_code = mconcat (fmap abc_code lst)
        , abc_data = mconcat (fmap abc_data lst)
        , abc_toks = mconcat (fmap abc_toks lst)
        }

impossible :: String -> a
impossible = error . abcErr

abcErr :: String -> String
abcErr = (++) "Wikilon.ABC: "

