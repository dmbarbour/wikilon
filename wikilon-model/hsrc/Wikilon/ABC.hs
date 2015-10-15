{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
-- | While Awelon Bytecode may be naively interpreted, doing so is
-- not efficient. Wikilon shall pre-process bytecode to support many
-- performance tweaks:
--
-- * bytecode held in relatively compact bytestrings
-- * fast slicing for texts, blocks, tokens in bytecode
-- * quoted values may be computed once ahead of time
-- * faster copy via copyable annotations for values
-- * larger-than-memory values supportable via VCache
-- * shared structure for large values via VCache
-- * ABCD-like extended dictionary of accelerated ops
-- 
module Wikilon.ABC 
    ( ABC(..)
    , Op(..)
    , V(..)
    , PrimOp(..)
    , Token, Text
    , decodeOp, decodeOps, encodeOps

    , purifyABC, valueToPureABC
    , copyable, droppable
    ) where

import Data.Typeable (Typeable)
import Data.Word
import Data.Bits
import Data.Monoid
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
data ABC = ABC
    { abc_code :: !LBS.ByteString
    , abc_data :: ![V]
    } deriving (Eq, Typeable)

-- | A simple ABC operation, with potential for accelerated
-- operations corresponding to common sequences of ABC, and
-- for ad-hoc values (rather than just texts and blocks).
data Op
    = ABC_Prim  !PrimOp     -- the basic 42 ABC operators
    | ABC_Tok   !Token      -- e.g. value sealers, annotations
    | ABC_Ext   !ExtOp      -- ABCD-like performance extensions
    | ABC_Val   V           -- a quoted value, block, or text
    deriving (Eq, Typeable)

-- | Values that can be represented in ABC, with a simple
-- extension for larger than memory values. Later, I might
-- add additional extensions for fast vector and matrix
-- processing and similar.
data V 
    = N !Integer        -- number (integer)
    | P V V             -- product of values
    | L V | R V         -- sum of values (left or right)
    | U                 -- unit value
    | B ABC Flags       -- block value
    | S !Token V        -- sealed and special values
    | T !Text           -- embedded text value
    | X (VRef V) Flags  -- external value resource
    deriving (Eq, Typeable)


-- | Accelerated operations corresponding to common substrings of
-- Awelon Bytecode (and hence to common functions). These encode
-- as a single character and are processed by a dedicated function.
data ExtOp
    = ExtOp_Inline -- i; vr$c, also for tail calls etc.
    -- I'll probably want:
    --  common data plumbing
    --  fixpoint function
    --  list processing (reverse, append, etc.)
    --  leverage lists as vectors or matrices
    --  functions for rational numbers
    --  functions for a floating point encoding

    deriving (Eq, Ord, A.Ix, Enum, Bounded)



-- | Wikilon ABC is a compact representation for [Op].
decodeOps :: ABC -> [Op]
decodeOps abc = case decodeOp abc of
    Just (op, abc') -> op : decodeOps abc'
    Nothing -> []

-- | Obtain the compact encoding for a list of ABC operations. 
encodeOps :: [Op] -> ABC
encodeOps = error "todo: encode ops"

-- | Decode the first operation from bytecode.
decodeOp :: ABC -> Maybe (Op, ABC)
decodeOp = error "todo: decode op"

-- | We may 'purify' bytecode to recover the original ABC without any
-- special Wikilon extensions. This purification is naive and direct,
-- but will use difference lists 
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
-- with {&intern} to support recovery of deep structure.
valueToPureABC :: V -> Pure.ABC
valueToPureABC = Pure.ABC . flip valueToPureABC' []

-- conversion with a difference list
valueToPureABC' :: V -> [Pure.Op] -> [Pure.Op]
valueToPureABC' = vops where
    bytes = (++) . Pure.abcOps 
    pureOp = (:)
    vops (N i) = bytes $ Pure.itoabc i
    vops (P a b) = vops b . vops a . bytes "l"
    vops (L a) = vops a . bytes "V"
    vops (R b) = vops b . bytes "VVRWLC"
    vops (U) = bytes "vvrwlc"
    vops (B abc flags) = b . k . f where
        b = pureOp (Pure.ABC_Block (purifyABC abc))
        k = if f_copyable flags then id else bytes "k"
        f = if f_droppable flags then id else bytes "f"
    vops (S tok v) = vops v . pureOp (Pure.ABC_Tok tok)
    vops (T txt) = pureOp (Pure.ABC_Text txt)
    vops (X ref _) = vops (deref' ref) . pureOp (Pure.ABC_Tok "&intern")

-- monoid instance shall compose functionality, running all
-- operations (and popping all values) off the first function
-- before reaching any from the second.
instance Monoid ABC where
    mempty = ABC mempty mempty
    mappend a b = ABC
        { abc_code = abc_code a <> abc_code b
        , abc_data = abc_data a <> abc_data b
        }
    mconcat lst = ABC
        { abc_code = mconcat (fmap abc_code lst)
        , abc_data = mconcat (fmap abc_data lst)
        }
