{-# LANGUAGE BangPatterns, ViewPatterns, OverloadedStrings, DeriveDataTypeable #-}

-- | Wikilon uses Awelon Bytecode (ABC) to model user-defined behavior.
-- ABC has many nice properties for security, distribution, streaming,
-- simplicity, parallelism, and dynamic linking. 
--
-- See <https://github.com/dmbarbour/awelon/blob/master/AboutABC.md>.
--
-- ABC has a major weakness: naive interpretation is inefficient. ABC
-- must be compiled for effective use. JIT compilation is viable, as
-- is separate compilation via {#resourceId} tokens. Wikilon lacks JIT
-- compilation, though approaches using Haskell plugins or Lambdachine
-- may be viable. 
--
-- An intermediate concept for compilation to recognize a dictionary 
-- of common subprograms and accelerate them with built-in functions 
-- in the interpreter. A planned ABCD (ABC Deflated) is expected to
-- use this idea for compression and fast interpretation of streaming
-- code. But ABCD must be standardized carefully.
--
-- Wikilon aims to mitigate performance by the following:
--
--  * dictionary of ABCD-like accelerated operators
--  * separate values for efficient quotation
--  * lazy loading of large values through VCache 
--  * fast slicing for texts, blocks, and tokens
--  * fast compression for storage of large ABC
--
-- Unlike ABCD, the internal Wikilon dictionary doesn't wait on any
-- standards committee. Unlike {#resourceId} tokens, VCache refs are
-- cheap and support implicit reference counting garbage collection.
--
module Wikilon.ABC
    ( ABC(..)
    , Value(..)
    , Rsc
    , Text
    , Token

    , copyable
    , droppable
    , toText, toTextB
    
    , ExtOp(..), extOpTable

    , Flags
    , flag_affine
    , flag_relevant

    ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Typeable (Typeable)
import qualified Data.Array.IArray as A
import qualified Data.List as L
import Data.Word
import Data.Bits
import Data.Ratio
import Data.Char
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.UTF8 as UTF8
import Database.VCache
import qualified ABC.Basic as Pure

type Token = UTF8.ByteString
type Text = LazyUTF8.ByteString

-- | Wikilon's internal representation of Awelon Bytecode (ABC). This
-- encoding has several features:
--
--  * blocks and texts are tuned for fast slicing
--  * extended op set to accelerate common functions
--  * support to lazily load large values in VCache
--  * stack of values for fast quotation and laziness
--
-- Equality for bytecode is inherently structural.
data ABC = ABC
    { abc_code :: !LBS.ByteString -- Wikilon internal ABC variant 
    , abc_data :: [Value]         -- stack of precomputed values
    } deriving (Eq, Typeable)
    -- any relationship to VCache must be modeled using a Value.

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
-- Tentatively, I would like in the future to optimize representations
-- for common data structures, especially vectors and matrices.
data Value
    = Number !Rational
    | Pair Value Value
    | SumL Value
    | SumR Value
    | Unit
    | Block !ABC {-# UNPACK #-} !Flags
    | Sealed !Token Value
    | Linked !Rsc {-# UNPACK #-} !Flags
    deriving (Eq, Typeable)

-- | Awelon Bytecode (ABC) defines conventions for dynamic linking and
-- separate compilation of resources. A {#resourceId} token identifies
-- the resource using a secure hash and inlines the bytecode. Variants
-- exist to support lazy loading when the resource has a value type, 
-- i.e. type ∀e.(e→(Value*e)).
--
-- But full {#resourceId} tokens are heavy-weight, and they complicate
-- garbage collection. 
--
-- So Wikilon instead uses VCache, which provides implicit links, cache,
-- and reference counting garbage collection. Sadly, this does sacrifice
-- operation in a networked setting. But it is still very convenient and
-- enables Wikilon to scale to very large values, e.g. whole filesystems
-- can be modeled using a trie.
type Rsc = VRef ABC

-- | Flags for substructural types
--   bit 0: true if relevant (no drop with %)
--   bit 1: true if affine   (no copy with ^)
--   bit 2..7: Reserved. Tentatively: 
--     bit 2: true if local     (no send as message content)
--     bit 3: true if ephemeral (no store in machine state)
--
-- Besides the basic substructural types, it might be worth using
-- annotations to enforce a few new ones.
type Flags = Word8

flag_affine, flag_relevant :: Flags
flag_affine   = 0x02
flag_relevant = 0x01

flags_include :: Flags -> Flags -> Bool
flags_include f fs = f == (f .&. fs)
{-# INLINE flags_include #-}

f_copyable :: Flags -> Bool
f_copyable = not . flags_include flag_affine
{-# INLINE f_copyable #-}

f_droppable :: Flags -> Bool
f_droppable = not . flags_include flag_relevant
{-# INLINE f_droppable #-}

-- | Is this value copyable (not affine)
copyable :: Value -> Bool
copyable (Number _) = True
copyable (Pair a b) = copyable a && copyable b
copyable (SumL a) = copyable a
copyable (SumR b) = copyable b
copyable Unit = True
copyable (Block _ f) = f_copyable f
copyable (Sealed _ v) = copyable v
copyable (Linked _ f) = f_copyable f

-- | Is this value droppable (not relevant)
droppable :: Value -> Bool
droppable (Number _) = True
droppable (Pair a b) = droppable a && droppable b
droppable (SumL a) = droppable a
droppable (SumR b) = droppable b
droppable Unit = True
droppable (Block _ f) = f_droppable f
droppable (Sealed _ v) = droppable v
droppable (Linked _ f) = f_droppable f

-- | Try to convert a value into text.
toText :: Value -> Maybe Text
toText v = BB.toLazyByteString <$> toTextB v

-- | Try to convert a value into a text builder.
toTextB :: Value -> Maybe BB.Builder
toTextB (Pair (Number r) b) = do
    c <- numToChar r
    cs <- toTextB b
    return (BB.charUtf8 c <> cs)
toTextB Unit = return mempty
toTextB _ = mzero

numToChar :: Rational -> Maybe Char
numToChar r =
    let n = numerator r in
    let d = denominator r in
    let ok = (1 == d) && (0 <= n) && (n <= 0x10ffff) in
    if not ok then Nothing else
    Just (chr (fromInteger n)) 

readVarNat :: LBS.ByteString -> (Int, LBS.ByteString)
readVarNat = r 0 where
    r !n !t = case LBS.uncons t of
        Nothing -> impossible "bad VarInt in Wikilon.ABC code"
        Just (byte, t') ->
            let n' = n `shiftL` 7 .|. (fromIntegral (byte .&. 0x7f)) in
            let bDone = (0 == (byte .&. 0x80)) in
            if bDone then (n', t') else r n' t'

impossible :: String -> a
impossible eMsg = error $ "Wikilon.ABC: " ++ eMsg

-- | Extended Operations are essentially a dictionary recognized by
-- Wikilon for specialized implementations, e.g. to accelerate an
-- interpreter or support tail-call optimizations. ExtOp is similar
-- to ABCD, but doesn't require careful standardization.
data ExtOp
    -- common inline behaviors
    = Op_Inline -- vr$c (full inline)
    | Op_Apc    -- $c (tail call)

    -- favorite fixpoint function
    -- 
    | Op_Fixpoint -- [^'ow^'zowvr$c]^'ow^'zowvr$c

    -- mirrored v,c operations
    | Op_Intro1L  -- vvrwlc
    | Op_Elim1L   -- vrwlcc
    | Op_Intro0L  -- VVRWLC
    | Op_Elim0L   -- VRWLCC

    | Op_prim_swap    -- vrwlc
    | Op_prim_mirror  -- VRWLC
    -- stack swaps?
    -- hand manipulations?
    -- more as needed!
    deriving (Ord, Eq, Bounded, Enum, A.Ix)


-- | Table of ExtOps and relevant semantics.
--
-- I'll follow ABCD's mandate to leave the ASCII range to future ABC
-- expansions (though such expansions are very unlikely). ExtOps is
-- effectively a prototype for ABCD.
extOpTable :: [(ExtOp, Char, Pure.ABC)]
extOpTable =
    [(Op_Inline,  '£', "vr$c")
    ,(Op_Apc,     '¢', "$c")
    ,(Op_Fixpoint, '¥', "[^'ow^'zowvr$c]^'ow^'zowvr$c")

    ,(Op_Intro1L, 'ń', "vvrwlc")
    ,(Op_Elim1L,  'ć', "vrwlcc")
    ,(Op_Intro0L, 'Ń', "VVRWLC")
    ,(Op_Elim0L,  'Ć', "VRWLCC")

    ,(Op_prim_swap,   'ś', "vrwlc")
    ,(Op_prim_mirror, 'Ś', "VRWLC")
    ]

-- QUESTION: How are resources represented in VCache?
--
-- Wikilon's internal ABC uses 'escapes' to access precomputed data.
-- Thus, to properly encode our bytecode resources, we must reliably
-- regenerate any precomputed content.
--
-- Following ABC's example, I'll make these implicit escapes very
-- explicit and functionally meaningful. Each block will also pop a
-- value because it recursively contains some precomputed data. The
-- ability to push a value allows me to directly encode blocks and
-- avoid recursive encodings in the 'precompute the value' sections.
--
--      [       pop value for encoded block
--      DLE     pop value resource from stack
--      DC1     push value resource to stack
--      ESC     inline a bytecode resource
--
-- All link resources must be pushed to the toplevel. So let's say
-- we start with a stack of link resources, use this to compute our
-- main values stack, then use our main values stack for our ABC.
-- This avoids arbitrary recursion.
--
-- Additionally, we'll capture sizes for every block, token, and text:
--
--      [(Size)bytecode]
--      {(Size)token}
--      "(Size)textWithoutEscapes~
--
-- The final character isn't essential, but serves as a sanity check
-- and a visible delimiter for debugging. In case of tokens and text, 
-- I'll perform ABC.Base16 compression before computing the size, such
-- that binaries at least have a compact encoding at the final storage
-- layers.
--
-- The whole stream will finally be subjected to a fast compression.
-- Candidate algorithms: LZ4, Snappy. These algorithms aren't great
-- for compaction, but ABC should be a relatively easy target. And
-- even a 50% compaction could make a useful performance difference
-- on read. (Besides, decompression simply becomes part of copying
-- the input.)
--
















--  * develop dictionary of built-in operators to accelerate performance
--    * each operator has simple expansion to ABC
--    * recognize or parse operator from raw ABC
--    * how to best encode? Not sure. Maybe 0xFD+VarNat?
--  * precomputed values and cheap quotations 
--    * stack of values per node 
--    * similar to how VCache uses stack of VRefs?
--  * support linked ABC resources with VCache
--    * resources involve VRefs to more ABC nodes
--    * preferably leverage same stack of values
--    * lazy loading of value resources
--  * rewrite texts, blocks, and tokens to support fast slicing
--    * "(length)(content)~
--    * [(links)(length)(content)]
--    * {(length)(content)}
--    * length and links have VarNat representation
--    * escapes are removed from the text content
--  * Compression of larger ABC at VCache layer (option?)
--    * combine texts for precomputed values and ABC for best compression
--    * Base16 compression for tokens and texts
--

{-

extOpCharArray :: A.Array ExtOp Char 
extOpCharArray = A.accumArray ins eUndef (minBound, maxBound) lst where
    lst = fmap (\(op,c,_)->(op,c)) extOpTable
    eUndef = impossible "missing character encoding for ABC ExtOp"
    ins = flip const

extOpDefArray :: A.Array ExtOp Pure.ABC
extOpDefArray = A.accumArray ins eUndef (minBound, maxBound) lst where
    lst = fmap (\(op,_,d)->(op,d)) extOpTable
    eUndef = impossible "missing definition for ABC ExtOp"
    ins = flip const

extCharOpArray :: A.Array Char (Maybe ExtOp) 
extCharOpArray = A.accumArray ins Nothing (lb, ub) lst where
    lst = fmap (\(op,c,_)->(c,op)) extOpTable
    lb = L.minimum (fmap fst lst)
    ub = L.maximum (fmap fst lst)
    ins _ op = Just op

extOpToChar :: ExtOp -> Char
extOpToChar op = extOpCharArray A.! op

extOpToDef  :: ExtOp -> Pure.ABC
extOpToDef op = extOpDefArray A.! op

extCharToOp :: Char -> Maybe ExtOp
extCharToOp c | inBounds = extCharOpArray A.! c
              | otherwise = Nothing
    where inBounds = (lb <= c) && (c <= ub)
          (lb,ub) = A.bounds extCharOpArray

-}

{-
Todo: Quotation of Wikilon's ABC back into pure ABC
-}

-- stub to prevent type errors for now
instance VCacheable ABC where
    put abc = error "TODO: VCacheable ABC"
    get = error "TODO: VCacheable ABC"

instance VCacheable Value where
    put v = error "TODO: VCacheable Value"
    get = error "TODO: VCacheable Value"

