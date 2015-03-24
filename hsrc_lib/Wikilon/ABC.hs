{-# LANGUAGE BangPatterns, ViewPatterns, OverloadedStrings, DeriveDataTypeable #-}

-- | Wikilon uses Awelon Bytecode (ABC) to model user-defined behavior.
-- ABC has many nice properties for security, distribution, streaming,
-- simplicity, parallelism, and dynamic linking. 
--
-- See <https://github.com/dmbarbour/awelon/blob/master/AboutABC.md>.
--
-- For performance, ABC supports {#resourceId} tokens for separate
-- compilation and dynamic linking, and ABC is extensible to ABCD 
-- (ABC Deflated) which includes a dictionary of common functions as
-- operators. ABC is intended for mix of just in time and ahead of 
-- time compilation.
--
-- Wikilon doesn't provide any just-in-time compilation, mostly because
-- GHC Haskell doesn't make JIT easy. Plugins might eventually be used
-- for this role. 
--
-- But, in the short term, we can mitigate interpreter performance by
-- pre-processing the bytecode:
--
--  * dictionary of built-in operators to accelerate performance
--  * support precomputed values and cheap large-value quotations 
--  * linked ABC resources (including value resources) with VCache
--  * fast slicing for texts, blocks, and tokens
--  * preserve base-16 compression in texts and tokens
--  * GZip compression for larger ABC texts.
--
-- Unlike ABCD, the internal Wikilon dictionary doesn't wait on any
-- standards committee. Unlike {#resourceId} tokens, VCache refs are
-- cheap and support reference counting garbage collection.
--
module Wikilon.ABC
    ( ABC(..)
    , Value(..)
    , Rsc, Token
    ) where

import Data.Typeable
import qualified Data.Array.IArray as A
import qualified Data.List as L
import Data.Word
import Data.Bits
import qualified Data.ByteString.Lazy as LBS
import Database.VCache
import Wikilon.Text


-- | Wikilon's internal ABC has two parts: the main bytecode, and
-- any large embedded values. The bytecode is not pure ABC. Rather,
-- the representations are tuned so texts and blocks can be quickly
-- sliced, and a stack of precomputed values may be loaded.
--
-- Equality for bytecode is inherently structural.
data ABC = ABC
    { abc_code :: !LBS.ByteString
    , abc_data :: ![Value]
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
-- Blocks may be affine or relevant, and other substructural types
-- are possible. Sealed values are not always discretionary like the
-- {:foo}. If cryptographically protected, they use `{$format}`. 
--
-- At the moment, Wikilon will focus on the basics forms plus:
--
--  * dedicated support for text
--  * lazily loaded value resources
--
-- I would like to eventually have efficient vectors and matrices,
-- but those are relatively low priority.
--
-- Equality on values is structural.
data Value
    = Number {-# UNPACK #-} !Rational
    | Pair Value Value
    | SumL Value
    | SumR Value
    | Unit
    | Block !ABC {-# UNPACK #-} !Flags
    | Sealed {-# UNPACK #-} !Token Value
    | Link {-# UNPACK #-} !Rsc {-# UNPACK #-} !Flags
    deriving (Eq, Typeable)

type Token = Text

-- | Awelon Bytecode (ABC) supports dynamic loading of bytecode resources
-- via {#resourceId} tokens in the source code. The resourceId uses a 
-- secure hash of the bytecode for provider independence, and may also
-- include a decryption key.
-- 
-- To support value resources in particular, ABC supports suffixes. For
-- example, {#resourceId'} and {#resourceId'kf} to access a plain value
-- or a linear value respectively. The type information here is coarse, 
-- just enough for whole-value data plumbing. The value may be lazily
-- loaded, rather than processed immediately. 
--
-- But to avoid the heavy-weight resource IDs, Wikilon uses a VRef for
-- internal resources (serializing with a control code).
type Rsc = VRef ABC

-- | Flags for substructural types
--   bit 0: true if relevant (no drop with %)
--   bit 1: true if affine   (no copy with ^)
--   bit 2..7: Reserved. Tentatively: 
--     bit 2: true if local     (no send as AVM content)
--     bit 3: true if ephemeral (no store in AVM state) 
--
-- Besides the basic substructural types, it might be worth using
-- annotations to enforce a few new ones.
type Flags = Word8

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
--  * precompress ABC's Base16 for texts and tokens (tune text type)
--  * GZip compression of larger ABC at VCache layer (option?)
--    * combine texts for precomputed values and ABC for best compression
--

{-

-- | Awelon Bytecode is simply a sequence of operations.
newtype ABC = ABC { abcOps :: [Op] }
    deriving (Eq, Typeable)
-- Thoughts: a list isn't ideal for modeling composition as concatenation.
-- However, we can alternatively represent composition using a block with
-- the inline function (vr$c).
--
-- Also, I'm not sure it wouldn't be better to simply process bytestrings
-- directly... excepting that I need to model values, and it would be useful
-- to know exactly how large to slice

-- | Wikilon's internal variant of ABC for efficient computation.
data Op 
    = ABC_Prim !PrimOp   -- ABC primitives
    | ABC_Ext  !ExtOp    -- extended set of single-character operators
    | ABC_Val  !Value    -- texts, blocks, values (∀e.e→(Val * e)).
    | ABC_Lnk  {-# UNPACK #-} !Rsc {-# UNPACK #-} !TyF -- internal {#resourceId} 
    | ABC_Tok  {-# UNPACK #-} !Token -- {token} text
    deriving (Eq, Typeable)
-- Thoughts:
--   for performance, it might be better to use `ABC_Prims [PrimOp]`, or
--   even a vector of primitives, or perhaps even a bytestring (since each
--   prim may be encoded using one byte). This could improve locality when
--   processing code
--
--   also, is this the best way to model internal resources? I'm not sure.



-- | Extended Operations are essentially a dictionary recognized by
-- Wikilon for specialized implementations, e.g. to accelerate an
-- interpreter or support tail-call optimizations. ExtOp is similar
-- to ABCD, but doesn't require careful standardization.
--
-- A dictionary of extended ops shall be available for HTTP access
-- via something like a \/builtins request. 
data ExtOp
    -- common inline behaviors
    = Op_Inline -- vr$c (full inline)
    | Op_Apc    -- $c (a useful tail-call)

    -- favorite fixpoint function
    | Op_FixHalf --  ^'ow^'zowvr$c 
    | Op_FixFull -- [^'ow^'zowvr$c]^'ow^'zowvr$c

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
-- I've decided to follow ABCD's mandate to leave the ASCII range to
-- future ABC expansions. ExtOps is effectively a prototype for ABCD.
extOpTable :: [(ExtOp, Char, Pure.ABC)]
extOpTable =
    [(Op_Inline,  '¥', "vr$c")
    ,(Op_Apc,     '¢', "$c")
    ,(Op_FixHalf, 'ȳ',  "^'ow^'zowvr$c")
    ,(Op_FixFull, 'Ȳ', "[^'ow^'zowvr$c]^'ow^'zowvr$c")

    ,(Op_Intro1L, 'ǔ', "vvrwlc")
    ,(Op_Elim1L,  'ç', "vrwlcc")
    ,(Op_Intro0L, 'Ǔ', "VVRWLC")
    ,(Op_Elim0L,  'Ç', "VRWLCC")

    ,(Op_prim_swap,   'ƨ', "vrwlc")
    ,(Op_prim_mirror, 'Ƨ', "VRWLC")
    ]

impossible :: String -> a
impossible eMsg = error ("Wikilon.ABC: " ++ eMsg)

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



-- | Values in Awelon project (and ABC) have a few basic forms:
--
--    (a * b) -- pairs    ~Haskell (a,b)
--    (a + b) -- sums     ~Haskell (Either a b)
--    1       -- unit     ~Haskell ()
--    0       -- void     ~Haskell EmptyDataDecls
--    N       -- numbers  ~Haskell Rational
--    [a→b]   -- blocks   ~Haskell functions or arrows
--    a:foo   -- sealed   ~Haskell newtype (`:foo` label)
--
-- Further, blocks may be affine or relevant, ABC defines a format
-- for cryptographically sealed values, and high performance compact
-- data structures (e.g. vectors and matrices) could be used under
-- the hood with some clever annotations and extended operations. 
--
-- At the moment, Wikilon will focus on the basics plus:
--
--  * dedicated support for text
--  * lazily loaded value resources
--
-- I would like to eventually have efficient vectors, matrices, and
-- cryptographic content. But a lot of these ideas can easily wait.
--
data Value
    = Number {-# UNPACK #-} !Rational
    | Pair !Value Value
    | SumL Value
    | SumR Value
    | Unit
    | Block !ABC {-# UNPACK #-} !TyF
    | Sealed {-# UNPACK #-} !Token !Value
    | Load {-# UNPACK #-} !Rsc {-# UNPACK #-} !TyF
    deriving (Eq, Typeable)



-}




{-


import Control.Applicative ((<$>), pure)
import Control.Monad (join)
import Control.Exception (assert)
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Codec.Binary.UTF8.Generic as UTF8
import qualified Data.List as L
import qualified Data.Array.Unboxed as A
import Data.Ratio
import Data.Word (Word16)
import Data.String
import Wikilon.Char
import qualified Wikilon.ParseUtils as P


-- NOTE: Binaries can be embedded in ABC text or tokens by use of a specialized 
-- base16 alphabet: bdfghjkmnpqstxyz. This is a-z minus the vowels and `vrwlc` 
-- data plumbing. A special compression pass then encodes binaries with 0.8%
-- overhead (for large binaries) compared to a raw encoding. Some binaries can
-- be further compressed by the normal LZSS compression pass.

skip :: a -> b -> b
skip = flip const

-- help type inference a little
primOp :: PrimOp -> Op
primOp = ABC_Prim


putABC :: (Quotable ext) => [ABC_Op ext] -> B.PutM ()
putABC = mapM_ putOp

putOp :: (Quotable ext) => ABC_Op ext -> B.PutM ()
putOp (ABC_Prim op) = B.put (abcOpToChar op)
putOp (ABC_Block abc) = B.put '[' >> putABC abc >> B.put ']'
putOp (ABC_Text txt) = B.put '"' >> putMLT txt >> B.put '\n' >> B.put '~'
putOp (ABC_Tok tok) = assert (validTok tok) $ B.put '{' >> mapM_ B.put tok >> B.put '}'
putOp (ABC_Ext ops) = putABC (quote ops)

putMLT :: String -> B.PutM ()
putMLT ('\n':cs) = B.put '\n' >> B.put ' ' >> putMLT cs
putMLT (c:cs) = B.put c >> putMLT cs
putMLT [] = return ()



-}
