{-# LANGUAGE ViewPatterns, OverloadedStrings, DeriveDataTypeable #-}

-- | Wikilon uses Awelon Bytecode (ABC) to model user-defined behavior.
-- ABC has many nice properties for security, distribution, streaming,
-- simplicity, parallelism, and dynamic linking. However, ABC is based
-- on a purely functional style that is pretty far removed from modern
-- CPUs. Direct interpretation of ABC is not performance competitive.
--
-- Further, while ABC's dynamic linking model (use tokens to encrypt 
-- and name provider-independent resources by secure hash) has great
-- potential for distributed computing, its overhead is a little high 
-- for internal use - requiring naming, caching, GC, etc..
--
-- To address these issues, Wikilon internally uses a variant of ABC
-- with operators to accelerate common subprograms and integration
-- with VCache for interning, structure sharing, and linking. ABC is
-- compiled to the internal variant, and may be decompiled back to
-- pure ABC.
--
module Wikilon.ABC
    ( ABC(..)
    , Op(..)
    , PrimOp(..)
    , ExtOp(..), extOpTable
    , Token
    ) where

import Data.Typeable
import qualified Data.Array.IArray as A

import Wikilon.ABC.Pure (Token, PrimOp(..))
import qualified Wikilon.ABC.Pure as Pure
import Wikilon.Value
import Database.VCache

newtype ABC = ABC { abcOps :: [Op] }
    deriving (Eq, Typeable)

data Op 
    = ABC_Prim !PrimOp      -- ABC primitives
    | ABC_Ext  !ExtOp       -- extended set of single-character operators
    | ABC_Val  !(Value ABC) -- texts, blocks, values (∀e.e→(Val * e)).
    | ABC_Link !(VRef ABC)  -- lazily load the referenced subprogram
    | ABC_Tok {-# UNPACK #-} !Token -- {token} text
    deriving (Eq, Typeable)


-- | Extended Operations are essentially a dictionary recognized by
-- Wikilon for specialized implementations, e.g. to accelerate an
-- interpreter or support tail-call optimizations. This is similar
-- to ABCD, though developed independently from ABCD.
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
-- future ABC expansions (though such are extremely unlikely at this
-- point in time). So, ExtOps can be considered a prototype for ABCD.
extOpTable :: [(ExtOp, Char, Pure.ABC)]
extOpTable =
    [(Op_Inline,  '£', "vr$c")
    ,(Op_Apc,     '¢', "$c")
    ,(Op_FixHalf, 'ȳ', "^'ow^'zowvr$c")
    ,(Op_FixFull, 'Ȳ', "[^'ow^'zowvr$c]^'ow^'zowvr$c")

    ,(Op_Intro1L, 'û', "vvrwlc")
    ,(Op_Elim1L,  'ç', "vrwlcc")
    ,(Op_Intro0L, 'Û', "VVRWLC")
    ,(Op_Elim0L,  'Ç', "VRWLCC")

    ,(Op_prim_swap,   'ƨ', "vrwlc")
    ,(Op_prim_mirror, 'Ƨ', "VRWLC")
    ]


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



-- | abcSimplify performs a simple optimization on ABC code based on
-- recognizing short sequences of ABC that can be removed. E.g.
--
--   LF, SP, ww, zz, vc, cv, rl, lr, WW, ZZ, VC, CV, RL, LR
-- 
-- In addition, we translate 'zwz' to 'wzw' (same for sums).
--
abcSimplify :: [ABC_Op ext] -> [ABC_Op ext]
abcSimplify = zSimp []

zSimp :: [ABC_Op ext] -> [ABC_Op ext] -> [ABC_Op ext]
zSimp (ABC_Prim a:as) (ABC_Prim b:bs) | opsCancel a b = zSimp as bs
zSimp rvOps (ABC_Block block : ops) = zSimp (ABC_Block (abcSimplify block) : rvOps) ops
zSimp rvOps (ABC_Prim ABC_SP : ops) = zSimp rvOps ops
zSimp rvOps (ABC_Prim ABC_LF : ops) = zSimp rvOps ops
zSimp (ABC_Prim ABC_w : ABC_Prim ABC_z : rvOps) (ABC_Prim ABC_z : ops) =
    zSimp rvOps (ABC_Prim ABC_w : ABC_Prim ABC_z : ABC_Prim ABC_w : ops)
zSimp (ABC_Prim ABC_W : ABC_Prim ABC_Z : rvOps) (ABC_Prim ABC_Z : ops) =
    zSimp rvOps (ABC_Prim ABC_W : ABC_Prim ABC_Z : ABC_Prim ABC_W : ops)
zSimp rvOps (b:bs) = zSimp (b:rvOps) bs
zSimp rvOps [] = L.reverse rvOps

opsCancel :: PrimOp -> PrimOp -> Bool
opsCancel ABC_l ABC_r = True
opsCancel ABC_r ABC_l = True
opsCancel ABC_w ABC_w = True
opsCancel ABC_z ABC_z = True
opsCancel ABC_v ABC_c = True
opsCancel ABC_c ABC_v = True
opsCancel ABC_L ABC_R = True
opsCancel ABC_R ABC_L = True
opsCancel ABC_W ABC_W = True
opsCancel ABC_Z ABC_Z = True
opsCancel ABC_V ABC_C = True
opsCancel ABC_C ABC_V = True
opsCancel _ _ = False

-}
