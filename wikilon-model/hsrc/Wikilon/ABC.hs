{-# LANGUAGE DeriveDataTypeable #-}
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
    , Token
    , Text
    ) where

import Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Array.IArray as A
import Wikilon.ABC.Pure (PrimOp(..))
import qualified Wikilon.ABC.Pure as Pure
import Wikilon.Text
import Wikilon.Token

-- | Wikilon's internal representation of Awelon Bytecode is 
-- associated with a stack of quoted (or partially evaluated)
-- values. The values in question might be computed lazily or
-- in parallel.
--
-- This is essentially an encoding for `[Op]`, albeit while
-- preserving a compact bytestring for the bulk of the code.
data ABC = ABC
    { abc_code :: !LBS.ByteString
    , abc_data :: ![V]
    } deriving (Eq, Typeable)

-- | A simple ABC operation, with potential for accelerated
-- operations corresponding to common sequences of ABC. 
-- (I.e. ABCD-like, except without the standards committee.)
--
-- Blocks and embedded texts are modeled as quoted values. 
data Op
    = ABC_Prim  !PrimOp     -- the basic 42 ABC operators
    | ABC_Tok   !Token      -- e.g. value sealers, annotations
    | ABC_Ext   !ExtOp      -- ABCD-like performance extensions
    | ABC_Val   V           -- a quoted value

-- | A basic value type for performance extensions.
data V = V -- todo
    deriving (Eq, Typeable)

-- | Accelerated operations corresponding to common substrings
-- of Awelon Bytecode.
data ExtOp = ExtOp -- todo 
    deriving (Eq, Ord, A.Ix, Enum, Bounded)

