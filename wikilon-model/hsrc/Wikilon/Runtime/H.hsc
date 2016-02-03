
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, GeneralizedNewtypeDeriving #-}
-- | This module wraps the `wikilon-runtime.h` for `libwikilon-runtime.so`.
-- The high performance core of our Wikilon runtime is implemented at the C
-- layer. 
module Wikilon.Runtime.H
    ( WIKRT_ENV, WIKRT_CX, WIKRT_VAL
    , WIKRT_ERR(..), table_WIKRT_ERR, WIKRT_ERR_NUM
    , WIKRT_OPCODE(..), table_WIKRT_OPCODE, WIKRT_OPCODE_NUM
    , WIKRT_VTYPE(..), table_WIKRT_VTYPE, WIKRT_VTYPE_NUM
    , WIKRT_ABC_OPTS(..), table_WIKRT_ABC_OPTS, WIKRT_ABC_OPTS_NUM
    , wikrt_unit, wikrt_unit_inr, wikrt_unit_inl
    , wikrt_tok_buffsz
    , wikrt_env_create, wikrt_env_destroy, wikrt_env_sync
    , wikrt_cx_create, wikrt_cx_destroy, wikrt_cx_reset, wikrt_cx_env
    , wikrt_cx_size_min, wikrt_cx_size_max
    , wikrt_abcd_operators, wikrt_abcd_expansion, wikrt_valid_token
    ) where

#include <wikilon-runtime.h>

import Foreign
import Foreign.C
import Data.Word
import qualified Data.Array.Unboxed as A


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data WIKRT_ENV
data WIKRT_CX
type WIKRT_VAL = #type wikrt_val
type CBool = #type bool

wikrt_unit, wikrt_unit_inr, wikrt_unit_inl :: WIKRT_VAL
wikrt_unit = #const WIKRT_UNIT
wikrt_unit_inr = #const WIKRT_UNIT_INR
wikrt_unit_inl = #const WIKRT_UNIT_INL

wikrt_tok_buffsz :: Int
wikrt_tok_buffsz = #const WIKRT_TOK_BUFFSZ


-- | Possible error codes for Wikilon runtime functions.
data WIKRT_ERR
    = WIKRT_OK
    | WIKRT_INVAL
    | WIKRT_DBERR
    | WIKRT_NOMEM
    | WIKRT_CXFULL
    | WIKRT_BUFFSZ
    | WIKRT_TXN_CONFLICT
    | WIKRT_STREAM_WAIT
    | WIKRT_QUOTA_STOP
    | WIKRT_ASSERT_FAIL
    | WIKRT_TYPE_ERROR
    deriving (Eq, Ord, Bounded, A.Ix, Show)

type WIKRT_ERR_NUM = CInt

table_WIKRT_ERR :: [(WIKRT_ERR, WIKRT_ERR_NUM)]
table_WIKRT_ERR =
    [(WIKRT_OK, #const WIKRT_OK)
    ,(WIKRT_INVAL, #const WIKRT_INVAL)
    ,(WIKRT_DBERR, #const WIKRT_DBERR)
    ,(WIKRT_NOMEM, #const WIKRT_NOMEM)
    ,(WIKRT_CXFULL, #const WIKRT_CXFULL)
    ,(WIKRT_BUFFSZ, #const WIKRT_BUFFSZ)
    ,(WIKRT_TXN_CONFLICT, #const WIKRT_TXN_CONFLICT)
    ,(WIKRT_STREAM_WAIT, #const WIKRT_STREAM_WAIT)
    ,(WIKRT_QUOTA_STOP, #const WIKRT_QUOTA_STOP)
    ,(WIKRT_ASSERT_FAIL, #const WIKRT_ASSERT_FAIL)
    ,(WIKRT_TYPE_ERROR, #const WIKRT_TYPE_ERROR)
    ]

-- | Complete list of ABC and ABCD opcodes supported by Wikilon.
-- All ABC opcodes are supported. ABCD will be supported more
-- gradually and incrementally as it is developed.
data WIKRT_OPCODE
    = ABC_PROD_ASSOCL
    | ABC_PROD_ASSOCR
    | ABC_PROD_W_SWAP
    | ABC_PROD_Z_SWAP
    | ABC_PROD_INTRO1
    | ABC_PROD_ELIM1
    | ABC_SUM_ASSOCL
    | ABC_SUM_ASSOCR
    | ABC_SUM_W_SWAP
    | ABC_SUM_Z_SWAP
    | ABC_SUM_INTRO0
    | ABC_SUM_ELIM0
    | ABC_COPY
    | ABC_DROP
    | ABC_SP
    | ABC_LF
    | ABC_APPLY
    | ABC_COMPOSE
    | ABC_QUOTE
    | ABC_REL
    | ABC_AFF
    | ABC_INEW
    | ABC_ID1
    | ABC_ID2
    | ABC_ID3
    | ABC_ID4
    | ABC_ID5
    | ABC_ID6
    | ABC_ID7
    | ABC_ID8
    | ABC_ID9
    | ABC_ID0
    | ABC_IADD
    | ABC_IMUL
    | ABC_INEG
    | ABC_IDIV
    | ABC_IGT
    | ABC_CONDAP
    | ABC_DISTRIB
    | ABC_FACTOR
    | ABC_MERGE
    | ABC_ASSERT
    deriving (Eq, Ord, Bounded, A.Ix)

type WIKRT_OPCODE_NUM = CInt

table_WIKRT_OPCODE :: [(WIKRT_OPCODE, WIKRT_OPCODE_NUM)]
table_WIKRT_OPCODE =
    [(ABC_PROD_ASSOCL, #const ABC_PROD_ASSOCL)
    ,(ABC_PROD_ASSOCR, #const ABC_PROD_ASSOCR)
    ,(ABC_PROD_W_SWAP, #const ABC_PROD_W_SWAP)
    ,(ABC_PROD_Z_SWAP, #const ABC_PROD_Z_SWAP)
    ,(ABC_PROD_INTRO1, #const ABC_PROD_INTRO1)
    ,(ABC_PROD_ELIM1, #const ABC_PROD_ELIM1)
    ,(ABC_SUM_ASSOCL, #const ABC_SUM_ASSOCL)
    ,(ABC_SUM_ASSOCR, #const ABC_SUM_ASSOCR)
    ,(ABC_SUM_W_SWAP, #const ABC_SUM_W_SWAP)
    ,(ABC_SUM_Z_SWAP, #const ABC_SUM_Z_SWAP)
    ,(ABC_SUM_INTRO0, #const ABC_SUM_INTRO0)
    ,(ABC_SUM_ELIM0, #const ABC_SUM_ELIM0)
    ,(ABC_COPY, #const ABC_COPY)
    ,(ABC_DROP, #const ABC_DROP)
    ,(ABC_SP, #const ABC_SP)
    ,(ABC_LF, #const ABC_LF)
    ,(ABC_APPLY, #const ABC_APPLY)
    ,(ABC_COMPOSE, #const ABC_COMPOSE)
    ,(ABC_QUOTE, #const ABC_QUOTE)
    ,(ABC_REL, #const ABC_REL)
    ,(ABC_AFF, #const ABC_AFF)
    ,(ABC_INEW, #const ABC_INEW)
    ,(ABC_ID1, #const ABC_ID1)
    ,(ABC_ID2, #const ABC_ID2)
    ,(ABC_ID3, #const ABC_ID3)
    ,(ABC_ID4, #const ABC_ID4)
    ,(ABC_ID5, #const ABC_ID5)
    ,(ABC_ID6, #const ABC_ID6)
    ,(ABC_ID7, #const ABC_ID7)
    ,(ABC_ID8, #const ABC_ID8)
    ,(ABC_ID9, #const ABC_ID9)
    ,(ABC_ID0, #const ABC_ID0)
    ,(ABC_IADD, #const ABC_IADD)
    ,(ABC_IMUL, #const ABC_IMUL)
    ,(ABC_INEG, #const ABC_INEG)
    ,(ABC_IDIV, #const ABC_IDIV)
    ,(ABC_IGT, #const ABC_IGT)
    ,(ABC_CONDAP, #const ABC_CONDAP)
    ,(ABC_DISTRIB, #const ABC_DISTRIB)
    ,(ABC_FACTOR, #const ABC_FACTOR)
    ,(ABC_MERGE, #const ABC_MERGE)
    ,(ABC_ASSERT, #const ABC_ASSERT)
    ]

-- | Possible responses to a value type query
data WIKRT_VTYPE
    = WIKRT_VTYPE_UNIT
    | WIKRT_VTYPE_PRODUCT
    | WIKRT_VTYPE_INTEGER
    | WIKRT_VTYPE_SUM
    | WIKRT_VTYPE_BLOCK
    | WIKRT_VTYPE_SEALED
    | WIKRT_VTYPE_PENDING
    | WIKRT_VTYPE_STOWED
    deriving (Eq, Ord, Bounded, A.Ix, Show)

type WIKRT_VTYPE_NUM = CInt

table_WIKRT_VTYPE :: [(WIKRT_VTYPE, WIKRT_VTYPE_NUM)]
table_WIKRT_VTYPE =
    [(WIKRT_VTYPE_UNIT, #const WIKRT_VTYPE_UNIT)
    ,(WIKRT_VTYPE_PRODUCT, #const WIKRT_VTYPE_PRODUCT)
    ,(WIKRT_VTYPE_INTEGER, #const WIKRT_VTYPE_INTEGER)
    ,(WIKRT_VTYPE_SUM, #const WIKRT_VTYPE_SUM)
    ,(WIKRT_VTYPE_BLOCK, #const WIKRT_VTYPE_BLOCK)
    ,(WIKRT_VTYPE_SEALED, #const WIKRT_VTYPE_SEALED)
    ,(WIKRT_VTYPE_PENDING, #const WIKRT_VTYPE_PENDING)
    ,(WIKRT_VTYPE_STOWED, #const WIKRT_VTYPE_STOWED)
    ]

-- | Serialization options for bytecode input or output.
--
-- The deflate option will use known ABCD operators where possible.
--
-- Stowage restricts to round tripping with the original environment
-- but may reduce serialization overheads for large objects. 
data WIKRT_ABC_OPTS 
    = WIKRT_ABC_DEFLATE
    | WIKRT_ABC_STOWAGE
    deriving (Eq, Ord, Bounded, A.Ix, Show)

type WIKRT_ABC_OPTS_NUM = CInt

table_WIKRT_ABC_OPTS :: [(WIKRT_ABC_OPTS, WIKRT_ABC_OPTS_NUM)]
table_WIKRT_ABC_OPTS =
    [(WIKRT_ABC_DEFLATE, #const WIKRT_ABC_DEFLATE)
    ,(WIKRT_ABC_STOWAGE, #const WIKRT_ABC_STOWAGE)
    ]

-- FFI
--  'safe': higher overhead, thread juggling, allows callbacks into Haskell
--  'unsafe': lower overhead, reduced concurrency, no callbacks into Haskell
foreign import ccall "wikilon-runtime.h wikrt_env_create" 
 wikrt_env_create :: Ptr (Ptr WIKRT_ENV) -> CString -> Word32 -> IO WIKRT_ERR_NUM

foreign import ccall "wikilon-runtime.h wikrt_env_destroy"
 wikrt_env_destroy :: Ptr WIKRT_ENV -> IO ()

foreign import ccall "wikilon-runtime.h wikrt_env_sync"
 wikrt_env_sync :: Ptr WIKRT_ENV -> IO ()

foreign import ccall "wikilon-runtime.h wikrt_cx_create"
 wikrt_cx_create :: Ptr WIKRT_ENV -> Ptr (Ptr WIKRT_CX) -> Word32 -> IO WIKRT_ERR_NUM

wikrt_cx_size_min, wikrt_cx_size_max :: Word32
wikrt_cx_size_min = #const WIKRT_CX_SIZE_MIN
wikrt_cx_size_max = #const WIKRT_CX_SIZE_MAX

foreign import ccall "wikilon-runtime.h wikrt_cx_destroy"
 wikrt_cx_destroy :: Ptr WIKRT_CX -> IO ()

foreign import ccall "wikilon-runtime.h wikrt_cx_reset"
 wikrt_cx_reset :: Ptr WIKRT_CX -> IO ()

foreign import ccall unsafe "wikilon-runtime.h wikrt_cx_env"
 wikrt_cx_env :: Ptr WIKRT_CX -> Ptr WIKRT_ENV

foreign import ccall unsafe "wikilon-runtime.h wikrt_abcd_operators"
 wikrt_abcd_operators :: CString

foreign import ccall unsafe "wikilon-runtime.h wikrt_abcd_expansion"
 wikrt_abcd_expansion :: WIKRT_OPCODE_NUM -> CString

foreign import ccall unsafe "wikilon-runtime.h wikrt_valid_token"
 wikrt_valid_token :: CString -> CBool

