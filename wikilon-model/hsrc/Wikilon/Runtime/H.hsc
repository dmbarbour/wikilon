
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
-- | This module wraps the `wikilon-runtime.h` for `libwikilon-runtime.so`.
-- The high performance core of our Wikilon runtime is implemented at the C
-- layer. 
module Wikilon.Runtime.H
    ( WIKRT_ENV
    , WIKRT_CX
    , WIKRT_ERR(..)
    , WIKRT_OPCODE(..)
    , WIKRT_VTYPE(..)
    , WIKRT_VAL(..)
    ) where

#include <wikilon-runtime.h>

import Foreign
import Foreign.C
import Data.Word
import qualified Data.Array.Unboxed as A


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data WIKRT_ENV
data WIKRT_CX
newtype WIKRT_VAL = WIKRT_VAL Word32


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

wikrtErrVals :: [(WIKRT_ERR, Int)]
wikrtErrVals =
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

-- ABC and ABCD opcodes supported by Wikilon
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

wikrtOpCodes :: [(WIKRT_OPCODE, Int)]
wikrtOpCodes =
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

wikrtVType :: [(WIKRT_VTYPE, Int)]
wikrtVType =
    [(WIKRT_VTYPE_UNIT, #const WIKRT_VTYPE_UNIT)
    ,(WIKRT_VTYPE_PRODUCT, #const WIKRT_VTYPE_PRODUCT)
    ,(WIKRT_VTYPE_INTEGER, #const WIKRT_VTYPE_INTEGER)
    ,(WIKRT_VTYPE_SUM, #const WIKRT_VTYPE_SUM)
    ,(WIKRT_VTYPE_BLOCK, #const WIKRT_VTYPE_BLOCK)
    ,(WIKRT_VTYPE_SEALED, #const WIKRT_VTYPE_SEALED)
    ,(WIKRT_VTYPE_PENDING, #const WIKRT_VTYPE_PENDING)
    ,(WIKRT_VTYPE_STOWED, #const WIKRT_VTYPE_STOWED)
    ]

{-
-- FFI
--  'safe': higher overhead, thread juggling, allows callbacks into Haskell
--  'unsafe': lower overhead, reduced concurrency, no callbacks into Haskell
foreign import ccall unsafe "wikilon-runtime.h 
foreign import ccall unsafe "lmdb.h mdb_version" _mdb_version :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CString
 

data WIKRT_OPCODE
    = 
-}


-- | for now, just providing enough to test our 
--foreign import ccall unsafe "wikilon-runtime.h wikrt_hello" _wikrt_hello :: CString -> IO ()

--wikrt_hello :: String -> IO ()
--wikrt_hello = flip withCString _wikrt_hello

