

module Wikilon.Eval
    (


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


    ) where

import Control.Monad
-- fork/join parallelism on eval
import Control.Concurrent.MVar

import Wikilon.ABC
{-
-- | Evaluator state. (Not sure exactly how much I want here.)
--
-- When we evaluate content, we have some stack of precomputed values
-- for fast quotation (which must be used precisely) and a sequence 
-- of bytes indicating continuation behavior. In some cases, for tail
-- calls, we might extend these stacks and continuations at the near
-- end.
--
-- Additionally, we have very minimal interaction with IO. While ABC
-- does have potential effects via {tokens}, Wikilon doesn't leverage
-- this feature. Instead, effects are modeled via machines, network,
-- and messaging `(InMsg*State)→(OutMsgList*State)`. However, I need
-- effects for:
--
--  * fork-join parallelism
--  * primitive debug traces
--  * timeout, interruption
--
-- At the Haskell layer, this will be modeled using unsafePerformIO.
--
data EvalSt = EvalSt
    { eval_env   :: !Value -- ^ primary input for evaluation
    , eval_cont  :: !Bytes
    , eval_data  :: ![Value] -- ^ stack of values for 
    , eval_cost  :: !Int -- ^ decide when to tick
    , eval_meta  :: !EvalMeta -- ^ relatively stable elements
    } 
data EvalMeta = EvalMeta
    { eval_tid   :: !Integer -- ^ thread ID; 2* 1+ for left, 2* 2+ for right
    , eval_join  :: ![JoinHandle] -- ^ threads to join
    , eval_fork  :: !(EvalSt -> IO JoinHandle)
    , eval_trace :: !(Value -> IO ()) -- ^ debug output
    , eval_tick  :: !(EvalSt -> IO Bool) -- ^ debug and interrupt support
    , eval_space :: !VSpace
    }


data EvalErr = EvalErr 
    { err_dump   :: !EvalSt
    , err_arg    :: !Value
    , err_msg    :: !Text
    } deriving (Show)
type EvalErrors = [EvalErr]
type JoinHandle = MVar EvalErrors

newtype Eval a = Eval { runEval :: EvalCtx -> Either EvalErrors (a, EvalCtx) }

runEvalMaybe :: Eval a -> Maybe a
runEvalMaybe = error "TODO"

-}

