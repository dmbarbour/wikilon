
-- | 
module Wikilon.ABC.Eval
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

import Wikilon.ABC.Code
import Wikilon.ABC.Value
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




asUnit :: Value -> Maybe ()
{-# INLINE asUnit #-}
asUnit Unit = return ()
asUnit _ = mzero

asPair :: (Monad m) => Value -> m (Value,Value)
{-# INLINE asPair #-}
asPair (Pair a b) = return (a,b)
asPair v = fail $ abcErr $ "expecting pair, received " ++ show v

asSum :: (Monad m) => Value -> m (Either Value Value)
{-# INLINE asSum #-}
asSum (SumL a) = return (Left a)
asSum (SumR b) = return (Right b)
asSum (Text t) = case LazyUTF8.uncons t of
    Nothing -> return $ Right Unit
    Just (c,t') -> 
        let n = Number (fromIntegral (ord c)) in
        return $ Left (Pair n (Text t'))
asSum v = fail $ abcErr $ "expecting sum, received " ++ show v

asNumber :: (Monad m) => Value -> m Rational
{-# INLINE asNumber #-}
asNumber (Number r) = return r
asNumber v = fail $ abcErr $ "expecting number, received " ++ show v

asCharNum :: (Monad m) => Value -> m Char
{-# INLINE asCharNum #-}
asCharNum num = 
    asNumber num >>= \ r ->
    let n = numerator r in
    let d = denominator r in
    let ok = (1 == d) && (0 <= n) && (n <= 0x10ffff) in
    if ok then return (chr (fromInteger n)) else
    fail $ abcErr $ "expecting utf8 codepoint, received " ++ show num

asBlock :: (Monad m) => Value -> m (ABC, Flags)
{-# INLINE asBlock #-}
asBlock (Block abc f) = return (abc,f)
asBlock v = fail $ abcErr $ "expecting block, received " ++ show v

-- | asText will short-circuit if the value is obviously a text.
-- Otherwise it will prepare a bytestring builder.
asText :: (Monad m) => Value -> m Text
{-# INLINE asText #-}
asText (Text t) = return t
asText v = asText' mempty v 

asText' :: (Monad m) => BB.Builder -> Value -> m Text 
{-# INLINE asText' #-}
asText' bb (Text t) = return $! (BB.toLazyByteString bb) <> t
asText' bb s = asSum s >>= \ lr -> case lr of
    Right u -> asUnit u >> return $! BB.toLazyByteString bb
    Left p -> 
        asPair p >>= \ (n , t') -> 
        asCharNum n >>= \ c ->
        asText' (bb <> BB.charUtf8 c) t'

-- | Obtain a list from a value, with monadic failure. In this case
-- we expect an argument of form µL.((a*L)+1).
asListOf :: (Monad m) => (Value -> m a) -> Value -> m [a]
{-# INLINE asListOf #-}
asListOf fn = asListOf' [] where
    asListOf' xs = asSum >=> \ e -> case e of
        Right u -> asUnit u >> return (L.reverse xs) 
        Left p -> asPair p >>= \ (a,l) -> fn a >>= \ x -> asListOf' (x:xs) l

-- | Encode a list into a value, using ABC's underlying list structure,
-- which is λa.µL.((a*L)+1). Every item in a list is a sum.
fromList :: [Value] -> Value
{-# INLINE fromList #-}
fromList (x:xs) = SumL (Pair x (fromList xs))
fromList [] = SumR Unit


-- | Obtain a stack of values. A stack is just pairs terminated by a
-- Unit value. Within ABC, it is generally necessary that knowledge of
-- stack size be implicit in context. Values on a stack may generally
-- be of heterogeneous type.
asStackOf :: (Monad m) => (Value -> m a) -> Value -> m [a]
{-# INLINE asStackOf #-}
asStackOf fn (Pair a b) =
    fn a >>= \ x ->
    asStackOf fn b >>= \ xs ->
    return (x:xs)
asStackOf _ Unit = return []
asStackOf fn (Stowed rsc) = asStackOf fn (loadVal rsc)
asStackOf _ v = fail $ abcErr $ "expecting stack, received " ++ show v

-- | Convert to a stack of values with error-based failure.
asStack :: Value -> [Value]
asStack (Pair a b) = a : asStack b
asStack Unit = []
asStack (Stowed rsc) = asStack (loadVal rsc)
asStack v = impossible $ "expecting stack, received " ++ show v

-- | Convert a list of values to a stack of values. Trivial.
fromStack :: [Value] -> Value
fromStack (x:xs) = (Pair x (fromStack xs))
fromStack [] = Unit


-}

