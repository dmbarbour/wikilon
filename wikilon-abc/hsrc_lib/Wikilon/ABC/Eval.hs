{-# LANGUAGE BangPatterns, OverloadedStrings  #-}
-- | A simplistic evaluator for Wikilon ABC, uses a similar technique as
-- Awelon.ABC.Eval to support pure interpretation with flexible effects
-- handlers.
module Wikilon.ABC.Eval
    ( Evaluator
    , Quota
    , Stack(..)
    , Cont(..)
    , Stuck(..)

    , evaluate

    , expand
    , abcOpEvalTable
    , extOpEvalTable
    , frameQuotaCost
    ) where

import Control.Applicative
import Data.Monoid
import Data.Int
import Data.Char
import Data.Bits
import qualified Data.Array.IArray as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8

import Awelon.ABC (abcDivMod)
import Wikilon.ABC.Code hiding (null)
import qualified Wikilon.ABC.Code as ABC 
import Wikilon.ABC.Value 

-- | A simple, pure evaluation model. 
--
-- Evaluation either succeeds returning a value, or becomes stuck at
-- some point and returns control to the caller. Becoming stuck is 
-- not necessarily an error, especially in cases of token or quota.
--
-- Wikilon's evaluator includes a simple quota model such that long
-- running code can easily be interrupted or rescheduled without loss
-- of partial computations.
type Evaluator r = Value r -> Cont r -> Quota -> Either (Stuck r) (Value r)

-- | At the moment our quota is just an integer, which we'll reduce
-- based on the size of ABC bytecodes to which we commit. We'll also
-- correct our quotas for block and text operations, which consume a
-- large number of bytes but have a low evaluator cost. 
type Quota = Int64

-- | heuristic quota cost for operating on a given ABC frame, e.g.
-- via '$' or '?' or the tail-call variants.
frameQuotaCost :: ABC (Value r) -> Quota
frameQuotaCost abc = 16 + LBS.length (abc_code abc)

-- | When we call `$` or `?` we introduce a stage in a call stack.
-- Tail-calls are a special exception to this; tail calls in a tail
-- call position (i.e. the last operation) will not grow the stack.
data Stack r
    = Apply (Value r) !(Cont r) -- ^ after $, cv first in pair
    | Cond  (Value r) !(Cont r) -- ^ after ?, cv inL first in pair
    | Return -- all done

-- | Our Continuation is simply a sequence of operations to perform
-- on the current value, including a stack representing the future 
-- from prior calls. 
data Cont r = Cont
    { cc_code   :: !(ABC (Value r))
    , cc_stack  :: !(Stack r)
    }

-- | When an evaluator becomes Stuck, we have the input value, the
-- operation on which it became stuck, and a continuation.
data Stuck r = Stuck 
    { stuck_arg   :: !(Value r)
    , stuck_curr  :: !(Op (Value r))
    , stuck_cont  :: !(Cont r)
    , stuck_quota :: {-# UNPACK #-} !Quota
    }

-- low level evaluators!
abcOpEvalTable :: [(PrimOp, Evaluator r)]
abcOpEvalTable =
    [(ABC_l,ev_l),(ABC_r,ev_r),(ABC_w,ev_w),(ABC_z,ev_z),(ABC_v,ev_v),(ABC_c,ev_c)
    ,(ABC_L,ev_L),(ABC_R,ev_R),(ABC_W,ev_W),(ABC_Z,ev_Z),(ABC_V,ev_V),(ABC_C,ev_C) -- 12

    ,(ABC_copy,ev_copy),(ABC_drop,ev_drop)
    ,(ABC_add,ev_add),(ABC_negate,ev_negate)
    ,(ABC_multiply,ev_multiply),(ABC_reciprocal,ev_reciprocal)
    ,(ABC_divMod,ev_divMod),(ABC_compare,ev_compare) -- 8

    ,(ABC_apply,ev_apply),(ABC_condApply,ev_condApply) 
    ,(ABC_quote,ev_quote),(ABC_compose,ev_compose) 
    ,(ABC_relevant,ev_relevant),(ABC_affine,ev_affine)
    ,(ABC_distrib,ev_distrib),(ABC_factor,ev_factor) 
    ,(ABC_merge,ev_merge),(ABC_assert,ev_assert) -- 10

    ,(ABC_newZero,ev_newZero),(ABC_d0,ev_d0)
    ,(ABC_d1,ev_d1),(ABC_d2,ev_d2),(ABC_d3,ev_d3)
    ,(ABC_d4,ev_d4),(ABC_d5,ev_d5),(ABC_d6,ev_d6)
    ,(ABC_d7,ev_d7),(ABC_d8,ev_d8),(ABC_d9,ev_d9) -- 11

    ,(ABC_SP,ev_SP),(ABC_LF,ev_LF) -- 2
    ]

abcOpEvalArray :: A.Array PrimOp (Evaluator r)
abcOpEvalArray = A.array (minBound,maxBound) abcOpEvalTable

-- extended operators
extOpEvalTable :: [(ExtOp,Evaluator r)]
extOpEvalTable =
    [(ExtOp_Apc,ev_Apc)
    ,(ExtOp_Inline, ev_Inline)
    ,(ExtOp_Intro1L,ev_Intro1L)
    ,(ExtOp_Elim1L,ev_Elim1L)
    ,(ExtOp_Intro0L,ev_Intro0L)
    ,(ExtOp_Elim0L,ev_Elim0L)
    ,(ExtOp_prim_swap,ev_prim_swap)
    ,(ExtOp_prim_mirror,ev_prim_mirror)
    ]

extOpEvalArray :: A.Array ExtOp (Evaluator r)
extOpEvalArray = A.array (minBound,maxBound) extOpEvalTable

-- get stuck
stuck :: Op (Value r) -> Evaluator r
stuck op v cc qu = Left $! Stuck 
    { stuck_arg = v
    , stuck_curr = op
    , stuck_cont = cc
    , stuck_quota = qu
    }

stuckOnQuota :: Evaluator r
stuckOnQuota = stuck (ABC_Tok "quota")


-- | Basic evaluator, without any effects model. All tokens cause
-- this evaluator to become stuck, as will attempting to expand a
-- Resource. In special cases, we'll inject tokens into the code so
-- we can indicate why we halted:
--
--   {quota} - quota underflow
--   {error decode} - failure to decodeOp cc_code
--
-- In most cases, whatever reason we get stuck must be inferred.
--
-- Quotas use a simple estimate based on the size of abc_code, then
-- add a positive correction upon parsing a block or text. 
--   
evaluate :: Evaluator r
evaluate !v !cc !qu = case ABC.decodeOp (cc_code cc) of
    Just (op,code') -> let cc' = cc { cc_code = code' } in case op of
        (ABC_Prim primOp) -> evalPrimOp primOp v cc' qu where
        (ABC_Block abc) -> evaluate v' cc' qu' where
            qu' = qu + (LBS.length (abc_code abc)) -- quota correction
            v' = Pair (Block abc 0) v
        (ABC_Text txt) -> evaluate v' cc' qu' where
            qu' = qu + (LBS.length txt) -- quota correction
            v' = Pair (Text txt) v
        (ABC_Quote val) -> evaluate (Pair val v) cc' qu
        (ABC_Ext extOp) -> evalExtOp extOp v cc' qu where
        tokOp@(ABC_Tok _) -> stuck tokOp v cc' qu
    Nothing | ABC.null (cc_code cc) -> case cc_stack cc of
        (Apply env cc') -> evaluate (Pair v env) cc' qu
        (Cond env cc') -> evaluate (Pair (SumL v) env) cc' qu
        Return -> Right v -- all done!
    _ -> stuck (ABC_Tok "error decode") v cc qu

evalPrimOp :: PrimOp -> Evaluator r
evalPrimOp = (A.!) abcOpEvalArray 

evalExtOp :: ExtOp -> Evaluator r
evalExtOp = (A.!) extOpEvalArray

-- | Evaluators for PrimOp only attempt to match primitive expansions
-- for values. For example like (Text "hi") will fail to match its
-- expansion `(SumL (Pair 104 (SumL (Pair 105 (SumR Unit)))))`.
-- At the moment, Text is the only issue. But there may later be some
-- models for annotated values, e.g. for fast 'copyable' validation.
--
-- So, after a match failure, we'll try to expand the input. Only if
-- the value cannot be expanded further will we consider eval stuck.
expand :: Int -> Value r -> Maybe (Value r)
expand 0 _ = Nothing
expand n (Text txt) = Just (expandText (max n 64) txt)
expand n (SumL v) = SumL <$> expand (n-1) v
expand n (SumR v) = SumR <$> expand (n-1) v
expand n (Pair a b) =
    case expand (n-1) a of
        Nothing -> Pair a <$> expand (n-1) b
        Just a' -> return (Pair a' b)
expand _ (Number _) = Nothing
expand _ Unit = Nothing
expand _ (Block _ _) = Nothing
expand _ (Sealed _ _) = Nothing -- don't peek inside Sealed values!
expand _ (Resource _ _) = Nothing

-- expand some given number of characters in Text.
expandText :: Int -> Text -> Value r
expandText 0 t = Text t
expandText n t = case LazyUTF8.uncons t of
    Just (c, t') -> nc `seq` SumL (Pair nc vt') where
        nc = Number (fromIntegral (ord c))
        vt' = expandText (n-1) t'
    Nothing -> SumR Unit

-- If a primitive operation fails, try expanding input before giving
-- up entirely. 
primOpFail :: PrimOp -> Evaluator r
primOpFail op v = case expand 4 v of
    Just v' -> evalPrimOp op v'
    Nothing -> stuck (ABC_Prim op) v

ev_l,ev_r,ev_w,ev_z,ev_v,ev_c :: Evaluator r
ev_L,ev_R,ev_W,ev_Z,ev_V,ev_C :: Evaluator r
ev_copy,ev_drop :: Evaluator r
ev_add,ev_negate :: Evaluator r
ev_multiply,ev_reciprocal :: Evaluator r
ev_divMod,ev_compare :: Evaluator r
ev_apply,ev_condApply,ev_quote,ev_compose :: Evaluator r
ev_relevant,ev_affine :: Evaluator r
ev_distrib,ev_factor,ev_merge,ev_assert :: Evaluator r
ev_newZero :: Evaluator r
ev_d0,ev_d1,ev_d2,ev_d3,ev_d4 :: Evaluator r
ev_d5,ev_d6,ev_d7,ev_d8,ev_d9 :: Evaluator r
ev_SP,ev_LF :: Evaluator r

ev_l (Pair a (Pair b c)) = evaluate (Pair (Pair a b) c)
ev_l v = primOpFail ABC_l v

ev_r (Pair (Pair a b) c) = evaluate (Pair a (Pair b c))
ev_r v = primOpFail ABC_r v

ev_w (Pair a (Pair b c)) = evaluate (Pair b (Pair a c))
ev_w v = primOpFail ABC_w v

ev_z (Pair a (Pair b (Pair c d))) = evaluate (Pair a (Pair c (Pair b d)))
ev_z v = primOpFail ABC_z v

ev_v v = evaluate (Pair v Unit)
-- ev_v does not fail

ev_c (Pair a Unit) = evaluate a
ev_c v = primOpFail ABC_c v

ev_L (Pair a@(SumL _) e) = evaluate (Pair (SumL a) e)
ev_L (Pair (SumR (SumL b)) e) = evaluate (Pair (SumL (SumR b)) e)
ev_L (Pair (SumR c@(SumR _)) e) = evaluate (Pair c e)
ev_L v = primOpFail ABC_L v

ev_R (Pair (SumL a@(SumL _)) e) = evaluate (Pair a e)
ev_R (Pair (SumL (SumR b)) e) = evaluate (Pair (SumR (SumL b)) e)
ev_R (Pair c@(SumR _) e) = evaluate (Pair (SumR c) e)
ev_R v = primOpFail ABC_R v

ev_W (Pair a@(SumL _) e) = evaluate (Pair (SumR a) e)
ev_W (Pair (SumR b@(SumL _)) e) = evaluate (Pair b e)
ev_W v@(Pair (SumR (SumR _)) _) = evaluate v
ev_W v = primOpFail ABC_W v

ev_Z v@(Pair (SumL _) _) = evaluate v 
ev_Z (Pair b@(SumR (SumL _)) e) = evaluate (Pair (SumR b) e)
ev_Z (Pair (SumR c@(SumR (SumL _))) e) = evaluate (Pair c e)
ev_Z v@(Pair (SumR (SumR (SumR _))) _) = evaluate v
ev_Z v = primOpFail ABC_Z v

ev_V (Pair a e) = evaluate (Pair (SumL a) e)
ev_V v = primOpFail ABC_V v

ev_C (Pair (SumL a) e) = evaluate (Pair a e)
ev_C v = primOpFail ABC_C v

ev_copy p@(Pair a _) | copyable a = evaluate (Pair a p)
ev_copy v = primOpFail ABC_copy v

ev_drop (Pair a e) | droppable a = evaluate e
ev_drop v = primOpFail ABC_drop v

ev_add (Pair (Number a) (Pair (Number b) e)) = 
    let n' = Number (a+b) in
    n' `seq` evaluate (Pair n' e) 
ev_add v = primOpFail ABC_add v

ev_negate (Pair (Number n) e) =
    let n' = Number (negate n) in
    n' `seq` evaluate (Pair n' e)
ev_negate v = primOpFail ABC_negate v

ev_multiply (Pair (Number a) (Pair (Number b) e)) =
    let n' = Number (a * b) in
    n' `seq` evaluate (Pair n' e)
ev_multiply v = primOpFail ABC_multiply v

ev_reciprocal (Pair (Number n) e) | (n /= 0) =
    let n' = Number (recip n) in
    n' `seq` evaluate (Pair n' e)
ev_reciprocal v = primOpFail ABC_reciprocal v

ev_divMod (Pair (Number b) (Pair (Number a) e)) | (b /= 0) =
    let (q,r) = abcDivMod a b in
    let nq = Number q in
    let nr = Number r in
    nr `seq` nq `seq` evaluate (Pair nr (Pair nq e))
ev_divMod v = primOpFail ABC_divMod v

ev_compare (Pair nx@(Number x) (Pair ny@(Number y) e)) = evaluate (Pair s e) 
    where s = if (y > x) then SumR (Pair nx ny) 
                         else SumL (Pair ny nx) 
ev_compare v = primOpFail ABC_compare v

-- No tail-calls here (shifted to ExtOp for Wikilon.ABC).
-- But we do need to deal with quotas. 
ev_apply (Pair (Block abc _) (Pair arg env)) cc qu =
    let qu' = qu - frameQuotaCost abc in
    let cc' = Cont { cc_code = abc, cc_stack = Apply env cc } in
    let v' = arg in
    if qu' < 0 then stuckOnQuota v' cc' qu'
               else evaluate v' cc' qu'
ev_apply v cc qu = primOpFail ABC_apply v cc qu

-- condApply is only permitted for 'droppable' functions. We'll say
-- a branch not taken is nearly (not quite) free. 
ev_condApply (Pair (Block _ f) v@(Pair (SumR _) _)) cc qu | f_droppable f = evaluate v cc qu
ev_condApply (Pair (Block abc f) (Pair (SumL arg) env)) cc qu | f_droppable f =
    let qu' = qu - frameQuotaCost abc in
    let cc' = Cont { cc_code = abc, cc_stack = Cond env cc } in
    let v' = arg in
    if qu' < 0 then stuckOnQuota v' cc' qu'
               else evaluate v' cc' qu'
ev_condApply v cc qu = primOpFail ABC_condApply v cc qu

-- Wikilon.ABC quotation doesn't require serialization, though we
-- might later wish to serialize as part of a simplification.
ev_quote (Pair val e) = evaluate (Pair (Block abc flags) e) where
    abc = ABC { abc_code = LazyUTF8.fromString "»"
              , abc_data = [BQuote val] }
    flags = rel .|. aff
    rel = if droppable val then 0 else f_rel
    aff = if copyable val then 0 else f_aff 
ev_quote v = primOpFail ABC_quote v

ev_compose (Pair (Block xy fxy) (Pair (Block yz fyz) e)) =
    let xz = xy <> yz in
    let fxz = fxy .|. fyz in
    evaluate (Pair (Block xz fxz) e)
ev_compose v = primOpFail ABC_compose v

ev_relevant (Pair (Block fn flags) e) = evaluate (Pair (Block fn flags') e) where
    flags' = flags .|. f_rel
ev_relevant v = primOpFail ABC_relevant v

ev_affine (Pair (Block fn flags) e) = evaluate (Pair (Block fn flags') e) where
    flags' = flags .|. f_aff
ev_affine v = primOpFail ABC_affine v

ev_distrib (Pair a (Pair (SumL b) e)) = evaluate (Pair (SumL (Pair a b)) e)
ev_distrib (Pair a (Pair (SumR c) e)) = evaluate (Pair (SumR (Pair a c)) e)
ev_distrib v = primOpFail ABC_distrib v

ev_factor (Pair (SumL (Pair a b)) e) = evaluate (Pair (SumL a) (Pair (SumL b) e))
ev_factor (Pair (SumR (Pair c d)) e) = evaluate (Pair (SumR c) (Pair (SumR d) e))
ev_factor v = primOpFail ABC_factor v

ev_merge (Pair (SumL a) e) = evaluate (Pair a e)
ev_merge (Pair (SumR a') e) = evaluate (Pair a' e)
ev_merge v = primOpFail ABC_merge v

ev_assert (Pair (SumR a) e) = evaluate (Pair a e)
ev_assert v = primOpFail ABC_assert v

ev_newZero = evaluate . Pair (Number 0)
ev_d0 = ev_d 0 ABC_d0
ev_d1 = ev_d 1 ABC_d1
ev_d2 = ev_d 2 ABC_d2
ev_d3 = ev_d 3 ABC_d3
ev_d4 = ev_d 4 ABC_d4
ev_d5 = ev_d 5 ABC_d5
ev_d6 = ev_d 6 ABC_d6
ev_d7 = ev_d 7 ABC_d7
ev_d8 = ev_d 8 ABC_d8
ev_d9 = ev_d 9 ABC_d9

ev_d :: Int -> PrimOp -> Evaluator r
{-# INLINE ev_d #-}
ev_d d _ (Pair (Number n) e) =
    let n' = Number ((10 * n) + fromIntegral d) in
    n' `seq` evaluate (Pair n' e)
ev_d _ op v = primOpFail op v

ev_SP = evaluate
ev_LF = evaluate

-- Evaluator for extended operations.
-- For now, using same expansion effort as for PrimOp.
extOpFail :: ExtOp -> Evaluator r
extOpFail xop v = case expand 4 v of
    Just v' -> evalExtOp xop v'
    Nothing -> stuck (ABC_Ext xop) v

ev_Apc, ev_Inline :: Evaluator r
ev_Intro1L, ev_Elim1L :: Evaluator r
ev_Intro0L, ev_Elim0L :: Evaluator r
ev_prim_swap, ev_prim_mirror :: Evaluator r

-- supports tail-call optimization
ev_Apc (Pair (Block fn _) (Pair arg Unit)) = _inline fn arg
ev_Apc v = extOpFail ExtOp_Apc v

-- supports tail-call optimization
ev_Inline (Pair (Block fn _) arg) = _inline fn arg
ev_Inline v = extOpFail ExtOp_Inline v

_inline :: ABC (Value r) -> Value r -> Cont r -> Quota -> Either (Stuck r) (Value r)
_inline abc v cc qu = ev v cc' qu' where
    ev = if (qu' < 0) then stuckOnQuota else evaluate
    cc' = cc { cc_code = (abc <> cc_code cc) }
    qu' = qu - frameQuotaCost abc
    
ev_Intro1L a = evaluate (Pair Unit a)

ev_Elim1L (Pair Unit a) = evaluate a
ev_Elim1L v = extOpFail ExtOp_Elim1L v

ev_Intro0L (Pair a b) = evaluate (Pair (SumR a) b)
ev_Intro0L v = extOpFail ExtOp_Intro0L v

ev_Elim0L (Pair (SumR a) b) = evaluate (Pair a b)
ev_Elim0L v = extOpFail ExtOp_Elim0L v

ev_prim_swap (Pair a b) = evaluate (Pair b a)
ev_prim_swap v = extOpFail ExtOp_prim_swap v

ev_prim_mirror (Pair (SumL a) e) = evaluate (Pair (SumR a) e)
ev_prim_mirror (Pair (SumR b) e) = evaluate (Pair (SumL b) e)
ev_prim_mirror v = extOpFail ExtOp_prim_mirror v


