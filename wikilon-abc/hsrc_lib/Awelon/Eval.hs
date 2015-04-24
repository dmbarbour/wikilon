{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
-- | A simplistic evaluator for purely functional ABC in the style
-- of free monads. This can be wrapped with ad-hoc effects handlers.
--
-- This evaluator does perform a tail-call optimization. However, 
-- quotation is very expensive (serializes values out to relevant
-- code), as is copy and drop. I wouldn't expect good performance.
-- 
module Awelon.Eval 
    ( Value(..)
    , copyable
    , droppable
    , toText
    , fromText
    , Cont(..)
    , Stack(..)
    , Stuck(..)
    , Evaluator
    , evaluate
--    , eval
    , abcOpEvalTable
    ) where

import Data.Monoid
import Data.Word
import Data.Char
import Data.Bits
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.Array.IArray as A
import Awelon.ABC

-- | simplistic value representation
data Value 
    = Number !Rational
    | Pair Value Value
    | Unit
    | SumL Value
    | SumR Value
    | Block ABC {-# UNPACK #-} !Flags
    | Sealed !Token Value
    deriving (Eq, Ord)

type Flags = Word8

flags_include :: Flags -> Flags -> Bool
{-# INLINE flags_include #-}
flags_include f fs = (f == (f .&. fs))

prop_relevant, prop_affine :: Flags
prop_relevant = 0x01
prop_affine = 0x02

f_droppable, f_copyable :: Flags -> Bool
f_droppable = not . flags_include prop_relevant
f_copyable = not . flags_include prop_affine

copyable :: Value -> Bool
copyable (Number _) = True
copyable (Pair a b) = copyable a && copyable b
copyable Unit = True
copyable (SumL a) = copyable a
copyable (SumR b) = copyable b
copyable (Block _ f) = f_copyable f
copyable (Sealed _ v) = copyable v

droppable :: Value -> Bool
droppable (Number _) = True
droppable (Pair a b) = droppable a && droppable b
droppable Unit = True
droppable (SumL a) = droppable a
droppable (SumR b) = droppable b
droppable (Block _ f) = f_droppable f
droppable (Sealed _ v) = droppable v

instance Quotable Value where
    quotes (Number r) = quotes r
    quotes (Pair a b) = quotes b . quotes a . quotes ABC_l
    quotes (SumR b) = quotes b . quotes ("VVRWLC" :: ABC)
    quotes (toText -> Just t) = quotes (ABC_Text t)
    quotes (SumL a) = quotes a . quotes ABC_V
    quotes Unit = quotes ("vvrwlc" :: ABC)
    quotes (Block abc flags) = quotes (ABC_Block abc) . k . f where
        has prop op = if flags_include prop flags then quotes op else id
        k = has flag_relevant ABC_relevant
        f = has flag_affine ABC_affine
    quotes (Sealed tok val) = quotes val . quotes (ABC_Tok tok)
instance Show Value where 
    shows = shows . quote


toText :: Value -> Maybe Text
toText = tt mempty where
    tt bb (SumR Unit) = Just (BB.toLazyByteString bb)
    tt bb (SumL (Pair (Number (numToChar -> Just c)) more)) =
        tt (bb <> BB.charUtf8 c) more
    tt _ _ = Nothing

numToChar :: Rational -> Maybe Char
numToChar r =
    let n = numerator n in
    let d = denominator n in
    let bOK = (1 == d) && (0 <= n) && (n <= 0x10ffff) in
    if bOK then Just $! chr (fromIntegral n) else Nothing

fromText :: Text -> Value
fromText t = case LazyUTF8.uncons t of
    Just (c, t') -> 
        let n = Number (fromIntegral (ord c)) in
        SumL $! n `seq` (Pair n (fromText t'))
    Nothing -> SumR Unit


-- | A simple continuation passing evaluation model. Note that if we
-- get 'Stuck', that doesn't necessarily imply error. 
--
-- In particular, our basic evaluation function will always get stuck
-- on tokens, even annotations, providing opportunity for intercession
-- in a manner similar to free monads.
type Evaluator = Value -> Cont -> Either Stuck Value

-- | When we call `$` or `?` we introduce a stage in a call stack.
-- The exception to this is `$c` at the end of a sequence, which 
-- is a simple tail call. We
data Stack 
    = Apply Value !Cont -- ^ after $, cv first in pair
    | Cond  Value !Cont -- ^ after ?, cv inL first in pair
    | Return -- all done

-- | Our Continuation is simply a sequence of operations to perform
-- on the current value (which is held by the evaluator). When we
-- reach the end of this list, we will pop the stack and continue
-- further, or we're done.
data Cont = Cont
    { cc_cont  :: ![Op]
    , cc_stack :: !Stack
    }

-- | It is possible that our evaluator will get stuck. In this case,
-- we just return where it becomes stuck. A separate evaluator might
-- resolve the issue (especially if we're stuck on a token).
newtype Stuck = Stuck { unStuck :: (Value, Cont) }

-- low level evaluators!
abcOpEvalTable :: [(PrimOp, Evaluator)]
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

    ,(ABC_newZero,ev_newZero),(ABC_d0,ev_d 0)
    ,(ABC_d1,ev_d 1),(ABC_d2,ev_d 2),(ABC_d3,ev_d 3)
    ,(ABC_d4,ev_d 4),(ABC_d5,ev_d 5),(ABC_d6,ev_d 6)
    ,(ABC_d7,ev_d 7),(ABC_d8,ev_d 8),(ABC_d9,ev_d 9) -- 11

    ,(ABC_SP,ev_SP),(ABC_LF,ev_LF) -- 2
    ]

abcOpEvalArray :: A.Array PrimOp Evaluator
abcOpEvalArray = A.array (minBound,maxBound) abcOpEvalTable

-- | Basic evaluator, without any effects model. All tokens shall
-- cause this evaluator to become 'stuck'.
evaluate :: Evaluator
evaluate v cc = case cc_cont cc of
    (op:ops) -> let cc' = cc { cc_cont = ops } in case op of
        (ABC_Prim primOp) -> (A.!) abcOpEvalArray primOp v cc'
        (ABC_Block abc) -> evaluate (Pair (Block abc 0) v) cc'
        (ABC_Text txt) -> evaluate (Pair (fromText txt) v) cc'
        _ -> Left (Stuck (v,cc))
    [] -> case cc_stack cc of
        (Apply v2 cc') -> evaluate (Pair v v2) cc'
        (Cond v2 ccStack) -> evaluate (Pair (SumL v) v2) cc'
        Return -> Right v -- all done!

-- | Evaluator that carries a little state.

-- | A common evaluator, sufficient for testing, which recognizes and
-- handles a few simple tokens. In particular: 
--
--    {&≡} - assert two values are equal
--    {&fork} - fork the top of the stack
--    {



-- push operator back in case of failure!
primOpFail :: Value -> PrimOp -> Cont -> Stuck
primOpFail v op cc = Stuck (v,cc') where
    cc' = cc { cc_cont = (ABC_Prim op : cc_cont cc) }

ev_l,ev_r,ev_w,ev_z,ev_v,ev_c :: Evaluator
ev_L,ev_R,ev_W,wv_Z,ev_V,ev_C :: Evaluator
ev_copy,ev_drop :: Evaluator
ev_add,ev_negate :: Evaluator
ev_multiply,ev_reciprocal :: Evaluator
ev_divMod,ev_compare :: Evaluator
ev_apply,ev_condApply,ev_quote,ev_compose :: Evaluator
ev_relevant,ev_affine :: Evaluator
ev_distrib,ev_factor,ev_merge,ev_assert :: Evaluator
ev_newZero :: Evaluator
ev_d :: Int -> Evaluator
ev_SP,ev_LF :: Evaluator

ev_l (Pair a (Pair b c)) = evaluate (Pair (Pair a b) c)
ev_l v = primOpFail v ABC_l

ev_r (Pair (Pair a b) c) = evaluate (Pair a (Pair b c))
ev_r v = primOpFail v ABC_r

ev_w (Pair a (Pair b c)) = evaluate (Pair b (Pair a c))
ev_w v = primOpFail v ABC_w

ev_z (Pair a (Pair b (Pair c d))) = evaluate (Pair a (Pair c (Pair b d)))
ev_z v = primOpFail v ABC_z

ev_v v = evaluate (Pair v Unit)
-- ev_v does not fail

ev_c (Pair a Unit) = evaluate a
ev_c v = primOpFail v ABC_c

ev_L (Pair a@(SumL _) e) = evaluate (Pair (SumL a) e)
ev_L (Pair (SumR (SumL b)) e) = evaluate (Pair (SumL (SumR b)) e)
ev_L (Pair (SumR c@(SumR _)) e) = evaluate (Pair c e)
ev_L v = primOpFail v ABC_L

ev_R (Pair (SumL a@(SumL _)) e) = evaluate (Pair a e)
ev_R (Pair (SumL (SumR b)) e) = evaluate (Pair (SumR (SumL b)) e)
ev_R (Pair c@(SumR _) e) = (Pair (SumR c) e)
ev_R v = primOpFail v ABC_R

ev_W (Pair a@(SumL _) e) = evaluate (Pair (SumR a) e)
ev_W (Pair (SumR b@(SumL _)) e) = evaluate (Pair b e)
ev_W v@(Pair (SumR (SumR _)) _) = evaluate v
ev_W v = primOpFail v ABC_W

ev_Z v@(Pair (SumL _) _) = evaluate v 
ev_Z (Pair b@(SumR (SumL _)) e) = evaluate (Pair (SumR b) e)
ev_Z (Pair (SumR c@(SumR (SumL _)) e) = evaluate (Pair c e)
ev_Z v@(Pair (SumR (SumR (SumR _)) _) = evaluate v
ev_Z v = primOpFail v ABC_Z

ev_V (Pair a e) = evaluate (Pair (SumL a) e)
ev_V v = primOpFail v ABC_V

ev_C (Pair (SumL a) e) = evaluate (Pair a e)
ev_C v = primOpFail v ABC_C

ev_copy p@(Pair a _) | copyable a = evaluate (Pair a p)
ev_copy v = primOpFail v ABC_copy

ev_drop (Pair a e) | droppable a = evaluate e
ev_drop v = primOpFail v ABC_drop

ev_add (Pair (Number a) (Pair (Number b) e)) = 
    let n' = Number (a+b) in
    n' `seq` evaluate (Pair n' e) 
ev_add v = primOpFail v ABC_add

ev_negate (Pair (Number n) e) =
    let n' = Number (negate n) in
    n' `seq` evaluate (Pair n' e)
ev_negate v = primOpFail v ABC_negate

ev_multiply (Pair (Number a) (Pair (Number b) e)) =
    let n' = Number (a * b) in
    n' `seq` evaluate (Pair n' e)
ev_multiply v = primOpFail v ABC_multiply

ev_reciprocal (Pair (Number n) e) | (n /= 0) =
    let n' = Number (recip n) in
    n' `seq` evaluate (Pair n' e)
ev_reciprocal v = primOpFail v ABC_reciprocal

ev_divMod (Pair (Number b) (Pair (Number a) e)) | (b /= 0) =
    let (q,r) = abcDivMod a b in
    let nq = Number q in
    let nr = Number r in
    nr `seq` nq `seq` evaluate (Pair nr (Pair nq e))
ev_divMod v = primOpFail v ABC_divMod

ev_compare (Pair nx@(Number x) (Pair ny@(Number y) e)) = evaluate (Pair s e) 
    where s = if (y > x) then SumR (Pair nx ny) 
                         else SumL (Pair ny nx) 
ev_compare v = primOpFail v ABC_compare

-- ev_apply has a special condition to test for tail call
ev_apply (Pair (Block fn _) (Pair arg env)) cc =
    case (cc_cont cc, env) of
        ([ABC_PrimOp ABC_c], Unit) -> 
            let cc' = cc { cc_cont = abcOps fn } in
            evaluate arg cc' -- tail call; cc_stack does not grow
        _ -> evaluate arg $ Cont { cc_cont = abcOps fn, cc_stack = Apply env cc }
ev_apply v cc = primOpFail v ABC_apply cc

-- condApply is only permitted for 'droppable' functions
ev_condApply (Pair (Block _ f) v@(Pair (SumR _) _)) cc | f_droppable f = evaluate v cc
ev_condApply (Pair (Block fn f) (Pair (SumL arg) env)) cc | f_droppable f =
    evaluate arg $ Cont { cc_cont = abcOps fn, cc_stack = Cond env cc }
ev_condApply v cc = primOpFail v ABC_condApply cc

-- quotation is very expensive in this simplistic model of ABC!
ev_quote (Pair a e) = evaluate (Pair (Block fn flags) e) where
    fn = ABC (quote a)
    flags = rel .|. aff
    rel = if droppable a then 0 else prop_relevant
    aff = if copyable a then 0 else prop_affine 
ev_quote v = primOpFail v ABC_quote

ev_compose (Pair (Block xy fxy) (Pair (Block yz fyz) e) =
    let xz = ABC (abcOps xy ++ abcOps yz) in
    let fxz = fxy .|. fyz in
    evaluate (Pair (Block xz fxz) e)
ev_compose v = primOpFail v ABC_compose

ev_relevant (Pair (Block fn flags) e) = evaluate (Pair (Block fn flags') e) where
    flags' = flags .|. prop_relevant
ev_relevant v = primOpFail v ABC_relevant

ev_affine (Pair (Block fn flags) e) = evaluate (Pair (Block fn flags') e) where
    flags' = flags .|. prop_affine
ev_affine v = primOpFail v ABC_affine

ev_distrib (Pair a (Pair (SumL b) e)) = evaluate (Pair (SumL (Pair a b)) e)
ev_distrib (Pair a (Pair (SumR c) e)) = evaluate (Pair (SumR (Pair a b)) e)
ev_distrib v = primOpFail v ABC_distrib

ev_factor (Pair (SumL (Pair a b)) e) = evaluate (Pair (SumL a) (Pair (SumL b) e))
ev_factor (Pair (SumR (Pair c d)) e) = evaluate (Pair (SumR c) (Pair (SumR d) e))
ev_factor v = primOpFail v ABC_factor

ev_merge (Pair (SumL a) e) = evaluate (Pair a e)
ev_merge (Pair (SumR a') e) = evaluate (Pair a' e)
ev_merge v = primOpFail v ABC_merge

ev_assert (Pair (SumR a) e) = evaluate (Pair a e)
ev_assert v = primOpFail v ABC_assert

ev_newZero = evaluate . Pair (Number 0)
ev_d d (Pair (Number n) e) =
    let n' = Number ((10 * n) + fromIntegral d) in
    n' `seq` evaluate (Pair n' e)

ev_SP = evaluate
ev_LF = evaluate






