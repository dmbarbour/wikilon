{-# LANGUAGE OverloadedStrings, BangPatterns, ViewPatterns #-}
-- | A simplistic evaluator for purely functional ABC in a similar
-- style as free monads. May be wrapped with ad-hoc effects handlers.
--
-- This evaluator does perform a tail-call optimization. However, 
-- quotation is very expensive (serializes values out to relevant
-- code), as are copy and drop. I don't expect great performance.
-- 
module Awelon.ABC.Eval 
    ( Value(..)
    , Quota
    , copyable
    , droppable
    , toText
    , fromText
    , Cont(..)
    , Stack(..)
    , Stuck(..)
    , Evaluator
    , evaluate
    , abcOpEvalTable
    , module Awelon.ABC
    ) where

import Data.Monoid
import Data.Word
import Data.Char
import Data.Bits
import Data.Ratio
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
-- to consider: I might want to more precisely model a sealer's information,
-- i.e. as Discretionary Token | Cryptographic Format Key | etc.

-- | When performing evaluation, we'll use a simple quota system to
-- occasionally return control to the caller. The motivation is to
-- guard against runs-forever behavior and provide opportunity to
-- reschedule or optimize any long-running operation.
-- 
-- Quota is checked just after '$' and '?' operations that add to 
-- the current workload. When stuck on quota, I inject token {&quota}.
type Quota = Int

-- | When introducing a frame, apply a minimal cost because frames
-- themselves aren't free.
frameQuotaCost :: ABC -> Quota
frameQuotaCost = max 32 . abcOpsCount

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
        k = has prop_relevant ABC_relevant
        f = has prop_affine ABC_affine
    quotes (Sealed tok val) = quotes val . quotes (ABC_Tok tok)
instance Show Value where 
    showsPrec _ = shows . quote


toText :: Value -> Maybe Text
toText = tt mempty where
    tt bb (SumR Unit) = Just (BB.toLazyByteString bb)
    tt bb (SumL (Pair (Number (numToChar -> Just c)) more)) =
        tt (bb <> BB.charUtf8 c) more
    tt _ _ = Nothing

numToChar :: Rational -> Maybe Char
numToChar r =
    let n = fromInteger $ numerator r in
    let d = denominator r in
    let bOK = (1 == d) && (0 <= n) && (n <= 0x10ffff) in
    if bOK then Just $! chr n else Nothing

fromText :: Text -> Value
fromText t = case LazyUTF8.uncons t of
    Just (c, t') -> 
        let n = Number (fromIntegral (ord c)) in
        SumL $! n `seq` (Pair n (fromText t'))
    Nothing -> SumR Unit


-- | A simple continuation passing evaluation model. 
--
-- Note: if we reach a 'Stuck' state that doesn't necessarily imply 
-- error. In particular, our basic evaluation function will always 
-- get stuck on tokens, even annotations, allowing external handling
-- of the logic.
type Evaluator = Value -> Cont -> Quota -> Either Stuck Value

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

instance Show Cont where showsPrec _ = shows . quote
instance Show Stack where showsPrec _ = shows . quote

instance Quotable Stack where
    quotes Return = id 
    quotes (Apply env cc) = quotes env . quotes ("vrwlc" :: ABC)  . quotes cc
    quotes (Cond env cc)  = quotes env . quotes ("vrwlcV" :: ABC) . quotes cc 
instance Quotable Cont where
    quotes cc = quotesList (cc_cont cc) . quotes (cc_stack cc) where

-- Thoughts: I'd like to model fork-join parallelism using effects.
-- The difficulty is that, if I just use 'par', it becomes difficult
-- to capture the 'stuck' points. I'll need some way to interfere
-- with running behaviors.

-- | It is possible that our evaluator will get stuck. In this case,
-- we just return where it became stuck. The caller might know how to
-- resolve this issue, e.g. in case of token operators. 
data Stuck = Stuck 
    { stuck_arg  :: !Value
    , stuck_curr :: !Op
    , stuck_cont :: !Cont
    , stuck_quota :: {-# UNPACK #-} !Quota
    }

-- get stuck!
stuck :: Op -> Evaluator
stuck op v cc qu = Left $ Stuck
    { stuck_arg = v
    , stuck_curr = op
    , stuck_cont = cc
    , stuck_quota = qu
    }

-- | quota is an annotation. If an interpreter ignores it, the 
-- impact on observable behavior is negligible, though performance
-- may take a hit since we'll get stuck on every $ and ? operation.
--
-- The best response to getting stuck on quota is to add 
stuckOnQuota :: Evaluator
stuckOnQuota = stuck (ABC_Tok "&quota")

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

    ,(ABC_newZero,ev_newZero),(ABC_d0,ev_d0)
    ,(ABC_d1,ev_d1),(ABC_d2,ev_d2),(ABC_d3,ev_d3)
    ,(ABC_d4,ev_d4),(ABC_d5,ev_d5),(ABC_d6,ev_d6)
    ,(ABC_d7,ev_d7),(ABC_d8,ev_d8),(ABC_d9,ev_d9) -- 11

    ,(ABC_SP,ev_SP),(ABC_LF,ev_LF) -- 2
    ]

abcOpEvalArray :: A.Array PrimOp Evaluator
abcOpEvalArray = A.array (minBound,maxBound) abcOpEvalTable

-- | Basic evaluator, without any effects model. All tokens shall
-- cause this evaluator to become 'stuck'. Effects handlers can be
-- installed by wrapping this evaluator.
evaluate :: Evaluator
evaluate v cc qu = case cc_cont cc of
    (op:ops) -> let cc' = cc { cc_cont = ops } in case op of
        (ABC_Prim primOp) -> (A.!) abcOpEvalArray primOp v cc' qu
        (ABC_Block abc) -> evaluate (Pair (Block abc 0) v) cc' qu
        (ABC_Text txt) -> evaluate (Pair (fromText txt) v) cc' qu
        _ -> stuck op v cc' qu -- tokens always get stuck
    [] -> case cc_stack cc of
        (Apply v2 cc') -> evaluate (Pair v v2) cc' qu
        (Cond v2 cc') -> evaluate (Pair (SumL v) v2) cc' qu
        Return -> Right v -- all done!

-- push operator back in case of failure!
primOpFail :: PrimOp -> Evaluator
primOpFail = stuck . ABC_Prim

ev_l,ev_r,ev_w,ev_z,ev_v,ev_c :: Evaluator
ev_L,ev_R,ev_W,ev_Z,ev_V,ev_C :: Evaluator
ev_copy,ev_drop :: Evaluator
ev_add,ev_negate :: Evaluator
ev_multiply,ev_reciprocal :: Evaluator
ev_divMod,ev_compare :: Evaluator
ev_apply,ev_condApply,ev_quote,ev_compose :: Evaluator
ev_relevant,ev_affine :: Evaluator
ev_distrib,ev_factor,ev_merge,ev_assert :: Evaluator
ev_newZero :: Evaluator
ev_d0,ev_d1,ev_d2,ev_d3,ev_d4 :: Evaluator
ev_d5,ev_d6,ev_d7,ev_d8,ev_d9 :: Evaluator
ev_SP,ev_LF :: Evaluator

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

-- ev_apply has a special condition to test for tail call.
-- Also, ev_apply may halt on quota.
ev_apply (Pair (Block fn _) (Pair arg env)) cc qu =
    let qu' = qu - frameQuotaCost fn in
    let cc' = case (cc_cont cc, env) of
          ([ABC_Prim ABC_c],Unit) -> cc { cc_cont = abcOps fn }
          _ -> Cont { cc_cont = abcOps fn, cc_stack = Apply env cc }
    in
    if (qu' < 0) then stuckOnQuota arg cc' qu'
                 else evaluate arg cc' qu'
ev_apply v cc qu = primOpFail ABC_apply v cc qu

-- condApply is only permitted for 'droppable' functions
-- may halt on quota
ev_condApply (Pair (Block _ f) v@(Pair (SumR _) _)) cc qu | f_droppable f = evaluate v cc qu
ev_condApply (Pair (Block fn f) (Pair (SumL arg) env)) cc qu | f_droppable f =
    let qu' = qu - frameQuotaCost fn in
    let cc' = Cont { cc_cont = abcOps fn, cc_stack = Cond env cc } in
    if (qu' < 0) then stuckOnQuota arg cc' qu'
                 else evaluate arg cc' qu'
ev_condApply v cc qu = primOpFail ABC_condApply v cc qu

-- quotation is very expensive in this simplistic model of ABC!
ev_quote (Pair a e) = evaluate (Pair (Block fn flags) e) where
    fn = mkABC (quote a)
    flags = rel .|. aff
    rel = if droppable a then 0 else prop_relevant
    aff = if copyable a then 0 else prop_affine 
ev_quote v = primOpFail ABC_quote v

ev_compose (Pair (Block xy fxy) (Pair (Block yz fyz) e)) =
    let xz = xy <> yz in
    let fxz = fxy .|. fyz in
    evaluate (Pair (Block xz fxz) e)
ev_compose v = primOpFail ABC_compose v

ev_relevant (Pair (Block fn flags) e) = evaluate (Pair (Block fn flags') e) where
    flags' = flags .|. prop_relevant
ev_relevant v = primOpFail ABC_relevant v

ev_affine (Pair (Block fn flags) e) = evaluate (Pair (Block fn flags') e) where
    flags' = flags .|. prop_affine
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

ev_d :: Int -> PrimOp -> Evaluator
{-# INLINE ev_d #-}
ev_d d _ (Pair (Number n) e) =
    let n' = Number ((10 * n) + fromIntegral d) in
    n' `seq` evaluate (Pair n' e)
ev_d _ op v = primOpFail op v

ev_SP = evaluate
ev_LF = evaluate

