{-# LANGUAGE BangPatterns, ViewPatterns, OverloadedStrings #-}
-- | Minimal evaluator for Wikilon.ABC.Fast
module Wikilon.ABC.Eval (eval, Quota, OnTok, defaultOnTok) where

import Control.Monad
import Data.Word (Word64)
import Data.Int (Int64)
import Data.Bits
import Data.Monoid
import qualified Data.List as L
import Wikilon.ABC.Pure (PrimOp(..)) 
import qualified Data.Array.IArray as A
import Wikilon.ABC.Fast (V(..), Op(..), ExtOp(..))
import qualified Wikilon.ABC.Fast as ABC
import Wikilon.Token
import qualified Data.ByteString.UTF8 as UTF8
import qualified Debug.Trace as Debug


type Quota = Word64

-- Handle the token. May expand into extra operations. May 
-- modify our value directly. Should have a pure effect.
type OnTok = Token -> V -> Maybe ([Op],V)
data CX = CX 
    { cx_quota :: {-# UNPACK #-} !Quota 
    , cx_stack :: [[Op]] 
    , cx_ontok :: !OnTok
    }

defaultOnTok :: Token -> V -> Maybe ([Op],V)
defaultOnTok tok@(Token s) = 
  case UTF8.uncons s of
    Nothing -> const Nothing
    Just (c,s') -> case c of 
        ':' -> seal tok
        '.' -> unseal tok
        '&' -> case s' of
            "trace" -> anno_trace
            "trash" -> anno_trash
            "lazy"  -> anno_lazy
            "join"  -> anno_join
            _       -> \v -> return ([],v)
        _ -> const Nothing

anno_trace :: V -> Maybe ([op],V)
anno_trace (P a b) = do
    let msg = "[{&trace}]% " ++ show a
    Debug.traceM msg
    return ([], P (trashed a) b)
anno_trace _ = mzero

anno_trash :: V -> Maybe ([op],V)
anno_trash (P a b) = return ([], P (trashed a) b)
anno_trash _ = mzero

anno_lazy :: V -> Maybe ([op],V)
anno_lazy (P (B abc kf) e) = return ([], P b' e)
    where b' = B abc (ABC.f_lazy .|. kf)
anno_lazy _ = mzero

anno_join :: V -> Maybe ([Op], V)
anno_join (P (Z abc arg) e) = 
    let block = (B abc zeroBits) in
    let cont = [ABC_Prim ABC_apply] in
    return (cont, P block (P arg e))
anno_join _ = mzero

trashed :: V -> V
trashed _ = S "&trash" (B mempty linear) -- assume linearity
    where linear = ABC.f_rel .|. ABC.f_aff

seal :: Token -> V -> Maybe ([op], V)
seal s (P a b) = return ([], P (S s a) b)
seal _ _ = mzero

unseal :: Token -> V -> Maybe ([op], V)
unseal u (P (S s a) b) | matchSealerUnsealer s u 
    = return ([], P a b)
unseal _ _ = mzero

matchSealerUnsealer :: Token -> Token -> Bool
matchSealerUnsealer (Token s) (Token u) = maybe False id $ 
    UTF8.uncons s >>= \ (ps, s') ->
    UTF8.uncons u >>= \ (pu, u') ->
    return $! (ps == ':') && (pu == '.') && (s' == u')

-- | Perform evaluation. Return either a final result in the right,
-- or an incomplete result in the left. If we halt on quota, the 
-- `{&quota}` token will be the first action.
--
-- At the moment, this does not perform range checks on integers
-- nor validate substructure for copy and drop of values.
eval :: Quota -> OnTok -> [Op] -> V -> ([Op],V)
eval quota ontok = evalStack (CX quota [] ontok)

type EvalStep = CX -> [Op] -> V -> ([Op],V)

-- when evaluation is stuck...
evalStuck :: EvalStep
evalStuck cx ops v = (L.concat (ops : cx_stack cx), v)

-- basic evaluation path
evalStack :: EvalStep
evalStack !cx ops@(op:ops') v = case op of
    ABC_Prim p -> evalPrim  p cx ops' v
    ABC_Ext  x -> evalExt   x cx ops' v
    ABC_Val  n -> evalStack   cx ops' (P n v)
    ABC_Tok  t -> case (cx_ontok cx) t v of
        Nothing      -> evalStuck cx ops v
        Just (cc,v') -> evalStack cx (cc <> ops') v' 
evalStack !cx [] v = case cx_stack cx of
    (ops':s') -> case cx_quota cx of
        0 -> evalStuck cx [ABC_Tok "&quota"] v
        q -> evalStack cx' ops' v where
            cx' = CX (q - 1) s' (cx_ontok cx)
    [] -> ([], v) -- evaluation complete

evalPrim :: PrimOp -> EvalStep
evalPrim = (A.!) primOpStepArray

evalExt :: ExtOp -> EvalStep
evalExt = (A.!) extOpStepArray

primOpStepArray :: A.Array PrimOp EvalStep
primOpStepArray = fillFrom primOpStepTable where
    fillFrom = A.accumArray ins dv (minBound,maxBound)
    dv = error "missing evaluator for PrimOp"
    ins _ c = c

extOpStepArray :: A.Array ExtOp EvalStep
extOpStepArray = fillFrom extOpStepTable where
    fillFrom = A.accumArray ins dv (minBound,maxBound)
    dv = error "missing evaluator for ExtOp"
    ins _ c = c

primOpStepTable :: [(PrimOp, EvalStep)]
primOpStepTable = 
    [(ABC_l, ev_l), (ABC_r, ev_r), (ABC_w, ev_w), (ABC_z, ev_z), (ABC_v, ev_v), (ABC_c, ev_c)
    ,(ABC_L, ev_L), (ABC_R, ev_R), (ABC_W, ev_W), (ABC_Z, ev_Z), (ABC_V, ev_V), (ABC_C, ev_C)

    ,(ABC_copy, ev_copy), (ABC_drop, ev_drop)

    ,(ABC_apply, ev_apply), (ABC_condApply, ev_condApply)
    ,(ABC_quote, ev_quote), (ABC_compose, ev_compose)
    ,(ABC_relevant, ev_relevant), (ABC_affine, ev_affine)

    ,(ABC_distrib, ev_distrib), (ABC_factor, ev_factor)
    ,(ABC_merge, ev_merge), (ABC_assert, ev_assert)

    ,(ABC_add, ev_add), (ABC_negate, ev_negate)
    ,(ABC_multiply, ev_multiply), (ABC_divMod, ev_divMod)
    ,(ABC_compare, ev_compare)

    ,(ABC_newZero, ev_newZero)
    ,(ABC_d0, dN 0), (ABC_d1, dN 1), (ABC_d2, dN 2), (ABC_d3, dN 3), (ABC_d4, dN 4)
    ,(ABC_d5, dN 5), (ABC_d6, dN 6), (ABC_d7, dN 7), (ABC_d8, dN 8), (ABC_d9, dN 9)

    -- identity functions can just continue with evalStack
    ,(ABC_SP, evalStack), (ABC_LF, evalStack)
    ]

extOpStepTable :: [(ExtOp, EvalStep)]
extOpStepTable = 
    [(ExtOp_Inline, accel_Inline)
    ,(ExtOp_Swap, accel_Swap)
    ,(ExtOp_Mirror, accel_Mirror)
    ]

isLazy :: ABC.Flags -> Bool
isLazy kf = (zeroBits /= (ABC.f_lazy .&. kf)) 

extStuck :: ExtOp -> EvalStep
extStuck x cx ops v = evalStuck cx (ABC_Ext x : ops) v

apply :: ABC.ABC -> ABC.Flags -> EvalStep
apply abc kf cx ops arg
    | isLazy kf = evalStack cx ops (Z abc arg)
    | L.null ops = -- tail call optimization
        let s' = block_ops : cx_stack cx in
        let cx' = cx { cx_stack = s' } in
        evalStack cx' [] arg -- [] for quota check
    | otherwise = -- normal evaluation
        let s' = ops : cx_stack cx in
        let cx' = cx { cx_stack = s' } in
        evalStack cx' block_ops arg
    where block_ops = ABC.expandOps abc

accel_Inline :: EvalStep
accel_Inline cx ops v = case v of
    (P (B abc kf) arg) -> apply abc kf cx ops arg
    _ -> extStuck ExtOp_Inline cx ops v

accel_Swap :: EvalStep
accel_Swap cx ops v = case v of
    (P a b) -> evalStack cx ops (P b a)
    _ -> extStuck ExtOp_Swap cx ops v

accel_Mirror :: EvalStep
accel_Mirror cx ops v = case v of
    (L a) -> evalStack cx ops (R a)
    (R a) -> evalStack cx ops (L a)
    _ -> extStuck ExtOp_Mirror cx ops v

primStuck :: PrimOp -> EvalStep
primStuck p cx ops v = evalStuck cx (ABC_Prim p : ops) v

dprim :: Int64 -> PrimOp
dprim = L.genericIndex dList where
    dList = [ABC_d0, ABC_d1, ABC_d2, ABC_d3, ABC_d4
            ,ABC_d5, ABC_d6, ABC_d7, ABC_d8, ABC_d9]

dN :: Int64 -> EvalStep
dN digit cx ops v = case v of
    (P (N n) b) -> evalStack cx ops (P (N (10*n + digit)) b)
    _ -> primStuck (dprim digit) cx ops v

ev_newZero :: EvalStep
ev_newZero cx ops v = evalStack cx ops (P (N 0) v)

ev_l, ev_r, ev_w, ev_z, ev_v, ev_c :: EvalStep
ev_l cx ops v = case v of
    (P a (P b c)) -> evalStack cx ops (P (P a b) c)
    _ -> primStuck ABC_l cx ops v
ev_r cx ops v = case v of
    (P (P a b) c) -> evalStack cx ops (P a (P b c))
    _ -> primStuck ABC_r cx ops v
ev_w cx ops v = case v of
    (P a (P b c)) -> evalStack cx ops (P b (P a c))
    _ -> primStuck ABC_w cx ops v
ev_z cx ops v = case v of
    (P a (P b (P c d))) -> evalStack cx ops (P a (P c (P b d)))
    _ -> primStuck ABC_z cx ops v
ev_v cx ops v = evalStack cx ops (P v U)
ev_c cx ops v = case v of
    (P a U) -> evalStack cx ops a
    _ -> primStuck ABC_c cx ops v

ev_L, ev_R, ev_W, ev_Z, ev_V, ev_C :: EvalStep
ev_L = onFirst ABC_L ev_L'
ev_R = onFirst ABC_R ev_R'
ev_W = onFirst ABC_W ev_W'
ev_Z = onFirst ABC_Z ev_Z'
ev_V = onFirst ABC_V ev_V'
ev_C = onFirst ABC_C ev_C'

onFirst :: PrimOp -> (V -> Maybe V) -> EvalStep
onFirst p rw cx ops v = case v of
    (P (rw -> Just a') b) -> evalStack cx ops (P a' b) 
    _ -> primStuck p cx ops v

ev_L', ev_R', ev_W', ev_Z', ev_V', ev_C' :: V -> Maybe V

-- L :: (a+(b+c)) → ((a+b)+c)
ev_L' la@(L _) = return (L la)
ev_L' (R rc@(R _)) = return rc
ev_L' (R (L b)) = return (L (R b))
ev_L' _ = mzero

-- R :: ((a+b)+c) → (a+(b+c))
ev_R' rc@(R _) = return (R rc)
ev_R' (L la@(L _)) = return la
ev_R' (L (R b)) = return (R (L b))
ev_R' _ = mzero

-- W :: (a+(b+c)) → (b+(a+c))
ev_W' la@(L _) = return (R la)
ev_W' (R lb@(L _)) = return lb
ev_W' rrc@(R (R _)) = return rrc
ev_W' _ = mzero

-- Z :: (a+(b+(c+d))) → (a+(c+(b+d)))
ev_Z' la@(L _) = return la
ev_Z' (R bcd) = fmap R (ev_W' bcd)
ev_Z' _ = mzero

ev_V' v = return (L v)

ev_C' (L v) = return v
ev_C' _ = mzero

ev_copy, ev_drop :: EvalStep
ev_copy cx ops v = case v of
    -- assuming copyable.
    (P a e) -> evalStack cx ops (P a (P a e))
    _ -> primStuck ABC_copy cx ops v
ev_drop cx ops v = case v of
    -- assuming droppable
    (P _a e) -> evalStack cx ops e
    _ -> primStuck ABC_drop cx ops v

ev_apply, ev_condApply :: EvalStep
ev_apply cx (ABC_Prim ABC_c : ops') (P (B abc kf) (P arg U)) 
    -- dynamic guarantee of `$c` tail call optimization
    = apply abc kf cx ops' arg
ev_apply cx ops v = case v of
    (P (B abc kf) (P arg e)) -> apply abc kf cx ops' arg
        where ops' = (ABC_Val e : ABC_Ext ExtOp_Swap : ops) 
    _ -> primStuck ABC_apply cx ops v

ev_condApply cx ops v = case v of
    (P (B abc kf) (P (L arg) e)) -> apply abc kf cx ops' arg
        where ops' = (ABC_Val e : ABC_Ext ExtOp_Swap : ABC_Prim ABC_V : ops)
    (P (B _ _) e@(P (R _) _)) -> evalStack cx ops e
    _ -> primStuck ABC_condApply cx ops v

ev_quote, ev_compose :: EvalStep
ev_quote cx ops v = case v of
    (P a e) -> 
        let qa = B (ABC.fastQuote a) zeroBits in
        evalStack cx ops (P qa e)
    _ -> primStuck ABC_quote cx ops v

ev_compose cx ops v = case v of
    (P (B ab kf1) (P (B bc kf2) e)) -> 
        let b' = B (ab <> bc) (kf1 .|. kf2) in
        evalStack cx ops (P b' e)
    _ -> primStuck ABC_compose cx ops v

ev_relevant, ev_affine :: EvalStep
ev_relevant cx ops v = case v of
    (P (B abc kf) e) -> evalStack cx ops (P b' e) 
        where b' = B abc (kf .|. ABC.f_rel)
    _ -> primStuck ABC_relevant cx ops v
ev_affine cx ops v = case v of
    (P (B abc kf) e) -> evalStack cx ops (P b' e)
        where b' = B abc (kf .|. ABC.f_aff)
    _ -> primStuck ABC_affine cx ops v
   

ev_distrib, ev_factor, ev_merge, ev_assert :: EvalStep

-- D :: (a * ((b+c) * e)) → (((a*b)+(a*c))*e)
ev_distrib cx ops v = case v of
    (P a (P (L b) e)) -> evalStack cx ops (P (L (P a b)) e)
    (P a (P (R c) e)) -> evalStack cx ops (P (R (P a c)) e)
    _ -> primStuck ABC_distrib cx ops v

-- F :: ((a*b) + (c*d)) * e → (a+c) * ((b+d) * e)
ev_factor cx ops v = case v of
    (P (L (P a b)) e) -> evalStack cx ops (P (L a) (P (L b) e))
    (P (R (P c d)) e) -> evalStack cx ops (P (R c) (P (R d) e))
    _ -> primStuck ABC_factor cx ops v

-- M :: (a + a') * e → a * e
ev_merge cx ops v = case v of
    (P (L a) e)  -> evalStack cx ops (P a  e)
    (P (R a') e) -> evalStack cx ops (P a' e)
    _ -> primStuck ABC_merge cx ops v

-- K :: (a + b) * e → b * e; must be in b
ev_assert cx ops v = case v of
    (P (R b) e) -> evalStack cx ops (P b e)
    _ -> primStuck ABC_assert cx ops v

ev_add, ev_negate, ev_multiply, ev_divMod, ev_compare :: EvalStep
ev_add cx ops v = case v of
    (P (N a) (P (N b) e)) -> evalStack cx ops (P (N (a+b)) e)
    _ -> primStuck ABC_add cx ops v
ev_negate cx ops v = case v of
    (P (N a) e) -> evalStack cx ops (P (N (negate a)) e)
    _ -> primStuck ABC_negate cx ops v
ev_multiply cx ops v = case v of
    (P (N a) (P (N b) e)) -> evalStack cx ops (P (N (a*b)) e)
    _ -> primStuck ABC_multiply cx ops v
ev_divMod cx ops v = case v of
    (P (N divisor) (P (N dividend) e)) | (0 /= divisor) ->
        let (q,r) = dividend `divMod` divisor in
        evalStack cx ops (P (N r) (P (N q) e))
    _ -> primStuck ABC_divMod cx ops v
ev_compare cx ops v = case v of
    (P (N x) (P (N y) e)) 
        | (y > x)   -> evalStack cx ops (P (R (P (N x) (N y))) e)
        | otherwise -> evalStack cx ops (P (L (P (N y) (N x))) e)
    _ -> primStuck ABC_compare cx ops v

