{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Minimal evaluator for Wikilon.ABC.Fast
module Wikilon.ABC.Eval (eval, Quota, Stuck) where

import Data.Word (Word64)
import qualified Data.List as L
import qualified Wikilon.ABC.Pure as P
import qualified Data.Array.IArray as A
import Wikilon.ABC.Fast (V(..), Op(..))
import Wikilon.Token

type Quota = Word64
type Stack = [[Op]]
type Stuck = (V, [Op])

-- | Evaluate a program out to some quota. Return the remaining
-- program along with the resulting value. The remaining program
-- will start with a `{&quota}` token if we halted on quota. If
-- not, is empty if evaluation completes, and otherwise indicates
-- an erroneous program.
eval :: Quota -> [Op] -> V -> ([Op], V)
eval quota prog = evalStack quota prog []

evalStack :: Quota -> [Op] -> Stack -> V -> ([Op], V)
evalStack !q ops@(op:ops') s v = case op of
    ABC_Prim p -> evalPrim p q ops' s v
    ABC_Ext  x -> evalExt  x q ops' s v
    ABC_Val  n -> evalStack  q ops' s (P n v)
    ABC_Tok  t -> evalTok  t q ops' s v
    
evalStack !q [] (ops:s') v 
    | (0 == q)  = (cc, v)
    | otherwise = evalStack (q - 1) ops s' v
    where cc = L.concat ((ABC_Tok "&quota" : ops) : s')
evalStack _ [] [] v = ([],v)

evalPrimOp