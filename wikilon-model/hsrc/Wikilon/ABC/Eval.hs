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
evalStack !q (op:ops) s v = undefined
evalStack !q [] (ops:s) v  
    | (0 == q) = let cc = L.concat $ ((ABC_Tok "&quota" : ops) : s) in (cc, v)
    | otherwise = evalStack (q - 1) ops s v
evalStack _ [] [] v = ([],v)

