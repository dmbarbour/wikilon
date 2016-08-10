{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Minimal evaluator for Wikilon.ABC.Fast
module Wikilon.ABC.Eval (eval, Quota, Stuck) where

import Data.Word (Word64)
import qualified Data.List as L
import qualified Wikilon.ABC.Pure as P
import Wikilon.ABC.Fast (V(..), Op(..))
import Wikilon.Token

type Quota = Word64
type Stack = [[Op]]
type Stuck = (V, [Op])

-- | Evaluate a program to compute a value. If we return in a stuck
-- state, a token is injected as the first item in the program to 
-- indicate the nature of the error. E.g. `{&etype}` or `{&quota}`.
--
-- The quota corresponds to a number of blocks to evaluate.
eval :: Quota -> [Op] -> V -> Either Stuck V
eval quota prog = evalStack quota prog []

stuck :: Token -> [Op] -> Stack -> V -> Stuck
stuck tok ops ss v = (v, prog') where
    prog' = L.concat ((ABC_Tok tok : ops) : ss)

evalStack :: Quota -> [Op] -> Stack -> V -> Either Stuck V
evalStack !q ops@(op:ops') s v = case evalOp op v of
    Nothing -> Left $ stuck "&etype" ops s v
    Just v' -> evalStack q ops' s v'
evalStack !q [] (ops:s) v 
    | (0 == q)  = Left $ stuck "&quota" ops s v
    | otherwise = evalStack (q - 1) ops s v
evalStack _ [] [] v = Right v

evalOp :: Op -> V -> Maybe V
evalOp = undefined


