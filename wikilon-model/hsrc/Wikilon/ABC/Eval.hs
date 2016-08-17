{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Minimal evaluator for Wikilon.ABC.Fast
module Wikilon.ABC.Eval (eval, Quota, OnTok, defaultOnTok) where

import Data.Word (Word64)
import qualified Data.List as L
import Wikilon.ABC.Pure (PrimOp(..)) 
import qualified Wikilon.ABC.Pure as P
import qualified Data.Array.IArray as A
import Wikilon.ABC.Fast (V(..), Op(..), ExtOp(..))
import qualified Wikilon.ABC.Fast as ABC
import Wikilon.Token
import qualified Data.ByteString.UTF8 as UTF8
import qualified Debug.Trace as Debug


type Quota = Word64
type Stack = [[Op]]
type OnTok = (Token -> V -> Maybe V)

defaultOnTok :: Token -> V -> Maybe V
defaultOnTok tok@(Token s) = 
  case UTF8.uncons s of
    Nothing -> const Nothing
    Just (c,s') -> case c of 
        ':' -> seal tok
        '.' -> unseal s'
        '&' -> case s' of
            "trace" -> trace
            "trash" -> trash
            _       -> return
        _ -> const Nothing

trace :: V -> Maybe V
trace (P a b) = Debug.traceM msg >> return (P (trashed a) b) where
    msg = "[{&trace}]% " ++ show a
trace _ = Nothing

trash :: V -> Maybe V
trash (P a b) = return (P (trashed a) b)
trash _ = Nothing

trashed :: V -> V
trashed _ = S "trash" U -- for now

seal :: Token -> V -> Maybe V
seal s (P a b) = Just (P (S s a) b)
seal _ _ = Nothing

unseal :: UTF8.ByteString -> V -> Maybe V
unseal u (P (S (Token s) a) b) | match u s = Just (P a b) where
    match u s = (Just (':', u) == UTF8.uncons s)
unseal _ _ = Nothing

-- | Evaluate a program out to some quota. Returns a remaining
-- program and intermediate result, or a final value.
eval :: Quota -> OnTok -> [Op] -> V -> ([Op], V)
eval quota ontok prog = evalStack quota ontok prog []

evalStack :: Quota -> OnTok -> [Op] -> Stack -> V -> ([Op], V)
evalStack !q ontok ops@(op:ops') s v = case op of
    ABC_Prim p -> evalPrim p q ops' s v
    ABC_Ext  x -> evalExt  x q ops' s v
    ABC_Val  n -> evalStack  q ontok ops' s (P n v)
    ABC_Tok  t -> case ontok t v of
        Nothing -> (L.concat (ops:s), v)
        Just v' -> evalStack q ontok ops' s v'
evalStack !q ontok [] s@(ops:s') v
    | (0 == q)  = (ABC_Tok "&quota" : L.concat s, v)
    | otherwise = evalStack (q - 1) ontok ops s' v
evalStack _ _ [] [] v = ([], v)


evalPrim = error "todo: Eval Prim ops"
evalExt = error "todo: Eval EXT ops"

