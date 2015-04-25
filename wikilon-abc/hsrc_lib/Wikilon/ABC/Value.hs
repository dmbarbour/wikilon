{-# LANGUAGE BangPatterns, ViewPatterns, OverloadedStrings #-}

-- | Wikilon's value model has performance extensions and a slot for
-- value resources. 
module Wikilon.ABC.Value
    ( Value(..)
    , Flags
    , copyable
    , droppable
    , f_rel, f_aff, f_par
    ) where

import Data.Word
import Data.Bits

import Awelon.ABC (Quotable(..))
import qualified Awelon.ABC as Pure
import Wikilon.ABC.Code

-- | Wikilon's basic values and performance extensions.
data Value r
    -- normal ABC values
    = Number {-# UNPACK #-} !Rational
    | Pair (Value r) (Value r)
    | SumL (Value r)
    | SumR (Value r)
    | Unit
    | Block !(ABC (Value r)) {-# UNPACK #-} !Flags
    | Sealed !Token (Value r)
    -- performance extensions
    | Text !Text
    | Resource r {-# UNPACK #-} !Flags
    | Copyable (Value r)
    deriving (Eq)


-- | Flags for blocks and resources.
--
--   bit 0: relevant (no drop with %)
--   bit 1: affine   (no copy with ^)
--   bit 2: parallelism {&fork}
--   bit 3..7: Reserved. Tentative ideas:
--     memoization features for blocks
--     local values (cannot send as message content)
--     ephemeral values (cannot store in state)
--     mark impure blocks (AVMs don't use impure blocks)
--     termination guarantees for blocks
--
-- Besides the basic substructural types, it might be worth using
-- annotations to enforce a few new ones.
type Flags = Word8

f_rel, f_aff, f_par :: Flags
f_rel = 0x01
f_aff = 0x02
f_par = 0x04

hasF :: Flags -> Flags -> Bool
hasF f bf = (f == (f .&. bf))
{-# INLINE hasF #-}

f_copyable, f_droppable :: Flags -> Bool
f_copyable = not . hasF f_aff
f_droppable = not . hasF f_rel

-- | Is this value copyable (not affine)
copyable :: Value r -> Bool
copyable (Number _) = True
copyable (Pair a b) = copyable a && copyable b
copyable (SumL a) = copyable a
copyable (SumR b) = copyable b
copyable Unit = True
copyable (Block _ f) = f_copyable f
copyable (Sealed _ v) = copyable v
copyable (Text _) = True
copyable (Resource _ f) = f_copyable f
copyable (Copyable _) = True

-- | Is this value droppable (not relevant)
droppable :: Value r -> Bool
droppable (Number _) = True
droppable (Pair a b) = droppable a && droppable b
droppable (SumL a) = droppable a
droppable (SumR b) = droppable b
droppable Unit = True
droppable (Block _ f) = f_droppable f
droppable (Sealed _ v) = droppable v
droppable (Text _) = True
droppable (Resource _ f) = f_droppable f
droppable (Copyable v) = droppable v

-- Value will quote as ABC that regenerates it. This includes
-- regenerating any linked or stowed value resources. Note that
-- no simplification is performed. Developers should consider
-- simplifying values (e.g. to precipitate text) before quotation.
instance (Quotable r) => Quotable (Value r) where
    quotes (Number r) = quotes r
    quotes (Pair a b) = quotes b . quotes a . quotes ABC_l 
    quotes (SumR b) = quotes b . quotes ("VVRWLC" :: Pure.ABC)
    quotes (SumL a) = quotes a . quotes ABC_V
    quotes Unit = quotes ("vvrwlc" :: Pure.ABC)
    quotes (Block abc flags) = 
        let block = quotes (Pure.ABC_Block $ Pure.ABC $ Pure.quote abc) in
        let bAff = hasF f_aff flags in
        let bRel = hasF f_rel flags in
        let bPar = hasF f_par flags in
        let k = if bRel then quotes ABC_relevant else id in
        let f = if bAff then quotes ABC_affine else id in
        let j = if bPar then quotes (Pure.ABC_Tok "{&fork}") else id in
        block . k . f . j
    quotes (Sealed tok a) = quotes a . quotes (Pure.ABC_Tok tok)
    quotes (Text t) = quotes (Pure.ABC_Text t)
    quotes (Resource r _) = quotes r . quotes (Pure.ABC_Tok "{&stow}")
    quotes (Copyable v) = quotes v . quotes (Pure.ABC_Tok "{&^}")

{-
abcErr :: String -> String
abcErr = (++) "Wikilon.ABC.Value: "

impossible :: String -> a
impossible = error . abcErr
-}
