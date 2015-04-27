{-# LANGUAGE BangPatterns, ViewPatterns, OverloadedStrings #-}

-- | Wikilon's value model has performance extensions and a slot for
-- value resources. 
module Wikilon.ABC.Value
    ( Value(..)
    , Flags
    , copyable
    , droppable
    , f_rel, f_aff
    , f_droppable, f_copyable
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
    | Block !(ABC (Value r)) Flags
    | Sealed !Token (Value r)
    -- performance extensions
    | Text !Text
    | Resource r {-# UNPACK #-} !Flags
    deriving (Eq)

-- Aside: an idea I've considered is to use Copyable and Droppable
-- wrappers for values.
--
--  | Copyable (Value r)     -- quotes as value{&^}
--  | Droppable (Value r)    -- quotes as value{&%}
--
-- This would allow fast-copy or fast-drop for very large values. OTOH,
-- the benefit might be marginal if we make good use of resources.
--
-- Resolution: optimize later. Come back to this when we can profile
-- in realistic scenarios.

-- | Substructural properties for blocks and resources.
--
--   bit 0: relevant (no drop with %)
--   bit 1: affine   (no copy with ^)
--   bit 2..7: Reserved. Tentative:
--     local values (resists send as msg content)
--     ephemeral values (resists storage in state)
-- 
-- Aside: I had some other properties here such as parallelism and
-- memoization for blocks. However, they aren't a good fit for the
-- continuation passing interpreter I'm currently using. I'll need
-- to model fork-join parallelism another way, perhaps use {&fork}
-- to parallelize the top element on the call stack.
type Flags = Word8

f_rel, f_aff :: Flags
f_rel = 0x01
f_aff = 0x02

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
        let k = if bRel then quotes ABC_relevant else id in
        let f = if bAff then quotes ABC_affine else id in
        block . k . f
    quotes (Sealed tok a) = quotes a . quotes (Pure.ABC_Tok tok)
    quotes (Text t) = quotes (Pure.ABC_Text t)
    quotes (Resource r _) = quotes r . quotes (Pure.ABC_Tok "{&stow}")
   

{-
abcErr :: String -> String
abcErr = (++) "Wikilon.ABC.Value: "

impossible :: String -> a
impossible = error . abcErr
-}
