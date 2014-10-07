
-- | Values in Awelon project are of a few basic forms:
--
--    (a * b) -- pairs    ~Haskell (a,b)
--    (a + b) -- sums     ~Haskell (Either a b)
--    1       -- unit     ~Haskell ()
--    0       -- void     ~Haskell EmptyDataDecls
--    N       -- numbers  ~Haskell Rational
--    [a→b]   -- blocks   ~Haskell functions or arrows
--
-- Blocks may additionally be marked affine or relevant (or both),
-- thereby supporting substructural type based reasoning, though 
-- those attributes will frequently be dynamically enforced.
--
-- Awelon project is designed to be statically typechecked, but can
-- enforce types dynamically. In the normal case, you cannot ask if
-- a value is a pair or a sum. You have to know in advance. In some
-- cases, introspection may be available as a capability.
--
-- Via capabilities, developers can access a few more value concepts.
--
-- Sealed values, cryptographic or discretionary, resist observation
-- until the unsealer is applied. This supports rights amplification,
-- serves a replacement for `newtype` declarations, and also as a tag
-- to guide optimizers to use specialized constructors. 
--
-- Additionally, we can apply new substructural concepts, such as 
-- abstract location information or expiration of values. Long term,
-- this will be very important for RDP.
--
-- Reactive models such as RDP typically will use a different 
-- representation of values to support incremental updates.
--
-- 
module Wikilon.Value
    (
    ) where
