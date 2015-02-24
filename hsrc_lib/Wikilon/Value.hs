
-- | Values in Awelon project are of a few basic forms:
--
--    (a * b) -- pairs    ~Haskell (a,b)
--    (a + b) -- sums     ~Haskell (Either a b)
--    1       -- unit     ~Haskell ()
--    0       -- void     ~Haskell EmptyDataDecls
--    N       -- numbers  ~Haskell Rational
--    [a→b]   -- blocks   ~Haskell functions or arrows
--
-- Plus there is affine and relevant substructural types, discretionary
-- sealed values, cryptographically sealed values (eventually), etc. And
-- I'd like to experiment with new substructure, such as where or when a
-- value is logically located, or when a block logically expires.
--
-- It seems feasible to optimize interpretation by recognizing common
-- data structures - e.g. text, bytestrings, vectors and matrices - 
-- then optimizing their representation. 
--
-- For very large values, such as maps with a million elements, I'm 
-- interested in the possibility of using data pointers to avoid
-- loading them all at once into memory. VCache's VRefs seem viable.
--
-- Anyhow, regardless of the value type, the value in question should
-- be serializable into ABC that, when executed, regenerates the value.
--
-- I may need a different representation for values in an RDP context.
-- 
module Wikilon.Value
    (
    ) where
