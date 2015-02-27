
-- | Values in Awelon project have a few basic forms:
--
--    (a * b) -- pairs    ~Haskell (a,b)
--    (a + b) -- sums     ~Haskell (Either a b)
--    1       -- unit     ~Haskell ()
--    0       -- void     ~Haskell EmptyDataDecls
--    N       -- numbers  ~Haskell Rational
--    [a→b]   -- blocks   ~Haskell functions or arrows
--    foo:a   -- sealed   ~Haskell newtype
--
-- There are others - substructural types, cryptographic sealed values,
-- etc.. And it may be worth supporting inexact arithmetic with doubles
-- via the {&float} annotation.
-- 
-- In Wikilon, I'm interested in the possibility of lazily loading large
-- values from disk as-needed, using VCache VRefs. I'm also interested
-- in keeping binaries and large texts and other data structures compact.
-- 
module Wikilon.Value
    (
    ) where
