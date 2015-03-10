
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
-- etc.. Also, it may be worth supporting inexact arithmetic with double
-- precision floating point numbers, e.g. via {&float} annotation.
--
-- How to represent values is a good question. I'm really not certain 
-- what the best approach is for a good balance of performance and other
-- nice features. For now, I'm just going for something that works, and
-- I'll see how it can be tuned later.
--
-- But one feature I want is good support for very large structures.
-- If a map contains a million elements, it would be nice to load parts
-- as needed to answer queries and similar. My idea here is to basically
-- use the {#resourceId'} mechanism for values, albeit anonymously with
-- VCache.
-- 
module Wikilon.Value
    ( Value(..)
    ) where

-- import Data.Ratio

data Value abc
    = Number {-# UNPACK #-} !Rational
    deriving (Eq)


