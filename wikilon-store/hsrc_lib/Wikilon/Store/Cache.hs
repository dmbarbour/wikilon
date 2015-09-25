
-- | This module provides a simple cache implementation. Mostly, just a
-- trie under a PVar with (1) a simple exponential decay model, (2) a
-- basic implementation to hide parallel background computations.
--
-- Managing parallel background computations is perhaps the biggest
-- issue. To any given computation using the cache, I want to present
-- an image that the cached value has already been computed, even if
-- it must wait. This might be achieved by MVars or similar.
module Wikilon.Store.Cache
    (
    ) where

-- What do I want to cache?
--
-- COMPILATION:
--  intermediate (value,compiler) pair, if non-trivial to recompute
--  'compiled' function (from applying compiler to value), iff non-trivial to compute
--  optimized functions (as values)?
--  
--  type information for generated function
--  indicators for obvious compilation or type errors
--  function as compiled to JavaScript?
--
-- RESOURCES:
--  computed texts (e.g. css, HTML front page)
--  computed images (e.g. icons)
--  ad-hoc binaries
--
