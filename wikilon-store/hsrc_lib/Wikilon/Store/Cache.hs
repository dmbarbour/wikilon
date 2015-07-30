
-- | This module supports per-word caching based on the 'version hash'
-- of words within a dictionary. Cached values should be valid across
-- multiple versions and forks of a dictionary.
--
-- While there is no need to invalidate this cache, we may need to 
-- gradually cull the cache to keep it from growing too large. I'm
-- not sure how I want to go about this at the current time, except
-- maybe to use an exponential decay model.
--
-- Fortunately, I don't need to be especially careful with the cache.
-- If the versions don't match, I can simply replace the cache with 
-- an empty.
--
-- Per-word caching isn't sufficient for some tasks, e.g. if I want
-- to maintain a list of words with type-errors then I'll need something
-- else. I'll need to think about this later. This particular module
-- will focus on per-word caching.
module Wikilon.Store.Cache
    (
    ) where

-- What do I want to cache?
--
-- COMPILATION:
--  intermediate (value,compiler) pair
--  'compiled' function (from applying compiler to value)
--  type information for generated function
--  indicators for obvious compilation or type errors
--  function as compiled to JavaScript?
--
-- RESOURCES:
--  computed texts (e.g. css, HTML front page)
--  computed images (e.g. icons)
--  ad-hoc binaries
--
