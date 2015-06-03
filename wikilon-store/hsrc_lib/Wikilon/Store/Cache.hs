
-- | Wikilon will cache a lot of computations over dictionaries.
--
-- In a general sense, each cache entry has a set of dependencies
-- on variables or other cache entries. When those variables change,
-- we must delete the cache entry.
--
-- This idea can also apply recursively. Conceptually, a variable may
-- be a cached computation, thus invalidation of the variable may 
-- require cascading invalidation of dependent variables.
--
-- A cache is thus very similar to the model of a dictionary with a
-- reverse lookup index. 
--
-- The other major feature is that our cache must gradually invalidate
-- content that is old and unused. I'm not sure how to combine these
-- two forces at the moment.
module Wikilon.Store.Cache
    (
    ) where
