-- | Compile some AO code, or at least link it.
--
-- Ideally we should support some sort of caching behavior.
-- However, for the moment I just want the bare minimum logic
-- to compile for use with a REPL.
module Wikilon.Compile
    ( 
    ) where

--
-- I want generic linking and compiler functions for AO dictionaries.
-- However, it isn't very clear to me how to present this to the user
-- yet, other than as a normal set of messages and a set of utilities
-- to provide common compiler definitions regardless of backend.
--
-- Integrating the cache seems to be difficult, conceptually.
-- Support for a cache would be useful, even if the default is an empty cache that
-- is active only for the duration of one compilation effort. 
-- 

{-
data KeyValStore db m k v = KeyValStore 
    { kv_get :: k -> db -> m (Maybe v)
    , kv_put :: k -> v -> db -> m db
    }
data Cache db m k v = Cache (KVDB db m k v) 
-}






