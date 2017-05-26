-- | Abstract Runtime Context for Awelon
--
-- Awelon is purely functional. But use of annotations, accelerators,
-- and lazy linking may require some side-effects during evaluations.
-- I'd prefer to avoid implementing Awelon directly in IO, because
-- the possibility of having some pure evaluation contexts is nice
-- also.
--
module Awelon.CX 
    ( CX(..)
    , ByteString
    , module Awelon.Hash
    ) where

import Data.ByteString.Lazy (ByteString)
import Awelon.Hash

class (Monad m) => CX m where
    -- | Awelon supports a (trace) annotation that serves a similar
    -- role as Debug.Trace or fprintf(stderr,...).
    --
    --      [message](trace) => [message]       formally
    --       ... and adds "[message]" to our trace log
    --
    -- If a context doesn't support trace logs, this might just drop
    -- the input. But it's a very convenient feature for debugging.
    trace :: ByteString -> m ()
    trace _ = return ()

    -- | Awelon supports fork-join parallelism via (par) annotations.
    -- The parallel computation needs equal access to our context.
    -- In this case, the join is implicit to the monadic wrapper.
    fork :: m a -> m (m a)
    fork = return

    -- | Awelon is deterministic, but we may leverage race conditions
    -- to reduce latency for some hidden forms of non-determinism. An
    -- acceleration of Kahn Process Networks would rely on this.
    race :: m a -> m b -> m (Either (a, m b) (m a, b))
    race l r = do
        l' <- fork l
        b <- r
        return (Right (l', b))


    -- | Awelon assumes a context of binaries named by secure hashes,
    -- and uses secure hashes as pointers between binaries. Binaries 
    -- may be loaded from this context or stowed into it to move large
    -- resources out of working memory.
    --
    -- Note: Resources are generally subject to conservative GC, so 
    -- ensure all references can be scanned using a simple `hashDeps`
    -- search of the stowed value.
    load :: Hash -> m (Maybe ByteString)
    stow :: ByteString -> m Hash 

    -- | Awelon systems rely upon memoization and caching as the basis
    -- for incremental computations.
    --
    -- Operations on the cache resemble a simple key-value database, but
    -- the key must fully describe the computation, and the value may be
    -- deleted at any time to recover space and must be regenerable.
    -- 
    -- To maximize sharing and reuse of the cache, it's important that the
    -- key *precisely* describe a computation, e.g. versioning individual
    -- words instead of including a root secure hash of the dictionary.
    -- Achieving this precision is the primary challenge for effective use
    -- of the cache.
    cacheGet :: ByteString -> m (Maybe ByteString)
    cacheGet _key = return Nothing
    
    cachePut :: ByteString -> ByteString -> m ()
    cachePut _key _val = return ()

    

-- What's Missing?
--
-- So far I have basic parallelism, concurrency, debug output, 
-- dynamic linking, virtual memory via stowage, and caching.
--
-- It isn't clear whether I should handle quotas here, nor how.
--
-- I could maybe add a `once` or `lazy` operation to memoize a result.
-- But I don't have a strong use case for this yet.
--
-- I don't have an idea in-place list-as-vector manipulations.
-- Not yet, at least. Though I could just use persistent-vector
-- or Data.Sequence for that use case.
 

