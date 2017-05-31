{-# LANGUAGE TypeFamilies, ExistentialQuantification #-}

-- | Abstract Runtime Context for Awelon
--
-- Awelon is purely functional. But use of annotations, accelerators,
-- and lazy linking may require some side-effects during evaluations.
-- I'd prefer to avoid implementing Awelon directly in IO, because
-- the possibility of having some pure evaluation contexts is nice
-- also.
--
module Awelon.CX 
    ( Par(..)
    , Async(..), F_(..)
    , CX(..)
    , ByteString
    , module Awelon.Hash
    ) where

import Data.ByteString.Lazy (ByteString)
import Awelon.Hash

-- | A context that supports simple fork-join parallelism.
class Par m where
    data F m :: * -> *          -- ^ a future value
    fork :: m a -> m (F m a)    -- ^ parallel future
    join :: F m a -> m a        -- ^ wait for result
    
-- | A context that supports race conditions and concurrency.
--
-- In Awelon, use of Async can accelerate Kahn Process Networks.
-- It's okay so long as the end result of evaluation is the same.
class (Par m) => Async m where
    peek :: F m a -> m Bool     -- ^ is join immediate?
    wait :: [F_ m] -> m ()      -- ^ wait for available result

-- | Trivial wrapper to satisfy Haskell's existential types.
data F_ m = forall a . F_ (F m a)

-- | CX is logically pure, but exposes some "effects" for performance
-- or debugging as needed to support useful Awelon annotations. 
class (Monad m) => CX m where
    -- | Awelon permits a (trace) annotation to serve a similar
    -- role as Haskell's Debug.Trace or C's fprintf(stderr,...).
    --
    --      [message](trace) => [message]       
    --       ... and adds "[message]" to our trace log
    --
    -- If a context doesn't support trace logs, this may just drop
    -- the input. But it's a very convenient feature for debugging.
    trace :: ByteString -> m ()
    trace _ = return ()

    -- | For a sort of lazy memoized computation, we can request that
    -- a computation be performed only once and the result shared.
    once :: m a -> m (m a)
    once = return

    -- | Awelon assumes a context of binaries named by secure hashes,
    -- and uses secure hashes as pointers between binaries. Binaries 
    -- may be loaded from this context or stowed into it to move large
    -- resources out of working memory.
    --
    -- Note: Resources are generally subject to conservative GC, so 
    -- ensure all references can be scanned using a simple `hashDeps`
    -- search of the stowed value.
    load :: Hash -> m (Maybe ByteString)
    load _ = return Nothing

    stow :: ByteString -> m Hash 
    stow = return . hashL

    -- | Awelon systems rely upon memoization and caching as the basis
    -- for incremental computations.
    --
    -- Operations on the cache resemble a simple key-value database. But
    -- the key in this case must fully describe the value, and the value
    -- may be deleted at any time and require recomputation. Ideally, the
    -- key is also precise, so it excludes irrelevant differences between
    -- two codebases.
    cacheGet :: ByteString -> m (Maybe ByteString)
    cacheGet _key = return Nothing
    
    cachePut :: ByteString -> ByteString -> m ()
    cachePut _key _val = return ()
    

--
-- So far I have basic parallelism, debug output, dynamic linking,
-- virtual memory via stowage, and caching.
--
-- A missing feature is efficient concurrency, e.g. what I need to
-- optimize Kahn Process Networks. Some support for race conditions
-- or explicit asynchronous 'futures' may be necessary, and the
-- ability to wait on multiple futures.
--
-- I might wish to remodel concurrency around asynchronous futures.
--
-- It isn't clear whether I should handle quotas here, nor how.
--
-- I don't have an idea in-place list-as-vector manipulations.
-- Not yet, at least. Though I could just try persistent-vector
-- or Data.Sequence for this use case.
 

