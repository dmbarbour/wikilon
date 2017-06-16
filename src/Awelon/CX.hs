{-# LANGUAGE TypeFamilies, FlexibleContexts, ExistentialQuantification #-}

-- | Abstract Runtime Context for Awelon
--
-- Although Awelon is purely functional, it does leverage annotations
-- and accelerators to support performance or debugging. Rather than
-- a concrete implementation in IO, Awelon.CX represents the required 
-- evaluation contexts with an abstract typeclass-based API admitting
-- some purely functional implementations.
--
module Awelon.CX 
    ( Par(..), Async(..)
    , Lazy(..), Trace(..)
    , Stowage(..), Cache(..)
    , CX(..)
    , F, F_(..), eraseF
    , ByteString
    , module Awelon.Hash
    ) where

import Data.ByteString.Lazy (ByteString)
import Awelon.Hash

-- | A single-assignment, asynchronous future value.
--
-- This value is used to represent data that will become available in
-- the future (modulo divergence), usually from another thread. But a
-- future is also a monad. We may logically compose future results. 
data family F (m :: * -> *) :: * -> *

-- | A future that hides the value type, for use with Async wait.
data F_ m = forall a . F_ (F m a)

eraseF :: F m a -> F_ m
eraseF = F_

-- | Fork-join parallelism via composable futures.
class (Monad (F m), Monad m) => Par m where
    fork :: m a -> m (F m a)    -- ^ parallel future
    join :: F m a -> m a        -- ^ wait for result
 
-- | Concurrency with arrival-order race conditions.
--
-- Leveraging arrival-order indeterminism can improve latency and 
-- utilization within a computation. For Awelon, this is necessary
-- for acceleration of Kahn Process Networks. However, this does
-- leave the challenge of ensuring confluence to the accelerator.
-- 
class (Par m) => Async m where
    peek :: F m a -> m Bool     -- ^ will `join` be immediate?
    wait :: [F_ m] -> m ()      -- ^ wait for available result

-- note: I might also need to lift operations into the future.

-- | Note on Acceleration of Kahn Process Networks
--
-- Use of Async peek and wait can support KPN accelerators via the
-- arrival-order merge for outputs from child processes. This can
-- reduce processing latencies, and improve utilization of our CPUs.
-- Further, except where a message's destination depends on content,
-- it is feasible to route future message values, such that routing
-- is performed ahead of processing. (With a little filtering, we 
-- may even route some messages to the wrong destination.)
--
-- It is feasible to perform better by explicitly routing with shared
-- memory, for example by use of `ivar :: m (a -> m (), F m a)` for
-- single-assignment variables, to model channels with push behavior.
-- However, ivars align poorly to Awelon's semantics, serializability,
-- memoization, persistence, local reasoning. And they're also bad for
-- physical distribution, requiring shared memory and distributed GC. 
--
-- Fortunately, explicit routing is unlikely to be our bottleneck. So
-- the clean but inexpressive `Async` class is favored at this time.

-- | Lazy (Call by Need) Evaluation
--
-- A lazy computation will be performed only once, and only when some
-- demand exists, with the result being memoized. This could be useful
-- for some evaluation modes, JIT, lazy linking and evaluation of the
-- dependencies, CSE optimization, etc.. Some accelerators might also
-- use laziness.
class Lazy m where 
    lazy :: m a -> m a     -- ^ wrap computation for memoization

-- | Trace Message Debugging
--
-- Debug messages are a simple, convenient technique for debugging.
-- Awelon may support debug messages via (trace) annotations, and 
-- thus generate an additional sequence of messages. 
class Trace m where
    trace :: ByteString -> m ()

-- | Data Stowage
--
-- Awelon heavily utilizes a notion of 'secure hash resources', which
-- are essentially just binary resources that are globally identified
-- by secure hash. (Specifically, Awelon.Hash.) I call this "stowage".
-- Stowage doesn't quite need an effects model, modulo the potential 
-- that we might fail to load a resource.
-- 
-- Stowed resources are garbage collected. In general, the resource 
-- must be rooted for it to be protected, but we must ensure recently
-- stowed resources are guarded against GC until we have time to put
-- them into our databases.
--
-- This current API is conservative, everything stowed is guarded up
-- to the implicit transaction. If more precision is required, I may
-- need to integrate this with a checkpointing model or similar so we
-- can determine which resources are rooted.
-- 
class Stowage m where
    load :: Hash -> m (Maybe ByteString)
    stow :: ByteString -> m Hash

-- question: how do 

-- | Cached Computations
--
-- Caching plays an essential role in Awelon systems, the basis for
-- incremental computing. Caching may be implicit for toplevel words,
-- or explicit for compositional views of persistent data structures.
--
-- The latter requires the compositional property:
--
--    ∃F.∀X,+,Y. V(X + Y) == F(V(X), '+', V(Y))
--
-- That is, the view on a composite is a function of the views on the
-- individual components. With this, we can easily cache V(X) and V(Y)
-- so when we update Y and compute V(X+Y') we can reuse V(X). If this
-- caching is performed deeply, computing V(Y') might then reuse some
-- structure internally.
--
-- I favor a monotonic cache, which avoids the challenges surrounding
-- cache invalidation. The key must uniquely identify the computation, 
-- including the dependencies (e.g. by including a secure hash). The
-- runtime may heuristically delete cache entries to recover space.
--
-- To maximize reuse, cache keys should also be precise, such that
-- most irrelevant changes to a codebase don't affect the key. But
-- there is a heuristic tradeoff between precision and performance
-- of the key computation.
--
class Cache m where
    cacheGet :: ByteString -> m (Maybe ByteString)  -- ^ cacheGet key
    cachePut :: ByteString -> ByteString -> m ()    -- ^ cachePut key val

-- | An Awelon context simply aggregates the aformentioned features.
class   ( Monad m, Monad (F m)
        , Par m, Async m, Lazy m
        , Stowage m, Cache m, Trace m
        ) => CX m

-- Am I missing anything?
--
-- It isn't clear whether I should handle quotas here. Nor how.
--
-- The Haskell runtime cannot easily track unique references, so I
-- won't support in-place list manipulations. That said, I could
-- leverage Data.Sequence or persistent-vector in that role.
 

