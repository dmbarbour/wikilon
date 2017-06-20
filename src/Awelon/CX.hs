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
    ( Par(..), Async(..), race'
    , Lazy(..), Trace(..)
    , Stowage(..), load
    , Cache(..)
    , CX(..)
    , ByteString
    , module Awelon.Hash
    ) where

import Control.Applicative
import Data.ByteString.Lazy (ByteString)
import Awelon.Hash

-- | Fork-join parallelism via composable futures.
class (Monad (F m), Monad m) => Par m where
    data F m :: * -> *          -- ^ a future value
    fork :: m a -> m (F m a)    -- ^ parallel future
    join :: F m a -> m a        -- ^ wait for result

-- Might need some means to extend monadic operations.
-- Something like the following: 
--
--  andThen :: F m a -> (a -> m b) -> m (F m b) -- ^ extend parallel task
--   might be able to provide a default impl with fork and join
--
-- Or perhaps:
--
--  liftF :: m a -> F m a
--
-- Not really sure what's best here, nor how much expressiveness
-- we need. 
 
-- | Concurrency via arrival-order indeterminism.
--
-- The `race` operation enables clients to process results based on
-- the order in which they become available. This introduces a form
-- of non-determinism, which is generally impure. But indeterminism
-- can be leveraged to accelerate confluent computations. So this is
-- a feature to use, albeit with some care.
class (Par m) => Async m where
    race  :: F m a -> F m b -> F m (Either a b)

-- | Data-preserving race, for linearity.
race' :: Async m => F m a -> F m b -> F m (Either (a, F m b) (F m a, b))
race' fa fb = k <$> race fa fb where
    k (Left a) = Left (a,fb)
    k (Right b) = Right (fa,b)

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
-- by secure hash, specifically Awelon.Hash.
--
-- This API for stowage is simple. Load errors require raising an
-- exception within the monad. The monad should ensure that stowed
-- resources are not garbage collected before we can assume they
-- have been rooted, such as a transaction commit or checkpoint.
--
-- This API does support potential for asynchronous load in case
-- we're using some sort of distributed database, and to support
-- reporting more than one missing resource at a time.
-- 
class Stowage m where
    stow :: ByteString -> m Hash
    load_async :: Hash -> m (m ByteString)

load :: (Stowage m, Monad m) => Hash -> m ByteString
load h = load_async h >>= id

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
 

