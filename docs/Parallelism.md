
# Parallel Runtime

It turns out that use of fork-join process functions as a basis for parallelism is a source of much pain downstream, when we might later attempt to serialize the computation containing the futures, or 'move' values to another context. 

* Par/seq parallelism preserves locality so doesn't complicate serialization
* Later, model single-assignment futures/promises as monad then accelerate it

## Context Local Parallelism

Multi-context parallelism simplifies GC but complicates space quotas, local sharing for futures, etc.. So let's go back to single-context parallelism, but carry some lessons back. In particular, reverse older ideas: free-lists within a thread, occasional compacting collection.

The main difficulty with multiple threads per context is:

* need thread-local allocation regions
* need larger duty cycles between compacting GC
* need extra free memory per migrant thread 
* failed allocation must be resumable error

Our thread-local allocator can be modeled passing an extra variable around. We can use thread local GC (free lists) to recover memory and extend our duty cycle. Main thread will perform compaction as a major GC as needed. Rather than 'protect' allocations, favor a fail-safe approach: build 'loose' values then update things only when our state is easily resumable. Upon allocation failure, then, our worker thread - perhaps even the main thread - can just long-jump out of there. 

* workers are active only during eval steps
* free-list GC locally within each thread
* compacting GC full context by API thread
* reader-writer lock to guard compaction
* long-jump and drop work in case of failures
* only write valid states into context
 * nothing we can't easily resume
* context tracks *available work*
 * not individual computations

## Shared Local Objects

Shared context-local objects seem a lot more promising if most 'copies' are local to our environment. We might need to handle it as a special for the *rare* case of copying data between environments, but that's quite feasible. Actually, if we restrict *shared local objects* to *binaries* 

## A runAsync Monad Accelerator? (low priority)

Single-assignment futures are useful for building channels between arbitrary subprograms. An interesting possibility is to create a monad for single-assignment futures, then provide an accelerated `runAsync` implementation that uses more direct communication methods. Getting a static typechecker to the point where it could call an acceleration 'safe' would be very useful.

This is low priority, especially compared to accelerating vector/matrix math and graphics processing.

