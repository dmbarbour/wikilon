
# Wikilon Performance

I don't expect Wikilon to be blazing fast, not anytime soon. However, I expect to achieve a reasonable degree of performance. Developing simple text games, turtle graphics, or visual novels should be within easy reach. To achieve this, Wikilon will compile ABC to a more efficient internal representation that can trivially be converted back to ABC.

* Compact: use bytestrings in memory rather than expanded lists.
* Slicing: no parsing to end of block or text, no escaping text.
* Embedding: compute constant values only once within a loop.
* Stowage: structure sharing and lazy loading of large values.
* Accelerators: common sequences of ABC recognized and optimized.
* Simplifiers: clean up unnecessary data plumbing, partial evaluations.
* Parallelism: `{&par}` and `{&seq}` annotations, strategies.

Additionally, I have several ideas to improve performance further:

* lazy or parallel computations that survive within the cache
* accelerated data structures, especially vectors 
* annotation to copy affine vector into 'mutable' vector
* compile loopy subprograms to machine code for fast evaluation
* compile subprograms written in constrained manner to GPGPU

However, some of these require a lot more work to implement or use effectively.

## Stowage for Large Values

        {&stow} :: (val * env) → (val(stowed) * env)

Stowage is useful for modeling larger-than-memory values in cases where only a small portion of the value needs our attention at a given moment. Databases, filesystems, streaming media, and more fit this criteria. The `{&stow}` annotation suggests to an ABC runtime that it serialize the value to cold storage. The value would be transparently loaded if later needed. Transitive substructural properties (affine, relevant) of stowed values will be tracked so the primitive copy and drop operators (`^` and `%`) need not invoke the loader. 

Structure sharing is a common feature for stowage. Computations producing the same stowed values will share the same space. In addition to potential space savings, structure sharing simplifies other potential features such as managing cached computations. 

Stowage of values is designed to closely model ABC value resources, i.e. where we use `{#resourceId'kf}` to name a bytecode resource. Here, `resourceId` is a secure hash of the bytecode, and the `'kf` suffix identifies our resource as a linear quoted value to permit lazy loading and forbid copy/drop. Stowed values could be directly implemented as ABC resources. However, `{&stow}` leaves the representation opaque and local to the runtime, which simplifies GC and provides more room for optimizations.

A suggestion for stowage may be silently rejected, e.g. because a value is not significantly larger than its stowage overhead. This helps developers a bit because it leads to larger 'chunks' of data corresponding to a couple layers of tree nodes that can be loaded together or compressed together.

When serialized for export, stowed values will re-inject the `{&stow}` annotations.

## Parallelism 

Awelon Bytecode has a lot of latent parallelism. So the problem becomes teasing it out at an appropriate granularity to keep the overheads low and the processors busy. We can utilize a simple variant of [parallel Haskell's](https://hackage.haskell.org/package/parallel-3.2.0.6/docs/Control-Parallel.html) `par` and `seq` techniques.

        {&par} :: (block * env) → (block with 'par' attribute * env)
        USAGE: (prep args)[(expensive)]{&par}$(move pending result)

        {&seq} :: (lazy or pending value * env) → (value * env)

Our runtime would support opaque 'pending' results. Trying to operate on the pending result may have us waiting. But we're at least free to move it around, e.g. tuck it into a data structure and spark off a few more parallel computations. With just this much ABC and AO gain access to all of Haskell's well developed [strategies](https://hackage.haskell.org/package/parallel-3.2.0.6/docs/Control-Parallel-Strategies.html) for purely functional parallelism. It's also trivial to implement!

It is also feasible to support massively parallel GPU computations. Assume a subprogram is constructed from a constrained set of recognizable functions and data structures that we know how to easily represent on the GPU. Annotate a block containing this subprogram for evaluation on the GPU. The runtime compiles the block to CUDA or OpenCL or a shader language. When we later apply the function, it will compute on the GPU. Other forms of heterogeneous computing, e.g. FPGAs, will frequently follow a similar pattern. Haskell has similar precedent with the [accelerate](https://hackage.haskell.org/package/accelerate) package.

In context of a dictionary application, every form of parallelism could kick in for every evaluation. This includes partial evaluations and cache maintenance. I imagine a mature, active, and popular AO dictionary full of apps could productively leverage as many CPUs, GPUs, and other computing devices as you're willing to throw at them.

## Lazy Computation

Laziness involves construction of a suspended computation, a pending result, that doesn't need to be completed immediately. In Haskell, laziness has some semantic aspects in that we can have lazy undefined or divergent computations. However, in ABC, laziness would be expressed by annotation `{&lazy}`. Annotations *must not* affect the observable behavior of the program. So laziness must be a performance-only consideration.

Wikilon interprets bytecode. We are not pre-computing our substructural type attributes. Hence, we must not copy or drop our lazy values before fully computing them. A consequence is that we don't need stateful thunks at the level of independent computations. However, at the higher *dictionary application* layer with partial evaluation and cached computations, we may still reuse lazy thunks. Lazy, stateful thunks might give us some nice properties in this context, effectively giving developers access to construct computations in cache. 

Are there better approaches to cached computations? Memoization seems a promising technique, but has the difficulty of uniquely identifying a computation. Even more so if we begin to inject stateful thunks. 

I may need to experiment with laziness to see whether it is worth using in Wikilon.

## Linear Cached Computations

A very cache-friendly pattern for dictionary applications is where each word begins with another word. Conveniently, this also fits the *command pattern* common to many dictionary applications.

        @foo.v1 {%foo.v0}(a command)
        @foo.v2 {%foo.v1}(another command)
        @foo.v3 {%foo.v2}(yet another command)

A very simple technique in context is to cache evaluation results relative to individual words in the dictionary.

## Quotas and Caching

As a development platform, Wikilon will host a lot of long-running and non-terminating code. Ideally, ABC programs should always terminate, but divergent code is possible because it's buggy or malign. And long-running code may exist because it's non-terminating or simply really expensive. I've been wondering how to best address this concern in context of:

* reusable cached computations
* controlling efforts expended

Laziness could possibly help by allowing fine-grained reuse of a computation that isn't generated until after it is cached. However, laziness isn't ideal for further simplifications and optimizations.