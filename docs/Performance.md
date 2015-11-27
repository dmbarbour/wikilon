
# Wikilon Performance

I don't expect Wikilon to be blazing fast, but I would like to give it the best chance possible. I expect to at least achieve a reasonable level of performance: enough to develop a multiplayer game server, or a robot control system. Developing simple interactive fictions, turtle graphics, or visual novels should be within easy reach. Wikilon shouldn't be a *toy*, but for people to take it seriously we need serious performance.

I have multiple strategies to achieve performance, which should stack nicely:

1. Internal bytecode representation. Fast slicing of bytecode arrays. Quoted values. This internal bytecode shall have a rather trivial expansion back into Awelon bytecode, such that we don't lose any information.

2. Large-value *stowage*. A `{&stow}` annotation tells a runtime to tuck large values into a database, like ABC value resources. Structure sharing is implicit. This allows us to work with larger-than-memory values, e.g. values modeling entire databases. Structure sharing supports Wikilon's application model: we can expect to recompute values many times. Stowage might also find use for discretionary sealers and unsealers, just for structure sharing.

3. Parallelism. At the very least, we can support Haskell's par-seq parallelism by providing `{&par}` and `{&seq}`. We can use work-stealing techniques, with one thread per hardware processor. 

4. Simplification and partial-evaluation of bytecode. A lot of useful work can be performed directly on our bytecode representations or other intermediate representations.

5. ABCD-like accelerators. We can recognize specific subprograms and optimize for them. This includes optimized data representations. I especially want ABCD-like accelerations for: vector processing, affine vectors for mutability, working with binaries (and affine binaries), models of floating point computations.

6. Compilation of bytecode. JIT. AOT. On request by annotations. We can pursue a flexible approach here: an ad-hoc mix of interpreted and compiled code. Ideally, compiled code is easily cached and persisted (e.g. via stowage).

7. Caching and memoization. Pure computations of AO should be relatively easy to memoize and cache. Structure sharing for large values allows us to cache computations on large values. We can also represent active or lazy computations in a cache: ABC can easily represent its own complete continuation. Effective caching is essential to the [dictionary applications model](ApplicationModel.md).

For several months, I have been trying to do this in Haskell. However, Haskell's high-level data structures and GC are interfering. They're too difficult to integrate with low-level JIT code. My new plan is to implement the performance-critical subset of my runtime in C. I will leverage LLVM as a basis for JIT. 

For stowage, I will try [sophia key-value storage](http://sphia.org/), which is a better fit for my use case compared to LMDB. The API documentation is very poor, but some of the FFIs for it in other languages have better docs. And I can certainly get enough for my own use cases.

Wikilon will then integrate this runtime via Haskell's C FFI. Dictionaries will be modeled as as large ABC values. This seems like a useful idea for reflection anyway, or extending Wikilon via the dictionaries. I can model dictionaries, histories, reverse lookup indices, exponential decay models, etc..

I'll support named roots for persistence, but they'll all be for ABC values.

## Stowage for Large Values

        {&stow} :: (val * env) → (val(stowed) * env)

Stowage is useful for modeling larger-than-memory values in cases where only a small portion of the value needs our attention at a given moment. Databases, filesystems, streaming media, and more fit this criteria. The `{&stow}` annotation suggests to an ABC runtime that it serialize the value to cold storage. The value would be transparently loaded if later needed. Transitive substructural properties (affine, relevant) of stowed values will be tracked so the primitive copy and drop operators (`^` and `%`) need not invoke the loader. 

Structure sharing is a common feature for stowage. Computations producing the same stowed values will share the same space. In addition to potential space savings, structure sharing simplifies other potential features such as managing cached computations. 

Stowage of values is designed to closely model ABC value resources, i.e. where we use `{#resourceId'kf}` to name a bytecode resource. Here, `resourceId` is a secure hash of the bytecode, and the `'kf` suffix identifies our resource as a linear quoted value to permit lazy loading and forbid copy/drop. Stowed values could be directly implemented as ABC resources. However, `{&stow}` leaves the representation opaque and local to the runtime, which simplifies GC and allows faster hashing and a far more parsimonious resource identifier.

With a specialized DB model, I can keep the `kf` information (and several more flag bits) directly in my key value, e.g. a simple 64-bit integer.

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

Laziness involves construction of a suspended computation, a pending result, one that doesn't need to be completed immediately. It seems feasible to model this as a special case for value stowage and/or cached computations. My main resistance to lazy computations is that they hinder structure sharing.

## Linear Cached Computations

A very cache-friendly pattern for dictionary applications is where each word begins with another word. Conveniently, this also fits the *command pattern* common to many dictionary applications.

        @foo.v1 {%foo.v0}(a command)
        @foo.v2 {%foo.v1}(another command)
        @foo.v3 {%foo.v2}(yet another command)

A very simple technique in context is to cache evaluation results for each step, and allow them to decay normally (e.g. via exponential decay models). 

## Quotas and Caching

As a development platform, Wikilon will host a lot of long-running and non-terminating code. Ideally, ABC programs should always terminate, but divergent code is possible because it's buggy or malign. And long-running code may exist because it's non-terminating or simply really expensive. I've been wondering how to best address this concern in context of:

* reusable cached computations
* controlling efforts expended

Caching an 'ongoing' computation with a PVar could be implicit for computations that fail to complete within the quota, enabling background computations or allowing users to poke at a computation until it finishes (202 Accepted). If lazy computations are supported, it is similarly feasible to place those in a PVar if not computed before we cache.

## ABCD-like Accelerators

I can feasibly gain a lot of performance by recognizing subprograms and replacing them with pre-compiled, optimized implementations (and dedicated opcodes). With the right accelerators, it is feasible to have high-performance interpretation.

The difficulty is finding and developing "the right accelerators". My intuition is that these will include:

* vectors, matrices, collections-oriented processing
* mutable vectors via the 'affine' type
* floating point models and operations
* supporting heterogeneous computation (GPGPU, FPGA).

Vectors could be a highly optimized 'view' for lists. Affine vectors might replace the terminating unit with `[]k`. For floating point operations, we will need to *model* the floating point numbers and operations, likely with their many flaws and foibles, such that we can replace the model with hardware floating point numbers. Floating point support is essential if we want to effectively leverage GPGPUs. Effective support for GPGPU computing could greatly improve the utility of ABC, e.g. enabling scientific computation.

Accelerators are wasted in cases where a simple compilation could do the job. Accelerators are best used in cases that JITs alone would have difficulty recognizing or optimizing, especially cases that imply use of specialized datatypes. Applying accelerators before compilation should allow a simple compiler to do a better job.

## Compilation

Awelon Bytecode is designed assuming compilation. Without it, ABC has no chance of competing with more machine-oriented bytecodes like CLR or JVM. The amount of allocation performed by a single ABC evaluation is significant. Compiling helps most in case of loopy and mathy code: the Z-combinator becomes a simple jump, internal data plumbing can be shifted to register allocations, computations can be optimized. 

With accelerators, it becomes feasible to recognize subprograms that can be easily compiled for SIMD or GPGPUs. With this, AO and ABC might achieve world-class performance without sacrificing its pure, trivial semantics.

Targeting LLVM has worked for many people. Seems worth trying.

