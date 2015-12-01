
# Wikilon Performance

I don't expect Wikilon to be blazing fast, but I would like to give it the best chance possible. I expect to at least achieve a reasonable level of performance: enough to develop a multiplayer game server, or a robot control system. Developing simple interactive fictions, turtle graphics, or visual novels should be within easy reach. Wikilon shouldn't be a *toy*, but for people to take it seriously we need serious performance.

I have multiple strategies to achieve performance, which should stack nicely:

1. Internal bytecode representation. Quoted values. Text and static value areas. This internal bytecode will have a trivial expansion back into Awelon bytecode, such that we don't need to store our program any other way. It should also have a trivial indexing mechanism, so we can track where we halt in case of failure.

2. Large-value *stowage*. A `{&stow}` annotation tells a runtime to tuck large values into a database, like ABC value resources. Structure sharing is implicit. This allows us to work with larger-than-memory values, e.g. values modeling entire databases. Structure sharing supports Wikilon's application model: we can expect to recompute values many times. Stowage might also find use for discretionary sealers and unsealers, just for structure sharing.

3. Parallelism. Within a computation, we can support Haskell's par-seq parallelism by providing `{&par}` and `{&seq}`. We can use work-stealing techniques, with one thread per hardware processor. In addition, we'll have multiple threads for propagating updates through dictionaries, or when working with multiple users.

4. Memory and GC per-thread. Dictionary application threads are truly *pure*, so there is very little sharing between them to worry about. It's easy to treat threads as processes with whole-arena GC. This gives us very predictable GC properties. This greatly augments stowage performance: we can perform it latently as part of GC, which saves us from stowing transient values. Further, we can heuristically stow values when memory runs low, instead of increasing the memory arena. Our arenas can be reasonably large (e.g. 10-20MB, plus binaries and texts, perhaps with multiple layers: nursery and generational). 

5. Simplification and partial-evaluation of bytecode. A lot of useful work can be performed directly on our bytecode representations or other intermediate representations.

6. Efficient deep copy. It is feasible to delay copy of deep structure by introducing coarse-grained memory-local reference counts when copying a large data structure. It is also feasible to delay analysis of whether a value is copyable (e.g. marking both copies as assumed copyable, and marking only one of them for validation). Analysis of 'copyable' on a proposed stowage target can be delayed until after stowage. Either way, we can limit the amount of work performed by a copy operator. Special values (blocks, vectors, large texts) might automatically use a reference count. Stowed values track their own copyability properties. (Other ideas: consider tracking substructural properties in pointer bits?)

7. ABCD-like accelerators. We can recognize specific subprograms and optimize for them. This includes optimized data representations. I especially want ABCD-like accelerations for: vector processing, working with binaries, models of floating point computations. Vectors, binaries, etc. can include their own reference counts, such that we can easily test whether a vector is unique when deciding copy-on-update vs. update-in-place. Affine vectors thus become the default behavior, with O(1) updates. Splits and rejoins on affine vectors can also be made very efficient, i.e. if we 'split' an affine vector, we get two affine vectors. If we later 'rejoin' affine vectors having adjacent storage, we can recombine them into a larger affine vector without copies. This allows very efficient fork-join behaviors.

8. Compilation of bytecode - AOT, JIT, on request by `{&compile}` annotations on a block. Compiled code might include a model of registers and jumps for use within loopy code. We should support an ad-hoc mix of interpreted and compiled code. Ideally, compiled code is easily cached and persisted (e.g. via stowage). Interpretation will be implemented first, of course, but it should be trivial to introduce compiled code.

9. Static or gateway typing. I can push type analysis to the edges of the program, make assertions about which values are copyable, which are numbers, force values to their target data structures. Then I can use an optimized interpreter or compiled region that assumes type safety. This is especially valuable for loopy code.

10. Caching and memoization. Pure computations of AO should be relatively easy to memoize and cache. Structure sharing simplifies caching for large values. We can represent active or lazy computations in a cache: ABC can easily represent its own complete continuation. Effective caching is essential to the [dictionary applications model](ApplicationModel.md).

Haskell's high level data structures are difficult to integrate with Haskell's low-level JIT code. Haskell's GC is nice, but it requires expensive interactions (ephemeron tables, etc.) with stowage and VCache.

My new plan is:

* implement performance-critical kernel in C
* leverage LLVM (or Clang) as a basis for JIT
* integrate LMDB as the basis for stowage
* support lightweight threads via `{&par}`
* limit number of passive computation threads
* dictionaries modeled as ABC values with stowage
* support 'named root' values, similar to VCache

I'm feeling very good about having this viable path forward after months (arguably, over a year, though most of that was refining the Wikilon application models) fighting with Haskell for performance. 

With high performance evaluation and caching, Wikilon becomes a super-spreadsheet of sorts. Especially if we later support GPU computations for rendering and physics, or remote distributed comptuations.

## Stowage for Large Values

        {&stow} :: (val * env) → (val(stowed) * env)

Stowage is useful for modeling larger-than-memory values in cases where only a small portion of the value needs our attention at a given moment. Databases, filesystems, streaming media, and more fit this criteria. The `{&stow}` annotation suggests to an ABC runtime that it serialize the value to cold storage. The value would be transparently loaded if later needed. Transitive substructural properties (affine, relevant) of stowed values will be tracked so the primitive copy and drop operators (`^` and `%`) need not invoke the loader. 

Structure sharing is a common feature for stowage. Computations producing the same stowed values will share the same space. In addition to potential space savings, structure sharing simplifies other potential features such as managing cached computations. 

Stowage of values is designed to closely model ABC value resources, i.e. where we use `{#resourceId'kf}` to name a bytecode resource. For ABC resources, `resourceId` is a secure hash of the bytecode, and the `'kf` suffix identifies our resource as a linear quoted value to permit lazy loading and forbid copy/drop. Stowed values are the same (more or less) but with parsimonious names (64-bit integers, specific to each runtime) and reference-counting GC.

A suggestion for stowage may be silently rejected, e.g. if a value is not significantly larger than its stowage overhead. Conversely, we may stow values even without annotated recommendations: when per-thread memory arenas run short on memory, we might heuristically select values from the higher generations for stowage.

### Stowage Implementation

I'll continue to use LMDB. It's effective, and I have experience with it. I believe some of the complications I faced early can be mitigated: 

* Instead of allocating addresses immediately, stow only upon full-GC passes.
* We only stow values that are held in the outer memory arenas.
* We write from an evaluation thread, grabbing exclusive control of LMDB.

Memory arenas give us an effective basis for *batching*. They effectively provide an upper limit for batch size. But we can model batches large enough for efficient writes. Structure sharing is computed by the writer. Addresses are never communicated with other threads until after storage. Transient values aren't stored: a value effectively needs to prove it's long-lived before we bother stowing it. So, stowing the 'head' of a database during streaming updates isn't a problem.

With this design, there is no need for a background writer. We still need incremental GC, but that's easy to do. 

Other thoughts: 

* Optimize representations upon stowage (e.g. recognize possible vectors). 
* Use Zstandard compression instead of Snappy. It's fast and effective.
* Modulo special cases, only stow values large enough for compression. 

## Parallelism 

Awelon Bytecode has a lot of latent parallelism. So the problem becomes teasing it out at an appropriate granularity to keep the overheads low and the processors busy. We can utilize a simple variant of [parallel Haskell's](https://hackage.haskell.org/package/parallel-3.2.0.6/docs/Control-Parallel.html) `par` and `seq` techniques.

        {&par} :: (block * env) → (block with 'par' attribute * env)
        USAGE: (prep args)[(expensive)]{&par}$(move pending result)

        {&seq} :: (lazy or pending value * env) → (value * env)

I'm going to assume we have a *memory arena per thread*, with no sharing. Splitting an arena immediately upon sparking evaluation seems inefficient, too eager. So `{&par}` will not *actually* fork a thread until it can gain its own memory arena. My idea for this is as follows:

* represent and note the spark, but continue evaluating non-par route. 
* construction of separate `{&par}` arena waits until we perform a gen-GC.
* might have limited pool for waiting sparks: wait for more than one GC.
* might sequence the evaluation before we ever spark it. 
* limited number of parallel computations and spark arenas.

It might be useful to track granularity in the `{&par}` request: e.g. the developer can specify whether a computation is likely light or heavy, perhaps via another annotation on the block. We could provide memory arenas of appropriate sizes.

We can operate on pending results in simple ways. We might permit applying additional `{&par}` blocks, for example. We can move pending results easily. We cannot generally *delete* or *copy* a pending result, but we might be able to mark where the assumption was made and compute whether it was a valid assumption later. In this sense, an evaluation might generally include a list of unverified assumptions, e.g. that a particular pending result was delete-able.

This is sufficient to support parallel computation strategies, but it's difficult for an evaluation to actually gain a parallel thread unless the result remains 'pending' until a gen-GC step. Consequently, parallel evaluation only triggers for stuff that we don't actually need for a while. But when it does trigger, we might manage multiple sparks at once. So, techniques like fork-join can be effective, with the first thread possibly processing several sparks before a few more trigger.

### Other Parallelism?

With accelerators, it is feasible to support SIMD and massively parallel GPU computations. Assume a subprogram is constructed from a constrained set of recognizable functions and data structures that we know how to easily represent on the GPU. Annotate a block containing this subprogram for evaluation on the GPU. The runtime compiles the block to CUDA or OpenCL or a shader language. When we later apply the function, it will compute on the GPU. Other forms of heterogeneous computing, e.g. FPGAs, will frequently follow a similar pattern. Haskell has similar precedent with the [accelerate](https://hackage.haskell.org/package/accelerate) package.

In context of a dictionary application, every form of parallelism could kick in for every evaluation. This includes partial evaluations and cache maintenance. I imagine a mature, active, and popular AO dictionary full of apps could productively leverage as many CPUs, GPUs, and other computing devices as you're willing to throw at them.

## Lazy Computation?

Laziness involves construction of a suspended computation, a pending result, one that doesn't need to be completed immediately. It seems feasible to model this as a special case for value stowage and/or cached computations. My main resistance to lazy computations is that they hinder structure sharing. But we could possibly stow a lazy continuation when we get started, then cache the computed result. 

Laziness+Parallelism might indicate 'background' computations that Wikilon should try to run for a while.

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

Vectors could be a highly optimized 'view' for lists. For floating point operations, we will need to *model* the floating point numbers and operations, likely with their many flaws and foibles, such that we can replace the model with hardware floating point numbers. Floating point support is essential if we want to effectively leverage GPGPUs. Effective support for GPGPU computing could greatly improve the utility of ABC, e.g. enabling scientific computation.

Accelerators are wasted in cases where a simple compilation could do the job. Accelerators are best used in cases that JITs alone would have difficulty recognizing or optimizing, especially cases that imply use of specialized datatypes. Applying accelerators before compilation should allow a simple compiler to do a better job.

## Compilation

Awelon Bytecode is designed assuming compilation. Without it, ABC has no chance of competing with more machine-oriented bytecodes like CLR or JVM. The amount of allocation performed by a single ABC evaluation is significant. Compiling helps most in case of loopy and mathy code: the Z-combinator becomes a simple jump, internal data plumbing can be shifted to register allocations, computations can be optimized. 

With accelerators, it becomes feasible to recognize subprograms that can be easily compiled for SIMD or GPGPUs. With this, AO and ABC might achieve world-class performance without sacrificing its pure, trivial semantics.

Targeting LLVM has worked for many people. Seems worth trying. Haskell has decent support for LLVM.

If I have a hand-rolled interpreter, I'll probably want to access those functions from LLVM. A potential techniq

## Optimized Sums

With sum types, we have some `LRLLRLR` sequence to reach them. I wonder if we can reduce this to a bitfield, maybe even track a few sum flags in value pointers? With even just `L` and `R` in pointers, we could model lists without an extra node. To support *trees* and a fast `Z` operator, we might additionally want `RL` and `RRL`.

We might similarly track affine and relevance properties in value pointers? I'm not sure that will work out. It might simplify copy and drop, or it might interfere with parallelism.


# ABC Runtime Design

The following is a sketch of the runtime I'm designing. 

## Environment, Contexts, Memory Layout

A toplevel *environment* will track: 

* LMDB integration for stowage
* A pool of worker threads for sparks.
* A semaphore for worker threads
* A set of active contexts

This allows multiple contexts to interact with the same LMDB environment and share labor for parallel sparks.

A *context* additionally will track:

* A memory region for a computation
* A set of thread contexts and nurseries
* callbacks for handling unrecognized tokens
* callbacks for handling erroneous inputs
* A set of waiting parallel threads
* A set of objects proposed for stowage

We can create a context per external computation, or we can use a single context to run multiple computations. The former provides greater control over memory consumption. The latter offers greater access to sharing for cached stowed values. 

I'll probably want to use `mmap` with 

## Addressing, Pointer Layout, and Size Limits

I'll be favoring 32-bit contexts with a maximum size of 4GB.

Reasoning:

* 4GB of memory is a lot for any one Wikilon computation.
* LMDB stowage mitigates need for large values in memory.
* We can optimize for large external binaries and texts.
* We can optimize for large external bytecode resources.
* Tight addressing improves memory locality and caching.
* Can model 'remote' computations via multiple contexts.

Memory will be addressed and aligned on 64-bit 'cells'. The lower 3 bits in each address will be leveraged to optimize common representations. It is most important that we optimize for: pairs, unit, lists, trees, booleans, and small numbers. 

The encoding I'll be trying:

* xy0 - small integers 
* 001 - tagged objects
* 101 - raw product or unit
* 111 - product or unit inL
* 011 - product or unit inR

Small integers encode plus or minus a billion: nine solid digits with a little play at the edges. 

A product is represented by addressing a single cell, just a pair of values. Address 0 represents 'unit'. We optimize a single level of sum types (inL vs. inR) for products and units. This allows us to directly represent lists, booleans, and simple trees (e.g. node vs. leaf) without an extra cell for the sum type.

Tagged objects are used for everything else: arbitrary sums, blocks, sealed values, stowed values, pending parallel computations, compact texts, vectors and matrices, etc.. Tagged objects may be more than one cell in size, depending on their type.

I haven't hammered out the details on 'external' resources, but they at least seem feasible, perhaps as a specialization on stowage.

## Memory Management

Nurseries are collected by a semi-space algorithm: 

* a thread grabs an *empty* nursery
* all living data is copied into it
* then we switch nurseries

Our context ensures enough empty nurseries that we never need to wait. And nurseries are small enough (e.g. 256kB or 2MB) that the copy runs very fast. In addition, we'll keep track of the highest address for data from one or two generations ago. Content that would survive a third collection (or a second one, if we have memory pressure) is instead shipped to the full heap.

Our full heap uses a combination of ownership and reference counting, with a conventional free list. Each thread keeps its own free list for the main heap to minimize synchronization when allocating and deleting content. The context's free list requires synchronization (via mutex).

A cell is "owned" by whichever thread has a reference to it, favoring linearity by default. When we make deep copies of large structures, we inject reference count objects (e.g. wrapping every 60-120 items) to avoid performing a full copy immediately. This mostly saves work for future copies and potential copy-delete patterns. The costs of the reference count are amortized.

*Aside:* I've considered a more conventional GC model. But 'ownership by default' allows for some very nice optimizations: a lot of AO data plumbing becomes non-allocating (simply moving pointers or values). Reference counting simplifies potential accelerations for vectors or arrays: split, append, etc.. Also, the simplicity and predictability of reference counting is appealing: with the aforementioned design, Wikilon is hard real-time capable. Further, 'deep copy' seems a rare event in AO. Mostly, we shallow copy blocks as part of fixpoint loops.

## Tagged Objects

A tagged object is represented in memory with the first word (32 bits) indicating how the following words are understood. It's important that tagged objects be simple. Wikilon's GC must understand them.

Important tagged objects include:

* refct wrappers
* ad-hoc sum types
* arrays or compact lists
* compact texts, binaries, bitvectors 
* stowage indicators and wrappers
* pending computations
* sealed values
* blocks
* fixpoint blocks
* JIT or compiled code
* forwarding objects for copy collection

Most tagged objects will require two cells or more. But there are a few cases where we can squeeze down to just one cell: reference counting wrappers, and ad-hoc sum types. 

### Reference Count Wrappers

A reference count wrapper doesn't avoid a copy. It just delays it, until we're ready to unwrap the object then copy it for real. But this can save us a lot of work in cases where we copy a value many times, investigate it shallowly, and delete. We also want to optimize deletion of copied values, so reference count wrappers will include a bit for that.

I want to squeeze these wrappers down to 1 cell: `(refct, object)`. 

In the worst case, nearly every *word* in memory is pointing to the same refct object. This is feasible if: we build a large lists of copies of the object, compact the list into an array (eliminating the overhead list nodes), then rinse and repeat until memory is full. With 4GB memory, we have at most 1 billion (2^30) words, less the refct object itself.

So... we need to handle reference counts of up to 2^30 - 1. This leaves us only 2 lower bits: one to support fast delete, and one for *this is a refct object*. Consequently, *half* of our tag space will be dedicated to refcts. That's okay. We still have another two billion tags available.

Refct tags will be indicated with a low bit `1`. 

### Ad-hoc Sum Types



## Arrays 

A viable representation for arrays:

        (array, refct, size, pbuff)     2 cells
           pbuff → contiguous memory

Separating the refct, size, and pbuff from the actual contiguous memory is necessary for slicing. When refct is 1, we can cut an array into two smaller pieces each with refct 1. We can also append two adjacent units of refct 1 back into a contiguous array. This slice and append, together with some ability to update the array, is valuable for parallelism patterns.

Here's an alternative:

        (array, size, pbuff, pnext)
            pbuff → contiguous memory
            pnext → unit or another array

In this case, we don't have an internal reference count. So we're *assuming* we have ownership of the array. This greatly simplifies slicing and joining the buffers. We're using the recovered slot to model a chunked list. This guarantees our 'arrays' still have 'list' performance semantics: O(N) for everything. But it simplifies automatic defragging of arrays.

For a shared array, then, we'll need to wrap our array with refct objects. (I wonder if I can do reference count objects in just 1 cell? Might need to optimize its tag.)
