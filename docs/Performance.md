
# Wikilon Performance

I don't expect Wikilon to be blazing fast. But I want it to be reasonably performance competitive before other people start judging it. 

My proposed performance strategies are as follows:

1. Implement a runtime in a fast low-level language
1. Index ABC binaries for fast linking and processing
1. Anonymous, accelerated AO dictionary for common ops
1. JIT compilation for ABC code for faster evaluation
1. In-place update for uniquely owned data structures
1. In-place structure traversals (e.g. Morris algorithm)
1. Large value stowage for larger than memory data
1. Shared structures for ABC binaries, stowage, and JIT
1. Extensive use of caching, explicit and word level
1. Simple par/seq parallelism independent computations
1. KPN accelerators for large-scale concurrent parallelism
1. Linear algebra accelerators for small-scale parallelism

Because ABC is purely functional, I can focus on throughput rather than latency, and I can allow some recomputation if it doesn't occur too frequently. 

## Partial Evaluations and Quotas

ABC uses a rewrite semantics, which has some nice properties. We can continue computations from intermediate results, for example. And most runtime errors can be recorded directly in the program result using `{&error}` annotations. 

There is at least one source of runtime errors that I cannot record in the output: halting on time/space quota. When I hit any quota limit, computation must halt in a valid state so it can continue later. What I'd like to do is ensure the 'root' structure is always in a valid state, so even if I suddenly halt the most I lose some progress that I may recompute later.

## Caching and Memoization

ABC will support a generic `[computation]{&cache}` annotation that can be used for ad-hoc caching in the style of memoization. An implicit goal is to ensure caching is preserved across independent forks or versions of a dictionary

## Static Typing for Performance?

ABC will be statically typed where feasible, though I've introduced a `{&dyn}` annotation for programs whose structure is relatively difficult to predict. 

In ABC, all values are ultimately Church encoded. It isn't clear to me that static typing will much improve performance, unless I also isolate *representation* of values - e.g. knowing that my Church encoded natural is represented as a runtime word. In that case, I might benefit from removing representation-checks from JIT compiled code. 

OTOH, it may be I can use [basic block versioning](https://pointersgonewild.com/2015/09/24/basic-block-versioning-my-best-result-yet/) for JIT representations independently of static or dynamic types.

## Accelerators

Acceleration for collections processing will be valuable. Optimally, I can support GPGPUs by accelerating linear algebra, which in turn may require accelerating floating point number representations.

Accelerating Kahn Process Networks (together with value stowage) will be my primary basis for truly massive scaling of computations to distributed systems. KPNs are effectively purely functional flow-based programming with lightweight composition and communication. One can extract outputs and inject inputs in parallel with processing. First-class KPNs might even serve as a 'better' OOP object.






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

(Old content outdated, see [ABCRT.md](ABCRT.md) and [Parallelism.md](Parallelism.md).)

### Other Parallelism?

With accelerators, it is feasible to support SIMD and massively parallel GPU computations. Assume a subprogram is constructed from a constrained set of recognizable functions and data structures that we know how to easily represent on the GPU. Annotate a block containing this subprogram for evaluation on the GPU. The runtime compiles the block to CUDA or OpenCL or a shader language. When we later apply the function, it will compute on the GPU. Other forms of heterogeneous computing, e.g. FPGAs, will frequently follow a similar pattern. Haskell has similar precedent with the [accelerate](https://hackage.haskell.org/package/accelerate) package.

In context of a dictionary application, every form of parallelism could kick in for every evaluation. This includes partial evaluations and cache maintenance. I imagine a mature, active, and popular AO dictionary full of apps could productively leverage as many CPUs, GPUs, and other computing devices as you're willing to throw at them.

## Linear Cached Computations

A very cache-friendly pattern for dictionary applications is where each word begins with another word. Conveniently, this also fits the *command pattern* common to many dictionary applications.

        @foo.v1 {%foo.v0}(a command)
        @foo.v2 {%foo.v1}(another command)
        @foo.v3 {%foo.v2}(yet another command)

A very simple technique in context is to cache evaluation results for each step, and allow them to decay normally (e.g. via exponential decay models). 

## Explicit Caching

Implement a `[V]{&cache}` that uses the stowage layer for explicit cache control and incremental computing. 

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

*Aside:* It might be useful to indicate accelerators via annotations, such that we don't need to search for them arbitrarily. But it might not matter too much, unless accelerators look too similar.

## Compilation

Awelon Bytecode is designed assuming compilation. Without it, ABC has no chance of competing with more machine-oriented bytecodes like CLR or JVM. The amount of allocation performed by an ABC evaluation is significant. Compiling helps in case of loopy and mathy code: the Z-combinator becomes a simple jump, internal data plumbing can be shifted to register allocations, computations can be optimized. 

With accelerators, it becomes feasible to recognize subprograms that can be easily compiled for SIMD or GPGPUs. With this, AO and ABC might achieve world-class performance without sacrificing its pure, trivial semantics.

Targeting LLVM has worked for many people. Seems worth trying. Haskell has decent support for LLVM.

If I have a hand-rolled interpreter, I'll probably want to access those functions from LLVM. A potential techniq

## Optimized Sums

With sum types, we have some `LRLLRLR` sequence to reach them. I wonder if we can reduce this to a bitfield, maybe even track a few sum flags in value pointers? With even just `L` and `R` in pointers, we could model lists without an extra node. To support *trees* and a fast `Z` operator, we might additionally want `RL` and `RRL`.

We might similarly track affine and relevance properties in value pointers? I'm not sure that will work out. It might simplify copy and drop, or it might interfere with parallelism.


# ABC Runtime Design

See [ABCRT.md](ABCRT.md)

