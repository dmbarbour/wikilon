
# Awelon Runtime Design

In context of Awelon [applications](ApplicationModel.md), evaluation occurs is part of high frequency transactions on a dictionary in a multi-agent system. The goal for Awelon runtime is high performance applications, so that must include both evaluation and transactional update, and must support incremental computing.

## General Performance Notes

I will implement Awelon runtime in low level code with simple representations. The motivation is both for performance of simple interpretation and potential integration of JIT. I can implement many accelerators in the low-level language. 

Acceleration of definitions will be prominent in the design. The Z fixpoint combinator will certainly be accelerated to optimize loops. Common data plumbing operations will be accelerated. Arithmetic will be accelerated. Hopefully I'll reach a point where linear algebra is GPGPU accelerated and KPNs are cloud accelerated, because I believe that much would place Awelon near state of the art for performance. 

To avoid unnecessary copies during evaluation, I'll try to assume linear structures with ownership. However, I'll explicitly support some shared structure in contexts like accelerated fixpoints or shared references to words.

Evaluation will have quotas for both space and effort. If evaluation aborts on quota, it will halt in a valid intermediate state that may be continued later. I'll achieve this by limiting updates memory updates to prevent any invalid intermediate states and using a long jump or exception to escape upon quota failure. I'll design memory structures to support constant space traversals (e.g. via the Morris algorithm), such GC and extraction of large data structures are not failure points, and copy requires only as much extra memory as necessary for the object to be copied.

Other than quota failures, evaluation will not halt on error. Instead, errors are represented within a partially evaluated program. We can continue evaluation as far as it can go despite some errors. Breakpoints operate similarly, resulting in a checkpointable computation that is 'stalled' on one or more active gates.

Parallelism will get attention early. Worker threads can support background evaluations, and we might tune evaluation strategies to prioritize observation of a few partial outputs. Evaluation strategy may be tunable to support background parallel evaluation even while we work with partially computed results. For example, optimally it should be feasible to leverage background KPN evaluations while we leverage [KPN to express effects](KPN_Effects.md).

Concurrency must also receive attention. Multiple human and software agents need multiple transactions on a dictionary resource. In practice, most transactions should be mergeable, especially if we track dependencies precisely.

Integration of secure hash resources may need special attention. We should be able to efficiently determine which secure hash resources we need, or provide those we know on request by name. GC for secure hash resources should probably leverage lazy reference counting, which is effective in a context where resources tend to survive multiple collections and reference count updates are expensive. To help accommodate GC, inputting secure hash resources might be part of a first-class transaction.

## Persistent Storage

I'm leaning towards use of LMDB as the primary storage system, instead of using a filesystem directly. LMDB gives transactions and avoids the problem of managing open file handles. LMDB is primarily read optimized, but can efficiently support batch writes. Holding a read transaction can support zero-copy access to a resource. 

We might export patch files and similar when exporting a dictionary. But dealing with the filesystem handles, representing transactional update in the filesystem, and so on seems a hassle I'd prefer to avoid.

A consequence of controlling storage via LMDB is that we might need a FUSE adapter to effectively leverage conventional text editors. But that was going to happen regardless.

## Consistency Model

With multiple agents viewing and updating the dictionary, and performing external effects, I need a consistency model to guard against conflicting updates. It should be possible for our agents to update multiple definitions, and perhaps also assert that certain dependencies are not modified or remain constant during updates.

Minimally, dictionary patches can be merged in a DVCS style. That would give me atomic updates without isolation. However, I might also wish to detect read-write conflicts, application-layer command pattern updates, renaming of dictionary objects. It is feasible to recognize command pattern and rename at the level of patches, but it is simpler and more extensible to model transactions as a log of higher level actions.

To preserve the RESTful nature of dictionary applications, transactions should be RESTful resources in their own right - nameable, serializable. With that in mind, I need a representation for transactions. I might derive representation of transactions from patches by adding `@@COMMAND` actions, which simplify conflict detection then rewrite into a (potentially empty) sequence of updates.



## Design Goals

Performance in context of multi-agent updates and observations is the primary goal. But some others:

* control computation effort via space and time quotas
* real-time capable (if external factors controlled)
* suspend, checkpoint, debug, and continue computation
* easy to parallelize computation and external effects
* no callbacks, no external dependencies for evaluation

For active debugging, I will support `(@gate)` annotations, which may be configured for logging, breakpoints, etc.. For parallel computations and effects, I'd like to focus on [KPN-based effects](KPN_Effects.md) models and some form of background computation. I will need an API that enables ongoing evaluations with observations.

## API Concepts

A context will hold a single, implicit object. The API will directly manipulate this object: injection of input or code, stepwise evaluation with quota-based limits, flexible extraction or use of results. This API should more or less reflect how the program itself operates on code, albeit with somewhat more freedom.

I'd like to have 'errors' be an okay thing, like the worst that happens is we don't make any progress during evaluation, unless the problem was expressing an over sized program. If a program is too large, we might have options to divide it into smaller pieces automatically, or to resize our context to fit.

## Structure

        wikrt_env   - - -   database
            |          (stow, persist)
          (1:N)
            |
        wikrt_cx    - - -  mem & ssp
                         (data) (scratch)
                          (swap on GC)

I have a toplevel environment that binds a shared database. Then I can have multiple contexts, each with their own memory. I am currently using a semispace for simplified GC. A context doesn't need very much space, so I am contemplating use of a relative 32-bit representation within context local space.


## Memory Management

See [ABCRTMem.md](ABCRTMem.md).

### Parallelism 

Context-local parallelism is quite feasible. 

See [Parallelism.md](Parallelism.md)

It seems I'll take a hit, however, with respect to easy access to a scratch space stack for deep copies. The scratch space I've been using is *convenient* for fast copy and computing sizes of things. But synchronizing use of that space would easily become a synchronization bottleneck.

### Checkpoints

For safe evaluation and parallelism, an idea is to leverage a 'checkpoint' model. Intermediate constructions will not be rooted, so if I run out of memory during evaluation I don't need to recover from an out-of-memory status. Rooted constructs will always be a valid state, so the worst case is that I lose some progress on intermediate computations. The main issue here is potential for logging objects to be reported twice, which is not a big problem.

### Garbage Collection

Each thread manages its own free lists. The main thread will occasionally perform a full compaction. 

If I have large binary objects getting copied around frequently in context, consider a 'mature' space that is compacted less frequently to amortize the relative costs. The mature space can simply grow from the opposite edge of our memory as the normal allocation.

### Shared Objects

A 'shared object' is one where we copy our reference rather than the data. Motivations:

* avoid wasting memory on multiple copies of code
* reduce memory pressure and hence need for GC 

Shared objects work best for binaries, where I won't be destructively observing them. This includes texts and compact bytecode modeled as binaries. Potentially JIT code, which might need special attention.

Context-local shared objects should simplify some problems related to memory and quota management. This does complicate code by requiring different logics for most efficient 'local copy' vs. 'remote copy'. For now, I'll just drop the remote copy feature.

### Fast Deep Copy

With linear objects, much performance comes from having unique references I'm free to destructively manipulate. Unfortunately, assuming parallelism, I lose exclusive access to a per-context scratch space that is guaranteed to be large enough to serve as a stack for deep copies. And I'd rather avoid synchronization on the scratch space (e.g. use of a mutex).

Ideally, I want an efficient, high performance deep-copy with O(1) space.

The Morris algorithm could be adapted. This requires that all lists/stacks are predictably terminated by the same value, i.e. the equivalent of a `null`, such that I don't need to remember any value in particular. This seems viable, even if I optimize singleton lists, by having a smarter `cons` action. Granted, this ultimately amounts to providing extra space within the tree to perform the copy. But it should work for other traversals too - e.g. rendering/printing 


## Bit Representations

I assume allocations will be aligned to at least 8 bytes. This gives 3 flag bits per reference to provide a lightweight indicator of data type. 

This is the primary candidate at this time:

        x01     small natural numbers
        011     other small constants 
        111     common action codes

        000     cons cell (2 words)
        010     (unused, reserved)
        100     tagged objects
        110     tagged actions

        (Fast Bit Tests)
        Shallow Copy:   (1 == (x & 1))
        Action Type:    (6 == (6 & x))
        Tagged Item:    (4 == (5 & x))

Small constants would frequently be singleton blocks, but small texts or binaries might also be viable. 

## Representations


I'm giving a try to 64-bit representations. Absolute pointers. Bit flags. No special address checks.

* 000 - tagged object
* 001 - pair value
* 010 - pair in left
* 011 - pair in right
* 100 - small integers
* 101 - ref constant 
* 110 - ref constant in left
* 111 - ref constant in right

For the small integers with 64 bits, I will carry exactly 18 decimal digit integers, i.e. plus or minus 999999999999999999. (In part because it's easy to describe and explain.) I might eventually support big integer math. The small 'ref constants' can include: the unit value, an explicit 'garbage' value with substructure, and perhaps small texts.

Tagged objects include:

* deep sum values
* blocks of code
* sealed values
* pending computations
* compact structures: arrays, binaries, texts
* stowage wrappers

After implementing a few, I feel it is essential to keep tagged objects structurally simple (even at cost to performance opportunities). I'll eventually want to extend these further: floating point numbers, vectors and matrices thereof, etc.. 

### Deep Sums

        tag word: includes an LRLRLLLR string (2 bits each)
        second word is value reference

We'll use 2-bits per `L` or `R` in a deep-sum string. In a 32-bit system this gives depth 12 per allocation. In 64-bit system, depth 28 per allocation. 

Shallow sums apply to pairs and reference constants. Mostly, this enables lists and booleans to have high performance representations.

### Blocks

An efficient non-copying representation for blocks of code will be essential for performance with loopy code and fixpoints. However, for the moment, I'm favoring a naive representation: a simple list of operations. I might end up with mixed block models via composition. So "list of operations" should be the piece I optimize, similar to a binary.

To support lazy computation of substructural attributes, a flag is used for quoted values to indicate the parent should inherit its substructure.

#### Performance for Tight Loops

Loops are performed by a fixpoint operation, which repeatedly:

* copies a block
* fixpoints one copy
* applies the other

For performance, it would be most convenient if we do not deep-copy larger blocks or subprograms. Instead, we ideally have a *shared object* between copies. Any contained 'values' will simply be reproduced by the shared object as needed. 

Even better if our shared object block knows how much memory it needs, i.e. such that it can use preallocated space for the computation. 

When a block contains a representation for a complex value (potentially another block) I'll need some way to process values without extra copying. Thus, evaluation of the block needs to produce the value. And an inner block generated during evaluation can directly reference the outer block plus an offset or similar. Effectively, I need some compact representation of 'values' to go with other compact code. This particular aspect could be related to value stowage!

We can potentially have these shared objects be local to a context, i.e. keeping a stack of sorts. But having multi-context shared objects would have some advantages for performance (copying continuations, shared JIT efforts, etc.).

#### Serialization of Blocks?

For now, I'm using text as an intermediate structure for parsing blocks. It might be useful to eventually develop some means to stream text input and output, i.e. to work with very large blocks of code. But for now, focusing on small amounts of code or paragraph-structured program streams (i.e. paragraps separated by LF LF) would probably work well enough.

### Value Sealers

        (:foo) (.foo)

Awelon's discretionary value sealers. I can probably optimize for short, discretionary sealers, compacting them into a single 64-bit word. But it might not be necessary, depending on how objects are shared or interned normally.

### Value Stowage

Paired annotations: `{&stow}` and `{&load}`. I can stow a value to a backing database (LMDB) or load a value from said database. Latency on stowage is critical for several use cases. I'll probably want to perform value stowage *immediately before* garbage collection (so my scratch space is available and I avoid extra copies). And that only for values having survived at least one GC already. I might end up tracking stowage via some form of ephemeron table that is rebuilt upon GC.

I might want some explicit structure sharing for stowed values, e.g. some way to say that THIS stowed value is a copy of THAT one (in O(1) time), and they'll all share the same resource identifier when stowage finally commences. Dropping a stowed value is simpler, since I can just deep-drop the value. And if I load a copied, stowed value, I can explicitly copy it then.

Value stowage should ideally work nicely with the 'shared object' concept, enabling large blocks of code to be used efficiently without copying. This is not critical, but LMDB could potentially enable direct access to shared memory. Or I could use an explicit shared-object cache. 

### Computations

For pending computations, I could probably use a `(block * value)` pair together with a tagged object wrapper. This would work pretty well, so long as it isn't the representation used *during* evaluation. During evaluation, I'd divide the block into a stack of computations via `$` and `?` operations. Then, if a computation doesn't complete before its time quota, I simply rebuild a block via O(1) composition.

With a block and value, it's easy to extend the block with additional work (it's just composition). So I could have lazy application of blocks if I want them. The main difficulty, I think, would be working with parallel computation. Special attention will be needed for parallelism, I think. It's low priority at the moment. 

I've decided not to support client-defined tokens. Wikilon doesn't need them, and they complicate parallelism.

#### Parallel Computation

Due to use of a compacting collector and bump-pointer allocation, I cannot parallelize computation within a single context/heap. So I need a design suitable for process-level parallelism that does not share any heaps (though they might still share access to special objects - e.g. via stowage). Though, this isn't a bad thing: process-level parallelism has potential to be vastly more scalable (supporting mesh networks, cloud computations, etc..)

My idea is to use affine *process functions* (PF) together with asynchronous futures.

        PF i o      [i → (o * PF)]
        {&fork}     ((PF i o) * e) → ((PF i (future o)) * e)
        {&join}     ((future a) * e) → (a * e)

A forked PF may be computed as a separate process - a separate thread, generally with its own heap. Communication involves copying data between processes. When applied, the forked PF 'immediately' (modulo pushback) returns the future result and the next PF. Parallelism is achieved by performing work between applying the PF and joining the result. The next PF may be called immediately, effectively enqueing sequential calls to the process.

This design for parallelism is simple, highly scalable, and expressive within the limits of purely functional behavior. Costs of constructing a process can be amortized over multiple calls. There is no direct expression of non-determinism or deadlock. But it is possible to model process pipelines, message passing, one-off processes, etc.. The costs of constructing a heavier process can be amortized over multiple calls. Because processes have individual heaps, we avoid many GC scaling challenges of multi-threading.

I'll be brainstorming implementation ideas in [Parallelism.md](Parallelism.md)

*Pushback:* Fast 'producer' processes can easily run too far ahead of slow 'consumer' processes. This hurts performance and requires too much memory. We can mitigate this with pushback: each process is given a finite queue for pending messages in addition to whatever message it is processing. If this queue is at its limit, we should wait to call the PF.

*Peformance Notes:* Each process has a finite queue for pending requests (in addition to the request it is handling). When that queue is filled, the caller waits. This limits 'producer' processes from running too far ahead of 'consumer' processes while still enabling processes to operate outside of lockstep. Pushback, note, could be explicitly guided by annotations, or left to heuristic scheduling.

#### Lazy Computation? 

The concept of 'lazy' computations seems rather troublesome. The issues are: how should laziness interact with persistence, parallelism, distribution, substructural types, caching, termination, etc.? I haven't found an approach I find entirely satisfactory. A simplistic laziness option that I'm considering is this:

        {&lazy}     ([a → b] * e) → ([a → (future b)] * e)
        {&join}     ((future a) * e) → (a * e)

The block is marked lazy. When applied, the block returns future result. This result may be joined normally. Under the hood, a linear or affine future could be represented by a trivial `(block * value)` pair internally, while a copyable future might use the same representation until copied, at which point it might be evaluated or (if that fails) upgraded to a parallel future.

Lazy futures are useful for optimization and performance roles. Compared to explicit `(block * value)` pairs, lazy values indicate their value is a consequence of a specific computation, that the block and value are tightly coupled, which simplifies partial evaluations.

*Note:* Futures are algebraic types. Hence, applying `{&lazy}` twice to a block will have different behavior than applying it only once. But the wikilon runtime will probably optimize for the case where only a single evaluation-mode tweak is applied.

### Arrays or Compact Lists

Arrays are a compact representation for list-like structures `μL.((a*L)+b)`. In addition to arrays, I'm interested in array-chunked lists similar to Haskell's lazy bytestrings. Those are sufficient to offer most benefits of arrays, after all, while remaining structurally more flexible.

Desiderata: besides the benefits of a compact representation, arrays can easily accelerate a lot of list-processing functions. Fast logical slices, seamless append, logical reversal, size computations, indexed lookups and updates, chunked allocation, and compact binaries and texts are feasible. I'd also like (eventually) to support column-structured data, i.e. where a list of pairs is represented as a pair of lists.

Fast slicing implies use of a `buffer` pointer. The arbitrary list terminal and potential for array-chunked lists suggests a `next` pointer. Between two pointers and tagged-object header, I need at least two cells overhead for the array. Other information I need includes: reversal, size, type. Type information isn't immediately essential - if it becomes relevant later, I can probably tweak the array model at that time. For now, I probably can make due nicely with just "array of values" vs. "binary array" and "texts".

A viable representation:

        (array, size, buffer, next)

Our `array` header will include a 'reversal' bit. Also, we might record space on one side of the buffer (e.g. the left side, lower addresses) for efficient deallocation and seamless append? But that might not be worthwhile... not sure.

When slicing arrays, we might cut in the middle of a cell. In this case, only one of the two arrays will keep the cell, while the other will obtain a copy of the data. We'll need to restore these 'frayed' edges if later we append the two slices.

*NOTE:* If we have arrays with extra space, we might benefit from ArrayList like features. This would require accelerating either `append` or `cons`.
A useful related feature might involve array chunks with reasonably large 'capacity' at one or both ends, such that we may addend (or prepend) elements without extra allocations. I wonder how we'd annotate and accelerate these properly (some form of `{&pack}` annotation?)

#### Binaries

The current representation for a binary is:

        (binary, next, size, pBuffer)

This represents a fragment of a list, with the `next` continuing the list. The buffer is fully copied whenever the value is copied, including during compacting GC. 

A non-copying representation for binaries would need to have something like:

        (shared-binary, next, size, offset, head, buffer)

The `head` would need to include information to prevent GC. And the `buffer` would be for fast access, it might be recomputed from the `head` and `offset` after copying. A `size` would potentially allow an underlying shared binary to be divided into multiple chunks, i.e. a binary of 10kB could be divided into 10 1kB chunks. 


### Trashed Data and Memory Recycling

Assuming a value that might be relevant or linear but that will not be used, annotate it as garbage with `{&trash}`. This allows the runtime to recycle the memory immediately. A placeholder is left, requiring only a small constant amount of space. Attempting to observe the placeholder will appear as a type error. The value will preserve known substructural attributes of the original (with pending values or parallel futures treated as linear).

Trashed values can be serialized as `[]kf{&trash}` with `kf` depending on substructural type.

### Undefined Values and Computations

Trashed data also serves a potential useful role for TODO implementations, or serving similar to Haskell's `undefined` values.

## Shared Blocks and Internal Bytecode

I need high performance, non-copying, compact, shared memory blocks of code. The 'sharing' is performance essential for tight loops of code (because fixpoint copies code). But sharing for `wikrt_move()` could be very convenient. This is most essential for performance. I must be able to:

* interpret program directly from the shared byte string
* reference sub-blocks as sub-regions of the byte string
* reference large texts or binaries from the byte string

To simplify internal references, I must separate tracking of dependencies vs. access to stowed data. Value sharing will be limited to blocks and binaries, I think. And for data that can be efficiently represented as blocks or binaries, such as texts.

Anyhow, I need a representation for bytecode that is suitable for this compact usage. I might include a 'stop' or 'yield' bytecode to simplify termination.

## Debugging, Profiling, Feedback

The debugging story is greatly simplified by Awelon's program rewrite semantics. We can simply freeze a computation and inspect it. Use of `(@gate)` annotations can configurably support tracing, breakpoint-like behaviors, program animations, and profiling.
