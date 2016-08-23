
# ABC Runtime Design

As per [my performance strategy](Performance.md), I'm currently developing a C runtime: a good interpreter with a simple, uniform representation to enable LLVM or other JIT compilation. I'll be using something very like the Lisp/Scheme representation, with data consisting mostly of simple pairs of words.

It could be useful to also develop a command-line interface and computation experience at this layer, for testing and benchmarking, etc., via console. I would like to encode dictionaries in both value stowage. Dictionary import/export is viable. Supporting a Claw syntax is feasible.

NOTE: In retrospect, it may have been wiser to focus entirely on a Haskell+LLVM implementation, skipping the interpreter entirely. It could still operate on a memory-mapped region for data representation purposes. Essentially, I'd be developing a process model wherby I fill a volume with simply formatted data then prepare high performance code to operate upon it. At this point, however, backtracking is more expensive than I'd prefer. The JIT remains viable.

## Design Goals

Performance is the primary goal. But some others:

* precise memory control: model machine's memory in one arena
* hard real-time capable: predictable timing properties
* computation effort control: easily abort infinite loops
* suspend, checkpoint, persist, reload, resume, copy computations
* easy to resize, reflect, render, compact suspended computations
* monadic effects: return control of program environment to caller
* easily integrate effects models with [claw command sequences](CommandLine.md)
* on errors or unrecognized tokens, suspend and return to caller
* no callbacks or handlers, no external dependencies for progress

Also, it might be useful to support a second evaluation mode for type inference.

I'll avoid using tokens or callbacks for side-effects at this time. I'd prefer to encourage effects models that can be easily simulated and tested in a purely functional ABC environment.

NOTE: I must finish a 'fast' implementation before worrying about detailed performance issues.

## Extra Utilities

I'll actually include a copy of these directly, no shared libs.

* Murmur3 - fast, collision-resistant hash 
* LMDB - embedded key value database
 * considering LevelDB and similar

### Multi-Process Access?

With LMDB, I have an opportunity to support multi-process access to the associated database. This could be very convenient, as it would enable shells, CLIs, and so on to coexist without querying a web service. The primary difficulty would be be tracking ephemeral stowage within a process. I might need to use an extra file-based memory mmap for bloom filters. Alternatively, I could use simple timeout techniques when our value reaches a zero reference count, holding onto dead values for a few hours or days.

For my anticipated use case, multi-process access is not critical. Also, I can always develop command-line interpreters and shells that use HTTP queries against a local web service instead of direct access to the database.

### LMDB vs. LevelDB?

LMDB and LevelDB both are mature and efficient implementations of key-value databases backed by a filesystem. Both keep recently accessed or updated objects in memory. Both are effectively single-writer and multi-reader at any given time. 

LMDB uses the OS as its background process (via `mmap`). No additional threads or caching are needed. LevelDB has a significant disadvantage of being very sophisticated internally - background threads, data copies, etc.. OTOH, LevelDB does support shrinking the database files (via file compaction phases). And LevelDB can compress multiple keys and records together.

LevelDB doesn't truly fit Wikilon's use cases. For *any* wiki, the natural tendency is monotonic growth. This is even more so if we effectively track page histories. Thus, shrinking a database file isn't essential during normal use cases. If necessary, our file can be manually compacted by copying contained data. 

Compression might improve performance by a fair margin, but must be weighted against zero-copy for our data.

## API Concepts

A context will hold a single, implicit object. The API will directly manipulate this object: injection of input or code, stepwise evaluation with quota-based limits, flexible extraction or use of results. This API should more or less reflect how the program itself operates on code, albeit with somewhat more freedom.

Rather than fine-grained errors per API call, I'll try to report errors at the program's toplevel. Precise debugging will be driven more by annotations and automatic testing. 

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

### 32 bit Context Arenas

After exploring a 64 bit implementation, I'm leaning in favor of the 32-bit context (at least for its active memory). The 64 bit model with direct pointers did not (noticably) improve performance. And I could probably improve 32-bit performance by addressing memory directly from our `cx` pointer rather than `cx->mem`. The main benefit of using 32-bit is that it effectively doubles memory and CPU cache efficiency for the common case objects (lists, cons stacks, etc.).

I can encode context, memory, and semispace as one contiguous array.

        [context .... (mem) ..... (ssp)........]

I must tweak my allocator for this, e.g. to work in terms of negative offsets from context. And I might need to tweak my GC process just a bit so I can operate an intermediate context.

### Graduated Memory Use

For very large contexts (e.g. 100MB) and very small computations (e.g. 10kB) it is inefficient to consume the whole 100MB before performing GC. Doing so creates unnecessary memory and cache pressures. Fortunately, it should not be difficult at all to basically use a *soft cap* for how much we may allocate our next GC.

### Parallelism and Zero Copy Communications

Contexts are (mostly) shared-nothing, each able to run in its own thread. To minimize blocking and improve batching, and reduce need for data copies, I propose a simple communication mechanism. It works as follows:

* each context has a queue for outgoing messages.
* each context has a reader-writer lock for messages.
* a context may notify when it has a messages for another. 
* a context must grab the reader lock to access messages.
* the reader may freely mark its own messages for deletion.
* a context must grab the writer lock for compacting GC.

This gives us a relatively simple design that limits blocking to a short window surrounding GC.

### Shared Objects

A 'shared object' is one where we copy a reference rather than the data. 

Shared objects are essential for high performance code, especially in ABC where we don't have backwards 'jumps' and any iteration is instead modeled in terms of copying code (e.g. as part of a fixpoint loop). Shared objects are also separate from normal GC, and thus effectively reduce memory pressure and relative GC effort. Shared objects are closely related to value stowage. The main difference is the emphasis: shared objects are all about *low latency reads*, whereas for stowage, potential latency concerns are rather less essential.

I'm leaning towards tracking shared objects in the `wikrt_env` layer, such that sharing is uniform across contexts within the environment. An interesting possibility is to leverage LMDB's stowage layer directly for shared objects, via its memory mapped direct access. At least for cases where our object was already stowed.

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

Loops in ABC are performed by a fixpoint operation, which repeatedly:

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

ABC's discretionary value sealers. I'll optimize for short discretionary sealers like `{:map}` or `{:foo}`, which may be encoded within tag bits of a single cell. At the moment, this means up to six or seven bytes, which covers many sealers. I'm not going to 'intern' token strings, mostly to avoid synchronization overheads and to discourage use of large seals.

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

#### Texts - Wrap a Binary

Originally I modeled texts as a *specialization* on binaries. With hindsight, it might be better to treat texts as a simple wrapper for a shared binary, e.g. `(text, binary)`, requiring termination with a normal list finisher.

* reuse alternative binary data representations - shared binaries, too
* texts strictly valid for embedding in ABC, simplifies conversion code
* leave structuring large texts to client code (e.g. finger tree ropes)

Accelerators on texts are limited without an index, requiring a linear scan of the binary. However, this could be mitigated by encouraging programmers to model large texts using a finger-tree or other rope model.

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

Debugging, profiling, etc.. 

While Wikilon runtime is focused on 'pure' computations, profiling and debugging outputs don't really count against this assuming they don't affect the observable behavior of the program or mainline IO models.

### Printf Style Debugging (High Priority)

Haskell's `Debug.Trace` (i.e. debug by printf) is convenient as a short term debugging technique. This allows output of warnings, todos, ad-hoc traces, etc.. This is feasible by annotation. Proposed:

        {&trace} :: ∀v,e.(v * e)→((trashed v) * e)

For predictable performance, I keep trace messages in a separate buffer from the normal evaluation. This could be a buffer of structured values, or a buffer of pre-serialized values. (At the moment, I'm using pre-serialized.)

Trashing an argument additionally enables destructive read of text without implicit copies. Despite risk of losing some messages on overflow, this should cover the 99% use case easily enough. Even dumping 64kB should be sufficient for generating a useful debug-view of what's happening within a computation.

The decision to trace arbitrary values is... questionable. It does improve flexibility of expression, enabling structured data subject to ad-hoc external rendering techniques. OTOH, it might hinder legibility of a plain text rendering a bit. Fortunately, plain text embedded in ABC should be reasonably legible.

### Stack Traces (Mid Priority)

Stack traces are convenient for profiling and debugging both. A 'stack' is really a representation for a 'continuation'. So what I'd want for stack traces is to scan through a continuation and obtain a human-meaningful description for the current state of the computation.

This seems to be a bit of a challenge. It might be best to require explicit tracking of sources, e.g. something like `[foo] {&@location} inline`. Such stack trace annotations be introduced by a linker, not just by hand.

### Profiling (Mid Priority)

For profiling, the best idea I have so far is to leverage periodic stack traces (e.g. upon GC), especially if I can make those efficient. With each stack trace, I can record some metadata (time, memory stats, etc.). It might also be feasible to add a `{&prof}` annotation to a stack trace for a given block/location, i.e. for more precise profiling.

I had an interesting idea to use [opaque timer values](https://awelonblue.wordpress.com/2016/08/02/profiling-pure-code/). But in context of value stowage (and possible caching of computations), I think it wouldn't work out very nicely. 


## Miscellaneous Ideas

### Accelerators via DSLs

It might be worthwhile to pursue the 'accelerator via DSL' approach for performance at some point. If we want C language for a given subprogram, for example, we could model a C interpreter then accelerate its operation. C is perhaps not an optimal target to fit into a purely functional program, but there may be other low level languages for which this is a useful idea. (Especially anything that will fit casually into a GPGPU.)