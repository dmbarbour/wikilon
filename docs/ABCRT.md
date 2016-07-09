
# ABC Runtime Design

As per [my performance strategy](Performance.md), I'm currently developing a C runtime: a good interpreter with a simplified representation for values to eventually enable LLVM or other JIT compilation. I'll be using something very like the Lisp/Scheme representation, with data consisting mostly of simple pairs of words.

It may be useful to also develop a command-line interface and computation experience at this layer. I would like to encode dictionaries in both value stowage. Dictionary import/export is viable. Supporting a Claw syntax is feasible.

NOTE: In retrospect, it might have been wiser to focus entirely on a Haskell+LLVM implementation, skipping the interpreter entirely. It could still operate on a memory-mapped region for data representation purposes. Essentially, I'd be developing a process model wherby I fill a volume with simply formatted data then prepare high performance code to operate upon it. At this point, however, backtracking is more expensive than I'd prefer. The JIT remains viable.

## Design Goals

Performance is the primary goal. But some others:

* precise memory control: model machine's memory in one arena
* hard real-time capable: predictable timing properties
* computation effort control: easily abort infinite loops
* suspend, checkpoint, persist, reload, resume, copy computations
* easy to resize, reflect, render, compact suspended computations
* monadic effects: return control of program environment to caller
* easily integrate monadic effects with [claw command sequences](CommandSequences.md)
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

With LMDB, I have the opportunity to use a single-process or multi-process access to the database. Multi-process access to a database would be very convenient if I want to later develop shells, command-line interpreters, etc. to access this database in the background. However, multi-process manipulation does complicate tracking of ephemeral values. 

For my anticipated use case, multi-process access is not critical. Also, I can always develop command-line interpreters and shells that use HTTP queries against a local web service instead of direct access to the database.

### LMDB vs. LevelDB?

LMDB and LevelDB both are mature and efficient implementations of key-value databases backed by a filesystem. Both keep recently accessed or updated objects in memory. Both are effectively single-threaded writers and multi-threaded readers. LMDB mostly uses the OS as its background process (via `mmap`).

LevelDB has the disadvantage of being a lot more sophisticated internally (with background threads, lots of value copies, etc.). But it does have two advantages worth considering:

* LevelDB will automatically shrink due to compaction phases. 
* LevelDB can compress large chunks, across multiple values

Shrinking the database is *probably* a non-issue. Wikilon's expected use case is for monotonic growth, a little exponential decay as needed to limit the growth. I can *manually* compact a database if necessary, though it might require halting, and might require a fresh disk. (The fact that this compaction would mirror semi-space collection is kind of nice.)

Compression is a minor concern, but it might improve performance by a fair margin.

## Structure

I need an *environment* bound to an underlying stowage and persistence model, e.g. via LMDB. The environment may also be associated with worker threads for par-seq parallelism. I also need *contexts* that associate an environment with a heap of memory for computations. 

Context is a contiguous region of memory, up to ~4GB enabling local 32-bit addressing. In general, Wikilon shall produce one context to evaluate each web page. Contexts will generally be much smaller than the 4GB limit (e.g. 10-100MB). A context may interact with a lot more data than its size suggests via large value stowage. Stowage will serve a similar role to virtual memory, but does not consume the greater 'address space'.

In some cases, parallelism within a context could be useful. For this, I could support lightweight forks that share memory with our primary context.

### API Thoughts

Originally I was going for a more conventional API where values have handles outside the computation context. But I'm beginning to think this wasn't the best approach, that it would be better aligned with Awelon Bytecode to give each context a *single, implicit* value, a program environment that we manipulate just as we would a value within an ABC computation.

Originally I was returning an error value for most API calls. I'm beginning to believe this is a bad idea. It complicates and clutters my client's API. It might prove better to capture error state within a context after a stream of commands, and report upon request that errors have occurred. 

Alternatively, I can switch to using C++ internally with an efficient exceptions implementation. This would give me the 'zero cost' exceptions with a minimal use of runtime checks. However, this wouldn't be a good fit for the Haskell interaction...

### Memory Management

ABC is well suited for 'manual' memory management due to its explicit copy and drop operators. It's convenient if most memory is linear so that we can have non-allocating data plumbing operations (at least for common cases). And bump-pointer allocation and slab allocations are useful for locality and performance, potentially favorable to free lists (though something like TCMalloc wouldn't be bad).

Because I'm running pure code without side-effects, throughput is more important than latency during computation. There is a lot of freedom on what to do with memory. For now, I'm going for the 2-space compacting copying collector per context, which ensures fragmentation will never be an issue at the cost of copying memory occasionally.

Parallelism within a computation could also be valuable. I may need to eventually move away from the shared memory idea in favor of free spaces for multiple threads. Though, this could be mitigated by good support for non-copying shared memory and objects.

TODO: Consider 'graduating' context size between compactions. This would allow a 4GB context to act as a 4MB context when the data is small, for example, costing only address space. This would be achieved easily by allocating big chunks of memory after each compaction based on estimated usage. It could be configured by an extra function call on the process.

#### Support for Shared and Non-Copying Objects in Memory?

Linear memory is convenient but it requires frequent deep copies. This is especially a problem for blocks in a fixpoint loop, and for large binary data. I would like some form of stable memory for binaries and blocks of code. 

The ability to compute large objects without copying them every GC pass? It could be quite convenient, in general. *Stowage* sort of works that way, but has a large copy overhead per access. I need a lower latency option - objects *in memory* but not copied, i.e. in a separate region of memory. 

Some options:

* divide context into both a workspace and shared space.
* shared space at `wikrt_env` layer instead.
* shared space via C heap, malloc/free, reference counting.
* stable access for stowage addresses via read-only transactions.
* cache stowed values in memory via some other mechanism.

The simplest of these options to implement is to use the C heap and reference counting (or other GC) for shared objects. This would at least offer a simple, robust way to get started. Modeling shared memory in terms of stable access to stowed binary data would be better - simplify structure sharing and garbage collection, and more or less eliminate need for copying data from the stowage layer.

#### Other Ideas

Focusing on collections-oriented processing, e.g. with lists and vectors. 

### Error Handling?

The main errors I can expect are: type errors and quota errors. Neither error is especially recoverable. Rather than assume recovery from errors, it seems wise to halt a runtime shortly after an error is recognized. The main cost here is that we cannot 

Rather than return an error from each step, it might be more efficient to accumulate error information then report it upon request. OTOH, this doesn't really change the number of checks I need to perform, e.g. for type safety and safe allocation. Even if I used C++ exceptions upon error, I'd still need to perform the conditional check that leads to the exception call. 

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

64-bit small integers can carry 18 decimal digit integers. For simplicity, that's my current limit. I might eventually support big integer math. At the moment, the only 'ref constant' is the unit value.

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

For performance, it would be most convenient if we do not deep-copy larger blocks or subprograms. Instead, we favor a *shared object* between copies. It will be essential to have a decent shared-object model for this. 

When a block contains a representation for a complex value (potentially another block) I'll need some way to process values without extra copying. Thus, evaluation of the block needs to produce the value. And an inner block generated during evaluation can directly reference the outer block plus an offset or similar. Effectively, I need some compact representation of 'values' to go with other compact code. This particular aspect could be related to value stowage!

We can potentially have these shared objects be local to a context, i.e. keeping a stack of sorts. But having multi-context shared objects would have some advantages for performance (copying continuations, shared JIT efforts, etc.).

#### Serialization of Blocks?

For now, I'm using text as an intermediate structure for parsing blocks. It might be useful to eventually develop some means to stream text input and output, i.e. to work with very large blocks of code. But for now, focusing on small amounts of code or paragraph-structured program streams (i.e. paragraps separated by LF LF) would probably work well enough.

#### Binding a Dictionary?

It seems feasible to bind a context to a dictionary, i.e. such that the dictionary implicitly maps `{%word}` tokens to a block of code. This would greatly simplify interpretation of code.

### Accelerators via DSLs

It might be worthwhile to pursue the 'accelerator via DSL' approach for performance at some point. If we want C language for a given subprogram, for example, we could model a C interpreter then accelerate its operation. C is perhaps not an optimal target to fit into a purely functional program, but there may be other low level languages for which this is a useful idea. (Especially anything that will fit casually into a GPGPU.)

### Value Sealers

ABC's discretionary value sealers. I'll optimize for 3-character (or fewer) discretionary sealers like `{:map}` or `{:foo}`, which may be encoded within tag bits of a single cell. Otherwise, I'll simply copy the token together with the value. I'm not going to 'intern' token strings, mostly to avoid synchronization overheads and to discourage use of large seals.

### Value Stowage

We annotate a value to moved to persistent storage, or reverse this. Originally, I was planning on entirely *transparent* stowage. However, that complicates the path for normal computations, i.e. I must repeatedly test whether or not I have a stowed value. So I'm now favoring paired annotations: `{&stow}` and `{&load}`. 

IMPLEMENTATION: I know of two basic options. 

1. I can attempt to use the *same* representation for stowage that I use at runtime, and treat stowage as a micro-context of sorts to `wikrt_copy_move` between. This approach relies on relative addressing, e.g. such that pointers are all relative to a base address. Dealing with shared objects may be complicated (size computations for stowage vs. sharing would diverge).

2. I can create an alternative compact representation for values. A binary string, perhaps with specialized representation of additional stowage references. This is pretty much a 'VCacheable' approach. We could limit stowage binaries to (for example) one or two megabytes.

A `copy_move` option is tempting in its simplicity. But it seems fragile to changes in the runtime. Using separate representations is very likely to be more robust. Also, an approach to stowage will need to properly handle shared object data internally. This could become a complicating factor if we're not very careful about it. 

If stowage is used together with value sharing, I might also benefit from supporting stowage together with range slices. I'll need to think about it, at least.
 
### Computations

For pending computations, I could probably use a `(block * value)` pair together with a tagged object wrapper. This would work pretty well, so long as it isn't the representation used *during* evaluation. During evaluation, I'd divide the block into a stack of computations via `$` and `?` operations. Then, if a computation doesn't complete before its time quota, I simply rebuild a block via O(1) composition.

With a block and value, it's easy to extend the block with additional work (it's just composition). So I could have lazy application of blocks if I want them. The main difficulty, I think, would be working with parallel computation. Special attention will be needed for parallelism, I think. It's low priority at the moment. 

#### Lazy Computation?

Laziness with Wikilon Runtime is troublesome. It's easy to implement short-term laziness with paired annotations. The troublesome bit is how laziness should interact with cache, value stowage, substructural types, type safety, termination, and the like.

One option is to effectively treat lazy computations as *linear*. This would limit the damage from copying lazy values - shared dependencies at the dictionary level could still replicate lazy computations. Sadly, it still wouldn't provide useful laziness at a cache layer.

For now, I'll just table the whole issue. It might prove favorable to model laziness explicitly, e.g. via command sequences or other stepped streaming models. Parallelism is more critical, I think.

#### Parallel Computation?

I've been recently developing a simple model for pure functional parallelism suitable for heavy-weight processes. See [purely functional processes](http://lambda-the-ultimate.org/node/5357#comment-92995) and [pure process parallelism](https://awelonblue.wordpress.com/2016/07/08/pure-process-parallelism/).

The idea is that we annotate an affine *process function* (PF) of general type:

        type PF = argument → (result * PF)

Rather than offloading a *specific computation* into another thread, the idea is to offload a *process function* in a stable manner that can be called many times - with a large sequence of arguments and results. When we call the PF, the argument is essentially queued, the 'future' result is returned immediately, and returned PF is immediately available for the next call.

The sequence of arguments conveniently amortizes the overhead for offloading the function in the first place. This gives pretty good control over how many processes are created. It gives us something similar to 'partitioned' computations of RDP, without all the overhead of naming and identity. (Of course, we can always model identity - a lookup table, etc..) 

This works nicely with external, explicit orchestration of the network and pipeline parallelism.

Holding data in a PF could also serve a role similar to stowage. And variant on parallelism should work very well with cached computations.

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

#### Texts

Texts are a specialization on binaries. At the moment, limit sizes to 16 bits, and store both size-in-bytes and size-in-characters for each chunk. This gives a simple index into a very large text.

As with binaries, I'll probably eventually want a good non-copying shared text representation.

### More Numerics

ABC supports integers. But other numbers could be supported via arithmetic accelerators or sets of dedicated ops.

* I want good support for vectors, matrices, and related math operations.
 * Vector arithmetic with SIMD support, etc.
* Good support for floating point would be nice.
 * John Gustafson's open interval arithmetic 'unum' is also nice.


## Evaluation and Time or Effort Quotas

One of the bigger challenges will be supporting evaluation limits, i.e. effort quotas. 

## Static vs. Dynamic Typing?

I'd like to eventually have access to an "assume type safety" option on the interpreter for very fast evaluation, skipping dynamic runtime type checks. But I'll need to wait until I'm either confident in the type safety checks, or able to use a separate virtual machine for the evaluation. 

# Other ideas

## Accelerated Association Lists? (low priority)

It might be useful to heavily optimize an associative structure, e.g. the equivalent of a JSON Object. Motivations include:

* fast indexed lookup without disassembly of the structure
* optimized representations for small vs. large structures
* implicit batching of updates, log structured merge trees
* type covers a lot of use cases: records, databases, etc.
* provides a built-in, optimized model for AO dictionaries

An accelerated associative structure should have a canonical form, a representation deterministic from *content*. This excludes balanced binary search trees because their precise balancing structure depends on insertion order. However, I could use a *trie* or a *sorted association list*. 

Between these, the sorted association list is the superior option for acceleration. It is the simpler model. It allows many accelerated lookups and updates, ad-hoc representation (structs, tries, log structured merge trees, etc.). Further, it fits naturally with other list accelerators, e.g. as a basis for iteration. Besides that, the trie doesn't really *need* any acceleration.

More generally, extending from *association list* to proper sorted *relational table* might be useful.

I'll need to return to this concept later. I think supporting this idiom in both the representation and type system could greatly simplify modeling of more conventional programming models (OOP, stack frames, etc.). But I also need to be sure it is simple to implement and won't interfere with, for example, structure sharing and stowage. And of course I'll need to develop a proper set of annotations, assertions, and accelerators to make it worthwhile.

As a nice generalization of association lists, maybe we could try to optimize representation of 'tables' where we know each element has the same basic row structure. A list of rows might be represented by a row of lists.

## Evaluation with Dictionaries?

It could be useful to associate a dictionary with our evaluations, where we know how to read the dictionary. This would require a 'standard' representation for dictionaries in stowage... which is doable, especially if we accelerate association lists.

