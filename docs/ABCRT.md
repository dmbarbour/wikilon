
# ABC Runtime Design

As per [my performance strategy](Performance.md), I'm developing a C runtime: a good interpreter, hopefully enabling LLVM or other JIT compilation. One motivation for a C interpreter is that I need a simple, obvious data representation for my LLVM JIT. I'll be using something very like the Lisp/Scheme representation, consisting mostly of simple pairs of words.

It may be useful to also develop a command-line interface at this layer. I'm somewhat willing to encode Claw code and dictionaries at this level.

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

## External Utilities

I'll actually include a copy of these directly, no shared libs.

* LMDB - embedded key value database
 * considering LevelDB and similar
* Murmur3 - fast, collision-resistant hash 
* ZSTD(?) - real-time, streaming compression
 * promising... but not entirely stable

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

ABC is well suited for manual memory management due to its explicit copy and drop operators. This couples nicely with a preference for linear structure and 'move' semantics, deep copies over aliasing. Linearity allows me to implement many functions - especially data plumbing - without allocation.

Bump-pointer allocators, together with two-space compacting collectors, should also serve me well. I initially tried more conventional memory management with size-segregated free-lists. But that doesn't work nicely with slab allocations for copying composite values (e.g. allocating enough space for every element in a list or stack or tree). Free fragments are small, and allocated blocks are large, and we end up coalescing memory frequently.

The main issue with a two-space collector is working easily with parallelism. I need either to stop-the-world for GC and add a mutex for allocation, or move a parallel computation into a separate space so there is no interference between threads. I'm leaning more towards the non-synchronizing parallelism, even though it implies greater overheads for simplicity. The overheads shouldn't be too bad insofar as they're subtracted from our compaction efforts.

It might be useful to favor tighter context memory, e.g. based on observed memory pressure. Graduate from a few megabytes up to a given maximum size. Tight computations would then have tight memory locality properties. I'm not entirely convinced of this route, but it's easy enough to apply monotonically and profile later, or to test if memory pressure is low and occasionally shrink the space in use.

A compacting collector seems a good fit for Wikilon's common use cases. It's a stop-the-world collector, albeit constrained to a single thread. This shouldn't be a major issue for pure computations (no side effects to hurt latencies). It is feasible to combine a compacting collector with free-lists, to reuse the small spaces. However, I have doubts that adding this would offer sufficient benefit to pay for the complications it introduces. A two-space collector has the advantage of being delightfully simple.

One concern is *parallelism*. With a two-space collector and bump-pointer allocation, I cannot efficiently support parallelism within a context. It would require too much synchronization, e.g. for bumping the pointer, for halting other threads during compaction, etc.. I can copy data to another context, however. And I could presumably leverage `{&par}` and `{&seq}` annotations to drive parallelism. 

#### Support for Shared and Non-Copying Objects in Memory?

Linear memory is convenient but it requires many deep copies. The ability to compute large objects without copying them every GC pass? It could be quite convenient, in general. *Stowage* sort of works that way, but has a large copy overhead per access. I need a lower latency option - objects *in memory* but not copied, i.e. in a separate region of memory. 

Some options:

* stable access to stowage via long-running transactions.
* cache stowed values in memory via some other mechanism.
* use the C heap (malloc/free). unfortunate fragmentation.
* allocate an address space per `wikrt_env*` for sharing among contexts.
* split a context's space into both a workspace and shared space.

I like the idea of modeling shared objects as a form of *stable access to stowed data*. With LMDB, we can use direct pointers into mmap'd regions so long as we hold onto our read-only transactions. LMDB would not recycle the associated pages of data. An advantage is that we automatically get flexible structure sharing. This idea does need us to release transactions periodically (e.g. upon GC) and probably to support ranged access to values. 

Copying and caching a value seems like a lot of extra work for what LMDB already does via mmap + read-only transactions. I'd be forced to implement an additional form of cross-context GC to manage this cache. Let's avoid this if viable.

The other three options suffer for lack of local value identity. It becomes difficult to maintain structure sharing across copies of the value and especially when moving data into and out of stowage. And we have a bunch of new GC challenges!

So, for now, I'm going to try for 'Shared Memory = LMDB Stowage (mmap + volatile RO transactions)'.

I'm leaning towards a per-context shared object space, which might be shared for lightweight internal parallelism. This could simplify GC and allocation issues, and would eliminate potential of runtime quota failures due to activity within other computations.

GC of a non-copying shared space is an issue. Either we use reference counts (in which case we cannot short-circuit `wikrt_drop()`) or we can scan a linked list against some root-set (e.g. a bloom filter).

Avoidance of deep-copy will be essential for performance of loopy code and JIT'd compiled code.

*Aside:* I've thought about trying to use the context 2-space for shared objects. However, working with forwarding pointers and different kinds of copies seems problematic. If I could ensure structure sharing for shared objects (e.g indirecting through a 'shared object' hashtable) that could potentially work, but I'd rather avoid sophisticated lookup functions to access the shared objects.

### Error Handling?

The main errors I can expect are: type errors and quota errors. Neither error is especially recoverable. Rather than assume recovery from errors, it seems wise to halt a runtime shortly after an error is recognized. The main cost here is that we cannot 

Rather than return an error from each step, it might be more efficient to accumulate error information then report it upon request. OTOH, this doesn't really change the number of checks I need to perform, e.g. for type safety and safe allocation. Even if I used C++ exceptions upon error, I'd still need to perform the conditional check that leads to the exception call. 

## Representations

For now, I'm using 32-bit words. I might try to create a 64-bit runtime later, but getting the current one working is more critical. Memory in the main (linear object) space will be aligned for allocation of two words. With 32-bit words, this gives us 3 tag bits per pointer. Small numbers and a few special constants are also represented. 

Example: 
* xy1 - small integers; plus or minus (2^30 - 1).
* 000 - tagged objects (tag, data...)
* 010 - pairs (val, val)
* 100 - pair in left
* 110 - pair in right

With 32 bits, I don't have much room for encoding special constants like unit. However, I could overload the zero address for this role.

* 000 - void, an invalid or undefined value
* 010 - unit
* 100 - unit in left (false)
* 110 - unit in right (true, end of list)

Sadly, overloading this address adds a bunch of conditional checks and potential pipeline stalls. I'd prefer to avoid this. With 64 bits, I could afford to dedicate fewer bits to small integers, or use an extra alignment bit.

Other than pairs, most things are 'tagged objects'. The type of a tagged object is generally determined by the low byte in the first word. The upper three bytes are then additional data, e.g. sizes of things or a few flag bits. 

Tagged objects will include:

* big integers
* deep sum values
* blocks of code
* sealed values
* stowage wrappers
* pending computations
* arrays, binaries, texts

After implementing a few, I feel it is essential to keep tagged objects structurally simple (even at cost to performance opportunities). I'll eventually want to extend these further: floating point numbers, vectors and matrices thereof, etc.. 

#### 64-bit Absolute Addressing Representation?

If I go for a 64-bit representation, I'd use absolute addressing instead of offsets into a region. I also want to eliminate unit, void, etc. as pointers - i.e. I should not need to test whether a reference is NULL. So I can use some extra tag bits for that.

I cannot use direct value representations for stowed values or blocks. That isn't necessarily a bad thing - less fragile to changes in a runtime implementation, at least!

Absolute addressing may complicate some things. Relative addressing makes stowage easy, for example: we can represent stowed values as copies into a distinct context with local addressing. With absolute addressing, we essentially need an alternative value representation for stowing values - something more structured than bytecode.

I can probably disable 'big numbers' and restrict myself to 'small numbers' with (for example) 61-bit representations.

### Big Integers

Integers in the range plus or minus `2^30 - 1` are recorded directly in a value reference. In a 64-bit implementation, this could potentially extend to about `2^62-1` (big integers become unnecessary for most computations). Small integers are sufficient for a lot of use cases: characters, indexing vectors, etc.. For larger numbers, I'll use a tagged value representation.

        tag word: includes size (in words), sign (1 bit)
        followed by sequence 32-bit words of given size
        each word encodes a 'big digit' in range 0..999999999

In a 32-bit implementation, the tag word may also include the lowest 'big digit' if doing so is convenient.

*Note:* There may be some benefit from using an intermediate sized integer, e.g. one cell for ~56 bits of data. However, after experimenting with this idea, I don't feel the space savings are sufficient compared to the complexity to deal with another case. Further, I find it unlikely that the optimization will be significant: the use of big integers of any sort is expected to be rare.

### Deep Sums

        tag word: includes an LRLRLLLR string (2 bits each, depth 1..12)
        second word is value reference

We'll use 2-bits per `L` or `R` in a deep-sum string, with up to 24-bits of tag data. This enables sums of up to depth 12 per allocation. The value in question may be yet another sum. Compacting deep sums into a single allocation should enable sums to serve a much more effective role as an alternative to enumerations. In a 64-bit system, this could be bumped up to depth 28.

Shallow sums refer to pair or unit in left or right, which is instead represented in the tag bits. Shallow sums are convenient as an optimization for representing lists and basic (node + leaf) tree structures.

### Blocks

An efficient representation for code will be essential for performance.

Naively, I could represent code as a simple list of operations. I could extend this via special objects for tokens and values. It might be convenient to start with a naive representation for code. But this representation won't be very efficient - expensive to copy and process, with poor locality properties.

An alternative is to develop a compact representation - a compact block of operations together with a stack of quoted values, a special case operator to pop one value from this stack. This might not even be a 'block' representation, but rather a list fragment representation similar to how I represent arrays and texts.

To get started quickly, I'll implement a 'naive' block representation, consisting of a list of opcodes and quoted values and a few header bits (affine, relevant, parallel, etc.). If I go for tracing JIT, keeping a copy count in the header bits might also be useful. A minimal, naive block representation:

        (block header, list of operations)

Operations in a block are either small integers indicating a single opcode (possibly an accelerator), or quoted values (block, text, partial evaluation, quotation, etc.). Fast composition may be represented as quoting and inlining a block. Our block header will contain a few flags (affine, relevant, etc.). This naive representation may prove convenient as a base representation for simplification and compilation. 

A challenge for this representation of blocks is *precise, efficient, dynamic* tracking of substructural type attributes. Internal substructure from partial evaluation as in `[[xyzzy]kf]` mustn't be confused with external sub-structure from quotations like `[xyzzy]kf' == [[xyzzy]kf]kf`. For the moment, I'll track substructure by recording a flag bit to track lazy substructure for quoted values.

Long term, I'll also want *compact* blocks (e.g. a compact text plus a stack of quoted values, or internalizing quoted values similar to stowage) and *just-in-time compiled blocks*. These will be essential for performance at large scale.

#### Performance for Tight Loops

Loops in ABC are performed by a fixpoint operation, which repeatedly:

* copies a block
* fixpoints one copy
* applies the other

For performance, it would be most convenient if we do not deep-copy larger blocks or subprograms. Instead, we favor a *shared object* between copies. It will be essential to have a decent shared-object model for this. 

When a block contains a representation for a complex value (potentially another block) I'll need some way to process values without extra copying. Thus, evaluation of the block needs to produce the value. And an inner block generated during evaluation can directly reference the outer block plus an offset or similar. Effectively, I need some compact representation of 'values' to go with other compact code. This particular aspect could be related to value stowage!

We can potentially have these shared objects be local to a context, i.e. keeping a stack of sorts. But having multi-context shared objects would have some advantages for performance (copying continuations, shared JIT efforts, etc.).

#### Reading and Writing of Blocks?

To incrementally read a large block, we should first translate it into a large text or binary. For symmetry, pushing a large block into our code should transform large texts or binaries. The choice of text vs. binary is a bit more questionable, but the performance difference should be negligible. Either should work easily enough.

#### Binding a Dictionary?

It might be feasible to bind a dictionary value to a context, i.e. such that the dictionary maps `{%word}` tokens to more bytecode. This could be achieved by a simple callback if I don't want to commit to stowed dictionaries. OTOH, this isn't how I want to handle dictionaries long term. Or at all, really.

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

Besides active computations, I expect I'll be wanting parallel or lazy computations eventually. Originally, I was imagining that I'd handle these behaviors almost transparently. However, in retrospect, I don't believe this is a good idea. I'd prefer to avoid any transparent translations between representations because those greatly complicate the runtime and reasoning about progress, quotas, etc..

I've decided instead to update the documentation of ABC to describe 'coupled' annotations. So, for laziness, I might require a user couple it with an annotation to force a computation. This way, I don't need to check for each operation whether I have yet to evaluate something or other. Similar, synchronization for parallel computations would be more precise.

It might also be wise to conservatively treat pending (lazy or parallel) values as linear with respect to copy and drop operations. This enables use of sequencing to copy a value. Copying of parallel values may be problematic for other reasons, though.

Do I really need parallelism within a computation? Hmm. It could be very useful, for scalability, to have computations that pick apart a much larger database. So, I suppose it may be worthwhile?

With coupled annotations, I can focus most runtime code on just a fast straightline interpreter (with very few extra conditional behaviors). Even better if I can later eliminate gateway checks for type safety, though that would probably require JIT.

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

Probably a simple variant of: 

        (binary, size, (pointer to) buffer, next)

The pointer to a buffer enables breaking a binary into slices.

#### Texts

An idea for texts is to use a 'split' size information. E.g. if I used 16 bits for the number of chars and 16 bits for the number of bytes, I get a total 32 bits of size info... and text chunks of no more than 65536 bytes. Since most splitting/slicing/indexing/etc. will need to scan all or part of a buffer, this gives me an implicit buffer index based on our chunk sizes, i.e. enabling me to avoid scanning most of each buffer.

### More Numerics

ABC supports integers. But other numbers could be supported via arithmetic accelerators or sets of dedicated ops.

* I want good support for vectors, matrices, and related math operations.
 * Vector arithmetic with SIMD support, etc.
* Good support for floating point would be nice.
 * John Gustafson's open interval arithmetic 'unum' is also nice.


## Evaluation and Time or Effort Quotas

One of the bigger challenges will be supporting evaluation limits, i.e. effort quotas. 

## Parallelism

Parallelism is perhaps most easily handled with paired annotations, e.g. a `{&fork}` annotation coupled to a `{&join}` annotation. The `{&fork}` annotation might apply to a lazy computation (like in Haskell) or perhaps as a specialized block attribute. 

With parallelism and semi-space GC, I'll probably need *multiple* contexts. Or perhaps to (somehow) divide an existing context into several smaller fragments. Division of a context into multiple parts is feasible, at least. But it seems likely to become overly complicated. It should be a lot easier to just have a pool of small contexts for parallelism at the wikrt_env layer, or to create fresh contexts as needed.

## Static vs. Dynamic Typing?

I'd like to eventually have access to an "assume static type safety" option for faster evaluation, i.e. skipping all the runtime checks. However, this might only work for compiled code. I might also need to add gateway checks for inputs.
































# older content



## Tagged Objects

Eventually, I may support floating point numbers, vectors, matrices. I'd love for Wikilon to become a go to solution for scientific computations (perhaps distributed on a cloud and leveraging GPGPU). These are frequently expressed in terms of vector and matrix computations.

*Aside:* I had an earlier concept of enabling arbitrary values to be reference counted. However, this idea doesn't have very nice predictability properties, especially in context of parallelism. Fortunately, *large value stowage* serves the same role of limiting depth of copies, and does so in a manner more comprehensible to users (conceptually, just copying a stowage address).

#### Binaries

We can specialize for a list of bytes. A 'byte' is simply an integer in the range 0..255. 

        (binary, size+offset, pbuff, pnext)

The annotation `{&binary}` is taken as an assertion of both binary type and desired representation. If an argument is not a valid binary, this assertion will fail (statically or dynamically). The weaker `{&binary~}` allows the runtime to use a chunked list representation and allocate in the nursery region. 

I'd like to support arrays of other fixed-width structures: floats, fixed-width integers, combinations of these, etc.. But I'll first need to model these fixed width structures. 

#### Texts

Text is represented by a list of unicode codepoints, with a short blacklist: C0 (except LF), DEL, C1, surrogates (U+D800-U+DFFF), and the replacement character (U+FFFD). The `{&text}` annotation will compact a list into a UTF-8 binary, or perhaps a chunked list thereof. 

Due to the variable size of characters, utf-8 texts cannot have array performance characteristics. But with a little indexing, we can support skipping through and splitting large texts far more efficiently than we would achieve with linear scanning. 

# Accelerated Association Lists? (low priority)

It might be useful to heavily optimize an associative structure, e.g. the equivalent of a JSON Object. Motivations include:

* fast indexed lookup without disassembly of the structure
* optimized representations for small vs. large structures
* implicit batching of updates, log structured merge trees
* type covers a lot of use cases: records, databases, etc.
* provides a built-in, optimized model for AO dictionaries

An accelerated associative structure should have a canonical form, a representation deterministic from *content*. This excludes balanced binary search trees because their precise balancing structure depends on insertion order. However, we could use a *trie* or a *sorted association list*. 

Between these, the sorted association list is the superior option for acceleration. It is the simpler model. It allows many accelerated lookups and updates, ad-hoc representation (structs, tries, log structured merge trees, etc.). Further, it fits naturally with other list accelerators, e.g. as a basis for iteration.

I'll need to return to this concept later. I think supporting this idiom in both the representation and type system could greatly simplify modeling of more conventional programming models (OOP, stack frames, etc.). But I also need to be sure it is simple to implement and won't interfere with, for example, structure sharing and stowage. And of course I'll need to develop a proper set of annotations, assertions, and accelerators to make it worthwhile.

As a nice generalization of association lists, maybe we could try to optimize representation of 'tables' where we know each element has the same basic row structure. A list of rows might be represented by a row of lists.

# Large Value Stowage

We'll use 62-bit identifiers for stowed values. Addresses are allocated once and never reused. This is convenient from a security and caching perspective: we can securely share stowed data with external systems via simple HMAC. And there are no worries about running out. Allocating 2^62 addresses at the best throughput LMDB can manage today would take almost a million years.

The reason for 62-bit identifiers is that we can use the remaining two bits for substructural attributes: affine, relevant. This allows us to cheaply validate simple copy/drop data plumbing without connecting to the database. It also corresponds nicely to ABC resources where we might use `{#resourceId'kf}` to indicate the identifier constructs a linear value. 

Developers may represent intention to stow any value by simple annotation, `{&stow}`. 

This results in a simple object in memory: `(stow, target)`. No action is performed immediately. Many stowage requests will be transient, e.g. when applying a stream of updates to a trie represented in our database. So we'll provide ample opportunity for transient requests to be destroyed. Any subsequent access to the target will delete the stow request. Eventually, our `(stow, target)` object is moved from the nursery into the heap. 

At that point, we'll construct a pending stowage object. We cannot perform compression and compaction, unfortunately, because our target may refer to other stowed objects in pending state that still lack an address. A pending stow is still transient, but a background thread may decide to stow it at any time. Hopefully, we can batch a lot of writes.

After stowage succeeds, the target data is cleared from memory. I'm not going to bother with caching content in memory, instead focus on fast loading of data on demand. (The owner-based purity I'm favoring doesn't benefit as much from caching anyway, and we effectively have caching via LMDB.) We'll have a simple stowage identifier. Whatever bindings we need to integrate LMDB's GC so we don't delete data that is rooted in memory. Fortunately, our writer knows that nobody is resurrecting old addresses because our writer assigns all addresses.

If stowage fails (e.g. because there isn't enough space) we could 

We might heuristically refuse to stow smaller fragments, such that we implicitly 'flatten' narrow, tree-structured data. It could be useful to focus stowage on larger chunks, e.g. kilobytes of data.

### Sealed Values

We'll only support discretionary sealing. Sealed values are thus straightforward: 

        (sealed, target, symbol...)

We'll probably just use a (size,utf8) for our symbol. We know from AO constraints that our symbol encoding is no more than 63 bytes UTF-8. For sealers of up to 7 bytes, we can use just two cells. Developers can leverage this effectively.

### Blocks, Fixpoints, Compiled Code

A good representation for blocks is critical. What properties do I need?

* O(1) fast composition
* O(1) fast quotation of values
* fast copy for loopy behaviors
* very fast interpretation
* (eventual) LLVM compilation!
 * with nice gateway analyses
 * with 

In the interest of getting started quickly, I'll deep-copy blocks like everything else. However, reference counts, aliasing and GC might do me much good here if it means I can reduce the number of actual copies of a function in memory or reduce memory-allocation burden. So, long term, this clearly is something to look into.

If I assume reference counts, this means 'fast composition' cannot be modeled with internal mutations. But I could still have a O(1) composition as a pair of blocks. 


Blocks are copied very frequently in loops. We process them incrementally, and we'll need to integrate them easily with LLVM compiled code. For debugging, it would be nice to have some *metric* as to where an error occurs, even if it's a rough metric: replay-based debugging would have us go to a point prior and try again in a debug mode.

Our blocks need at least:

* a bytecode representation 
* a stack for quoted values

Our stack can be modeled by a simple list and our bytecode by a binary. I'll be leveraging an internal bytecode representation, including accelerators and possible fast slicing for content. 

Compilation to native code requires extra consideration. Apparently, many operating systems enforce that memory cannot be both writable and executable. But I could probably keep LLVM bitcode together with a compiled block. The bigger compilation challenge is modeling the LLVM continuation as needed. 

Fixpoints can probably be optimized heavily. We can eliminate most overhead per loop, validate only once that our block is copyable, etc.. We also know to optimize for looping, e.g. to pull evaluations out of the loop where feasible.

*Note:* compilation for external runtimes (apps, unikernels, etc.) will be modeled as an extraction, distinct from Wikilon's internal runtime. My goal with compiled code is to make Wikilon fast enough for lots of immediately practical uses and eventual bootstrapping.

### Numbers other than Small Integers

ABC is required to support arbitrary precision integers. I would like to use a simple representation in this case. I'd also like to squeeze some extra data into our type tag.

A binary coded decimal representation is a viable option for fast translation, i.e. encoding 3 digits for every 10 bits, or perhaps 9 digits every 30 bits. But I'll need to determine how this affects performance of multiplication, addition, and division.

Eventually, I would like to support fixed-width numbers via accelerators: modulo arithmetic, models of floating point, etc..

### External Resources

For some use cases - scientific computing, gaming resources, etc. - I imagine there will be use for binaries multiple GB in size. Via accelerators for indexed access, slicing, lookups, in place updates, etc. it seems feasible to interact with many external resources the same as structures held within the context. 

A few external resources could greatly extend our 4GB arenas for practical use cases. And they could be more or less transparent, if we have specialized value stowage for large binaries or similar.

## Par/Seq Parallelism

The simplest parallel computation probably involves applying `{&par}` to a block before application. It might be useful to support a few variants, or alternatively to optimize parallelization on fixpoint blocks.

It will be important for parallelism to be *very* lightweight, especially with respect to time. This enables parallelism to be used in more cases or finer granularities. If overhead for parallelism is high, we are forced to more severely constrain parallelism. 

Additional parallelism may later be driven by accelerators, e.g. for matrix multiplication or compositions of FBP/FRP-like streaming dataflows. 

### Distributed Parallelism

Can we model parallelism distributed across multiple contexts, with dataflows between them? This seems like a good fit for the 'partitions' concept I developed in RDP, e.g. with annotations moving data between partitions for different parts of the computation.

## API

I'd like to get Wikilon runtime working ASAP. API development must be balanced with time to a usable partial solution.

The first big problem is IO. I have this context. I need to inject data into it, and extract data out from it. One option, perhaps, is to focus on streaming bytecode. We can inject a stream of bytecode, apply it, and extract data as another stream (via quotation of a block). But I have some doubts about this approach... mostly, it seems difficult to use the data meaningfully without processing it again - this time outside of the context, which largely defeats the purpose...

My other option is to enable entry for common data types: integers, lists, texts, pairs, etc.. And also fast extraction of them. 

### Time Quotas?

Wikilon will need to abort computations that run too long. Use of some sort of time or effort-based quota is relevant. It might also be useful to perform 'incremental' computations that only run so long between steps, enabling intervention or re-prioritization.

Incremental time quotas are challenging because I must have a well-defined intermediate state that may be continued. It is feasible to only 'halt' on quota at well-defined locations, but it might require extra work for compiled loops (maybe some sort of voluntary yield and continue for loops?)



Still need to think about this one.

### Context and Environment Management

Ability to create an environment and multiple contexts, and interact with them. Also, a simple API for persistent data is important - e.g. a key-value database with atomic transactions will do nicely.

### Evaluation with Dictionaries?

It could be useful to associate a dictionary with our evaluations, where we know how to read the dictionary. This would require a 'standard' representation for dictionaries in stowage... which is doable, especially if we accelerate association lists.

### Direct Value Manipulations

Ability to observe, construct, and manipulate values directly would be useful when interfacing between a Wikilon context and some external resources. 

Originally I planned for non-destructive views on values. However, this seems... complicated. It is inconsistent with Awelon Bytecode, move semantics, pending computations, parallel effects (e.g. a debug `{&trace}`). Also, there's little point to non-destructive access unless it's also non-allocating, but it's difficult to (for example) access a few steps in a deep sum value without either destroying it or allocating a new sum object.

So, value manipulations will all be destructive with ownership semantics. Developers will need to copy values if they want a safe reference to the original.

### Large Binary Inputs and Outputs

Something that has concerned me is how Wikilon will interact with large binary content. Working directly with multi-gigabyte binaries is problematic if I restrict to 4GB working spaces. Modeling very large binaries as external resources might help, but I'm not comfortable with consuming arbitrary amounts of address space. I can used `mmap'd` files or whatever to avoid loading them all at once. What other options do we have?

One viable possibility is to have Wikilon understand a relatively simple model or API for ropes or streams, or perhaps even HTTP request handlers, with which we may easily wrap whichever models we favor within our dictionary. 

This would enable large binary *outputs* to be loaded (perhapse even computed) incrementally. Ropes would have the advantage of enabling indexed access, answering those region-based HTTP requests, etc.. Large binary *inputs* are probably less an issue because we simply have some software agent representing the binary within the dictionary in an ad-hoc way (ropes or streams or whatever). 

I think I'll try that route, rather than attempt to solve the problem of representing and directly streaming very large binaries.

## Other Ideas

### External Parallelism?

At the moment, a 'context' is held by only one thread. I support internal parallelism by `{&par}` annotations. External parallelism could be useful, however, e.g. to enable parallel rendering of a value while computation is ongoing. I could enable this with something like `wikrt_cx_fork(wikrt_cx*, wikrt_cx**)`. Or I could potentially enable values to be moved between contexts?

### Moving Values between Contexts?

Should I have a function for this?

        wikrt_err wikrt_move(wikrt_cx* src, wikrt_val, wikrt_cx* dst, wikrt_val*);

I'm tempted to require that all movement of values between computations be processed through a transactional database system. This would structurally guarantee some nice consistency properties, should not hinder development of transactional queues or pipelines, would enable better logging and histories, etc..

In any case, this would be very low priority. Wikilon itself doesn't need this features.

## Dead Ideas

### Array Stacks

The array representation could feasibly be applied to the `(a * (b * (c * (d * e)))))` stack-like structure. In this case, however, we might need to focus on rapid increase and decrease in the stack size, i.e. by providing empty space for a stack to 'grow' or 'shrink' with push and pop operations. And there is almost no deep indexed access. I think the benefits are likely to be marginal especially since we already support mutation for most stack ops.


