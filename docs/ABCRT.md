
# ABC Runtime Design

As per [my performance strategy](Performance.md), I'm developing a C runtime: a good interpreter, with room for LLVM based JIT compilation. 

One motivation for the C interpreter is that I need a simple, low-level data representation for the LLVM JIT. I'll be using something very like the Lisp/Scheme representation, consisting mostly of simple pairs of words.

It may be useful to also develop a command-line interface at this layer, if I'm willing to encode Claw code and dictionaries at this level.

## Design Goals

Performance is the primary goal. But some others:

* suspend, checkpoint, persist, reload, resume, copy computations
* easy to resize, reflect, render, compact suspended computations
* monadic effects: return control of program environment to caller
* easily integrate monadic effects with [claw command sequences](CommandSequences.md)
* on errors or unrecognized tokens, suspend and return to caller
* precise memory control: model machine's memory in one arena
* hard real-time capable: predictable timing and garbage collection
* no callbacks or handlers, no external dependencies for progress

Also, it might be useful to support a second evaluation mode for type inference.

## External Utilities

I'll actually include a copy of these directly, no shared libs.

* LMDB - embedded key value database
* Murmur3 - fast, collision-resistant hash 
* ZSTD(?) - real-time, streaming compression
 * very promising, but not entirely stable

## Environment, Contexts, Memory Layout

A toplevel *environment* will track: 

* LMDB integration for stowage
* A set of computation contexts
* A pool of worker threads for sparks
* All needed semaphores and mutexes

This allows multiple contexts to interact with the same LMDB environment and share labor for parallel sparks. Sharing the LMDB environment is critical. Sharing worker threads makes sense because our machine has a limited amount of physical parallelism and I can easily schedule it myself.

A *context* must track:

* Multiple active computations (roots and par/seq)
* A set of nursery regions for active computations 
* A main heap shared by all computations, tenured data
* A set of pending parallel computations
* A set of objects proposed for stowage

We can create a big context for a lot of computations, or many smaller contexts. The latter gives better control over memory constraints per computation. But shared contexts might enable more effective sharing of cached data from stowage. Pointers within a context are internal. 

Computations - including stacks and continuations - shall be modeled entirely within our contexts. Computations may exist in a suspended state. A fully suspended computation will release its nursery (tenuring all associated data), but we could support an intermediate suspension state for monadic effects and the like.

I'm not sure about modeling 'thread contexts' as a separate data structure. It might be better to model the entire thread context as a normal data structure (modeled in terms of cells, arrays, etc.) subject to GC. This would make suspending a computation more precise, and might improve reflection on our computations.

We could dedicate a page to context within the memory region itself, e.g. the free list, pending computations, active computations, stowed values, available nurseries, etc.. Modeling the entire computation state in a closed system seems like it should be useful for debugging, suspending, saving and restoring, etc. our computations.

For "effects" integration, I'll favor monadic programming with RESTful interactions: mailboxes or queues for eventful updates, pubsub arenas for watching external data, etc.. similar to how I model this stuff internally within dictionary applications. 

I'm not sure what I want to do with nurseries, except perhaps to configure them on a per-context basis. A dynamic set of nurseries is tempting, but I don't think it offers sufficient benefits to really pursue.

There will be no callbacks. 

## Addressing, Pointer Layout, and Size Limits

### Word Size?

In favor of 32 bits:

* 4GB of memory is a lot for any one Wikilon computation.
* LMDB stowage mitigates need for large values in memory.
* Binaries, texts, bytecode could use external resources.
* Model larger computations with distributed parallelism.
* Tight addressing improves memory locality and caching.
* Easy to upgrade to 64-bit runtime later, if necessary.

In favor of 64 bits:

* Simplicity: scale without adding annotations to code.
* Extra tag bit: eliminate header for more value types.

For Wikilon, I think the 32-bit option is favorable. I doubt I'll want to offer more than 4GB server space to compute a single webpage, for example. And the pressure for users to leverage stowage isn't a bad thing. If implemented well, stowage should be very efficient. Unlike virtual memory pages, stowage frees up the address space.

That said, the ease of scaling makes the 64-bit option very tempting. I occasionally read about smart phones with 32GB RAM, or servers with 512GB RAM. While stowage can certainly take advantage of large amounts of RAM, it still involves repeated copying of values.

There is potential for large binaries to be modeled as external to our 32-bit context. I'm not yet convinced this is worth the extra complexity outside of some special circumstances, but it could prove a powerful technique and would effectively extend our runtime context to 64-bits for shared binaries.

### Pointer Layout

With 32-bit addressing aligned on 64-bit cells (representing pairs and lists), 3 bits in each address can be leveraged to optimize common representations. The encoding I'll be trying:

* xy0 - small integers
* 001 - tagged objects
* 011 - pair of values 
* 101 - pair in left
* 111 - pair in right
* NULL address - unit

This gives us compact representation for pairs, units, booleans, lists, simple trees (e.g. node+leaf), and small integers within range of plus or minus a billion. Anything else is represented by tagged objects.

For tagged objects, the first word provides information about how to interpret the remainder of the object. The presence of tagged objects does complicate GC because we cannot interpret a pair without knowing an extra bit (literally, one bit) of information from its reference. OTOH, this simplifies normal processing because special cases are recognized before we've loaded the value from memory.

## Memory Management

Awelon Bytecode has manual memory management via explicit copy and drop operations (`^` and `%`). The normal problems of manual memory management are mitigated: leaks are less likely because they're visible in the program environment and type system, and dangling references aren't possible because there is no mutable state. 

To get started quickly (spike solution, etc.), I should probably just leverage normal manual memory management techniques - a free list, etc.. 

But long term, I believe that I would benefit from bump-pointer per-thread allocations and compacting collections. This theoretically should avoid synchronization, reduce fragmentation, improve memory locality, and generally enhance performance. When moving large amounts of memory from the nursery into the main allocation space, I'd also avoid lots of fine-grained allocations.

How this works: a computation allocates from a nursery, then when full it moves living content to another nursery.

I would like to try, for example, a 1MB nursery per thread. We can use a semi-space GC method within the nursery, such that we're only using 512kB. This 512kB could further be divided into an allocation space (128kB) and a survivor space (384kB), where our survivors have survived at least *two* allocation-space collections. For the allocation space, we'll track the watermark from the prior copy, and only content from below this watermark that survives the next pass may be moved to the survivor space. The survivor space may then use a similar technique for deciding what may move to the main heap.

In both cases, we may use height of prior watermark as a pressure heuristic to predictably decide when to promote content to the next generation. Any percentage-based clearing of GC stages (to a final non-copying space) is sufficient to guarantee constant-time amortized overhead for GC.

Good properties for compacting algorithms:

* after compaction, data references point backwards
* lists compact so spine and referenced data separate.
* lists feasibly compact directly into data arrays. 
* tree structures compact along the tree branches.

This same compacting algorithm should also work for moving surviving content into the main heap, leveraging 'slab' allocations. All of this is feasible, but it requires a stack. Fortunately, we can make some guarantees about stack size, perhaps use the empty half of the nursery as our stack.

Do we want more than two stages? I think the benefit is marginal at that point. 

### Memory Allocation Algorithms and Structures

With par-seq parallelism, I'll need a good multi-threaded allocator. TCMalloc is one promising option in this arena. However, a lot of algorithms, including TCMalloc, don't work nicely with the idea of splitting and recombining structures (arrays, etc.) or of fast slab-allocations in context of ABC update patterns.

I need the following properties:

* I can allocate a large space and free smaller fragments of it
* When we free adjacent spaces, we can eventually coalesce them
* We can easily find space of a given size
* Minimal synch between par-seq threads

Ability to free fragments of a larger allocation is important for the ability to split/recombine arrays, and also to perform fast reads from stowage: allocate everything you'll need at once, then fill the space. The last point - minimizing synchronization - is feasible if we simply give each processor its own free-list, and allocate in relatively large chunks from a shared page-heap or similar. This is what TCMalloc does, for example. It works well.

For the other stuff: some ad-hoc mix of QuickFit and perhaps a few size-segregated first-fit free lists seems appropriate.

Or I might benefit from a simple 'bump-pointer' allocation together with a deferred coalescing model for free'd content. Hmm. The latter option does seem promising. So let's say we do this:

* we have size-segregated allocation-lists.
 * 
* we have a 'free list' for all free'd content.
 * when we free a block, it goes on this list
 * when this list reaches a certain size:
  * sort, coalesce, push back into alloc lists

For the other stuff... a variation of QuickFit seems appropriate. I can use a size-sorted array of free lists, or similar. Coalescing of lists can occur as needed.

*Note:* I'd prefer to avoid tree-structured free lists, in part because we cannot iterate through them in constant space, in part because balancing them requires relatively sophisticated algorithms. Let's see how far we can get with singly linked lists, e.g. of (size,next) pairs.








For the other stuff, I'm thinking QuickFit is pretty good for my use cases. This means, ideally, that we have a separate linked list for every 'size' we might allocate. But we could probably compromise, and use a set of 'size classes' each with a separate free list, so at least we aren't searching through a bunch of too-small or too-large


        
        
        


A good question is how to quickly find a node of a given size

The greater challenge is to enable coalescing of free'd space together with fast, sized allocations. I essentially need size-sorted structures, plus some ability to find adjacent addresses. 

From a given address+size, it should at least be easy to find the 'next' address and find it in our free list. But this seems like too much work for a fast free operation, and might result in too many splits upon allocation. 

A more promising option, perhaps, is heuristically 'defrag' as a batch process. This wouldn't require indexing both size and address at the same time. A defrag would be driven by some combination of requirement and heuristics. As a batch process, defrag should have relatively good throughput and performance compared to lots of small coalescing operations.

So... maybe size-sorted free lists, like 'QuickFit' algorithm, with occasional coalescing.




 I can create an array of free addresses, sort them, coalesce adjacent addresses (assuming each free address knows its own size, e.g. as the first word), then free the data. 

 free some count (or so many megabytes) of 'chunks'. This could be achieved by a simple algorithm: create a big array, sort it by address, combine adjacent free addresses.
Each 'address' in our free list could use a simple pattern: (size, next) in a free list. Assuming we know how many free chunks we have, we should be able to easily find  


One option, perhaps, is to focus on 




seems a lot more challenging.




I don't have any special ideas here. To avoid synchronization, I may track a per-thread free list, rather than using the shared context list for everything. I'm wondering whether I should use a weighted or Fibonacci buddy system.

## Tagged Objects

A tagged object is represented in memory with the first word (32 bits) indicating how the following words are understood. It's important that tagged objects be simple. Wikilon's GC must understand them.

Important tagged objects include:

* deep sum values
* blocks of code
* large integers
* arrays, binaries, texts
* stowage wrappers
* pending computations
* sealed values

Arrays, binaries, and texts are compact representations of *lists* (i.e. type `∃a.∃b.μL.((a*L)+b)`). Binaries and texts additionally constrain the element type (to a subset of small integers). Taken together with accelerators (strings of bytecode that we reduce to a single operator under the hood), we may achieve very high performance access and even in-place update for unshared arrays.

Eventually, I may support floating point numbers, vectors, matrices. I'd love for Wikilon to become a go to solution for scientific computations (perhaps distributed on a cloud and leveraging GPGPU). These are frequently expressed in terms of vector and matrix computations.

Some tagged objects - blocks, arrays, stowage, pending - might be shared or aliased. In these cases, they may contain reference counts under the hood. 

*Aside:* I had an earlier concept of enabling arbitrary values to be reference counted. However, this idea doesn't have very nice predictability properties, especially in context of parallelism. Fortunately, *large value stowage* serves the same role of limiting depth of copies, and does so in a manner more comprehensible to users (conceptually, just copying a stowage address).

### Deep Sum Values

Deep sums are represented by a tagged object, with our sum path (such as `LRLLLRLRRL`) represented directly in the tag. The low eight bits will indicate that our object is a sum, then we'll use two bits per path item (up to depth 12), e.g. `LRLLRLRLRRRL` could be compacted into one tagged object. Deeper sums would just consist of a chain of deep sum objects. We'll always compact sums as much as possible.

This technique conveniently enables developers to treat deep sums as little different from tagged unions with implicit flattening... even when the number of tags is pretty large.

### Blocks of Code

How should we represent blocks of code under the hood?

The technique I was using earlier was to compose a binary for bytecode plus a stack for values. The values are popped off the list as needed. I was also using a technique that would allow 'lazy' computations of values, but this is unlikely to remain a useful technique without as much value sharing.

### Arrays and Chunked Lists

An array is an optimized representation for any structure of form: `μL.((a*L)+b)`. This includes normal lists, but also heterogeneous lists, and lists that do not terminate with unit. 

Compared to a list, arrays reduce memory overhead by half and ensure tight memory locality. But the primary benefit regards how easily common list functions can be accelerated when applied to an array: O(1) length, O(1) indexed lookups and updates, O(1) logical reversal, O(1) split and seamless append, and so on. 

The proposed representation for arrays is:
                
        (array, size+offset, pbuff, pnext)
            size+offset → small int
                28 bit size in bytes
                3 bit offset in bytes
                word-aligned for arrays
            pbuff → 
                (111) contiguous memory
                (001) special case tag
                  e.g. refct for sharing
            pnext → whatever follows

*Note:* It might be worthwhile to represent smaller and simpler arrays (limited size, simple slicing, terminating with unit in right) using only a single cell overhead. 

This tagged object actually represents an `(a * List a b)` pair. We'll further wrap it with another cell for the `inL` sum type. Together with this sum tag, the overhead for an array is 6 words. Lists require two word per item, so we break even on storage costs when encoding 6 items.

Annotation `{&array}` asserts the argument is a list of values and compacts this list into an array with a single contiguous buffer with `pnext` pointing to the terminal value (usually `unit inR` for a plain list). If allocation fails (e.g. due to memory fragmentation), or if the argument is not a valid representation of a list, computation will halt. 

I might enable a weaker `{&array~}` to support compact lists with array-like segments, or 'chunked lists'. Chunked lists are a high performance data structure, sufficient for many applications of arrays.

*NOTE:* Explicit arrays via `{&array}` must be allocated on the heap. Copy-collection within the nursery interferes with preserving a seamless underlying structure if it occurs during a logical split. The `{&array~}` constructor doesn't have this limitation, and should be favored for small or transient arrays.

#### Seamless, Unshared, and Affine Arrays

As far as ABC semantics are concerned, arrays are plain old lists. We can transparently append them to a list, for example. But from a *performance semantics* perspective arrays and lists diverge wildly. To protect performance semantics, I propose two annotations: `{&seamless}` and `{&unshared}`.

When the array is encoded as one large chunk, and `pnext` points to the terminal value (no further list elements), we call this a *seamless* array. Seamless arrays guarantee many O(1) accelerated operations. This is valuable in many contexts. We provide a `{&seamless}` annotation to assert that an array is seamless. 

Unshared arrays can be split, seamlessly appended, and updated in place, each in O(1) time. These functions provide a powerful foundation for conventionally imperative algorithms and data structures: union find, hashtables, abstract register machines. They also enable logically reshaping arrays into matrices and back. To protect these properties, the `{&unshared}` annotation will assert that our buffer is not shared.

Both `{&seamless}` and `{&unshared}` can be validated dynamically, and efficiently. But we can also give these annotations static semantics for a linter or typechecker, e.g. so we get a compile-time error indicating where sharing was introduced.

As a simple technique, we can also explicitly mark arrays 'affine' by replacing the typical termal unit value with `[]f`. This prevents accidental copy and sharing of the array. 

#### Shared Array Buffers 

The copy function `^` will recognize arrays and wrap a reference count around the buffer. Accelerators can recognize this special case, i.e. such that we still have O(1) indexed lookup and non-copying fold/foreach, etc.. It is possible for a chunked list to have different sharing for each chunk. When updating a shared array, we'll implicitly perform a copy-on-write. 

By default, once a buffer is shared, it remains forever shared. Even if the reference count is reduced to 1, we'll perform copy-on-write and the `{&unshared}` annotation will reject. The goal with this policy is early detection of erroneous conditions, e.g. race conditions with par/seq parallelism or potential for future reordering optimizations.

When developers are confident in their assumption, they may use the `{&unshare}` annotation. This allows developers to indicate that a buffer should no longer be shared. Making this assumption explicit is a useful hint for static analysis. Dynamically, the annotation will validate ownership and remove the refct wrapper, returning the array to an unshared status.

#### Logical Split, Seamless Append, Alignment.

A logical split involves taking our array and constructing two non-overlapping arrays that point to the same underlying buffer. Logically, there is no sharing, so we can continue to update the component arrays in place. Later, if we append the two arrays along the same edge that we divided along earlier, we can transparently recombine into a single buffer.

In the ideal case, our logical division is aligned with memory. When this happens, our buffers are truly independent - not just logically, but also in terms of GC. However, in the worst case, our divided buffer will share cells at each edge. We must determine whom is responsible for destruction of the shared cell.

To handle this, one idea is to wrap the buffer like with do for shared buffers, except specialized for sharing edges of the array. This might look like:

        (overlap, pbuff, pshareL, pshareR)
            pshareL, pshareR: one of
                tagged (refct, unit) 
                unit (if no overlap)

If we delete our array, we'll decref our shares of the cells at each edge. If we held the last share, we'll take ownership of the associated cell and include it in our argument to the free() function. Otherwise, we leave it to the other shareholder. If instead we recombine our arrays, we can eliminate the share between them.

Alternatively, it might be better to recognize potential overlap upon destruction, and keep some extra metadata per context for tracking 'shared' cells only when destruction occurs. This is a tempting option.

*Note:* The overlap object can be omitted for arrays hosted in the nursery.

#### Logical Reversal

Logical reversal of a chunked list is achieved flipping a bit in the tag of each array chunk (so we have a `reversed array`) together with a conventional reversal of the `pnext` list. The `size+offset` values are not modified. We simply compute our index or pop data from the opposite end of the array.

We reverse *onto* a value, and simultaneously we expose the terminating value. Reversing lists serves as a basis for appending lists or accessing terminals. (Though we'll also have accelerators for those specific cases.)

#### Binaries

We can specialize for a list of bytes. A 'byte' is simply an integer in the range 0..255. 

        (binary, size+offset, pbuff, pnext)

The annotation `{&binary}` is taken as an assertion of both binary type and desired representation. If an argument is not a valid binary, this assertion will fail (statically or dynamically). The weaker `{&binary~}` allows the runtime to use a chunked list representation and allocate in the nursery region. 

I'd like to support arrays of other fixed-width structures: floats, fixed-width integers, combinations of these, etc.. But I'll first need to model these fixed width structures. 

#### Texts

Text is represented by a list of unicode codepoints, with a short blacklist: C0 (except LF), DEL, C1, surrogates (U+D800-U+DFFF), and the replacement character (U+FFFD). The `{&text}` annotation will compact a list into a UTF-8 binary, or perhaps a chunked list thereof. 

Due to the variable size of characters, utf-8 texts cannot have array performance characteristics. But with a little indexing, we can support skipping through and splitting large texts far more efficiently than we would achieve with linear scanning. 

### Accelerated Association Lists? (low priority)

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

### Large Value Stowage

We'll use 62-bit identifiers for stowed values. Addresses are allocated once and never reused. This is convenient from a security and caching perspective: we can securely share stowed data with external systems via simple HMAC. And there are no worries about running out. Allocating 2^62 addresses at the best throughput LMDB can manage today would take almost a million years.

The reason for 62-bit identifiers is that we can use the remaining two bits for substructural attributes: affine, relevant. This allows us to cheaply validate simple copy/drop data plumbing without connecting to the database. It also corresponds nicely to ABC resources where we might use `{#resourceId'kf}` to indicate the identifier constructs a linear value. 

Developers may represent intention to stow any value by simple annotation, `{&stow}`. 

This results in a simple object in memory: `(stow, target)`. No action is performed immediately. Many stowage requests will be transient, e.g. when applying a stream of updates to a trie represented in our database. So we'll provide ample opportunity for transient requests to be destroyed. Any subsequent access to the target will delete the stow request. Eventually, our `(stow, target)` object is moved from the nursery into the heap. 

At that point, we'll construct a pending stowage object. We cannot perform compression and compaction, unfortunately, because our target may refer to other stowed objects in pending state that still lack an address. A pending stow is still transient, but a background thread may decide to stow it at any time. Hopefully, we can batch a lot of writes.

After stowage succeeds, the target data is cleared from memory. I'm not going to bother with caching content in memory, instead focus on fast loading of data on demand. (The owner-based purity I'm favoring doesn't benefit as much from caching anyway, and we effectively have caching via LMDB.) We'll have a simple stowage identifier. Whatever bindings we need to integrate LMDB's GC so we don't delete data that is rooted in memory. Fortunately, our writer knows that nobody is resurrecting old addresses because our writer assigns all addresses.

If stowage fails (e.g. because there isn't enough space) we could 

We might heuristically refuse to stow smaller fragments, such that we implicitly 'flatten' narrow, tree-structured data. It could be useful to focus stowage on larger chunks, e.g. kilobytes of data.

### Pending Computations

Threads themselves will need to be modeled. In terms of data, we at least have an argument and a continuation. Active computations will have a nursery. We'll need to track computations that are *suspended* because they're waiting for this one. We'll probably want to track all other computations created by this one, hierarchically.

A pending computation will have a reference count. When we *copy* or *drop* a pending computation, we'll track this assumption rather than immediately suspend on it.

### Sealed Values

We'll only support discretionary sealing. Sealed values are thus straightforward: 

        (sealed, target, symbol...)

We'll probably just use a (size,utf8) for our symbol. We know from AO constraints that our symbol encoding is no more than 63 bytes UTF-8. For sealers of up to 7 bytes, we can use just two cells. Developers can leverage this effectively.

### Blocks, Fixpoints, Compiled Code

A good representation for blocks is critical. Blocks are copied very frequently in loops. We process them incrementally, and we'll need to integrate them easily with LLVM compiled code. For debugging, it would be nice to have some *metric* as to where an error occurs, even if it's a rough metric: replay-based debugging would have us go to a point prior and try again in a debug mode.

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

### Time Quotas

I'm thinking a good way to model time quotas is based on allocations, in particular nursery allocations. Each GC of the nursery could act as a unit of time (or perhaps one unit per megabyte). 

I have a couple options for time quotas. One is to explicitly model computations, 



### Context and Environment Management

Ability to create an environment and multiple contexts, and interact with them. Also, a simple API for persistent data is important - e.g. a key-value database with atomic transactions will do nicely.

### Evaluation with Dictionaries?

It could be useful to associate a dictionary with our evaluations, where we know how to read the dictionary. This would require a 'standard' representation for dictionaries in stowage... which is doable, especially if we accelerate association lists.

### Streaming Bytecode APIs?

I've been thinking about how (and whether) to support streaming code in Wikilon. 

One option is "open blocks", where we receive fragments of bytecode (not necessarily aligned with blocks, tokens, or texts). We can treat this block as a value. This idea is very flexible. We could compose streams in various ways, replicate them, etc.. But these options also lead to implementation challenges - e.g. we might 'copy' a stream, quote it, apply it in multiple parallel contexts, etc.. so there are potentially synchronization, replication, partial application issues.

A second option is to create 'stream evaluators', where we iteratively apply chunks of bytecode provided as C strings (and perhaps a few blocks, quoted values). This option has similar representation challenges, but none of the greater complications like replication and synchronization. 

I favor this second option at this time. Though, it might be feasible to use the same 'stream injection' API for both, via ad-hoc polymorphism. 

A related issue is whether we should allow observation (and manipulation) of intermediate values. I'm thinking not. We can easily (and far more generally and precisely) inject tokens for intermediate access. Further, access to intermediate content may hinder control over nursery arenas.

### Direct Value Manipulations

Ability to observe, construct, and manipulate values directly would be useful when interfacing between a Wikilon context and some external resources. 

Originally I planned for non-destructive views on values. However, this seems... complicated. It is inconsistent with Awelon Bytecode, move semantics, pending computations, parallel effects (e.g. a debug `{&trace}`). Also, there's little point to non-destructive access unless it's also non-allocating, but it's difficult to (for example) access a few steps in a deep sum value without either destroying it or allocating a new sum object.

So, value manipulations will all be destructive with ownership semantics. Developers will need to copy values if they want a safe reference to the original.

### Large Binary Inputs and Outputs

Something that has concerned me is how Wikilon will interact with large binary content. Working with multi-gigabyte binaries is problematic if I restrict to 4GB working spaces. Modeling very large binaries as external resources might help, but I'm not comfortable with consuming arbitrary amounts of address space. I used `mmap'd` files or whatever to avoid loading them all at once. What other options do we have?

One viable possibility is to have Wikilon understand a relatively simple model or API for ropes or streams, or perhaps even HTTP request handlers, with which we may easily wrap whichever models we favor within our dictionary. 

This would enable large binary *outputs* to be loaded (perhapse even computed) incrementally. Ropes would have the advantage of enabling indexed access, answering those region-based HTTP requests, etc.. Large binary *inputs* are probably less an issue because we simply have some software agent representing the binary within the dictionary in an ad-hoc way (ropes or streams or whatever). 

I think I'll try that route, rather than attempt to solve the problem of representing very large binaries.

## Dead Ideas

### Array Stacks

The array representation could feasibly be applied to the `(a * (b * (c * (d * e)))))` stack-like structure. In this case, however, we might need to focus on rapid increase and decrease in the stack size, i.e. by providing empty space for a stack to 'grow' or 'shrink' with push and pop operations. And there is almost no deep indexed access. I think the benefits are likely to be marginal especially since we already support mutation for most stack ops.


