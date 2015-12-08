
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
* real-time capable: predictable timing and garbage collection
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
* 101 - product or unit inL
* 111 - raw product or unit
* 011 - product or unit inR

Small integers encode plus or minus a billion: nine solid digits with a little play at the edges. 

A product is represented by addressing a single cell, just a pair of values. Address 0 represents 'unit'. We optimize a single level of sum types (inL vs. inR) for products and units. This allows us to directly represent lists, booleans, and simple trees (e.g. node vs. leaf) without an extra cell for the sum type.

Tagged objects are used for everything else: arbitrary sums, blocks, sealed values, stowed values, pending parallel computations, compact texts, vectors and matrices, etc.. Tagged objects may be more than one cell in size, depending on their type.

## Memory Management

Nurseries are collected by a semi-space algorithm. Motivation: efficient allocation, resist fragmentation, improve locality. 

Content is tenured to the main heap if it survives at least two collections - more, if memory pressure is low. Our main heap uses a conventional free list. By the generational hypothesis, we're less likely to delete tenured data immediately. Because we tenure in batches, we can avoid micro free-list overheads. Allocation on the nursery itself becomes a simple bump-pointer action. 

Nurseries are relatively small: somewhere between 128kB and 2MB, perhaps depending on our CPU cache sizes. These should be small enough to fit entirely into processor cache, yet large enough to do useful work. A generational collector requires careful interaction between generations. We must either track 'remembered sets' from the older generation, or copy data one direction or the other. I'm leaning towards the latter option, preserving an invariant that our heap never points into the nursery.

Even within a nursery, we must trace objects that we drop (by `%`) to validate deletion and reach deleted heap objects. For tracing within the nursery, we can reliably use the other half of our semi-space. We can guarantee this space is large enough, and it's otherwise unused space. 

On the main heap, large deletions by `%` may be performed incrementally, e.g. modeling a stack or queue of large objects to delete properly. We can also use a thread-local heap free list and deletion stack, such that we only push this up to the main heap just after a nursery GC cycle, e.g. to minimize synchronization, maximize batching, and enable reuse of a thread's local heap area.

*Aside:* It is feasible to 'defrag' the main heap, perhaps perform a big copy collection from one context to another (possibly resizing it in the process). But only if there are no active threads in that context.

### Semi-Space Algorithm

We could use a modified variant of Cheney's algorithm to copy content between spaces. We would keep an extra byte-stack, at the opposite end of our free space, that tracks for each allocation whether it is addressed as a 'tagged object' or a 'cell'. This would allow us to properly interpret the object and find the next set of pointers. However, Cheney's algorithm is breadth-first. This has a negative impact on data locality. 

I want a proper compacting collector. I want the following properties:

* after compaction, data references point backwards
* lists compact so the spine of data is adjacent.
* lists feasibly compact directly into data arrays. 
* tree structures compact along the tree branches.

Ideally, references within our nursery are frequently in the same cache line. And this same algorithm should work very nicely for shoving content out to the main heap. All of this is feasible, but it requires a stack. Fortunately, we can make some guarantees about stack size, perhaps use the opposite half of the nursery as our stack.

We can keep a pointer to the top of our nursery from just after collection. Upon the next collection, we can count how many addresses we preserve from below this register. This gives us a precise mark for "survived one collection". We'll never tenure anything that hasn't survived one collection and is *about* to survive a second. 

Further, we can count how many cells were carried from the prior generation, and how many were tenured. If we keep a few nursery stats in a ring buffer, we can use them to estimate memory pressure and guide heuristics for tenuring.

### Free List Algorithms

I don't have any special ideas here. To avoid synchronization, I may track a per-thread free list, rather than using the shared context list for everything. I'm wondering whether I should use a weighted or Fibonacci buddy system.

## Tagged Objects

A tagged object is represented in memory with the first word (32 bits) indicating how the following words are understood. It's important that tagged objects be simple. Wikilon's GC must understand them.

Important tagged objects include:

* refct wrappers
* ad-hoc sum types
* arrays, compact lists, binaries, texts
* stowage indicators and wrappers
* pending computations
* sealed values
* blocks, fixpoint blocks, compiled code
* numbers other than small integers
* external resources

Most tagged objects will require two cells or more. But there are a few cases where we can squeeze down to just one cell: reference counting wrappers, and ad-hoc sum types. 

### Reference Count Wrappers

With 4-byte words and a 4GB address space, we have at most 2^30 - 1 references to a refct object. This is almost even achievable via compacting vectors. But this leaves us the two low bits: one to tag refcts, one to tag delebility.

        (refct, object)
            refct → high 30 bits is count
                    lowest bit is 'refct' flag (1)
                    second lowest bit is 'known deleble'

A reference count qualifies in most cases as lazy or pending object. When we try to observe our refct, we must either assume ownership (if refct is 1), or copy the data (up to the next refct object, which we incref) then decref. When we first try to copy a large object (i.e. that we own entirely), we'll inject refcts every so many items. When we first try to delete an object, we'll mark all the 'known deleble' bits while validating the action.

The savings from injected reference counts is mostly:

* reduced memory pressure if copies are processed sequentially
* fast future copies, validation already performed behind refct
* very efficient copy-delete patterns with shallow observations

We represent reference counts as a single cell. This gives us a very good amortized overheads, e.g. if we refct every 64 objects we have a 0.5 bit overhead per object. 

Increments and decrements to refct objects on the heap must be atomic due to potential par/seq sharing. Within the nursery, we can perform conventional increment and decrement operations. Doing so may be worthwhile: atomic operations aren't CPU friendly, and we can cheaply test for membership in the nursery.

During copy-collection, we must replace reference count objects with forwarding pointers. Reference counts are the only objects with potential sharing, so this is a special case. We benefit from two more tags: deferred reference counts for wrappers in the nursery, and forwarding pointers used only during GC.

*Aside:* If our 'known deleble' bit is 0, that doesn't mean the object isn't deleble. It means we *don't know* whether it is deleble. We'll need to validate delebility (that a value contains no 'relevant' blocks) on our first attempt to delete. 

### Ad-hoc Sum Types

Sum objects will be represented by a single cell. 

        (sum, object)
            low bits: eight bits to flag our sum object.
            sum data: sequence of `10` for inL or `01` for inR
                      read from low to high. I.e. inLR is 0110
                      all zeroes at the top. 


This encodes up to 12 tags (24 bits of tag data) per sum object. We can chain sum objects if we need more. There is no guarantee that sum objects are as tightly compacted as possible, but they'll tend to compact where it's obvious to do so.

With 12 tags we can discriminate up to 4096 items. This is more than a human can effectively manage. However, machine-generated code might leverage this much or more, e.g. if we develop good optimizers for hierarchical conditional behaviors and leverage them via DSLs. 

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

This tagged object actually represents an `(a * List a b)` pair. We'll further wrap it with another cell for the `inL` sum type. Together with this sum tag, the overhead for an array is 6 words. Lists require two word per item, so we break even on storage costs when encoding 6 items.

Annotation `{&array}` asserts the argument is a list of values and compacts this list into an array with a single contiguous buffer with `pnext` pointing to the terminal value (usually `unit inR` for a plain list). If allocation fails (e.g. due to memory fragmentation), or if the argument is not a valid representation of a list, computation will halt. The weaker `{&array~}` (read: array handwave) allows our runtime to make heuristic chunking decisions, encoding a chunked list in `pnext`. Chunked lists are a high performance data structure, sufficient for many applications of arrays.

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

To handle this, my current best idea is to wrap the buffer like with do for shared buffers, except specialized for sharing edges of the array. This might look like:

        (overlap, pbuff, pshareL, pshareR)
            pshareL, pshareR: one of
                tagged (refct, unit) 
                unit (if no overlap)

If we delete our array, we'll decref our shares of the cells at each edge. If we held the last share, we'll take ownership of the associated cell and include it in our argument to the free() function. Otherwise, we leave it to the other shareholder. If instead we recombine our arrays, we can eliminate the share between them.

*Note:* The overlap object can be omitted for arrays hosted in the nursery.

#### Logical Reversal

Logical reversal of a chunked list is achieved flipping a bit in the tag of each array chunk (so we have a `reversed array`) together with a conventional reversal of the `pnext` list. The `size+offset` values are not modified. We simply compute our index or pop data from the opposite end of the array.

We reverse *onto* a value, and simultaneously we expose the terminating value. Reversing lists serves as a basis for appending lists or accessing terminals. (Though we'll also have accelerators for those specific cases.)

#### Binaries

We can specialize for a list of bytes. A 'byte' is simply an integer in the range 0..255. 

        (binary, size+offset, pbuff, pnext)

The annotation `{&binary}` is taken as an assertion of both binary type and desired representation. If an argument is not a valid binary, this assertion will fail (statically or dynamically). The weaker `{&binary~}` allows the runtime to use a chunked list representation, selecting a chunk size heuristically (e.g. favoring blocks of 32kB).

I'd like to support other arrays of fixed-width structure, e.g. if I can get some sort of template like `(i32 * (f32 * u64))`, then I could allocate two cells at a time and properly represent them on load. I'll need to think about this later.

#### Texts

Text is represented by a list-like structure of unicode codepoints, with a short blacklist: C0 (except LF), DEL, C1, surrogates (U+D800-U+DFFF), and the replacement character (U+FFFD). I'm precisely matching the constraints for text data in the AO dictionary. 

The `{&text}` annotation tries to compact valid text data one large UTF-8 array. If the text is not valid, this fails. The `{&text~}` annotation does the same but encodes the UTF-8 as a chunked list. 

A basic proposed representation for most text is:

        (text, utf8)
            utf8 is pointer to binary array

Due to the variable size of characters, texts don't have array semantics. Computing length requires scanning and counting characters. Updates require shrinking or expanding the utf8 representation. Further, in most cases, we don't actually want array semantics. We'll mostly be splitting texts based on *content* (e.g. comma separated values or line terminals) not *character count*. 

But we could improve character counting performance with an index:

        (text+index, utf8, index1, index2)

            utf8 is ptr to binary array

            index1: encoding (c, b) pairs
                skip forward c characters by skipping b bytes
                small values of c,b : range 1..256

            index2: encoding (c, b, i) triples
                skip forward c characters by skipping b bytes
                skip corresponds to i pairs in index1
                large values of c,b: range 1..65536
                small values of i: 1..256

These indices could be installed heuristically based on text size or need. The asymptotic overhead for the index is less than 0.8%. For texts of 4kB, the overhead is closer to 2.6%. The first layer index lets us scan, slice, and lookup on index 100x faster. The second layer bumps us up to 10000x - still O(N), but a nicer coefficient. These indices would support slicing texts of up to about 20MB. Though I would recommend finger-tree ropes and stowage long before reaching that point.

Conveniently, appending texts can be modeled by appending each array. Logical reversal requires special attention: we reverse each array, but we'll also need the ability to read reversed utf8. (It's probably easiest to do this transparently in our utf8 reader.)

### Accelerated Association Arrays? (low priority)

It might be useful to heavily optimize an associative structure, e.g. the equivalent of a JSON Object. Motivations include:

* fast indexed lookup without disassembly of the structure
* optimized representations for small vs. large structures
* implicit batching of updates, log structured merge trees
* provides a built-in, optimized model for AO dictionaries

Any accelerated associative structure must have a primary representation deterministic from content. This enables use of alternative representations under the hood, because we can recover the original representation. This excludes most binary search trees because their final structure depends on insertion order. However, this does permit use of a *trie* or of a *sorted association list*. Our keys could simply be a list of integers. This covers texts, binaries, vectors, etc.

The sorted association list is the superior option for acceleration. Other than accelerated lookup, we can't do much to accelerate a trie. Further, the trie representation is sophisticated, difficult to recover from other representations, and requires more logic than I'm comfortable requiring in the runtime. 

For a sorted association list, access as a plain old list is an effective model for iteration. Many array-based accelerators (splits, joins, logical reversals, etc.) are directly applicable. There's much we can do to accelerate large associations. Our runtime can freely trade simplicity and performance. 

Acceleration of association arrays is a tempting possibility.

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

*Note:* compilation for external runtimes (apps, unikernels, etc.) will be modeled as an extraction, distinct from Wikilon's internal runtime. My goal with compiled code is to make Wikilon fast enough for lots of immediately practical uses and eventual bootstrapping.

### Numbers other than Small Integers

ABC is required to support arbitrary precision integers. I would like to use a simple representation in this case. I'd also like to squeeze some extra data into our type tag.

I'm currently thinking we might use something like a var-nat representation, albeit in reverse (little-endian). We can probably squeeze 24-28 bits of data and the sign bit into our tag, such that many 'large' numbers can still be represented in just one cell. We could use 64-bit arithmetic, easily enough.

Eventually, I would like to support fixed-width numbers via accelerators: modulo arithmetic, models of floating point, etc..

### External Resources

These are low priority so I'm not going to bother with external resources beyond reasoning about future viability. They seem very viable. Imagine a future context including:

* scientific computing on very large data sets
* graphics processing with meshes and textures

In these cases, and likely others, we will wish to surpass our limited 4GB arena or at least reduce pressure memory and copying for very large objects. External resources will continue to model pure values. However, as with arrays, we can update in place when we know there is no sharing and we have appropriate accelerators.

Accelerators and accelerated data models, such as arrays, are essential for external resources. These allow us to perform deep observations, transforms, and updates without picking a structure apart. Consequently, external resources aren't great for ad-hoc data structures. Arrays, especially of fixed-width data (binaries, for example), are the more tempting targets. Texts probably don't need the same high performance treatment as scientific data and graphics processing.

To support suspension, checkpointing, persistence, and resumption of computations, it is necessary that any representation of external resources include a description on how to recover access to it. Some form of specialized stowage might work for this concern. Use of external files is also feasible, though only for read-only content.

## Par/Seq Parallelism

An active computation will construct child computations via `{&par}`. We'll not parallelize until we've proven we have enough work on the active route, e.g. by holding onto our `(par, continuation)` object until it leaves the nursery. We do need to track these computations, such that we can immediately continue them as needed.

## Dead Ideas

### Array Stacks

The array representation could feasibly be applied to the `(a * (b * (c * (d * e)))))` stack-like structure. However, the benefits would be marginal. We can accelerate swap, rot, etc.. But stacks are best known for their rapid changes in size: push, pop, etc.. In those cases, our compact data structure isn't helping. The main benefit would be compaction of a large stack. 

