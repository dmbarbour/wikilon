
# ABC Runtime Design

As per [my performance strategy](Performance.md), I'm developing a C runtime: a good interpreter, with a little extra room for JIT compilations. The following is a sketch of the runtime I'm designing. 

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

I don't have any special ideas here. To avoid synchronization, I may track a per-thread free list, rather than using the shared context list for everything. But even this isn't a very special idea. :)

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
* big integers
* forwarding objects for copy collection
* external resources

Most tagged objects will require two cells or more. But there are a few cases where we can squeeze down to just one cell: reference counting wrappers, and ad-hoc sum types. 

### Reference Count Wrappers

With 4-byte words and a 4GB address space, we have at most 2^30 - 1 references to a refct object. This is almost even achievable via compacting vectors. But this leaves us the two low bits: one to tag refcts, one to tag delebility.

        (refct, object)
            refct → high 30 bits is count
                    lowest bit is 'refct' flag (1)
                    second lowest bit is 'known deleble'

A reference count qualifies as a 'lazy' object. When we try to observe our refct object, we must either take ownership of the data (if refct is 1), or copy the data (up to the next refct object, which we incref) then decref. When we first try to copy a large object (i.e. that we own entirely), we'll inject refcts every so many items. When we first try to delete an object, we'll mark all the 'known deleble' bits while validating the action.

The savings from injected reference counts is mostly:

* reduced memory pressure if copies are processed sequentially
* fast future copies, validation already performed behind refct
* very efficient copy-delete patterns with shallow observations

We represent reference counts as a single cell. This gives us a very good amortized overheads, e.g. if we refct every 64 objects we have at most a 0.5 bit overhead per object. Increments and decrements to refct objects must be atomic... unless they're in our nursery, which we can test for easily.

During copy-collection, refct objects will change to a forwarding pointer. Reference counts are the only points of potential sharing, so this is a special case just for them.

*Aside:* If our 'known deleble' bit is 0, that doesn't mean this isn't deleble. It means we *don't know* whether it is deleble. We'll need to validate deletion on the first attempt to delete.

### Ad-hoc Sum Types

Sum objects will be represented by a single cell. 

        (sum, object)
            low bits: eight zeroes. flags our sum object.
            sum data: sequence of `10` for inL or `01` for inR
                      read from low to high. I.e. inLR is 0110
                      all zeroes at the top. 


This supports up to 12 tags (24 bits of tag data) per sum object. We can chain sum objects if we need more. There is no guarantee that sum objects are as tightly compacted as possible, though a compacting collector may freely do something here.

With 12 tags we can discriminate up to 4096 items. This is more than a human can effectively manage. However, machine-generated code might leverage this much or more, e.g. if we optimize hierarchical conditional behaviors then we could potentially have high performance unions. 

### Arrays and Chunked Lists

An array is an optimized representation for a list-like structure `λa.λb.μL.((a*L)+b)`. 

Compared to a list, arrays reduce memory overhead by half and ensure very tight memory locality. But the primary benefit regards how easily common list functions can be accelerated when applied to an array: O(1) length, O(1) indexed lookups and updates, O(1) logical reversal, O(1) split and seamless append, and so on. 

The proposed representation for arrays is:
                
        (array, size+offset, pbuff, pnext)
            offset → 1 bit, skip first word
            size → number of words in pbuff
            pbuff → 
                (111) contiguous memory
                (001) tagged buffer
                    e.g. refct wrapper
            pnext → whatever follows

This tagged object actually represents an `(a * List a)` pair. We'll further wrap it with another cell for the `inL` sum type. With this, the overhead for an array is 6 words, and we break even when encoding 6 items.

Annotation `{&array}` compacts a list of values into an array with a single contiguous buffer with `pnext` pointing to the terminal value (usually `unit inR` for a plain list). If allocation fails (e.g. due to memory fragmentation) the computation will halt. The much weaker `{&array~}` allows a runtime to make heuristic chunking decisions, also via `pnext`. Chunked lists are sufficient for many applications of arrays.

#### Seamless, Unshared, and Affine Arrays

As far as ABC semantics are concerned, arrays are plain old lists. We can transparently append them to a list, for example. But from a *performance semantics* perspective arrays and lists diverge wildly. To protect performance semantics, I'm proposing two annotations: `{&seamless}` and `{&unshared}`.

When the array is encoded as one large chunk, and `pnext` points to the terminal value (no further list elements), we call this a *seamless* array. Seamless arrays guarantee many O(1) accelerated operations. This is valuable in many contexts. We provide a `{&seamless}` annotation to assert that an array is seamless. 

Unshared arrays can be split, seamlessly appended, and updated in place, each in O(1) time. These functions provide a powerful foundation for conventionally imperative algorithms and data structures: union find, hashtables, abstract register machines. They also enable logically reshaping arrays into matrices and back. To protect these properties, the `{&unshared}` annotation will assert that our buffer is not shared.

Both `{&seamless}` and `{&unshared}` can be validated dynamically, and efficiently. But we can also give these annotations static semantics for a linter or typechecker, e.g. so we get a compile-time error indicating where sharing was introduced.

As a simple technique, we can also explicitly mark arrays 'affine' by replacing the typical termal unit value with `[]f`. This prevents accidental copy and sharing of the array. 

#### Shared Array Buffers 

The copy function `^` will recognize arrays and wrap a reference count around the buffer. Accelerators can recognize this special case, i.e. such that we still have O(1) indexed lookup and non-copying fold/foreach, etc.. It is possible for a chunked list to have different sharing for each chunk. When updating a shared array, we'll implicitly perform a copy-on-write. 

By default, once a buffer is shared, it remains forever shared. Even if the reference count is reduced to 1, we'll perform copy-on-write and the `{&unshared}` annotation will reject. The goal with this policy is early detection of erroneous conditions, e.g. race conditions with par/seq parallelism or potential for future reordering optimizations.

When developers are confident in their assumption, they may use the `{&unshare}` annotation. This allows developers to indicate that a buffer should no longer be shared. Making this assumption explicit is a useful hint for static analysis. Dynamically, the annotation will validate ownership and remove the refct wrapper, returning the array to an unshared status.

#### Logical Reversal

Logical reversal of a chunked list is achieved flipping a bit in the tag of each array chunk (so we have a `reversed array`) together with a conventional reversal of the `pnext` list. The `size+offset` values are not modified. We simply compute our index or pop data from the opposite end of the array.

We reverse *onto* a value, and simultaneously we expose the terminating value. Reversing lists serves as a basis for appending lists or accessing terminals. Though we'll also have accelerators for those specific cases.

#### Binaries

We can specialize for a list of bytes. A 'byte' is simply an integer in the range 0..255. 

        (binary, size & offset, pbuff, pnext)
            3 bit offset into first cell
            size in bytes, 28 bits (256MB max)

The annotation `{&binary}` is taken as an assertion of both binary type and desired representation. If an argument is not a valid binary, this will fail (statically or dynamically). The weaker `{&binary~}` allows the runtime to use a chunked list representation, selecting a chunk size heuristically (e.g. favoring blocks of 16-64kB).
 
#### Texts

Text is represented by a list-like structure of unicode codepoints, with a short blacklist: C0 (except LF), DEL, C1, surrogates (U+D800-U+DFFF), and the replacement character (U+FFFD). I'm precisely matching the constraints for text data in the AO dictionary. 

The `{&text}` annotation tries to compact valid text data one large UTF-8 array. If the text is not valid, this fails. The `{&text~}` annotation does the same but encodes the UTF-8 as a chunked list. In addition to our raw text, we need indices for fast slicing, indexing, length.

A proposed representation for text is:

        (text, utf8, index1, index2)

            utf8 is ptr to binary array

            index1: encoding (c, b) pairs
                skip forward c characters by skipping b bytes
                small values of c,b : range 1..256
                invariant: b/4 ≤ c ≤ b

            index2: encoding (c, b, i) triples
                skip forward c characters by skipping b bytes
                also skips forward i pairs in index1
                large values of c,b: range 1..65536
                small values of i: 1..256
                invariant: b/4 ≤ c ≤ b

Texts don't have array semantics. Updates to a character may require inserting or removing bytes from the utf-8 representation. The index is limited to a linear factor: this two-layer index allows us to index and slice texts up to 10000 times faster than scanning it character by character. The index has an asymptotic overhead of only 0.8%.

This should be sufficient in practice, e.g. for texts up to twenty megabytes. I would strongly recommend developers model finger-tree ropes and leverage stowage long before reaching that limit.

Conveniently, appending texts can be modeled by appending each array. Logical reversal requires special attention: we reverse each array, but when reading from a reversed 'utf8 binary' we must parse our utf8 binary backwards. We could just make a parser that doesn't care about direction.

### Large Value Stowage

*Aside:* We can inject refcts for stowed objects as we reconstruct them. This is easy because we know our affine and relevant properties for the stowed values holistically. We can easily compute this information more precisely during reconstruction.

### Pending Computations

### Sealed Values

### Blocks, Fixpoints, Compiled Code

### Big Integers

### External Resources

These are low priority so I'm not going to bother with external resources beyond reasoning about future viability. They seem very viable. Imagine a future context including:

* scientific computing on very large data sets
* graphics processing with meshes and textures

In such cases we might wish to surpass our limited 4GB arena, or reduce memory pressure. External resources are both simplest and most useful in context of *binaries*, or other arrays of fixed-width objects. We can transparently leverage list-processing accelerators for indexing, slicing, and updates. In terms of representation, external binaries could be modeled as a new tagged buffer type within an array. We'll need to consider external sharing and ownership issues. 

Texts probably don't need the same high performance treatment as scientific data and graphics processing, but we could externalize them. For ad-hoc ABC values, stowage is the better option: we really need accelerators, cheap copies, and predictable sizes to leverage external storage. 

All candidates for external representation are accelerated models. External data resources are wasted without accelerators. If we cannot access and update the data without all the intermittent dataplumbing, we might as well use normal stowage. 

If we externalize code resources, we'll keep a local copy of the stack of quoted data. This essentially becomes a specialized block representation, keeping the interpreted code and/or compiled native code in a separate arena.


## Par/Seq Parallelism

