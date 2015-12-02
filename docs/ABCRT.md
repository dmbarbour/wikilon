
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
* arrays, compact lists
* compact texts, binaries, bitvectors 
* stowage indicators and wrappers
* pending computations
* sealed values
* blocks
* fixpoint blocks
* JIT or compiled code
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

From the ABC's perspective, an array is *semantically equivalent* to a list-like structure `λa.λb.μL.((a*L)+b)`. Except this isn't true from a performance-semantics perspective. We will give developers a few annotations like `{&array}` and `{&binary}` to automatically rewrite lists into arrays. Further, we'll support annotations `{&unshared}` and `{&seamless}` to enforce performance semantics. 

The basic `{&array}` type contains normal values at one word each. The `{&binary}` type contains a list of bytes (integers in the range 0..255). An array might automatically be specialized into a binary if this is recognized, but developers are encouraged to request the most precise representation. In the future, we may support more array types (e.g. specialized to a value type), but these two cover most use cases.

The proposed representation for arrays is:

        (array, size, pbuff, pnext)
            size → just for pbuff
            pbuff → 
                (111) contiguous memory
                (001) tagged buffer
                    e.g. refct wrapper
            pnext → one of:
                an array (tag)
                a list (inL)
                b (inR)

The `pnext` directly continues the list after the contents of the buffer. In a *seamless* array, this simply points to our `b` value (inR). But we can leverage this for efficient append of arrays, resulting in chunked lists. A `{&seamless}` annotation asserts its first argument is a seamless array, allowing us to easily debug performance issues. Applying `{&array}` to a chunked array will simply flatten it again.

The 

Our 'array' tag may include type information. We'll want specialized arrays for binaries at least. A few 'size' bits may be used for offsets. Logical reversal could be represented with the size, the tag, or our pbuff tag bits.

Theoretically, arrays have the same O(N) properties as plain lists (via `pnext`). But in practice we can expect many arrays to behave as O(1) up to an acceptably large size. To debug performance issues, we need a `{&seamless}` annotation to assert that an alleged array is indeed represented by a single large chunk. Medium sized arrays could easily be wrapped with finger-trees to efficiently model large, flat data structures with high branching factors.

Shared arrays will be modeled by wrapping the buffer with a `refct` object. Usefully, this enables array-lists to have both shared and unshared buffers. Also, it's easy for accelerators to recognize this. We can perform O(1) lookup on a shared buffer.

Unshared arrays can be trivially *split*, seamlessly *rejoined*, and *updated in place*. With accelerators for these functions, many useful algorithms and data structures can be implemented efficiently: union find, hashtables, mutable variable models, abstract register machines, in-place quicksort, etc.. We can parallelize many divide-and-conquer techniques such as in-place quicksort. To debug performance issues, we need an `{&unshared}` annotation to assert that an array is not shared.


For performance debugging, we'll also want to give developers annotations to *assert* that an input is represented by an array, or an unshared array, or that two unshared arrays have the same type and are adjacent.

*Com

Separating the refct, size, and pbuff from the actual contiguous memory is necessary for slicing. When refct is 1, we can cut an array into two smaller pieces each with refct 1. We can also append two adjacent units of refct 1 back into a contiguous array. This slice and append, together with some ability to update the array, is valuable for parallelism patterns.

Here's an alternative:


In this case, we don't have an internal reference count. So we're *assuming* we have ownership of the array. This greatly simplifies slicing and joining the buffers. We're using the recovered slot to model a chunked list. This guarantees our 'arrays' still have 'list' performance semantics: O(N) for everything. But it simplifies automatic defragging of arrays.

For a shared array, then, we'll need to wrap our array with refct objects. (I wonder if I can do reference count objects in just 1 cell? Might need to optimize its tag.)

### Texts

The `{&text}` annotation is a specialization on arrays for lists of unicode codepoints, representing in UTF-8 under the hood. We'll make a special point of rejecting: C0 (except LF), DEL, C1, surrogates, and the replacement character. These are the same constraints on the AO dictionary. For performance and type safety, the `{&aolit}` annotation will assert that we can encode a proposed text as an AO literal (which also requires checking that it ends with unit).

        (text, utf8)                  1 cell
            utf8 → binary array

To support fast slicing, we need indices on the text. Our first index is a simple list of `(charCt,byteCt)` pairs at 16 bits per pair. Our second tier index would index the first index with `(charCt, pairCt)`, with 24 bits per pair. Indices may be constructed as needed.

        (text+index, utf8, index1, index2) 2 cells
            utf8   → binary array
            index1 → binary array (u8,u8) pairs, (charCt, byteCt)
            index2 → binary array (u16,u8) pairs, (charCt, pairCt)

Most accelerators for texts will also apply to binaries (and vice versa). Usefully, texts and indices can be logically reversed. 

*NOTE:* Texts in Wikilon runtimes will be constrained exactly the same way they are in the AO dictionaries: valid UTF-8, forbidding C0 (except LF), DEL, C1, surrogates, and the replacement character. If we try to convert a list to text

### Large Value Stowage

*Aside:* We can inject refcts for stowed objects as we reconstruct them. This is easy because we know our affine and relevant properties for the stowed values holistically. We can easily compute this information more precisely during reconstruction.

### External Resources

These are low priority so I'm not going to bother with external resources beyond reasoning about future viability. They seem very viable. Imagine a future context including:

* scientific computing on very large data sets
* graphics processing with meshes and textures

In such cases we might wish to surpass a limited 4GB arena, or at least reduce memory pressure. External resources are simplest in context of *binaries*, or other arrays of fixed-width objects. We can transparently leverage list-processing accelerators for indexing, slicing, and updates. We can support both shared, read-only resources and owned, read-write resources. We can atomically take ownership if possible otherwise copy-on-write.

In terms of representation, external binaries could be modeled as arrays of yet another type.

All candidates for external representation are accelerated models. External data resources are wasted without accelerators. If we cannot access and update the data without all the intermittent dataplumbing, we might as well use normal stowage. Texts probably don't need the same high performance treatment as scientific data and graphics processing, but we could externalize them.

If we externalize code resources, we'll keep a local copy of the stack of quoted data. This essentially becomes a specialized block representation, keeping the interpreted code and/or compiled native code in a separate arena.

## Par/Seq Parallelism

