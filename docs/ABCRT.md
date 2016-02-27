
# ABC Runtime Design

As per [my performance strategy](Performance.md), I'm developing a C runtime: a good interpreter, with opportunity for LLVM or other JIT compilation. One motivation for the C interpreter is that I need a simple, low-level data representation for the LLVM JIT. I'll be using something very like the Lisp/Scheme representation, consisting mostly of simple pairs of words.

It may be useful to also develop a command-line interface at this layer. I'm somewhat willing to encode Claw code and dictionaries at this level.

## Design Goals

Performance is the primary goal. But some others:

* precise memory control: model machine's memory in one arena
* hard real-time capable: predictable timing properties
* suspend, checkpoint, persist, reload, resume, copy computations
* easy to resize, reflect, render, compact suspended computations
* monadic effects: return control of program environment to caller
* easily integrate monadic effects with [claw command sequences](CommandSequences.md)
* on errors or unrecognized tokens, suspend and return to caller
* no callbacks or handlers, no external dependencies for progress

Also, it might be useful to support a second evaluation mode for type inference.

I'll avoid using tokens or callbacks for side-effects at this time. I'd prefer to encourage effects models that can be easily simulated and tested in a purely functional ABC environment.

## External Utilities

I'll actually include a copy of these directly, no shared libs.

* LMDB - embedded key value database
* Murmur3 - fast, collision-resistant hash 
* ZSTD(?) - real-time, streaming compression
 * very promising, but not entirely stable

## Structure

I need an *environment* bound to an underlying stowage and persistence model, e.g. via LMDB. The environment may also be associated with worker threads for par-seq parallelism. I also need *contexts* that associate an environment with a heap of memory for computations. 

Context is a contiguous region of memory, up to ~4GB allowing 32-bit addressing. In general, Wikilon will produce one context to evaluate each web page. Contexts will generally be much smaller than the 4GB limit (e.g. 10-100MB). A context may interact with a lot more data than its size suggests via large value stowage. Stowage serves a similar role to virtual memory, but does not consume address space.

At the moment, memory is manually allocated using conventional heap mechanisms: quick-fit and segregated size lists. I would like to support bump-pointer nursery allocations, too, though doing so may be limited to the duration of a particular evaluation, i.e. so it's easy to determine a 'root set'.

Wikilon runtime will favor linear semantics for values. A value reference has a single owner. Thus, pure functions on values can be implemented by mutations, and a lot of data-plumbing can be non-allocating. The cost is that we must perform deep-copies for `^` operator or when we otherwise share values. Deep copies may be constrained a bit by reference counting at certain boundaries such as stowage, and maybe blocks.

## Representations

Memory is 8-byte aligned, giving 3 tag bits in each pointer. Small numbers and a few small constants are also represented.

* xy0 - small integers; plus or minus (2^30 - 1).
* 991 - tagged objects (tag, data...)
* 011 - pairs (val, val)
* 101 - pair in left
* 111 - pair in right

The zero address is not allocated. Its meaning depends on the tag bits:

* 001 - void, invalid value
* 011 - unit
* 101 - unit in left (false)
* 111 - unit in right (true, end of list)

Other than pairs, most things are 'tagged objects'. The type of a tagged object is generally determined by the low byte in the first word. The upper three bytes are then additional data, e.g. sizes of things or a few flag bits. After implementing some, I feel it is essential to keep tagged objects structurally simple (even at cost to performance opportunities).

### Big Integers

        tag word: includes size (2..2^23-1) and sign (1 bit)
        followed by sequence of 30-bit digits

A large integer is represented as a contiguous sequence of 30-bit words. The number of words is recorded in the header, ranging from 2..(2^23 - 1). The sign bit is also in the header. Each word ranges `0 .. 999999999`. This gives us a variant of packed decimal encodings. This limits Wikilon runtime to integers of fewer than ~75 million digits - less than that if context memory is insufficient.

Integers in the range plus or minus `2^30 - 1` are recorded directly in the value reference. This is sufficient for a lot of use cases.

### Deep Sums

        tag word: includes an LRLRLLLR string (2 bits each, depth 1..12)
        second word is value reference

We'll use 2-bits per `L` or `R` in a deep-sum string, with up to 24-bits of tag data. This enables sums of up to depth 12 per allocation. The value in question may be yet another sum. Compacting deep sums into a single allocation should enable sums to serve a much more effective role as an alternative to enumerations.

Shallow sums refer to pair or unit in left or right, which is instead represented in the tag bits. Shallow sums are convenient as an optimization for representing lists and basic (node + leaf) tree structures.


### Blocks

Block representations have been giving me a fair bit of trouble. 

I imagine that I'll have a "compact" representation similar to what I developed in Haskell. That is essentially a compact array of opcodes coupled with a stack of quoted values. Unfortunately, a list of quoted values is perhaps not ideal for performance: I end up deep-copying the values every time I copy the block in any case. 

An alternative is to represent 'compact' values similar to how I would compact representations for stowage. In this case, I can get a blazing-fast slab allocation of a value when I need one. It may be feasible to 'lazily' load these values from the block. 

In any case, I probably need to push forward quickly - focus first upon a naive but usable representation. I can make it fast later.

Long term goals:

* fast quotation 
* O(1) composition
* fast interpretation
* easy simplification
* optimized LLVM variant
* efficient conditional behavior

A minimal 'naive' block representation:

        (block header, list of operations)

This is probably good enough for many use cases in the short term. It's at least not going to be worse than the Haskell's pure ABC.

### Arrays

Arrays 

### Reference Counted Objects?

I could probably generalize 'refct objects' to a variant of tagged object, even a single-cell object `(refct, value)`. In this case, if we'd overflow the refct (which can only reach 2^24-1) we could construct another refct object around the value. 

### Value Stowage

### Value Sealers

ABC's discretionary value sealers. We'll optimize for 3-character discretionary sealers like `{:map}` or `{:foo}`, which may be encoded in a single cell. Otherwise, I'll copy the entire token together with the value.

        

### Computations

An ongoing computation... a lazy or parallel value, perhaps.

# Older Stuff

Computations - including stacks and continuations - shall be modeled entirely within our contexts. Computations may exist in a suspended state. A fully suspended computation will release its nursery (tenuring all associated data), but we could support an intermediate suspension state for monadic effects and the like.


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

Arrays, binaries, and texts are compact representations of lists and list-like structures (i.e. type `∃a.∃b.μL.((a*L)+b)`). Binaries and texts additionally constrain the element type (to a subset of small integers). Taken together with accelerators (strings of bytecode that we reduce to a single operator under the hood), we may achieve very high performance access and even in-place update for unshared arrays.

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

*Note:* I need to decide whether an array represents `(a * List a b)` or just the list itself. I could possibly do both via a simple tag bit in the array type. But it might be simpler and more efficient to dismantle the head of an array whenever we try to access it as a sum type, losing only the ability to seamlessly rejoin.

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


