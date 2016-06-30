
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

ABC is well suited for 'manual' memory management due to its explicit copy and drop operators. It's convenient if most memory is linear so that we can have non-allocating data plumbing operations (at least for common cases). And bump-pointer allocation and slab allocations are useful for locality and performance, potentially favorable to free lists (though something like TCMalloc wouldn't be bad).

Because I'm running pure code without side-effects, throughput is more important than latency during computation. There is a lot of freedom on what to do with memory. For now, I'm going for the 2-space compacting copying collector per context, which ensures fragmentation will never be an issue at the cost of copying memory occasionally.

Parallelism within a computation could also be valuable. I may need to eventually move away from the shared memory idea in favor of free spaces for multiple threads. Though, this could be mitigated by good support for non-copying shared memory and objects.

#### Support for Shared and Non-Copying Objects in Memory?

Linear memory is convenient but it requires frequent deep copies. This is especially a problem for blocks in a fixpoint loop, and for large binary data. I would like some form of stable memory for binaries and blocks of code. 

The ability to compute large objects without copying them every GC pass? It could be quite convenient, in general. *Stowage* sort of works that way, but has a large copy overhead per access. I need a lower latency option - objects *in memory* but not copied, i.e. in a separate region of memory. 

Some options:

* divide context into both a workspace and shared space.
* shared space at `wikrt_env` layer instead.
* shared space via C heap, malloc/free, reference counting.
* stable access for stowage addresses via read-only transactions.
* cache stowed values in memory via some other mechanism.

The simplest of these options to implement is to use the C heap and reference counting for shared objects. This would at least offer a simple way to get started. I also like the idea of modeling shared memory in terms of stable access to stowed binaries via read-only LMDB transactions. That would preserve structure sharing of shared objects.

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


