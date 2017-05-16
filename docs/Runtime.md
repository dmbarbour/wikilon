
**NOTE (May 2017):** I feel I've gotten tangled in the weeds when re-implementing the runtime for the simplified Awelon language developed circa November 2017. It's the GC and such that are causing much heartache. So I'm tabling the C runtime for now. I'll return after I have a version working reasonably well in Haskell. 

# C Runtime Design

A primary goal for runtime is performance of *applications*. Awelon's [application model](ApplicationModel.md) involves representing state within the dictionary via RESTful multi-agent patterns (e.g. publish-subscribe, tuple spaces). Thus, performance of processing these updates must be considered part of application performance, not just final evaluation costs.

A related goal is predictable performance. Space and effort used by the runtime will be under client control. The costs of evaluation shouldn't vary widely from one run to another. Ideally, the runtime model should be suitable for soft real-time systems. 

Awelon is designed to afford efficient update patterns via secure hashes to name dictionaries from which another dictionary might inherit or contain.

If it comes without too much overhead, I want multi-process support so I can develop command line utility processes, hot backups, FUSE adapters, etc. without running through the web service. And also add new web services as separate processes if appropriate. 

## Evaluation 

Viable evaluation strategies:

* interpret program directly, perhaps with optimizations
* compile to an abstract register machine, interpret that
* JIT compile to machine code before evaluation proceeds

Direct interpretation is the simplest and easiest to get started. If we optimize by rewriting normal code, that can also be directly interpreted with some performance gains. JIT can probably be applied to normal blocks to help optimize loops.

I think I'll skip the interpretation of intermediate register machine code. Registers add complexity to the interpreter and its interaction with GC. But we might compile to an intermediate register machine code as part of JIT.

## Memory Management

My assumption is that contexts will generally see single-threaded use. A web service will use multiple contexts to serve multiple pages. But supporting lightweight parallel operations, e.g. with `(par)`, is valuable for reducing latency.

Each evaluation thread shall allocate a heap within a context. Using a bump-pointer allocation, this can serve also as a stack space or something like it. The thread heap serves as a basis for generational GC, since a thread can GC its heap many times for each GC of the shared context, and may gradually promote older objects into the context. This offers a good GC for lightweight parallelism: worker threads won't interfere or synchronize with each other, though we might need to interrupt on occasion for a context-level GC. Context memory serves as a shared memory region, enabling data and code to be shared among threads.

Allocation shall use an efficient bump-pointer mechanism. No free lists are used. A mark-compact algorithm will recover memory for further allocations. The mark algorithm can use one extra array of mark-bits to avoid interfering with object representations. With cell-aligned allocations, this would be about 0.78% memory on a 64-bit system. Marking can be achieved in O(1) space by using a fixed-size stack and ensuring progress even on stack overflow (so we can revisit until marked). I have ideas for an exponential decay approach that should be good in most cases: use stack of 'fields' in objects, use previous field address as hint to find next field (iteration), and use exponential decay to trim data: cut 66% in half (losing hints), move last 33% back, free up 33% of stack.

Objects can be shared, with efficient logical copying. But I also want to track sharing, such that I can accelerate update functions for linear arrays to perform in-place manipulations.

I'll use write barriers and track a write set for each thread. Minimally, I could keep track of just the field addresses written. If I want to support lightweight rollback/transaction based error handling, it might be better to keep a little information about a written field's prior value.

## Parallelism and Threads

Each thread will have its own small heap within the context and thus may achieve progress without coordinating with other threads. A thread can promote data from that heap into the context to either share results with other threads or just reduce GC burden (treating context as a survivor arena). A thread must never peek into another thread's heap because that heap might move data around without warning.

There is some need to track when data is "promoted" from a thread to the shared context, and thus may be observed from another thread. Especially for parallel tasks. To simplify management of the write set and other requirements, promotion shall be all-or-nothing within a thread. Promotion may also push `(trace)` logs and similar debug outputs.

Threads will operate within a context's effort quota. This doesn't feel very precise, but it is analogous to all tasks sharing a context's memory. And it should be precise enough, assuming I use multiple contexts to serve web clients.

Parallel tasks need special attention. They're referenced from one thread but evaluated wholly or partially from another. We must delay evaluations that require a values from other threads.

So each thread must:

* have a nursery volume of memory
* track cross-generation writes
* track current `(par)` task
* queue pending `(par)` tasks
* queue waiting `(par)` tasks

We can combine the pending/waiting sets, likely, using a single queue. We need only to track for each task which other task (if any) on which it might be waiting, and return it to the end of the queue as needed. But the separation should make it easier to track how much work is readily available, and easier to make progress without rescanning the same nodes too frequently. 

We could also track completed tasks, but that might be rolled into the local write barrier feature.

Each thread could support a couple GC generations. This could be replicated at the context layer. Thread structure would be eliminated by context-level GC and reallocated as needed. When threads are halted for context-level GC, we simply halt just before they'd normally perform a thread-local GC to avoid rework.

KPN acceleration is a special case, and may require explicit tracking of 'messages' between 'processes' so we can communicate asynchronously by moving either the message or the process into shared memory. This will likely require a threads extension to maximize utilization.

## Lazy Copy?

Awelon's basic evaluation strategy is lazy modulo copy, hence avoiding need for shared memory memoization. Yet lazy copies are feasible within a runtime, automatically sharing the result via shared memory. I'm curious whether doing so is worthwhile. Lazy copies only help if the result is used in neither copy, and they require evaluation at more ad-hoc locations in memory.

For now, I'll probably skip this feature.

## Stack Representations

The simplest stack representation is probably a linked list, representing composition cells. But this requires a lot of allocations. An array-based stack would likely serve more efficiently. I'll need to figure out how to represent these easily.

## Memory Representations

Say our allocations are two-word aligned, and our words are either 4 bytes or 8 bytes depending on machine. This gives us 3 reliable pointer tag bits. Use of pointer tag bits can discriminate a few 'small' values to reduce allocation costs. Proposed representation:

        _b0     small constants, operators, etc.
            x1    integers
            10    naturals
           000    extended
           100    object headers
        sb1     object reference, upper bits are address
            tagged object: first field is type header
            small object: first field is any other value

Small constants include small numbers, operators, and object type headers. Small texts or labels are also feasible, especially for a 64 bit system. Objects will be discriminated by their first field. If the first field is a type header (a special small constant) it will indicate object size and how to interpret the remainder of the object. Other small objects will be simple cells with two values, representing composition in most contexts, but potentially representing list cons cells or similar in context of a suitable object header.

An important feature to simplify GC is that we can determine the size and internal fields of an object given just its address. This allows us to mark addresses only.

The `b` pointer field indicates that an object may be treated as a block value (e.g. with respect to binding, data plumbing) while the `s` field for objects indicates that there is more than one reference to an object. A newly allocated object always has `s=0` while a logically copied object has `s=1` on both copies.

*Note:* Ideally, `(nc)` and `(nd)` would be represented as pointer bits due to how they're inherited upon bind. However, I'm all out of pointer bits. I'll need instead to attach these properties to an object wrapper that receives special attention upon bind, copy, drop, and read/serialization. Other annotations that attatch to a value, such as `(error)`, might be coupled into this.

### Small Constants

The essentials are: small naturals, operators, and object headers.  Support for 'raw bytes' - unparsed data in the program stream - may also prove valuable during serialization. Small integers would be a convenient addition. Small decimal values or labels for structured sums would also be relatively convenient. Small texts or binaries are feasible, but certainly non-essential.

Natural numbers will run up to at least 2^28 (over 250 million) on a 32-bit system, and much higher on a 64-bit system. 

*Note:* Operators shall use the zero suffix for small constants, to ensure the NOP operation is represented by the `0` constant value.

## Mutable Objects

A purely functional language can support in-place mutable data for unique references, e.g. if we have the only reference to an array then the array-update function may freely modify the object in-place. I currently track uniqueness in the object pointer (the `s` bit).

For generational GC, writes to fields that point into a nursery heap must be recorded in the write-set. It probably won't be worth modifying small objects in most cases, only things that would involve a large copy.

List and composition cells might be a special case for mutable objects. We cannot update such objects in place because we lack header bits to track uniqueness, and we lack generational write barriers. But copies also need attention, since they might reference unique objects. We may need to wrap copied lists with a header. I might convert a large list into a shared array upon copy.

### Arrays or Binaries

Arrays would be the most important mutable objects. Given effective acceleration of arrays, we can model many low level algorithms, mostly excepting those that require multi-threaded access to the same array.

It's tempting to support efficient logical composition and push/pop on arrays. But working implicitly with large logical arrays seems problematic if we later serialize or read the program, e.g. for stowage or debugging. It may be better to optimize for small array fragments then encourage developers to explicitly model finger-tree ropes for larger objects. Similarly, ring buffers, push/pop stacks, etc. may be explicitly modeled using indexes into structures with linear mutable arrays.



## Persistence and Resources

A challenge I must still consider is how to model persistence and secure hash resources.

Some basic options:

* persistent compacting "heap" per environment, relative offsets
* LMDB persistence, with tables and transactions
* Filesystem persistence, directory or tar file layout

The LMDB option currently seems like a Goldilocks solution, trading some flexibility and control but handling various problems including atomic update, zero-copy access, and resource management.

Some thoughts:

* resist timing attacks - secure hashes as capabilities, must not be guessable by timing "no entity" responses and incrementally modifying the hash. Proposed solution: use first 60 bits for fast table lookups, then compare the remaining hash bits via constant-time algorithm. This constrains timing attacks to revealing up to 60 bits of an unknown resource name, and 60 bits reduces collisions to one in a billion or so (via birthday paradox).

* global addressing locally - I can map global addresses to, for example, 64-bit local addresses. Doing so could save me a lot of space and lookup overheads, but only if I use the local address much more frequently than global addressing. Between stowage and dictionary patches, I doubt this will be the case. So I'll stick to global addressing until there is good reason to do otherwise.

* plain text internally - I can rewrite 60 bytes base64url to 45 byte binary keys, but the savings would be minor and would not extend to secure hash resources. The plain text format seems more convenient and self explaining in context of external tooling or debugging (`mdb_dump` and similar). So I'll stick to plain text for now.

* reference counting - a good algorithm for long-lived, persistent resources. A precise reference count may require type information that GC understands, such as `3b+11d` meaning "referenced three times as a binary, eleven times as a dictionary", so we know how to interpret a resource. Alternatively, a conservative reference count is feasible and should be reasonably precise.

* lazy reference counting - reference counts can lag behind updates to root objects. We could model this by tracking an RC deltas table separately from the main RC table, to support incremental GC of resources and perhaps avoid the initial parse and processing of resources that are only briefly referenced. NOTE: since we might actually *receive* resources out-of-order, an RC deltas table could usefully delay positive increfs until a parse can be run.

* ephemeral resources - a context can hold onto a resource that has a zero persistent reference count. I don't want to scan contexts when it's time to GC, especially since they might be spread across many processes. So I'll use a shared memory ephemeron table (likely a simple hash-to-refct table). A shared memory mutex (with the robust option) should be sufficient for performance since I don't expect a lot of contention on this table. It should be sufficient to reference only root values from our contexts.

GC for secure hash resources, and a shared ephemeron table between processes, is perhaps the main requirement for multi-process access. I could (and probably should) use shm_open for the ephemeron table.

I'm concerned that a crashed process might hold indefinitely onto resources. I could try an expiration model, with processes periodically scanning the contexts to maintain the ephemeron tables. But this seems like a problem to solve if it becomes an actual problem. Worst case, I just need to reset the machine.

*Thoughts:* I might benefit from reducing sizes of secure hash resources to limit the burden when loading a resource into context memory or persisting the object. I'll need to think about this. Limiting the size of words and definitions may similarly help.


## Dictionary Indexing

I must efficiently index a dictionary to:

* find definition, given a word
* find references to a word or annotation
* find words with common suffix (or prefix)

For efficient import/export of dictionaries, I want want to preserve structure of dictionary resources.

But it is feasible to operate mostly off the indexes, and to generate a new dictionary resource that represents the same dictionaries more efficiently. Importantly, such indices must be incremental and composable, such that a composition of patches can be indexed by a function composing their indices. Structure sharing, so updates aren't scattered throughout the dictionary, may also be valuable.

All of this suggests use of trees or tries to me. Tries, unlike most search trees, have a nice property of fully deterministic structure based on the *elements* they contain rather than their update order. I lean in favor of tries to improve structure sharing between independently developed dictionaries.

* to find a definition, use trie from word to definition
* to find clients, use trie from word to a set of words
* one trie encodes words backwards for suffix lookups
* set of words is encoded as a trie, using keys only

During evaluation, I'll probably need to maintain another index for cached evaluations, localization of words, etc. - again, having a lot of structure sharing would be convenient.

Tries require a lot of references between tree nodes, which seems a problem given 60-byte resource IDs. Use of stowage size heuristics - e.g. don't stow anything smaller than 256 or 512 bytes - might help by collapsing smaller nodes.

All of this involves a lot of references between index nodes. Of course, when references are large secure hashes, having lots of small nodes is problematic. This can be mitigated using stowage style heuristics, deciding whether to collapse a node based on its apparent size. 

Definitions could be included directly, again via stowage, instead of a `(resource, offset)` indirection. This would be important if we wish to share structure of indices independtly of dictionary update order or patch structure. 

Anyhow, the runtime will need to be relatively good at working with tries and stowage.

## Concurrent Update

A context shall represent a transaction on the dictionary, in the sense that it may conflict with updates from other contexts on the same dictionary. 

It seems feasible to model discretionary locks within a dictionary. Or to keep some form of write journal, to support DVCS-style workspaces with fork and ad-hoc merge. But such features won't be supported by the runtime.

## API Concepts

The runtime API should be oriented around an agent's view of the system.

Some operations we might want to perform:

* read or update a word's definition
* perform a set of updates atomically
* input a program, binary, or dictionary
* output a program, binary, or dictionary
* partial program output, background parallel
* process program as text (for editable views)
* evaluate, type, optimize, compile a program
* profile computation; line item accounting
* subscribe, alerts upon change (callbacks?)
* rename dictionary object, special edits
* find clients of a word, reverse lookup
* find words with a common prefix or suffix

It seems to me that most input and output can occur as binary streams or chunked lists. Fine grained program input (e.g. at the level of open/close block and add word) seems unnecessary, though I could support it anyway. Whether it's worth introducing depends on how much it complicates the runtime API. I may need to track missing secure hash resources, however. 

For export, we could export dictionaries by secure hash or by named transaction. An interesting point is that we could export ALL dictionaries as a single, hierarchical dictionary. Outputting all dictionaries as a massive .tar file would be an interesting import/export mechanism.

*Aside:* By 'optimized' output, I want *Awelon* code after all the rewrite optimizations, static linking, staging, etc.. By 'compiled' output, I would like relocatable JIT code (Clang or LLVM?) or similar.

*Note:* Security models (HTTP authentication, HMAC bearer tokens, etc.), collaborative conflict avoidance patterns (discretionary locks, [behavioral programming](http://www.wisdom.weizmann.ac.il/~bprogram/more.html)), and accounting (quotas, policy for costs shared via memoization or stowage), are not part of this C runtime API. I believe they can be implemented more or less independently, and I don't want to commit to any particular model.

## Streaming and Futures

I could support single-assignment futures when inputting data into a computation. This would be trivial, in terms of undefined words. But is doing so worthwhile? Without streaming IO, the C API is a lot simpler.

## Evaluations

We have at least a few evaluation statuses: evaluated, stalled, unevaluated. I might want an additional option for profiling evaluations on a block, i.e. so I can allocate a unique counter to track the evaluation.

## Extraction

Compilation of Awelon code for an external system is an interesting option, but is only feasible if we either limit the IO types at that layer (e.g. to integers, rationals, decimals, texts, labels, lists) or ensure that first-class function objects in the target language are coupled to Awelon representations.

## Computations

A computation should be snapshot consistent, at least. Like a lightweight transaction. 

Ideally, we should be able to take any computation and set callbacks recording invalidation with whatever precision we can afford.

## Memoization, Stowage, and Accounting

Memoization and stowage accounting should support potential sharing not just within a transaction, but between many dictionaries or many forks of a dictionary. 

Line item accounting should makes this feasible. We might track on a global scale that 'yup, that computation was also performed by Alice and Bob today' and appropriately divide compute and storage costs. This gives us 'economies of scale' for sharing objects.

## Value Sealers

Sealers have a trivial definition:

        (:foo) (.foo)   ==

The normal usage pattern is only a little more sophisticated:

        [(:foo)]b ...  i(.foo)

It doesn't seem these really need any special support, other than to associate `(.foo)` to `(:foo)` when initially interning so it's a pointer-level operation.

## Value Stowage

        [large value](stow) => [$secureHash]

I might want latent stowage, performed based on memory pressure, when promoting values during compaction. Latent stowage isn't too critical if I model batch data structures (like the log-structured merge tree). But it's still convenient.

## Numbers and Arrays

Numbers generally use a boxed representation, excepting small naturals.

I intend to eventually support 'unboxed' fixed-width numbers in context of typed arrays and matrices. This is necessary to accelerate linear algebra, leverage the GPGPU, or enter a domain of high performance computing in general. But I won't bother trying to optimize basic numeric processing for most Awelon code, not beyond simple acceleration of arithmetic.

## Optimizations

For visible optimizations, we can at least perform:

* staged partial evaluation up to arity
* static linking for words
* rewrite optimization

Some invisible performance enhancers:

* determine up-fun-arg vs. down-fun-arg types
* acceleration for common multi-word actions
* JIT compilation of code

## Memory Profiling

I don't currently propose to directly support memory profiling. 

Developers can at least take intermediate snapshots during evaluation, e.g. at breakpoints or stopping on effort quota. This would give developers a reasonable view of where memory would be consumed in an implementation of Awelon code without logical copies.

It would be feasible to count heap references to individual words and types of data (numbers, etc.), however.

How should I offer insight about where memory is being consumed? Ideally, non-invasively?

It seems feasible to track each block/closure to a syntactic origin. But it also seems expensive to record this information in memory, to add metadata about origin to each block or closure and to trace down the object while profiling. 

Alternatively, I could count references to words or annotations in the current program. This would offer an efficient (albeit imprecise) metric of memory usage. Small values (naturals, etc.) could also be grouped by type, as could be texts and binary arrays. This wouldn't give a good view of where memory was allocated.

A third option is to simply record intermediate evaluation states and report on them. This wouldn't provide much information about shared values via logical copies, but applying a compression algorithm might help highlight where most of our memory is used.

## JIT Compilation

I've discussed compilation to a register machine in the main Awelon language document. This seems highly applicable to compiling to a JIT target. A static type analysis might also help. 

It seems valuable to get JIT working early, otherwise it's easy to get caught up in optimizing the interpreter. That said, I do want to focus first on a fast interpreter, so I'm not relying entirely on JIT for performance.
