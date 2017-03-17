
# Runtime Design

A primary goal for runtime is performance of *applications*. Awelon's [application model](ApplicationModel.md) involves representing state within the dictionary via RESTful multi-agent patterns (e.g. publish-subscribe, tuple spaces). Evaluation, concurrent update, incremental computing, and so on each contribute to application performance.

I'm also interested in predictable performance, and precise cost accounting so heavy users can (eventually) be charged appropriately. The runtime will aim to keep unpredictable performance features under client control via annotations and similar. I intend for Awelon runtime to be suitable at least for soft real-time systems to the extent that programmers control the nature and frequency of transactions on the dictionary.

Awelon is designed to afford efficient update patterns via secure hashes to name dictionaries from which another dictionary might inherit or contain.

If it comes without too much overhead, I want multi-process support so I can develop command line utility processes, hot backups, FUSE adapters, etc. without running through the web service. And also add new web services as separate processes if appropriate. 

## Evaluation 

Viable evaluation strategies:

* interpret program (with call-return auxiliary stack)
* compile to abstract register machine, interpret that
* JIT compile to machine code before evaluation proceeds

There is a tradeoff. Compilation increases complexity and spinup time, but can significantly improve performance for long-running loops. I do know that I'll eventually want JIT compilation support. I favor interpretation as the basic option for getting started quickly. JIT can be performed for a subset of performance critical code, or eventually for full programs.

There isn't any fundamental reason I cannot support all three, eventually.

The program under evaluation is volatile, so might also mostly be represented as a data stack. Special handling is needed because not everything is 'data' - a computed program may contain words that have not been linked, and lazy linking might expand a word into data based on demand. 

Evaluation can proceed left-to-right through our program then evaluates remaining block values either right-to-left or in parallel. The result of this evaluation would be held on the program data stack.

*Note:* I like the idea of a "checkpointing" evaluation, such that we can always interrupt evaluation and have a valid program, at most losing some intermediate work. However, checkpointing evaluation requires some careful attention if I also want to support in-place update for unique array objects. An intermediate option is to support 'safe interrupt' points, but I also don't want expensive atomic ops on those actions. So my best option is perhaps to limit interrupts to some periodic event that would perform synchronization anyway such as GC. In any case, to support checkpointing evaluation, I need a small volume of volatile roots - registers - per thread.

## Memory Management

Some desiderata:

* logical copy on `c` for evaluated objects and data
* unique arrays with in-place updates, copy on write
* copying or compacting GC to resist fragmentation
* generational GC to avoid copying of stable objects
* hierarchical GC to limit synchronization frequency

I think my best bet for GC is a generational mark-compact GC. Mark-compact has a big advantage of preserving stack order, which is convenient for reasoning about cache performance and generations.

I expect type information in the pointer regarding the size of objects - two word cell vs. tagged object where size is based on the header. For mark-compact, I must record this type information during mark so I can robustly compute size upon compact. So I expect I'll need 3 bits per marked object - two for tri-color marking (white/grey/black) and one for object-vs-cell. This corresponds to about 3 bits per allocation: ~5% memory on a 32-bit system, ~3% on a 64-bit system memory that cannot be allocated to normal objects. This isn't ideal, but it is acceptable.

Generational GC requires we track mutations within the old generation. I would like to do this at the field granularity, which allows working with large arrays (mutating parts of the array, scanning just the parts we mutate, logically splitting arrays among multiple threads). My current plan is to use a hashtable of page→bitfield in the word size. Bitfields allow adjacent fields to be represented and scanned together, and reduce memory overhead. Assuming larger hashtables are at least half full, this results in at worst 12.5% overhead on a 32-bit system (or 6.25% on a 64-bit system) as a percentage of the old gen size. In practice, worst case behavior should be rare outside of working with massive mutable arrays.

I can try this at a level of objects or fields. To support large linear arrays and logical slicing (so I can divide an array among many threads then stitch it back together), it seems field-level tracking might be the better option. Unfortunately, I cannot think of a great way to do this.

Due to in-place update for uniquely referenced arrays and other objects, I need to track updates so we can scan for references from older objects to younger ones. This means a simple write barrier, one I'd need anyway due to copy-on-write for shared arrays, and a list of references from the older generation acting as extra roots. This is a small overhead for a significant reduction in allocation.

## Parallelism and Threads

I want parallelism with minimal (and very simple) synchronization.

Within a context, each thread should have a nursery - a volume of memory that it controls exclusively and hence may garbage collect *without* synchronization. This might start as, for example, 256kB. Overly large allocations might automatically operate in the shared context memory. When full of cruft, the thread is relocated with a fresh nursery. As the context fills, we can gradually GC old data.

Importantly, a thread must never reference into another thread's nursery. Given in-place mutations, tasks computed within a thread cannot robustly be shared until said thread promotes its nursery into the shared memory region. Conversely, a thread cannot consume the 'result' of an evaluation until said result is promoted into shared memory. So each nursery needs to track recently pending tasks and results.

Promotion of a nursery would reduce efficiency of a per-thread survivor space. OTOH, we can heuristically delay promotions based on pending tasks, whether the shared context is already busy, etc. to form a reasonable batch of tasks.

Effort quotas also require some attention. Do I associate efforts with a stream? a task? a context? For now, I'll use the context granularity because it's simple to implement. I might try to work out something more precise, later, if I feel a strong need to do so.

Each task should probably also track its dependencies - new tasks produced - such that we don't need to deep-scan a result to complete the evaluation.

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

## Lazy Evaluation and Copies

Awelon's basic evaluation strategy is lazy modulo copy. But with explicit laziness, we could also be lazy across copies, sharing the result of deferred computation based on knowing the logical copy structure of data. The main concern I have with this is how it interacts with serialization or distribution. I lose track of the copy origin when a program is wholly or partially serialized. 

This could be mitigated by somehow rejoining common substructure upon parse. But it might be better to stick with the basic strategy of strict copies, for now.

## Stack Representations

Evaluation has both the program/data stack and the auxiliary. Modeling multiple stacks and a heap within a single address space needs some attention. 

I will probably aim to model the stack as a normal heap object - a unique array, or linked list of array fragments, or similar. The latter option is especially nice because the 'top' pages of the stack would tend to be a newer object, and stack pages would generally align with locality and generation.

An advantage of modeling a stack as a unique array is that it becomes feasible (with appropriate accelerators) to represent the entire evaluation and GC techniques *within* Awelon code.

## Memory Representations

Let's say our allocations are two-word aligned, and our words are either 4 bytes or 8 bytes depending on machine. This gives us 3 reliable pointer tag bits. Use of pointer tag bits can discriminate a few 'small' values to reduce allocation costs. 

A modest proposal:

        b00     small constants, built-in operations
        b01     tagged objects (type in header)
        b10     composition cell (B, A) => [B A]
        b11     constructor cell (H, T) => [H T :]

Small constants would be values we can represent within a word. Small natural numbers and tags are perhaps the most critical, but it would be tempting to also support small integers, many decimals, small texts or labels, etc.. Covering accelerated functions might also be convenient. On a 32-bit system, we don't have a lot of room for innovation here. On a 64-bit system (which is the expected case) a 'small' constant can cover a lot of common values without allocations.

Cells cover list and composition constructures, which I imagine will be used very frequently based on my experience with Haskell. Both cells should be the same size to reduce GC mark-compact overheads.

The `b` bit indicates that the item represents a block or value word, something that can be bound with the `b` operator or moved by data shuffling operations. This extends the expressiveness of composition cells to support bind, compose, pairs, sums, etc.

Tagged objects cover everything else, at the cost of a little extra memory. I'll track an extra bit during mark-compact GC to distinguish double word cells vs. tagged objects. Uniquely referenced, writable memory will always be a tagged object.

Note: I might want to model buffered arrays, where we can push or pop elements efficiently. Logical reversal of lists and arrays could be useful here.

### Small Constants

I desire several small constant "types", so let's say three to four bits of type data. A viable list of types to support:

* built-in operations
* natural numbers
* integers
* rational numbers (two small nats)
* small decimal numbers (small exponent, large mantissa)
* labels for sums and variants
* short texts
* short binaries

I'd like to ensure that the zero word is equivalent to a built-in NOP action. Natural numbers should cover a reasonably large volume, and integers should ideally cover the same volume of naturals (plus and minus). So focusing on just these at the moment:

         00     extended
         10     naturals
         x1     integers

This is in addition to our `i00` for the small constants, so extended types are `...00i00` and naturals are `...10i00`. This means our small naturals on a 32-bit system cover at least 27 bits, which is sufficient to cover a volume roughly from zero to 128 million. On a 64-bit system (which is the expected case) the effective range is much larger. Integers encode a similar volume but in both positive and negative directions. 

The extended types would then cover everything else, consuming a few more bits to distinguish the data types. We don't need very many built-in operations.

### Sliceable Arrays and Ropes?

I would like easy support to reference and logically recompose array fragments. We might treat array-slice as an alternative representation of array. Maybe something like:

        (slice array-ref offset count)

This seems quite feasible, but would require additional support to represent composition of arrays. Rather than a linked list of chunks, perhaps logically appended slices and arrays.

        (append array array size)

Here the `size` field is just the sum of the sizes of the component arrays, as a natural number. This is important for efficient indexing of the array. We can move append slices around until a target offset is near the root, and we can cut an array into smaller slices if we want to focus on multiple locations (then append them later).

## Mutable Objects

A purely functional language can have in-place mutable data so long as it is not shared, a unique reference. For this runtime, I intend to support copy-on-write using dynamic information about sharing. Use of `(nc)` types can help enforce uniqueness (modulo debug or persistence snapshots).

I need some bits in the object header to indicate a unique object. Additionally, in-place update may interact with a generational GC: when we update an object from an older generation in place, we must also track it as a "root" for purpose of GC on younger generations. I need a second bit for this write barrier.

Evaluation doesn't necessarily eliminate uniqueness, e.g. because `(eval)` or `(par)` should preserve uniqueness of contained data. This is essential if we're to use patterns like dividing an array computation across multiple parallel threads then stitch the results back together. 

List and composition cells might be a special case for mutable objects. We cannot update such objects in place because we lack header bits to track uniqueness, and we lack generational write barriers. But copies also need attention, since they might reference unique objects. We may need to wrap copied lists with a header. I might convert large lists to a shared array upon copy.

Note: an intriguing option would be to allow the GC to track whether a reference is still shared, e.g. by clearing the shared bit and marking it again on second reference. But this seems unpredictable in its utility, and adds overhead for the GC, so I won't pursue it.

### Arrays

Arrays are the most important mutable objects. Given effective acceleration of arrays, we can model many low level algorithms, mostly excepting those that require multi-threaded access to the same array.

I've considered support for 'array buffers' - arrays with some extra space to push/pop data - but I've decided they're better modeled explicitly at the language layer so they have a more robust structure across composition or serialization. Developers could simply fill an array with `0` or `~` or `[]` values then modify it in place.

Array slices are an interesting option, too, enabling lightweight sharing of array data.

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

* reference counting - a good algorithm for long-lived, persistent resources. A precise reference count needs type information that GC understands, such as `3b+11d` meaning "referenced three times as a binary, eleven times as a dictionary", so we know how to interpret a resource. Conservative GC would essentially involve a one-size-fits-all parse algorithm (require hashes encode as 60 base64url bytes with a separator) but should also work reasonably well in practice.

* lazy reference counting - reference counts can lag behind updates to root objects. We could model this by tracking an RC deltas table separately from the main RC table, to support incremental GC of resources and perhaps avoid the initial parse and processing of resources that are only briefly referenced. NOTE: since we might actually *receive* resources out-of-order, an RC deltas table could usefully delay positive increfs until a parse can be run.

* ephemeral resources - a context can hold onto a resource that has a zero persistent reference count. I don't want to scan contexts when it's time to GC, especially since they might be spread across many processes. So I'll use a shared memory ephemeron table (likely a simple hash-to-refct table). A shared memory mutex (with the robust option) should be sufficient for performance since I don't expect a lot of contention on this table. It should be sufficient to reference only root values from our contexts.

GC for secure hash resources, and a shared ephemeron table between processes, is perhaps the main requirement for multi-process access. I could (and probably should) use shm_open for the ephemeron table.

I'm concerned that a crashed process might hold indefinitely onto resources. I could try an expiration model, with processes periodically scanning the contexts to maintain the ephemeron tables. But this seems like a problem to solve when it becomes an actual problem.

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

The runtime supports short-lived transactions. 

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

How can I gain a good insight about where memory is being consumed? Ideally, non-invasively?

Well, every value in Awelon is a block, a closure. A reasonable option is to track each block to its syntactic origin (modulo binding). 



## JIT Compilation

I've discussed compilation to a register machine in the main Awelon language document. This seems highly applicable to compiling to a JIT target. A static type analysis might also help. 

It seems valuable to get JIT working early, otherwise it's easy to get caught up in optimizing the interpreter. That said, I do want to focus first on a fast interpreter, so I'm not relying entirely on JIT for performance.
