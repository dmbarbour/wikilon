
# Runtime Design

A primary goal for runtime is performance of *applications*. Awelon's [application model](ApplicationModel.md) involves representing state within the dictionary via RESTful multi-agent patterns (e.g. publish-subscribe, tuple spaces). Evaluation, concurrent update, incremental computing, and so on each contribute to application performance.

I'm also interested in predictable performance, and precise cost accounting so heavy users can (eventually) be charged appropriately. The runtime will aim to keep unpredictable performance features under client control via annotations and similar. I intend for Awelon runtime to be suitable at least for soft real-time systems to the extent that programmers control the nature and frequency of transactions on the dictionary.

Awelon is designed to afford efficient update patterns via secure hashes to name dictionaries from which another dictionary might inherit or contain.

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

I think my best bet for GC is to use copying for a younger generation, gradually build a set of survivors at one side of the heap. Then, when we reach some thresholds, we apply a *mark-compact* algorithm on the full heap to clear survivors. This is a well proven, general design. It does risk large GC pauses for mark-compact on a large heap with many survivors. But GC pauses are a lesser issue with pure computations. 

Due to in-place updates on some linear objects potentially updating objects in the survivor set, I may need a simple write barrier to track which objects have been written recently to provide additional GC roots. Depending on use of registers and stacks and in-place updates and unboxed arrays, we might be able to avoid a lot of intermediate allocations for common patterns in Awelon. 

I would like to avoid full compactions in the common case. So I might need an extra, intermediate generation that performs either copy or compaction. This complicates write barriers a bit insofar as I need to maintain lists of potential roots between generations. There are diminishing returns for having many generations, but even one or two generations would cover most common cases.

*Note:* be sure to use *two* bitfields for mark-compact bits. One for grey, one for black. This enables us to test at a word level whether a grey word matches a black word (do we need to scan more?), and to easily mask or test both bitfields using the same offsets. I'd also need to ensure space is available for marking in the worst case, so this would be an upper constraint on the surviving set. Fortunately, I only need mark bits during mark and compaction.

## Parallelism and Threads

I have an idea to use hierarchical heaps to support confined multi-threaded `(par)` computations. Communicating threads (which require queues, etc.) shall be modeled indirectly, e.g. via accelerated evaluation of KPNs. 

Each heap would allow allocation and GC independently of sibling thread heaps, though a parent might need to wait on a child thread. When a child heap fills, we can allocate a new one in the parent (and optionally move the most volatile data). When the parent fills, we can perform a parent GC using the normal copy/compact options.

Write barriers are necessary to track external references from a parent into a child heap. Hierarchy effectively gives us an implicit form of generational collection, and we can probably leverage that by modeling our intermediate GC generations in terms of a few layers of hierarchy. 

We would need to scan not just the current generation, but objects from multiple parent generations, to track references into a child heap. But conveniently, we can guarantee against sharing of objects between child threads. So this can simplify write barriers, since a thread only needs to update its personal list of external roots.

*Aside:* Hierarchy can safely be lost when we perform the parent compaction, instead allocating new heaps for each thread. OTOH, there might be some advantages to preserving hierarchical structure.

## Stack Representations

Evaluation has both the program/data stack and the auxiliary. Modeling multiple stacks and a heap within a single address space needs some attention. 

I will probably aim to model the stack as a normal heap object - a unique array, or linked list of array fragments, or similar. The latter option is especially nice because the 'top' pages of the stack would tend to be a newer object, and stack pages would generally align with locality and generation.

An advantage of modeling a stack as a unique array is that it becomes feasible (with appropriate accelerators) to represent the entire evaluation and GC techniques *within* Awelon code.

## Memory Representations

Let's say our allocations are two-word aligned, and our words are either 4 bytes or 8 bytes depending on machine. This gives us 3 reliable pointer tag bits. Use of pointer tag bits can discriminate a few 'small' values to reduce allocation costs. 

A modest proposal:

        i00     small constants, actions, tags
        i01     tagged objects (type in header)
        i10     pair cons (B, A) => [B A]
        i11     list cons (H, T) => [H T :]

The `i` bit enables any value to be logically inlined as a function. This is mostly useful in context of operations like bind or compose, enabling uniform treatment of pair cons to support pairs, bindings, compositions, etc.. It also reduces overhead for arity checks, since we don't need to peek beyond the pointer to determine how an object behaves under bind, swap, etc..

Small constants would be values we can represent within a word. Small natural numbers and tags are perhaps the most critical, but it would be tempting to also support small integers, small texts or labels, etc.. Covering accelerated functions might also be convenient. On a 32-bit system, we don't have a lot of room for innovation here. But on a 64-bit system, 'small constants' can adequately cover a lot.

Tagged objects can cover any larger object, though the added header overhead may be significant. Modeling tags uniformly as small constants should also simplify the 'compact' phase of mark-compact algorithms. Programs, arrays, records, record-of-arrays vs. array-of-records, etc.. might be handled via tagged objects.

Optimizing the basic cons cell allows lightweight common constructions - the `[[B]A]` block bind, `[[B][A]]` pairs, and `[B A]` composition. List cons cells cover common list and tree structured data. Larger lists or tuples might later be represented using tagged arrays.

## Persistence and Resources

A challenge I must still consider is how to model persistence and secure hash resources.

Some basic options:

* persistent compacting "heap" per environment, relative offsets
* LMDB persistence, with tables and transactions
* Filesystem persistence, directory or tar file layout

The LMDB option currently seems like a Goldilocks solution, trading some flexibility and control but handling various problems including atomic update, zero-copy access, and resource management.

Some thoughts:

* resist timing attacks - secure hashes as capabilities, must not be guessable by timing "no entity" responses and incrementally modifying the hash. Proposed solution: use first 60 bits for fast table lookups, then compare the remaining hash bits via constant-time algorithm. This constrains timing attacks to revealing up to 60 bits of an unknown resource name, and 60 bits reduces collisions to one in a billion or so (via birthday paradox).

* global addressing locally - I can map global addresses to, for example, 64-bit local addresses. Doing so could save me a lot of space and lookup overheads, but only if I use the local address in most cases. For secure hash resources, this would never be the case. I suppose I could use local addressing for transactions, but even those I might later wish to share. So I'll stick to global addressing, use of full secure hashes.

* plain text internally - I can rewrite 64 bytes base64url to 48 byte keys. The performance benefits would be very minor, but the binaries would be much less convenient and self explaining in context of external tooling (`mdb_dump` and similar). Similarly, we might benefit from tracking associative metadata (reference counts, mostly) using plain text.

* multiple reference counts - since a resource is just a binary representation, I may (on rare occasios) need to interpret it in multiple ways. So reference counts on an object might include metadata regarding how a resource is interpreted through a reference. We might use `3b 11d` to encode a reference count that an object is referenced as type `b` (binary) and eleven times as type `d` (dictionary patch). 

* lazy reference counting - we can temporarily allow a `0d`, indicating that we need to parse the dictionary patch and decref its dependencies. A zero reference count table may help incremental GC quickly return to objects that must be collected. We could add expiration indicators to zero reference count objects, to keep them available for an extra while.

* ephemeral resources - a context can hold onto a resource that has a zero persistent reference count. I don't want to scan contexts when it's time to GC, so my best idea at the moment is: 
 * use shared *counting bloom filters* at the environment layer
 * from each context, maintain reference counts in bloom filters
 * active contexts may rotate to fresh bloom filters if nearly



## Concurrent Update?

A dictionary will be maintained by multiple agents. In practice, most updates will follow simple patterns - command pattern, publish-subscribe, futures/promises. We can explicitly model intermediate structure to help avoid conflict or even to support collaborative update. Our runtime must provide at least effective support that will cover common update patterns without conflict. 

A DVCS inspired approach might model a 'working' dictionary for updates. When another agent updates the shared dictionary before us, we must merge their updates before we can apply our own. This works most easily when the agents in question update independent parts of the dictionary. 

The main difficulties with a DVCS approach is that 'merge' is relatively ad-hoc. A lesser issue is that we cannot detect read-write merge conflicts. I could try to keep some extra metadata within the dictionary to help prevent merge conflicts, e.g. by tracking within a dictionary that a word was recently renamed or that a command-pattern update occurs after another but not necessarily immediately after.

A transactional model would keep more meta-data and gradually construct a new dictionary. 



Alternatively, we can model dictionary updates more explicitly. With this, we could include flexible metadata per update and help reduce conflicts by merging concurrent updates in some consistent manner. The main cost to modeling dictionaries in this manner is indirection - we aren't working directly with Awelon dictionaries. If we want import/export, we need both the dictionary and a fair bit of metadata. It is feasible to model this metadata within the dictionary, or as part of an external transactions model.

With the alternative, I have an option of tracking transactional information *internally* to a dictionary (e.g. as a special dictionary object), or externally (as a transaction). The external option seems both more broadly useful

At the moment I favor the more explicit model, even if it requires more information to track concurrent transactions. 

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

## JIT Compilation

I've discussed compilation to a register machine in the main Awelon language document. This seems highly applicable to compiling to a JIT target. A static type analysis might also help. 

It seems valuable to get JIT working early, otherwise it's easy to get caught up in optimizing the interpreter. That said, I do want to focus first on a fast interpreter, so I'm not relying entirely on JIT for performance.
