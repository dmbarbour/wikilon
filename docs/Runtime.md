
# Runtime Design

A primary goal for runtime is performance of *applications*. Awelon's application model involves representing state within the dictionary via RESTful multi-agent patterns (e.g. publish-subscribe, tuple spaces). Evaluation, concurrent update, incremental computing, and so on must each contribute to overall performance.

Besides efficiency and scalability, I'm also interested in predictable performance. The runtime will aim to keep unpredictable performance features under client control via annotations and similar. I intend for Awelon runtime to be suitable at least for soft real-time systems to the extent that programmers control the nature and frequency of transactions on the dictionary.

*Aside:* For naming, I'm likely to call this 'Wikilon Runtime' or `wikrt`. 

## Performance Strategy

* simple memory model
 * linear ownership of program structure
 * memory usage reflects program size
 * constant space traversal (Morris algorithm)
 * structure sharing is via unlinked words
 * use both free-list and compaction GC

* accelerated functions
 * Data plumbing, fixpoint loops
 * Numbers and arithmetic 
 * Linear algebras for GPGPU
 * KPNs for cloud computing
 * pure register machine variant

* JIT compilation
 * leverage LLVM for effective JIT compilations
 * interpreter with expectation of fragmented JIT
 * early focus on JIT as performance representation
 * support memoization and component reuse of JIT

* lightweight parallelism
 * parallelize evaluations within program
 * background evaluations of the program
 * prioritize partial evaluations for latency
 * accelerate bounded-buffer KPNs, eventually
 * stop the world only for compaction GC

* lightweight failure model
 * evaluation errors represented in code
 * quota failures by aborting computation
 * may lose work to checkpoint on failure

* lightweight debugging model
 * leverage `(@gate)` annotations
 * support trace logs or profiling
 * breakpoints and program animation
 * record intermittent frames on effort
 * stream compression of eval history

* incremental computing
 * implicit memoization of definitions
 * explicit memoization via `(memo)`
 * pattern based support from programmer
 * precisely track which words are linked
 * stage memoization for update frequency

* multi-agent concurrent codebase
 * LMDB for lightweight transactional data persistence
 * RESTful long-running transactions or sessions
 * reactive computing: callbacks, rate-limited quotas
 * recovery from transaction conflicts, not just abort
 * higher level actions to support semantic merging
 * prioritize transactions upon concurrent conflict

Long term, we might also support distributed transactions (via 2PC or 3PC, or X/Open XA). This could be useful for certain application models, and for integrating with external resources. However, it is low priority.

## API Concepts

The runtime API will be oriented around an agent's view of the system.

An agent will view and update a dictionary through the window of RESTful, hierarchical, long-running *transactions*. Transactions may have names, URLs, and serializable representation. Transactions generally track reads to support detection of read-write conflicts. Transactions may a few support structured updates that are merge-friendly, such as renaming a word or command pattern. Named dictionaries are effectively just 'rooted' transactions.

Views on a dictionary will generally involve reading a word's definition or evaluating some code. Evaluation of anonymous computations can generally be represented in terms of evaluating the definition of a corresponding `$secureHash` word. Word-level tracking is convenient for RESTful reference, caching, sharing, and further composition. Computations may need related attributes to control resources (space, effort), support active debugging and background parallelism, partial evaluation hiding some words, callbacks upon invalidation, etc..

There may also be some overall configuration of the virtual machine - LMDB storage, worker threads, etc.. But the API will be dominated by transaction management.

Between these, I imagine at least three handle types: a machine has multiple transactions, a transaction has multiple computations. Quotas may exist at multiple layers.

*Note:* Security models (HTTP authentication, HMAC bearer tokens, etc.) and collaborative conflict avoidance patterns (discretionary locks, [behavioral programming](http://www.wisdom.weizmann.ac.il/~bprogram/more.html)) are not part of this C runtime API but can be implemented in a separate layer.

## Entry and Extraction

Entry of a program as text, and extraction of results as the same, is the simplest option. I may need to accommodate large inputs or results regardless, and (potentially) streaming input or processing. While it might be convenient to support AST-level manipulations, I won't be doing so at the C runtime API. 

Defining a word would involve entering a program without evaluating it. Not that there's anything wrong with evaluating before setting a definition (i.e. storing a checkpoint). 

## Memory Representations

I assume 8-byte alignment, and 3 tag bits. 

        x01     smallish natural numbers
        011     value words and interned data
        111     action words and annotations

        000     cons cell (2 words)
        010     (unused, reserved)
        100     tagged objects
        110     tagged actions

        (Fast Bit Tests)
        Shallow Copy:   (1 == (x & 1))
        Action Type:    (6 == (6 & x))
        Tagged Item:    (4 == (5 & x))

Under the hood, I'll represent a basic program as a linked list of cons cells. Words will be interned. Small natural numbers will be represented within the pointer. Linked lists will be deep-copied and uniquely owned. Tagged items cover everything else, providing extra type information in the first byte referenced from the pointer.

A simple block can be directly represented by another linked list. However, blocks will frequently be tagged objects to support parallelism, memoization, evaluation status, substructural attributes, tuple assertions, and so on.

Linked lists will terminate in a common NULL value, a small constant representing `[]`. The space held by this NULL is important for later constant space traversal (via Morris algorithm), ensuring we won't need stack allocation for a lot of generic processing (copy, drop, serialization, etc..) 

## Memory Management

Memory will mostly be managed via free lists. 

Awelon doesn't need conventional GC because copy/drop are explicit, and forgetting to drop something is rather obvious in the program output. However, free lists can fragment, and I might favor an occasional defragmenting copy collection. This might be supported by explicit user action.

A related point is to ensure that, if computation halts for any reason, we always have a useful state that we can process.

## Evaluation Approach

Evaluation will primarily involve processing a linked list using a Huet zipper like structure to focus evaluation. 

It is possible to use idioms that will distribute evaluation reasonably well, e.g. parallelize evaluation on copy and delay the copy until we need the result, or in `[B][A]a => A [B]`, find out what happens to `[B]` before returning to `A`. But focusing on throughout and cache coherency, evaluating left to right, is probably okay, too.

To keep it simple, evaluation within a given block will likely be sequential by default. Parallelism will be kept to block values and specific accelerators like KPN, linear algebra.

My goal is a rasonably fast interpreter and early integration of JIT, switching to a naive compiler. I'll need to experiment with JIT early on, to avoid getting trapped by interpreter development. Awelon should be very efficient between JIT compilation and some well chosen accelerators. 

*Note:* Blocks will copy normally even in context of a fixpoint accelerator. However, JIT might bypass copying within loops based on an escape analysis.

## Computations

A computation should be snapshot consistent. We should also be able to determine when our computation invalidates, but instrumenting this might require a flag for the computation. We might track invalidation for transaction merges, too, which also requires configuration.

### Value Sealers

        (:foo) (.foo)

I can record in `(.foo)` the interned reference to `(:foo)` for efficient sealing/unsealing. 

### Value Stowage

        (stow)

I might want latent stowage, performed based on memory pressure. This isn't too critical if I model batch data structures (like the log-structured merge tree). But it's still convenient.

I can potentially compute the size, hash, and serialization of a subprogram by use of Morris traversals instead of allocating and resizing a binary. If so, this will be a lot simpler and more efficient than my earlier efforts.

