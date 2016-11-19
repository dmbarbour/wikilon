
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

An agent will view and update a dictionary through the window of RESTful, hierarchical, long-running *transactions*. Transactions will have plain-text names (if you ask for one) and serializable representation.

Transactions wrap a dictionary to add useful features:

* track reads and detect read-write conflict
* support merge-friendly structured edits (rename, command pattern)
* support naming conventions and dictionary objects
* discretionary cooperation - locks, behavioral programming

Dictionaries remain anonymous and immutable, named by secure hash. We'll use a transaction to name a mutable dictionary.

Some operations we might want to perform:

* read or update a word's definition
* input a program, binary, or dictionary
* evaluate a program, compile a program
* rename a word or dictionary object
* create new anon word with prefix
* command pattern updates

A 'compiled' program means whatever we have after evaluation, staging, static linking, and rewrite optimizations. It's still Awelon code at that point. It might be useful to go further, e.g. to view generated LLVM JIT code. 

For an evaluated program, it might be useful to extract some outputs. 





We might understand the former as a simple means of inputting a program. 

Views on a dictionary will generally involve reading a word's definition or evaluating some code. Evaluation of anonymous computations can generally be represented in terms of evaluating the definition of a corresponding `$secureHash` word. Word-level tracking is convenient for RESTful reference, caching, sharing, and further composition. Computations may need related attributes to control resources (space, effort), support active debugging and background parallelism, partial evaluation hiding some words, callbacks upon invalidation, etc..

There may also be some overall configuration of the virtual machine - LMDB storage, worker threads, etc.. But the API will be dominated by transaction management.

Between these, I imagine at least three handle types: a machine has multiple transactions, a transaction has multiple computations. Quotas may exist at multiple layers.

*Note:* Security models (HTTP authentication, HMAC bearer tokens, etc.) and collaborative conflict avoidance patterns (discretionary locks, [behavioral programming](http://www.wisdom.weizmann.ac.il/~bprogram/more.html)) are not part of this C runtime API but can be implemented in a separate layer.

## API Program Entry and Extraction

Entry of a program as text, and extraction of results as the same, is the simplest option. I may need to accommodate large inputs or results regardless, and (potentially) streaming text input and processing. While it might be convenient to support AST-level manipulations, I won't be doing so at the C runtime API. 

Defining a word would involve entering a program without evaluating it. Not that there's anything wrong with evaluating before setting a definition (i.e. storing a checkpoint). 

## Evaluation 

Evaluation will use a logical cursor within Awelon code:

        [C][B][A] \ a X =>  [C] \ A [B] X

Here the character `\` indicates our current focus. Alternatively, I could move my focus to `[C] A [B] \ X`, then eventually backtrack. But I think a left-to-right translation without backtracking is simple, efficient, and generally sufficient.

The only other interesting primitive is copy:

        [A] \ c X       =>  [A][A] \ X
                        OR  [A](par)c \ X

I can potentially parallelize evaluation and copy of `[A]` with progress in `X`. However, I'm not certain this will do much good in practice. Depending on how I approach representation of parallel copies, it might offer some benefits.

Anyhow, I currently plan to limit evaluation within a block to a single focus - a single thread. Parallelism is thus delimited by block values. 

I need an efficient representation that supports:

* bi-directional movement of the logical cursor
* efficient inlining of one program into another
* lightweight copy, growth of buffer, or blank spaces 
* lightweight drop, shrink buffer at arbitrary location or logically delete

## Memory Representations

Decisions made so far:

* intern words. Maybe other small constants and subprograms.
* efficient distinction of value words vs action words
* small natural numbers should have compact encoding
* deep copy block structure exposed on serialization
 * keep memory usage proportional to serialization
 * simplifies reasoning about program memory usage
* constant extra-space traversal of program structure
 * copy, drop, serialize without allocation
 * contiguous allocations on copy or read
 * evaluate without recursion on C stack
* support for tagged data
 * basis for accelerated representations
* don't do "logical inlining" at element level
 * logical inlining complicates evaluations, traversals
 * inline program list representations naturally

The [Ouroboros algorithm](http://math-porn.tumblr.com/post/98314044394/the-ouroboros-algorithm-a-constant-space-tree) or Morris algorithm should be good for constant space traversals. 

Things not decided:

* linked lists or chunks
* pointer tag bits

## Linked Lists or Linked Buffers

Linked lists seem less than optimal for my evaluator case. The fine-grained allocations, copies, pointer-chasing. High space overheads. Uni-directional structure that hinders moving the program 'focus' around as needed. But linked lists do have and advantage of uniform structure, simplicity, simple inlining.

Linking array-like buffers is an interesting alternative, especially if I can develop an efficient representation. They're inefficient for small blocks of just a few elements. But perhaps that's an acceptable tradeoff.

I can doubly-link the buffers to support the evaluation focus without a separate 'zipper' concept. The overhead for this is amortized by the buffer size. Buffers may have free space that we can write easily - we might have a value for 'unused slot' (like SP but without the space?)

An important design consideration for linked buffers is that we cannot encode buffer headers within the data without losing the ability to inline one buffer into another, unless we use a 'logical inline' reference which adds complexity I'd prefer to avoid.

## Memory Representations
An interesting option for large blocks is to use buffered array-lists. 

        (array-list-hdr next elementN .. element2 element1 element0)

        where
            array-list-hdr includes:
                array-list tag  (tagged values)
                buffer size     (grow without allocations)
                reverse-order   (logical reverse)
                an iterator     (for Morris traversal)
                element-count   (offset to element0)
            'next' is a continuing list, array-list, or NULL
            elements - one per word...
            
Buffered array lists are naturally encoded as tagged values. We can squeeze all the header information into a single word, with just a few bits to encode common buffer sizes. To support fly-weight allocations, we might also encode a reference to the 'next' buffered array list

Instead of linked lists, computing with buffered array lists is an interesting and very efficient alternative.

 It would eliminate allocations for small 'stacks'. 


could even be treated as distinct array list types.

. Relevantly, array lists aren't especially critical

Allowing for a 32-bit header, we might permit buffer sizes up to 1024 elements (in 8 common sizes - like 

Buffer size might be encoded as just a few bits, a small choice of common sizes. So we can probably encode the remaining count and iterator within the 

        

 Buffer size could be encoded in just a few bits, assuming if we assume powers-of-two or a fibonacci allocator. 


If buffer size is a power of two in a limited range (e.g. 16, 64, 256, 

The buffer size, iterator, and element count could easily be encoded within a 64-bit word, or even within a 32-bit word if we limit buffer sizes to about 256 elements. 


I could avoid the buffer size if I allocate a fixed buffer, perhaps 64 or 256 words.

In this case, we'd have one word per element. And the `





## Memory Representations

I assume 8-byte alignment, and 3 tag bits. A viable encoding:

        x01     smallish natural numbers
        011     value words and interned data
        111     action words and annotations

        000     cons cell (2 words)
        010     buffered array list
        100     tagged values
        110     tagged actions

        (Fast Bit Tests)
        Shallow Copy:   (1 == (x & 1))
        Action Type:    (6 == (6 & x))
        Tagged Item:    (4 == (5 & x))

I might sacrifice some natural numbers to support `[word]` singletons as a special value word.

Linked lists may be used for blocks, deep-copied such that we can destructively manipulate them and also to guarantee memory requirements are proportional to serialized program size (a convenient property for safe checkpoints). Small natural numbers are represented within the pointer. Words and common constants or source fragments may be interned, and hence do not need deep-copy. Tagged items can cover anything we miss, providing type information in the first byte(s) of the referenced memory.

An important consideration is constant-space traversals, which is critical for efficient copy, drop, serialization of a program, and for memory usage guarantees. 

I plan to use the Morris algorithm, which uses NULL slots in the tree structure to avoid stack allocations. Thus, lists must terminate in predictable NULL values. (*Note:* I can wrap `[A]` as `[[A] i]` if necessary to ensure NULL terminals.) The Morris algorithm can be adapted to more than two children per node, for example to represent arrays, at the cost of an extra 'iterator' slot internally to the array.



*Note:* A value slot remains available at bit pattern `010`. I'm reserving this in case opportunity arises for major performance benefits... something not adequately supported by tagged values.

## Memory Management

Memory will mostly be managed via free lists. 

Awelon doesn't need conventional GC because copy/drop are explicit, and forgetting to drop something is rather obvious in the program output. However, free lists can fragment, and I might favor an occasional defragmenting copy collection. This might be supported by explicit user action.

A related point is to ensure that, if computation halts for any reason, we always have a useful state that we can process.

## Computations

A computation should be snapshot consistent. We should also be able to determine when our computation invalidates, but instrumenting this might require a flag for the computation. We might track invalidation for transaction merges, too, which also requires configuration.

### Value Sealers

        (:foo) (.foo)

I can record in `(.foo)` the interned reference to `(:foo)` for efficient sealing/unsealing. 

### Value Stowage

        (stow)

I might want latent stowage, performed based on memory pressure. This isn't too critical if I model batch data structures (like the log-structured merge tree). But it's still convenient.

I can potentially compute the size, hash, and serialization of a subprogram by use of Morris traversals instead of allocating and resizing a binary. If so, this will be a lot simpler and more efficient than my earlier efforts.

