
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
 * leverage LLVM or Clang API for JIT compilations
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

Dictionaries remain anonymous and immutable, named by secure hash. We'll use a transaction to name a mutable dictionary.

Some operations we might want to perform:

* read or update a word's definition
* input a program, binary, or dictionary
* output a program, binary, or dictionary
* evaluate, optimize, compile a program
* subscribe, receive alerts upon change
* rename a word or dictionary object
* create new anon word with prefix
* command pattern updates

In context of a multi-agent REST system with caching and subscription concerns, we might need to ensure every program we interact with has an name for future lookups. Or at least that we're only able to subscribe to named computations. For subscriptions, I may need to support callbacks. 

Conveniently, I don't need sophisticated fine-grained input or output. That is, I don't need to input individual words, open and close blocks. I can push that sort of trivial work up to Haskell easily enough. Simple binary streams is sufficient, such that I can work from lazy byte strings in Haskell. I might need to indicate which secure hash dependencies we're missing, but that's about it.

An optimized program may involve applying partial evaluation, staging, static linking, rewrite optimizations, etc.. The output is still an Awelon program. JIT compilation might involve conversion to LLVM or C for Clang. It would be... convenient to readily study output from JIT compilations. (*Aside:* C would be more widely useful if I can avoid too many dependencies.)

*Note:* Security models (HTTP authentication, HMAC bearer tokens, etc.) and collaborative conflict avoidance patterns (discretionary locks, [behavioral programming](http://www.wisdom.weizmann.ac.il/~bprogram/more.html)) are not part of this C runtime API. I believe they can be implemented more or less independently, and I don't want to commit to any particular model.

## Evaluation 

Evaluation will use a logical cursor into an Awelon program:

         \ [A]          =>      [A] \
        [B][A] \ a      =>      \ A [B]
        [B][A] \ b      =>      [[B]A] \
           [A] \ c      =>      [A][A] \
           [A] \ d      =>      \

        \ word          =>      word \      (if not linked)
                        =>      \ word-def  (if linked)

The cursor here is indicated by the `\` character. It moves left to right through the program, leaving evaluated code in its wake. It moves left for application. After we finish evaluation, we'll go back and evaluate any functions that have not been hit. It would be useful to track some metadata indicating which values have been evaluated already.

The runtime representation of this cursor might use a Huet zipper or gap buffer of some sort. We'll only have one cursor per program, modulo parallel evaluation of values or words.

## Program Representation

Words should be interned. The interned reference will include associative data: link metadata (arity, available outputs), link data (static link code if needed), possible JIT or accelerator references (with alternative for static type safe operation). Interning of words supports a lot of memoization and caching at that layer.

Small natural numbers should also receive compact representation.

Programs and blocks, at least during evaluation, will primarily be represented by simple linked lists. The primary motivation for this is *simplicity*. I've considered representations involving lists of array fragments with logical split/inline actions. But they add a lot of complexity for marginal benefits. Linked lists can get us up and running quickly, and are simple enough for JIT code to work with easily. Performance will be the job of JIT. 

Constant space traversal requires all the linked lists terminate in 

## Bit Level Representations

I assume 8-byte alignment, and 3 tag bits. A viable encoding:

        x00     smallish natural numbers
        010     value words and interned data
        110     action words and annotations

        001     block cons cell
        011     list cons cell 
        101     tagged values
        111     tagged actions

        (Fast Bit Tests)
        Shallow Copy:   (0 == (x & 1))
        Action Type:    (6 == (6 & x))
        Tagged Item:    (4 == (5 & x))

This encoding ensures zeroed memory is filled with shallow-copy structures. 

List cons cells refer to an `[[A] [B] :]` structure, generally terminating in a nil. 

List cons cells refer to a `[[A] [[B] ~ :] :]` structure.

*Note:* A value slot remains available at bit pattern `010`. I'm tempted to use this for *data list* cons cells* terminating in a `nil`. That is, structures of general shape `[[A] [[B] ~ :] :]`. 

## Memory Management

Memory will be managed via free lists, primarily. I might also ensure sufficient memory is reserved for occasional compacting collections.

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

