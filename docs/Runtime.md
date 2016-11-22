
# Runtime Design

A primary goal for runtime is performance of *applications*. Awelon's application model involves representing state within the dictionary via RESTful multi-agent patterns (e.g. publish-subscribe, tuple spaces). Evaluation, concurrent update, incremental computing, and so on must each contribute to overall performance.

Besides efficiency and scalability, I'm also interested in predictable performance, and precise cost accounting so heavy users can be charged appropriately. The runtime will aim to keep unpredictable performance features under client control via annotations and similar. I intend for Awelon runtime to be suitable at least for soft real-time systems to the extent that programmers control the nature and frequency of transactions on the dictionary.

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

To support atomic updates, I have at least two options. One is the DVCS approach: an agent operates in a 'workspace' with its own logical copy of the dictionary, then we 'merge' updates from multiple agents with an ad-hoc best effort to resolve conflicts. Another approach is long-running hierarchical transactions, which is basically the same thing but we also track reads, evaluations, assumptions or assertions, and thus we can detect read-write conflicts and push them back to the agent. With transactions we might provide merge-friendly structured edits, such as renaming an object or constructing a new work order or command operation without concern about how things are named.

I think the 'transaction' oriented dictionary is generally better.

It seems to me that most input and output can occur as binary streams or chunked lists. Fine grained program input (e.g. at the level of open/close block and add word) seems unnecessary. Whether it's worth introducing depends on how much it complicates the runtime API. I may need to track down missing secure hash resources, however. 

*Aside:* By 'optimized' output, I want *Awelon* code after all the rewrite optimizations, static linking, staging, etc.. By 'compiled' output, I would like relocatable JIT code (Clang or LLVM?) or similar.

*Note:* Security models (HTTP authentication, HMAC bearer tokens, etc.), collaborative conflict avoidance patterns (discretionary locks, [behavioral programming](http://www.wisdom.weizmann.ac.il/~bprogram/more.html)), and accounting (quotas, policy for costs shared via memoization or stowage), are not part of this C runtime API. I believe they can be implemented more or less independently, and I don't want to commit to any particular model.

## Evaluation 

Evaluation will use a logical cursor into an Awelon program:

         \ [A]          =>      [A] \
        [B][A] \ a      =>      \ A [B]
        [B][A] \ b      =>      [[B]A] \
           [A] \ c      =>      [A][A] \
           [A] \ d      =>      \

        \ word          =>      word \      (if not linked)
                        =>      \ word-def  (if linked)

The cursor here is indicated by the `\` character. It moves left to right through the program, leaving evaluated code in its wake. Specializations may exist, e.g. for `i` or `a d` or `z` to support tail-calls. After we finish evaluation, we'll go back and evaluate blocks remaining in the output. It would be useful to track some metadata indicating which values have been evaluated already.

The runtime representation of this cursor could use a Huet zipper or a gap buffer of some sort. I imagine I'll only have one cursor by default, modulo parallel evaluation. Parallelism will be focused on the block values - I likely won't use parallelism by default, excepting potentially for final output values.

## Program Representation

Words, small constants, etc. will be interned. Programs will be represented as a linked list of words and programs. For now, I'll keep it simple and make it work.

A viable alternative to linked lists is a chunked array list with logical inlining and blank spaces to logically shrink. Logical inlining and blanks complicate interpreter logic, but improve memory locality, copy, and memory management. This might be worth pursuing once the basic link list approach is implemented.

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
        Shallow Copy:   (0 == (1 & x))
        Action Type:    (6 == (6 & x))
        Tagged Item:    (5 == (5 & x))

A cons cell has two words. Tagged values or actions may have more. List cons cells represent a `[[A][B]:]` structure, which must end in List nil or a normal block.

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

