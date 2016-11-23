
# Runtime Design

A primary goal for runtime is performance of *applications*. Awelon's application model involves representing state within the dictionary via RESTful multi-agent patterns (e.g. publish-subscribe, tuple spaces). Evaluation, concurrent update, incremental computing, and so on must each contribute to overall performance.

Besides efficiency and scalability, I'm also interested in predictable performance, and precise cost accounting so heavy users can be charged appropriately. The runtime will aim to keep unpredictable performance features under client control via annotations and similar. I intend for Awelon runtime to be suitable at least for soft real-time systems to the extent that programmers control the nature and frequency of transactions on the dictionary.

*Aside:* For naming, I'm likely to call this 'Wikilon Runtime' or `wikrt`. 

## Performance Strategy

* simple memory model
 * linear ownership of program structure
 * memory use reflects serialized program size
 * constant space traversal (Morris algorithm)
 * structure sharing via words only
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
 * failures leave program in valid state
 * may lose partial work upon failure

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

The cursor here is indicated by the `\` character. It moves left to right through the program, leaving evaluated code in its wake. Specializations for `a d`, `i`, and `z` will support tail-call optimizations.

## Program Representation

Words, small constants, accelerated subprograms, etc. may be interned. This allows me to share a lot of work (memoization, static linking, linker metadata, etc.) for multiple instances of a word, or avoid allocations to work with natural numbers.

Other content - blocks, texts, binaries, etc. - is deep-copied and uniquely owned. A general rule is that the cost of memory representation should directly reflect the cost of our serialized representation in a simple, predictable way. However, long term we might explore accelerated access to 'shared' objects via stowage and even logical trim/addend/etc..

Program structure will use a linked list of buffers. Viable representation:

        (header next ... elements ...)

The header must contain at least a buffer size and internal iterator. 

Buffer sizes include the header and may use a small set. Only a few bits are necessary. Conveniently, we can link buffers of 1024 and 512 together to support 1500 elements if we need to limit internal fragmentation. 

The iterator is necessary for constant-space deep copy, drop, etc.. It can be represented by a simple offset from the header into the elements. A buffer may have unused space at each end. Rather than try to track these within the header, I can represent special stop and skip actions within the buffer if there is sufficient space.

Remaining header bits can record useful metadata - substructural type, evaluation status. Maybe a tuple/sum/bool assertion. We may limit ourselves to a 32-bit header even on a 64-bit system. 

Evaluation uses at least two buffers: the 'stack' buffer to the left of the cursor and a 'program' buffer to the right. During evaluation, the stack buffer is reversed such that `next` is `prev`.

This simplifies the problem of logical inlining. Our cursor remains always at a border between two buffers, so we can simply use the `next` field to inline. This structure DOES mean we may need to allocate a buffer upon bind with `b`, if the receiving block does not have space. However, we can reserve a few slots in our blocks in anticipation of binding, and we need allocate at most once per several bind actions (e.g. allocate a block of size 8 to handle binding up to five more items). 

This program representation should give me stack-like performance, efficient deep-copy, minimal pointer chasing etc.. I expect a significant performance benefit over cons-cell linked lists. Meanwhile, the representation seems simple enough to work with JIT as needed.

## Bit Level Representations

I assume 8-byte alignment, and 3 tag bits. A viable encoding:

        x00     small natural numbers
        010     value words (interned)
        110     actions words (interned)

        001     normal block
        101     unused, reserved
        011     tagged values
        111     tagged actions

        (Fast Bit Tests)
        Deep Copy:      (1 == (1 & x))
        Tagged Item:    (3 == (3 & x))
        Action Type:    (6 == (6 & x))

Tagged items can have ad-hoc structure and keep extra type information in a header. Originally, the distinction between tagged values and normal blocks was more critical because I was using naive linked lists for blocks. I do not know whether it remains worth preserving this separation. But I can always tweak the representations later.

An interesting option for the reserved slot is representing linked-list structures - i.e. `[[hd][tl]:]` where `tl` is another list. Instead of a bunch of individual cons cells, our linked list could be modeled within a linked list of buffers. This would simplify a lot of collections-oriented operations, and even efficient representation for tries.

## Memory Management

Memory will be managed during normal computation by free lists. The explicit copy/drop actions of Awelon effectively gives us manual memory management.

A simple buddy-list allocator should be sufficient. In normal PLs, buddy lists introduce a lot of internal fragmentation. But with the linked-list-of-buffers as a primary data structure, that issue is mitigated because I can transparently compose small buffers to represent a larger one, and thus keep internal fragmentation under predictable control.

Memory compaction and defragmentation are also important, but I don't want to do unpredictable-cost things implicitly. Also, fragmentation won't be a problem for all programs, just those that have highly dynamic memory behaviors. So I'll leave this to an external API call, perhaps coupled to a notion of taking a program snapshot and opportunity to resize memory.

If allocation fails because we've hit a memory quota during evaluation, we must guarantee that we have a useful state. If we run out of memory on program *input*, that will need a distinct error type because we cannot guarantee a complete program. 

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

It doesn't seem these really need any special support, other than to associate `(.foo)` to `(:foo)` when initially interning so it's a symbol-level pointer test. 

## Value Stowage

        (stow)

I might want latent stowage, performed based on memory pressure. This isn't too critical if I model batch data structures (like the log-structured merge tree). But it's still convenient.

I can potentially compute the size, hash, and serialization of a subprogram by use of Morris traversals instead of allocating and resizing a binary. If so, this will be a lot simpler and more efficient than my earlier efforts.

## Numbers and Arrays

Other than small naturals, scalar numbers use tagged values, a boxed representation.

I intend to eventually support 'unboxed' fixed-width numbers in context of typed arrays and matrices. This is necessary to accelerate linear algebras, leverage the GPGPU, or enter a domain of high performance computing in general. But I won't bother trying to optimize basic numeric processing for most Awelon code.

## Optimizations

For visible optimizations, we can at least perform:

* staged partial evaluation up to arity
* static linking for words
* rewrite optimization

Some invisible performance enhancers:

* precompute copy size for fixpoint loop
* accelerate multi-word actions
* JIT compilation of code

For s

## JIT Compilation

I must review literature on JIT for gradually typed languages, basic block versioning, etc.. Once I have my interpreter and basic accelerators up to speed, I should aim to get JIT working very early.

JIT will essentially be performed 
