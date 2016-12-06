
# Runtime Design

A primary goal for runtime is performance of *applications*. Awelon's application model involves representing state within the dictionary via RESTful multi-agent patterns (e.g. publish-subscribe, tuple spaces). Evaluation, concurrent update, incremental computing, and so on each contribute to application performance.

I'm also interested in predictable performance, and precise cost accounting so heavy users can be charged appropriately. The runtime will aim to keep unpredictable performance features under client control via annotations and similar. I intend for Awelon runtime to be suitable at least for soft real-time systems to the extent that programmers control the nature and frequency of transactions on the dictionary.

*Aside:* For naming, I'm likely to call this 'Wikilon Runtime' or `wikrt`. 

## Evaluation 

Evaluation will essentially proceed as a Forth-like stack machine.

* output program is data stack
* auxiliary continuation stack
* conventional program pointer
* add special `\return` words
* track available stack size

Evaluation proceeds left-to-right through the program, then evaluates remaining block values either right-to-left or in parallel. The main difference from Forth is that our final 'output program' may contain words that do not evaluate further, due either to a trivial link context or lack of definition.

Use of an auxiliary continuation stack and `\return` words enables a near-conventional approach of a program counter and threaded interpreter. 

It is feasible to further compile programs into a register machine, essentially by allocating top stack elements into registers. I would like to pursue this technique further, albeit perhaps only for JIT code.

## Memory Management

For efficient use of memory and CPU, I want to logically copy objects on `c`. Consequently, I cannot recycle memory on `d` unless I know that it's the final logical copy. I will need automatic memory management.

Automatic memory management in Awelon is simpler than in most languages. Awelon is acyclic, so could support reference counting. Awelon does not support ad-hoc mutation, reducing need for write barriers. Awelon is pure, which reduces concerns about observable latency for side-effects. (Granted, I still want predictable computation performance for external effects models, and the ability to compute in the background in parallel.)

I am concerned about memory fragmentation as caused by free lists. Memory fragmentation results in degrading performance for long computations. I feel compacting collectors are simpler and more trustworthy.

Compacting collectors are notorious for poor memory utilization. A certain amount of space must be kept free to perform compaction. For example, with a two-space collector, 50% of memory is kept free. Fortunately, this issue is mitigated by parallelism in a multi-agent environment. For example, if I have a hundred computations allocated 10MB each, and less than 1% of my time is spent on compaction, then I could make do with just one blank 10MB heap and 1% memory overhead for GC. In practice I would want extra blanks to support parallel compaction, but the overhead can be well under the 50% case.

Another important concern is lightweight parallelism. Compaction with active mutators is not simple, so we'll want to stop work before performing compaction. But synchronization isn't cheap, so we'll want to amortize it by delaying compaction as much as feasible (using only simple techniques). 

To delay compaction, we *must* recycle memory before compaction.

An intriguing option is hierarchical compaction. We model evaluation as occurring within a 'child' heap. The parent heap can provide a survivor space. We promote objects that survive two collections in the child to a survivor space so they are not copied by further compactions. (We can also promote child heaps that survive.) This delays compaction of the full 'parent' heap until it is out of space. We can explicitly model survivor spaces *within* the parent that can be compacted together with their origin. This would further delay compaction of the parent, and would offer considerable benefits even if performed only out to a few generations.

So we can have a parent heap host a bunch of child heaps that are each compacted independently, growing up to multiple generations with minimum synchronization to allocate survivor and compaction spaces in the parent heap. When a thread reduces to a final value, we recover the extra heap space when the parent computation performs its next compaction. 

This gives us some relatively lightweight *confined* parallelism, like `(par)` where we don't share intermediate results with other threads before evaluation completes. For accelerated KPNs, we might need to process subnet inputs in confined batches, enabling intermediate results between batches and relatively coarse-grained threading.

Besides hierarchical structure, we might also try to use free-lists for certain flyweight pattern objects, like intermediate tuples. 

## Memory Representations

In memory, a program will be represented by a contiguous array of words terminating in a special `\return` word, or a variant for tail-call optimizations. A call-return auxiliary stack is used. We will also try to optimize for `[A]a` patterns, which become `\push A \pop`. This generalizes to a pattern `[A] b b b a` which we might process by moving the third item.



## Bit Level Representations

I assume 8-byte alignment, and 3 tag bits. A viable encoding:

        000     small natural numbers
        010     value words (interned)
        100     static link action words
        110     actions words (interned)

        001     normal block
        011     tagged values
        101     (unused action)
        111     tagged actions

        (Fast Bit Tests)
        Deep Copy:      (1 == (1 & x))
        Tagged Item:    (3 == (3 & x))
        Action Type:    (4 == (4 & x))

Tagged items can have ad-hoc structure and keep extra type information in a header. Originally, the distinction between tagged values and normal blocks was more critical because I was using naive linked lists for blocks. I do not know whether it remains worth preserving this separation. But I can always tweak the representations later.

An interesting option for the reserved slot is representing linked-list structures - i.e. `[[hd][tl]:]` where `tl` is another list. Instead of a bunch of individual cons cells, our linked list could be modeled within a linked list of buffers. This would simplify a lot of collections-oriented operations, and even efficient representation for tries.


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

* determine up-fun-arg vs. down-fun-arg types
* precompute copy size for fixpoint loop
* accelerate multi-word actions
* JIT compilation of code

For s

## JIT Compilation

I must review literature on JIT for gradually typed languages, basic block versioning, etc.. Once I have my interpreter and basic accelerators up to speed, I should aim to get JIT working very early.

JIT will essentially be performed 
