
# Runtime Design

A primary goal for runtime is performance of *applications*. Awelon's application model involves representing state within the dictionary via RESTful multi-agent patterns (e.g. publish-subscribe, tuple spaces). Evaluation, concurrent update, incremental computing, and so on each contribute to overall performance.

Besides efficiency and scalability, I'm also interested in predictable performance, and precise cost accounting so heavy users can be charged appropriately. The runtime will aim to keep unpredictable performance features under client control via annotations and similar. I intend for Awelon runtime to be suitable at least for soft real-time systems to the extent that programmers control the nature and frequency of transactions on the dictionary.

*Aside:* For naming, I'm likely to call this 'Wikilon Runtime' or `wikrt`. 

## Evaluation 

Evaluation will essentially proceed as a Forth-like stack machine.

* output program is data stack
* auxiliary call-return stack
* conventional program pointer
* add special `\return` words
* track available stack size

Evaluation proceeds left-to-right through the program without backtracking. The main difference from Forth is that our 'output program' may contain some words. We'll use a simple program pointer to represent what we're evaluating. 

I'll focus initially on an interpreter. But we could later compile to use a register machine, to handle allocation and writes of in-memory representations, etc..

## Memory Management

On copy with `c`, one option is to deep-copy structure. This trivializes memory management, at the cost of constructing many volatile copies. I would prefer something vastly more efficient. Deep copy on fixpoint loops is not efficient or optimal, for example. 

But logical copies require automatic memory management.

Awelon, fortunately, has some nice properties for memory management. Awelon never produces memory cycles, so plain old reference counting should work effectively. Awelon lacks stateful aliasing, so we can potentially avoid the whole issue of write barriers in a generational GC.

Naive reference counting is inefficient, especially in parallel code where RC updates must be atomic. A mitigating technique: defer RC for roots to discrete steps. At each step, incref the new roots then decref the old ones. This requires holding a snapshot of the old roots. The actual RC updates can be batch processed, guarded by a mutex, so those costs are amortized.

Ideally, I would also defer RC for volatile objects.

A viable option is support a per-thread 'nursery' where small, volatile objects are allocated and dropped without book keeping. We then require a copy-collector and bump-pointer allocator. When the nursery is out of space, we update reference counts then compact objects into a fresh nursery. 

We could augment the nursery with a survivor space - an extended life span for volatile objects. An intriguing possibility is to only perform the RC pass when we compact the survivor space. We might then have a dozen nursery first-gen nursery collections per RC pass, at the cost of always keeping the prior snapshot in the survivor space.

So... we'd have RC only for long-lived objects shared by multiple threads. 

An interesting option is to combine the RC heap with structure sharing or interning - implicit stowage. Doing so is certainly viable, and would guarantee a convenient structural property on representations. But it is not critical.




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
