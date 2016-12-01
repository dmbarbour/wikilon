
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

Awelon, fortunately, has some nice properties for memory management:

* Awelon never produces memory cycles. 
* Awelon does not mutate shared objects.

Both of these points can reduce complexity in GC, avoiding write barriers and any special cycle handling. RC is feasible, but I'm not certain that RC is a good option. An interesting possibility is to simply use a hierarchical compacting collectors model:


        [survivors|.... nursery ....]

A heap is divided into a survivors area and two-space nursery. We guarantee that the nursery is never more than half full. When we move content from nursery RHS to the LHS, we grow the survivors pool. Eventually, the survivors pool is too large for effective computation to continue. At that point, we must shift the whole nursery into another two-space, and compact the survivors.

        [survivors|.... nursery ....]
            ||
            \/
        [s'|........nursery.........]

Memory utilization may still be high due to reusing the free space between parallel threads. For example, if we have 20 threads and a pool of 21 heaps, we can guarantee progress and only have 5% space overhead for toplevel GC. Internally, we might allow a nursery to grow until the survivors arena is a fair portion of the total memory. (We could also use less than the total nursery, initially, sort of 'grow' into it in reasonable chunks.)

Hierarchical nurseries could use the same properties, except with an interesting feature: we could allocate the 'twin' of a child nursery on the opposite nursery arena. Then we only need to guarantee that a child nursery is located properly before the thread returns. This gives us better utilization for lightweight parallelism.


*Aside:* It is feasible to promote toplevel survivors into a reference-counted structure... an ultimate 'deferred' reference count of sorts. OTOH, we might limit that toplevel to stowage, memoized dictionary computations, and other persistent structures.






















Each thread will use a lightweight nursery with a survivor arena for its computation efforts. This enables a lot of computations without performing any RC updates. 

We can feasibly model *hierarchical* nurseries, where we model one nursery within another and we await all child computations nurseries before we can collect the parent. This would be suitable for lightweight parallelism. In addition, multiple *sibling* nurseries may share the same RC heap with minimal collusion. 

Interestingly, we could just use hierarchical nurseries 'all the way down' starting with a top-level computation. This would be much simpler than a hybrid model.

Efficient reference counting means not updating the RC with every pointer manipulation. I propose a variant of deferred, ulterior RC. A bounded nursery with a survivor arena enables a lot of computation with volatile objects between rare RC updates. 

For parallelism, I have a choice. Either I attempt to share parts of the nursery between multiple threads, or I must promote objects out of the nursery before I support parallelism. If I want coarse-grained RC *and* fine-grained parallelism, this really isn't much of a choice: I need a shared nursery. 

Of course, the moment I share my nursery, I'll want sub-nurseries to help delay synchronization of threads. So the problem is somewhat regressive. A nursery is what exists to give a single thread some coarse-grained 'work', so perhaps I should promote anything else to the RC arena early on. 

Or alternatively, I should get rid of the 'threads' idea and instead model each heap as a process with limited message passing. In this 

Objects are promoted to a 'survivor space' that may be shared between threads. When we eventually compact the heap, we'll have objects that survive the survivor spaces. Those will be promoted into the RC space, shrinking the effective nursery.





So we might do something like a shared survivor space for multiple nurseries. 







Idea: a big nursery, with small fixed-size segments. Some segments are 'survivors' and are shared among multiple threads in the nursery. 

Multiple threads share the nursery, each one taking ownership of specific segments but *sharing* the survivor spaces. 



* a small nursery per thread
* large, shared survivor arenas
* can promote nursery chunks into arena

* can promote multiple small nurseries to arenas
* 

 We might collect the nursery ten or a hundred times per RC step, depending on how much long-lived information survives and nursery size.

Deferral could use buffered decrefs, or a simple snapshot of the program in the nursery. This would happen quite naturally with nursery allocation anyway. We incref the next snapshot then decref the old one, with special additions for 'large' objects, and special 'fast' promotions for parallel computation. A snapshot could potentially serve as a failure fallback, too.

This structure does have some challenges. For parallelism, I probably want a nursery per thread rather than a shared nursery for many threads. Though, I suppose I could go either way with that, if I simply allocate chunks out of the nursery per thread or have a limited number of threads per nursery.


I could use a multi-generational nursery, i.e. with an intermediate survivor space to delay interaction with the RC heap. This would add moderate complexity, but could reduce checkpoint frequency by up to a couple orders of magnitude, and would certainly enable a great deal more tuning. I'm a bit hesitant about this, mostly because it will interact with parallelism. 

A simpler RC-per-nursery step, promoting references that survive twice, should be sufficient. And I can apply parallelism the moment it is promoted 

Interestingly, the 'decref old snapshot' goal only needs the information used to incref the prior snapshot. I could record this precisely. 

Use of program snapshots is interesting because it would also support a general failure fallback. 

The idea here is that most of our pointer manipulations will occur in the nursery, and we don't need to track any of those, perhaps with a special exception when we allocate a larger object than our nursery supports. For that, we might need to track special RC objects allocated by the nursery to 'decref' after we're finished.



Our initial program snapshot could be an empty program, easily enough. 

 unless we construct

This design has a nice feature: we always have a snapshot of our program. We could easily use this for simple failure handling, roll back. 


That would be useful for other reasons, anyway - backtracking each computation. 



Naive reference counting is inefficient, especially in parallel code where RC updates must be atomic. A mitigating technique: defer RC for roots to discrete steps. At each step, incref the new roots then decref the old ones. This requires holding a snapshot of the old roots. The actual RC updates can be batch processed, guarded by a mutex, so those costs are amortized.

Ideally, I would also defer RC for volatile objects.

A viable option is support a per-thread 'nursery' where small, volatile objects are allocated and dropped without book keeping. We then require a copy-collector and bump-pointer allocator. When the nursery is out of space, we update reference counts then compact objects into a fresh nursery. 

We could augment the nursery with a survivor space. The idea would be to promote only survivors of multiple nursery passes: nursery, nursery first pass survivor, second-gen nursery, second-gen survivor, reference-count. This would limit us to four copies of an object, with the primary nursery being processed multiple times for each second-gen nursery step. 

This way, small volatile objects are eliminated by the time we reach the mature space, and we can make some useful guarantees about exactly how much effort we expend on copies. The RC update pass would be deferred until we handle survivor-level copies, so would be a rare event (at cost of holding onto older checkpoints). 


An intriguing possibility is to only perform the RC pass when we compact the survivor space. We might then have a dozen nursery first-gen nursery collections per RC pass, at the cost of always keeping the prior snapshot in the survivor space.

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
