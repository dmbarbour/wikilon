
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

Use of an auxiliary continuation stack and `\return` words enables a near-conventional approach of a program counter and threaded interpreter. Relatedly, we can support `\push A \pop` actions in place of `[A]a`, hiding data temporarily on the auxiliary stack. This is a common practice in Forth, Factor, and similar languages but in Awelon is strictly disciplined by program structure. Users cannot directly perform `\push` or `\pop` or `\return`.

It is feasible to further compile programs to use a register machine, essentially by allocating top stack elements into registers. I would like to pursue this technique further, albeit perhaps only for JIT code.

## Memory Management

For efficient use of memory and CPU, I want to logically copy objects. Consequently, I cannot recycle memory on drop, unless I somehow know it's a final logical copy. I will need automatic memory management. Even so, Awelon's acyclic structure (or directed-acyclic structure, with logical copies) and purity (which reduces latency concerns) can potentially simplify memory management in Awelon relative to other languages. Also, I can potentially optimize temporary structures that are not copied.

I am concerned about memory fragmentation caused by free lists. Memory fragmentation results in degrading performance for long computations. Compacting collectors as a basis seem simpler and more trustworthy.

Compacting collectors are notorious for poor memory utilization. A certain amount of space must be kept free to perform compaction. For example, with a two-space collector, 50% of memory is kept free. Fortunately, this issue is mitigated by parallelism in a multi-agent environment. For example, if I have a hundred computations allocated 10MB each, and less than 1% of my time is spent on compaction, then I could make do with just one blank 10MB heap and 1% memory overhead for GC. In practice I would want extra blanks to support parallel compaction, but the overhead can easily be well under the 50% case.

Another concern is lightweight parallelism. Compaction with active mutators is not simple, so we'll want to pause work before compaction. But synchronization isn't cheap, so we'll want to amortize synchronization by delaying compaction as much as feasible using simple techniques. 

To delay compaction, we *must* recycle memory before compaction.

An intriguing option is hierarchical compaction. We model evaluation as occurring within a 'child' heap. The parent heap can then provide a survivor space. We promote objects that survive two collections in the child to a survivor space so they are not copied by further compactions. This both controls frequency of copies and prevents the parent from filling too quickly because we only promote relatively long-lived objects. 

We can explicitly model the survivor spaces within the parent, too. Thus, we can model an entire compaction heap hierarchically, with multiple generations of survivors. No information is shared between heaps, we only observe the final result of an evaluation. This works great for lightweight `(par)` style parallelism. For KPNs, we might need to evaluate in batches to move intermediate results between subnets and processes.

Besides hierarchical compaction, we might benefit from flyweight free-lists for short-lived objects for tuples or binding like `[A] b b b`. However, it isn't clear this will offer sufficient benefits to outweigh the extra costs of managing a free list and complexity of tracking sharing.

*Aside:* An unfortunate consequence of shared objects in a DAG is that I cannot perform constant-space traversals, since I don't know how many times I'll traverse an object. I need a stack to serialize values, or even to compute the size of a value.

## Memory Representations

A program will generally be represented by a header followed by a contiguous array of words terminating in a special `\return` word, or a variant for tail-call optimizations. An optimizer may rewrite certain patterns under the hood like `[A]b a` to `\push2nd A \pop`, but nothing that hinders serialization of the original Awelon code.

An evaluation 'thread' generally consists of a program under construction, the program under evaluation (a block offset pair), a call-return continuation stack, preallocated space and a heap for further allocations, etc.. For efficient compaction, our call-return stack will contain only plain old data objects. Thus, for `\return` we'll use a block offset pair instead of a direct reference into a block.

The block header would include size, substructural attributes, annotation flags, evaluation status, etc.. We might want to include a slot for extended metadata.

Within a block, we will reference Awelon words, annotations, and other blocks. 

We could use tag bits to distinguish some things. However, doing so is useful only for small values where the header is a significant burden. Small natural numbers would be a classic example. Ideas where tag bits might help:

* small natural numbers
* list cons cells - `[hd tl :]`
* optional values - `[value R]`
* efficient tuples, bindings
* distinguish values and actions
* support logical inline blocks

Distinguishing values vs. actions could improve performance for binding and data shuffling, as does an optimized representation for tuples and bindings. List cons cells are heterogeneous and provide an adequate basis for representing a wide variety of structured data (lists, trees, etc.). 

Optimizing list cons cells could offer adequate efficiency for representing many data structures (trees, lists). Optional or sum type values offer more modest benefits, potentially saving allocation of a word for sum wrappers. Optimizing tuples and bindings is useful because it's a common intermediate structure.

Consider one concrete bit-level representation:

        xy0     small natural numbers
        001     block bind, ([B], [A]) => [[B]A]
        011     list cons,  ([H], [T]) => [[H][T]:]
        101     tagged objects 
        111     tagged actions

Words would be tagged actions, likely interned for the evaluation.

I could reduce the range for small numbers a little to make room for more data. But I'd still have four value types. With three tag bits and four value types, I simply don't have bits to spare for 'optional' values. I could introduce a fourth tag bit for values in the right, and force 16-byte alignment for allocations (which isn't a problem for a 64-bit machine). Whether this would offer significant benefits depends on usage, e.g. if we use the Haskell style `Maybe` monad it could offer moderate benefits to avoid wrapping and unwrapping the state in each step.

I'll need to experiment with representations a bit to get it right. Fortunately, nothing depends on a stable bit-level representation in the evaluator, so I have freedom to experiment.

*Note:* During compaction, forwarding pointers might be represented by a specific header value and the following word pointing to the new destination. This can work for blocks and lists, too, if we ensure our header value is not a valid object. For example, if our header value is the NULL tagged action.

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

It seems to me that most input and output can occur as binary streams or chunked lists. Fine grained program input (e.g. at the level of open/close block and add word) seems unnecessary, though I could support it anyway. Whether it's worth introducing depends on how much it complicates the runtime API. I may need to track missing secure hash resources, however. 

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
