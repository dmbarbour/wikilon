
# Runtime Design

A primary goal for runtime is performance of *applications*. Awelon's application model involves representing state within the dictionary via RESTful multi-agent patterns (e.g. publish-subscribe, tuple spaces). Evaluation, concurrent update, incremental computing, and so on each contribute to application performance.

I'm also interested in predictable performance, and precise cost accounting so heavy users can be charged appropriately. The runtime will aim to keep unpredictable performance features under client control via annotations and similar. I intend for Awelon runtime to be suitable at least for soft real-time systems to the extent that programmers control the nature and frequency of transactions on the dictionary.

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

I'll logical copy on `c`. Logical copies reduce overheads for common patterns.

Awelon is a simpler target than most. Pure computations cannot directly observe internal evaluation latencies, so pause times aren't critical. Since we don't have ad-hoc mutation, we don't need write barriers in a generational collector. Pure `(par)` style parallelism can be isolated via hierarchical heaps. Awelon doesn't produce memory cycles. We can design such that heaps are 'unidirectional', no references up the heap.

Memory fragmentation hurts locality, performance, and predictability of performance. Copy collectors are a good alternative, except that they reorder the heap which also hurts predictability for cache behavior and can hinder lightweight tracking for multiple generations. I favor mark-compact algorithms that preserve heap order.

I'll probably use generational GC to reduce frequency of copying for long-lived objects. Also, hierarchical GC to support lightweight threads with coarse-grained synchronization. It seems feasible to simply copy a hierarchical heap without compacting it, assuming there are no external references directly into a hierarchical heap (i.e. such that all references are via thread-control-block).

Which mark-compact algorithm should I use?

I want constant space overhead for GC, such that we conceptually could have allocated all the space needed for GC up front. This means GC mustn't require a stack. I also want to use Lisp-like cons cells (simple word pairs) for a few cases like binding operator `b` and potentially for list data. Anything that requires an extra word per object seems relatively steep.

Boortz and Sahlin developed an algorithm specifically to leverage unidirectional heaps that avoids space overheads and is a fair bit simpler than conventional compaction algorithms. This algorithm does have disadvantages. First, it moves each surviving object twice per compaction (up a heap, then back down). Second, it reads dead objects, which means it has a cost proportional to total heap size instead of just the survivor set. Each of these disadvantages can be mitigated by a generational collector.

Conventional algorithms need bits for marking objects. We use a separate bitmap per heap to track GC bits. For tri-color marking, this would require about 2 bits per object. If I assume each object is at least two words, the space overhead is reduced to 1/32 or 1/64 (depending on word size). This seems feasible. A marking algorithm could then use a fixed-size stack together with incremental marking techniques. For example, we mark an object 'grey' if we reach the stack limit, then scan for remaining grey objects before compacting. Compaction can use an offset table approach.

I find the Boortz and Sahlin algorithm more appealing, despite its disadvantages. I'll try it first.

*Aside:* An unfortunate consequence of shared objects in a DAG is that I cannot perform constant-space traversals, since I don't know how many times I'll traverse an object. I need a stack to serialize a value.

## Stack Representations

Modeling two stacks and a heap in a single address space needs some attention. 

I don't want anything complicated here.

One option that appeals to me is to track the top of each stack in a fixed-size mutable region. When about to overflow or underflow the stack head region, we shunt part to or from the heap. We do so in large chunks, so we have sufficient buffer space to continue. The stack buffer determines the maximum 'bounce' of a function before we start thrashing, but even when thrashing we can guarantee some productive motion.

## Linear Space?

Linear objects could be used for high performance array processing, i.e. so we can edit a linear array without copying it. However, support for linear objects may be difficult for a unidirectional heap. 

Between mutation of an array to point at a new array, and construction of an array to point at an old array, we would tend to break directional invariants. I don't see a simple, obviously correct route forward for linear objects without an alternative GC algorithm. At least not for the general case.

For now, I won't bother optimizing for linear objects case. I suspect that most uses of linear space could be effectively supported by a few specialized list-processing or array-processing accelerators.

## Memory Representations

Probably common object types, now and later:

* programs, sequences of words and values
* small natural numbers
* `[[B]A]` partial binding of programs
* `[[Hd][Tl]:]` list and tree structures
* `[[A] inR]` optional values, potentially
* texts, binaries, arrays (logically lists)
* labels and labeled data (records, variants)
* unboxed vectors and matrices for math

I can use a few tag bits per pointer to discriminate a useful subset of values. This is mostly important for *small* objects, such as two-word cons cells where an extra word for the header would constitute 50% overhead. Larger objects can afford a header word, and need one anyway to distinguish types.

Distinguishing actions from blocks could be useful for fast bind, static linking, and arity checking for lazy link. Optimizing bind and linked lists via basic 'cons' cells should be very effective for lightweight code and data types. 

Intriguingly, I could probably keep bind-based cons cells in the 'linear' space to ensure . But list-based cons cells would probably be held in the shared space.

Support for 'linear' objects would be useful, but (excepting binaries) would likely violate the unidirectional heap structure. So I should probably encourage use of persistent data structures instead, even for linear objects. This does raise an issue that I cannot accelerate computations

Optimizing data lists to use Lisp-like cons cells could be useful as a basis for many lightweight data structures, especially given that the cons cells are easily heterogeneous. Lists would be used to model trees in terms of a `(node-value, list-of-children)` pair, for example. 

Optimizing for the common `[[A] inR]` case of binding, e.g. reducing it to a single tag bit, could potentially save allocations for activities that repeatedly wrap and unwrap optional values - e.g. the Maybe monad. But I'm not sure I have sufficient bits for this use case. The benefits would be marginal if I later perform allocations for operations on a stack. 

*Aside:* I wonder if optional/maybe values costing the same as singleton lists would encourage a preference for relational/logic programming, streamable outputs, backtracking, etc. via list monad. If so, I'm willing to call that a win.

Labels and labeled data need some careful consideration. I must still work out how they might interact with editable views and so on - ideally without direct language support. The partial binding could work for labeled values. Labeled records, in general, might benefit from memoizing a 'perfect hash' for each set of labels. Also, we might not model records directly, but rather record *construct

 Optimizing lists to use cons cells could simplify efficient representation for many list or tree data structures. Large lists might be compacted into arrays, but only by explicit action.

Programs can be represented by contiguous arrays of actions terminating in `\return` or one of a few variants for tail-call optimization. We might also have some means to represent that one block is logically 'inlined' into another.


A program will be represented by a header followed by an array of words terminating in a special `\return` word, or a variant for tail-call optimizations. The evaluator will rewrite certain patterns under the hood like `[A]b a` to `\push2nd A \pop` to use the auxiliary stack, but only for stuff we can serialize back to the original code without global rewrites.

Our evaluation thread will have its heap, a program under construction, and the call-return stack. For call-return, in context of compaction, we might need a pointer to the start of each block and our offset. OTOH, the Boortz algorithm mentions correct handling for overlapping objects. If that works out, call-return could be a simple pointer movement even when we compact code, and we might need a 'header' for our blocks only in context of tracking substructural attributes and so on.



We use tag bits to distinguish some things. However, doing so is useful only for small values where the header is a significant burden. Small natural numbers would be a classic example. Ideas where tag bits might help:

* small natural numbers
* list cons cells - `[hd tl :]`
* optional values - `[value R]`
* efficient tuples, bindings
* distinguish values and actions
* support logical inline blocks

Distinguishing values vs. actions could improve performance for binding and data shuffling, as does an optimized representation for tuples and bindings. List cons cells are heterogeneous and provide an adequate basis for representing a wide variety of structured data (lists, trees, etc.). 

An interesting opportunity is to represent lists via array-like constructs, perhaps with a terminal, to reduce pointer-chasing. 

 This could work especially well with an order-preserving, compacting GC because I could collect just part of a list. 

, especially if we can support overlapping objects. If we can support lists as arrays (and logical flatmaps of lists) that would cover a lot of basic data structures *very* efficiently.



Optimizing list cons cells could offer adequate efficiency for representing many data structures (trees, lists). Optional or sum type values offer more modest benefits, potentially saving allocation of a word for sum wrappers. Optimizing tuples and bindings is useful because it's a common intermediate structure.

Consider one concrete bit-level representation:

        xy0     small natural numbers
        001     block bind, ([B], [A]) => [[B]A]
        011     list cons,  ([H], [T]) => [[H][T]:]
        101     tagged objects 
        111     tagged actions


Words would be tagged actions, but might be managed in an external space separately from the evaluation heap.


An interesting possibility regards list representations. It seems feasible to model lists with array-like structures, such that we can split or recombine lists 

I could reduce the range for small numbers a little to make room for more data. But I'd still have four value types. With three tag bits and four value types, I simply don't have spare bits for 'optional' values. I could introduce a fourth tag bit for values in the right, and force 16-byte alignment for allocations (which isn't a problem for a 64-bit machine). Whether this would offer significant benefits depends on usage, e.g. if we frequently use the Haskell style `Maybe` or `Error` monad it could offer significant benefits to avoid wrapping and unwrapping the state in each step.



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
