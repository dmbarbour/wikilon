
# Runtime Design

A primary goal for runtime is performance of *applications*. Awelon's [application model](ApplicationModel.md) involves representing state within the dictionary via RESTful multi-agent patterns (e.g. publish-subscribe, tuple spaces). Evaluation, concurrent update, incremental computing, and so on each contribute to application performance.

I'm also interested in predictable performance, and precise cost accounting so heavy users can (eventually) be charged appropriately. The runtime will aim to keep unpredictable performance features under client control via annotations and similar. I intend for Awelon runtime to be suitable at least for soft real-time systems to the extent that programmers control the nature and frequency of transactions on the dictionary.

Awelon is designed to afford efficient update patterns via secure hashes to name dictionaries from which another dictionary might inherit or contain.

## Evaluation 

Viable evaluation strategies:

* interpret program (with call-return auxiliary stack)
* compile to abstract register machine, interpret that
* JIT compile to machine code before evaluation proceeds

There is a tradeoff. Compilation increases complexity and spinup time, but can significantly improve performance for long-running loops. I do know that I'll eventually want JIT compilation support. I favor interpretation as the basic option for getting started quickly. JIT can be performed for a subset of performance critical code, or eventually for full programs.

There isn't any fundamental reason I cannot support all three, eventually.

The program under evaluation is volatile, so might also mostly be represented as a data stack. Special handling is needed because not everything is 'data' - a computed program may contain words that have not been linked, and lazy linking might expand a word into data based on demand. 

Evaluation can proceed left-to-right through our program then evaluates remaining block values either right-to-left or in parallel. The result of this evaluation would be held on the program data stack.

*Note:* I like the idea of a "checkpointing" evaluation, such that we can always interrupt evaluation and have a valid program, at most losing some intermediate work. However, checkpointing evaluation requires some careful attention if I also want to support in-place update for unique array objects. An intermediate option is to support 'safe interrupt' points, but I also don't want expensive atomic ops on those actions. So my best option is perhaps to limit interrupts to some periodic event that would perform synchronization anyway such as GC. In any case, to support checkpointing evaluation, I need a small volume of volatile roots - registers - per thread.

## Memory Management

Some desiderata:

* logical copy on `c` for evaluated objects and data
* unique arrays with in-place updates, copy on write
* copying or compacting GC to resist fragmentation
* generational GC to avoid copying of stable objects
* hierarchical GC to limit synchronization frequency

I think my best bet for GC is to use copying for a younger generation, gradually build a set of survivors at one side of the heap. Then, when we reach some thresholds, we apply a *mark-compact* algorithm on the full heap to clear survivors. This is a well proven, general design. It does risk large GC pauses for mark-compact on a large heap with many survivors. But GC pauses are a lesser issue with pure computations. 

Due to in-place updates on some linear objects potentially updating objects in the survivor set, I may need a simple write barrier to track which objects have been written recently to provide additional GC roots. Depending on use of registers and stacks and in-place updates and unboxed arrays, we might be able to avoid a lot of intermediate allocations for common patterns in Awelon. 

I would like to avoid full compactions in the common case. So I might need an extra, intermediate generation that performs either copy or compaction. This complicates write barriers a bit insofar as I need to maintain lists of potential roots between generations. There are diminishing returns for having many generations, but even one or two generations would cover most common cases.

*Note:* be sure to use *two* bitfields for mark-compact bits. One for grey, one for black. This enables us to test at a word level whether a grey word matches a black word (do we need to scan more?), and to easily mask or test both bitfields using the same offsets. I'd also need to ensure space is available for marking in the worst case, so this would be an upper constraint on the surviving set. Fortunately, I only need mark bits during mark and compaction.

## Parallelism and Threads

I have an idea to use hierarchical heaps to support confined multi-threaded `(par)` computations. Communicating threads (which require queues, etc.) shall be modeled indirectly, e.g. via accelerated evaluation of KPNs. 

Each heap would allow allocation and GC independently of sibling thread heaps, though a parent might need to wait on a child thread. When a child heap fills, we can allocate a new one in the parent (and optionally move the most volatile data). When the parent fills, we can perform a parent GC using the normal copy/compact options.

Write barriers are necessary to track external references from a parent into a child heap. Hierarchy effectively gives us an implicit form of generational collection, and we can probably leverage that by modeling our intermediate GC generations in terms of a few layers of hierarchy. 

We would need to scan not just the current generation, but objects from multiple parent generations, to track references into a child heap. But conveniently, we can guarantee against sharing of objects between child threads. So this can simplify write barriers, since a thread only needs to update its personal list of external roots.

*Aside:* Hierarchy can safely be lost when we perform the parent compaction, instead allocating new heaps for each thread. OTOH, there might be some advantages to preserving hierarchical structure.

## Stack Representations

Evaluation has both the program/data stack and the auxiliary. Modeling multiple stacks and a heap within a single address space needs some attention. 

I will probably aim to model the stack as a normal heap object - a unique array, or linked list of array fragments, or similar. The latter option is especially nice because the 'top' pages of the stack would tend to be a newer object, and stack pages would generally align with locality and generation.

An advantage of modeling a stack as a unique array is that it becomes feasible (with appropriate accelerators) to represent the entire evaluation and GC techniques *within* Awelon code.

## Memory Representations

Let's say our allocations are two-word aligned, and our words are either 4 bytes or 8 bytes depending on machine. This gives us 3 reliable pointer tag bits. Use of pointer tag bits can discriminate a few 'small' values to reduce allocation costs. 

Proposal:

        00i     small constants, actions
        01i     pair cons (B, A) => [B A]
        11i     list cons (H, T) => [H T :]
        10i     tagged objects (size/type in header)

The `i` bit enables all values to be logically inlined as functions. In Awelon, all values are functions. This is mostly useful in context of operations like bind or compose, enabling uniform treatment of pair cons to support pairs, bindings, compositions, etc.. It also reduces overhead for arity checks, since we don't need to peek beyond the pointer to determine how an object behaves under bind, swap, etc..

Small constants would be things we can represent within the space of a pointer - natural numbers are the most critical, but other potentially valuable targets include: small integers, texts, labels, etc.. Since we lose three bits even before discriminating value types, we'll probably be limited (with 4-byte words) to about 256 million natural numbers before we must switch to a tagged representation. With 8-byte words, this limitation is much less significant.

Optimizing pair cons gives us efficient `[[B]A]` block bind, `[B A]` composition, and `[[B][A]]` pairs. Simple tuples, too - `[[C][B][A]]` can be represented as binding `[C]` to `[[B][A]]`. We can also represent `[[B]]` as a pair cons, i.e. `[B] [] b`, but recognizing pair constructions specifically is convenient. Labeled values, etc. can be represented this way. This seems very widely useful, convenient for rapid construction of ad-hoc computations and data.

Optimizing list cons is perhaps less generally useful. We can always represent `[[H] [T] :]` as two pair cons cells (`([H], [[T]:]i)` or `([[H][T]]i, :)`). But lists can adequately cover a lot of data structures, especially heterogeneous lists, including trees and streams. Halving the representation cost seems worthwhile.

While cons cells are reasonably compact, very useful for lightweight construction, they aren't optimal for large structures. Large tuples and lists could instead be accelerated to use tagged object array structures to again halve the representation costs and reduce pointer chasing. With uniqueness and acceleration, we can also support copy-on-write and efficient in-place updates. Between these, we could achieve very significant performance benefits - e.g. supporting many algorithms that use mutable arrays in a single threaded mode. *Aside:* Something like shared arrays are feasible if we use logical linear slices, rejoins, and combine this with KPN-style accelerated message passing.

Labeled data - variants, records - need some consideration. Labels (structured left-right sum paths) can be tagged objects or perhaps small constants. Records could be represented with hashmaps or similar (perhaps a perfect hashmap). Compiled record structures are viable, but might need be translated to and from a hashmap outside of type-safe code. Or we might want to logically translate the record to a tuple within compiled code, then back to a record at the end, enabling a record to be logically divided among different subprograms. In-place mutation of unique records could also be convenient.

Programs can be fully represented using pair cons cells, but large programs can be optimized to an array-like representation, terminating with a `\return` action or tail-call variant. This reduces pointer chasing and indirection.  We might specialize some computations, e.g. `[A] b a` to `\push2nd A \pop`, to help improve locality of computation. For interpretation, I'll focus on optimizations that can be serialized back to the original code without global rewrites.

An evaluation thread must have a heap, a program under construction, the call-return stack, and some general purpose registers. Our stacks and program will be represented within the heap, so could be understood as specialized registers.

Words would generally be represented as tagged objects, enabling direct association of code.

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

## Extraction

Compilation of Awelon code for an external system is an interesting option, but is only feasible if we either limit the IO types at that layer (e.g. to integers, rationals, decimals, texts, labels, lists) or ensure that first-class function objects in the target language are coupled to Awelon representations.

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
