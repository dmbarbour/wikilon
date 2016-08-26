
# Minimalist ABC

A simpler ABC is (hopefully) a better ABC.

THE BIG IDEAS: 

* confluent, context free rewriting combinators
* minimal base plus early start on accelerators
* elimination of any non 'program' data types
* evaluation has uniform type program → program
* evaluation may proceed to fixpoint or quota

Consider some code:

        [A] swap [B] [C] swap [D] cons
            -- evaluates to...
        [A] swap [C] [[B] D]

This looks and acts *a lot* like a stack language. However, there is no stack per se, just a program under evaluation by rewriting. A significant difference is that there is no *stack underflow*. When we do not have enough arguments to the left, no rewrite occurs. Simultaneously, our runtime is free to perform deeper rewriting. 

Potential benefits:

* easily evaluate and cache computations without context
* renders and views can focus on a common program type
* lazy blocks by default, reduces need for paired annotations
* pipeline processing implicit, much latent parallelism
* just one type simplifies compaction, stowage, sharing 
* toplevel doesn't need balanced `[]` for evaluation.

This idea for a minimalist ABC seems very promising. 

We should double check some things before committing...

* effective external systems integration
* obvious control over program behavior
* error recognition and static type safety
* performance, optimization, compilation

## Systems Integration with Minimalist ABC

It's necessary for Awelon's vision to support efficient and predictable integration with external systems. In a broad sense, such integration includes data input, extraction of results, and use of time and space. Rendering of intermediate computations may also be part of this integration. If these things aren't efficient and predictable, there is no way minimalist ABC could become a practical language.

I posit that I should minimally support:

* small numbers and basic arithmetic
* embedding and construction of text

Within the 'minimalist' goal of having only one data type - a program - these values should themselves be understood as programs that we can evaluate. I can easily eschew primitive support for negative integers. And if I'm just interested in natural numbers, I can easily give those *meaning as programs* by treating them as shorthand for a Church encoding. 

        [B][A] #2 i == [[B][A]#1 i] A   == [[B]A]A
        [B][A] #1 i == [[B][A]#0 i] A   ==  [B]A
        [B][A] #0 i == B

This encoding can work. But I'm not committed to it. I'm contemplating whether I can unify *Numbers, Literals, and Command Sequences* (see below). If I can do so, I'll probably go with whatever I come up with there. Regardless of the specific encoding, I assume we will be able to Church-encode useful arithmetic operations: successor, predecessor, addition, subtraction, multiplication, divmod, etc.. By accelerating these arithmetic operators, our performance won't be hurt by the Church encoding. 

Conveniently, number types also act as a basis for conditional behavior, i.e. selecting the `A` vs. `B` behavior.

For consistency, we can leverage a similar model for texts. Something like:

        [B][A] "hello" i == #104 [[B][A] "ello" i] A
        [B][A]  "ello" i == #101 [[B][A]  "llo" i] A
        ...
        [B][A]     "o" i == #111 [[B][A]     "" i] A
        [B][A]      "" i == B

The original ABC's embedding of text is pretty useful, so I'll probably stick with it. I don't want to sacrifice legibility, so I'm still leaning in favor of 

*Aside:* I might explicitly use the UTF-8 expansion for text, e.g. such that `→` expands as three bytes rather than one larger codepoint. This would be more convenient for many use cases, especially those involving construction of maps, tries, hashes, etc.. It's generally more convenient for efficient processing and extraction.

### Effect Models Integration

A good effects model for purely functional programming must support interception of effects, such that we can evaluate our program in a simulated environment or wrap and adapt the effects to a different context. Minimalist ABC mustn't compromise this, so any effects models must be at the value level, operating only on the right-hand side of the program stream. We could model effects via a monadic `[Continuation][Request]` pair, for example.

## Controlling Program Behavior

### Structurally Scoped Computation

Controlling scope of a computation doesn't improve expressiveness or power, but it does make easier the comprehension of our code, enabling efficient debugging and isolation of errors. Consider:

        [A][B]cat   == [A B]
        [A][B]cons  == [[A]B]
        [A[B]]read  == [A][B]

I believe these provide an effective and sufficient basis for scoped evaluation. Effectively, a block may double as an evaluation scope (which can receive inputs via `cons` or `cat`), while the `read` action forces evaluation at least far enough to extract some useful data.

*Aside:* I find `read` interesting. In addition to controlling scope, it provides a simple foundation for laziness and call-by-need evaluation and partial evaluations. Further, it very naturally expresses coroutines and continuations, i.e. the `[A]` block can be understood as where we continue after yielding.

### Discretionary Value Sealing

We can use paired sealer and unsealer tokens of form `{:x}` and `{.x}` to resist *accidental* access to a value's structure, i.e. providing a lightweight type wrapper of sorts. For *correct* code, removing sealer/unsealer pairs should have no effect (except perhaps to improve performance). But incorrect code will generally fail fast. And even if something funny is happening, our developers can easily search a codebase for tokens to isolate where the error might have been introduced.

Discretionary sealing is adapted to minimalist ABC by a simple pair of rewrite rules:

        {:s}{.s} == 
        {.s}{:s} ==

We must use the tokens within blocks `[{:x}]` for scoped application of seals. Also, while the latter rewrite rule is based on a valid optimization (unseal then seal with the same token is effectively a non-operation), it does improve our expressiveness because it means we can freely grow even sealed subprograms from both the left and the right. 

### Weak Substructural Types

Substructural types are very useful for structuring the *behavior* of an application, independently of structured syntax and data models. Minimalist ABC won't provide strong enforcement for these types, but will provide enough to guard against accidental errors. The proposal is as follows:

        [A]{&rel} == [A]    (mark relevant)
        [A]{&aff} == [A]    (mark affine)
        [A]{&nss} == [A]    (clear marks)

* a block marked relevant may not be dropped
* a block marked affine may not be copied
* a block both affine and relevant is called 'linear'
* block compositions preserve substructure

We could model a general copy or drop function that ignores substructure by prefixing with `[] cons {&nss}` (and unwrapping our copies). But the weak types could help developers isolate many errors.

## Recognizing Errors and Static Type Safety

Early detection of errors is very useful for software engineering. Ideally, we want to find errors before evaluation, or even during edits. If not that, we need to recognize error conditions dynamically, during evaluation, such that we can report, flag, count, and highlight the errors.

With term rewriting, we might understand an error as a 'stuck' computation - a computation that produces no values, no matter how many inputs it receives.

### Programmer Specified Errors

Programmers might specify errors to indicate:

* incomplete programs (todo some day soon!)
* incorrect use (partial functions)
* assertion failures (implementation error)

We could simply use an `{&error}` annotation for programmer errors.

        "todo: implement foo"{&error}

The annotation creates an *error value*. It can be copied or dropped like any other value. An attempt to observe the error value (e.g. through application) will cause our computation to become 'stuck' at that point. 

### Dynamic Error Recognition

Dynamic error recognition needs to be cheap or rare, preferably both. One obvious candidate for dynamic error recognition is runtime type assertions for data that our runtime recognizes specifically. This might be expressed by annotation. For example:

        #42 {&nat}
        "hello" {&lit}

It's also easy to recognize errors like:

        [A]{&aff} copy
        [A]{:s} action
        [] read

Weak substructural types are designed to support efficient recognition. For value sealer/unsealer tokens, the pattern `value token action` is easily recognized as an error. And for `read`, the only thing to do is to evaluate within the given block until at least one output is generated - because there are no more inputs during this computation, recognizing error conditions becomes trivial.

Taken together with `read`, it could be very useful to assert we have an empty program, e.g. `[]{&nop}`. This way we can make assertions about numbers of inputs and outputs, and that we're quite certain we're done. 

When dynamic errors are discovered in a subprogram, we should wrap them with `{&error}` (and inline the result) for easy and consistent reporting.

        #42 {&lit}      == [#42 {&lit}]{&error} inline
        [A]{&aff} copy  == [[A]{&aff} copy]{&error} inline

### Static Type Safety

Manifest typing is certainly feasible. Consider:

        [B]{&typeDescriptor}
        [B][typeDescriptor]{&type} drop

Together with a fair bit of type inference (look into bidirectional type checking), and potentially some static partial evaluation, static typing has excellent potential to be straightforward. Manifest type annotations also make an excellent location to flag with `{&error}` if there are issues.

Modeling type descriptors as ABC values is an excellent option. It enables abstraction and composition of types, gradual development of our type systems, and 

Use of an ABC value for the type descriptor is promising. It enables flexible abstraction and composition of type descriptors, expression of sophisticated types, and we can use such type descriptors directly if we later move type checking into user code.

## Runtime and Performance

### API

Rather than build a value that contains some blocks and computations, I'll build one larger program or block and iteratively evaluate against a quota. I'd like to support construction of the program *without* requiring an ABC parse, so this will work by:

* build program by injecting values and operations from left to right.
 * runtime may perform lightweight evaluations at each step.
* open and close block `[]` are effectively operators
 * closing a 'toplevel' block is main error at this point
* may capture toplevel as a block
* may move blocks to another context
* evaluate with quota-driven 'steps':
 * evaluating the 'program', not any specific object
 * evaluator may heuristically seek both depth and breadth
 * breadth necessary to initiate `{&par}` and similar
* extract values from right
 * easy access to numbers
 * destructive access to texts, binaries
 * reflection on how many values available

### Bit Representations

A pointer has spare bits that are convenient for tagging and embedding of values. For my original ABC, these bits were oriented around pairs, sums, and small integers. With minimalist ABC, any focus for tight representation would instead orient around natural numbers, opcodes, tokens, and block embeddings. In both cases, the general escape for 'tagged' objects, allowing me to handle arbitrary special cases (like stowage or texts) is essential.

Candidate representation:

        x01     small natural numbers
        011     other small constants 
        111     common operation codes

        000     block (list of ops)
        010     (a deep copy value)
        100     tagged objects
        110     tagged actions

        (Fast Bit Tests)
        Shallow Copy:   (1 == (x & 1))
        Action Type:    (6 == (6 & x))
        Tagged Item:    (4 == (5 & x))

        (Tagged Items)
        tag uses first byte via (uint8_t*) reference.
        same tag set is used for both. 
        special tags for 'small' binaries/texts.
            (smaller, reverse ordered)
        actions include compact bytecode, etc..

        (Small Constants)
        singleton blocks, one for every opcode!
        (that's all we NEED including identity op)

        (Common Opcodes)
        A very few ABC codes
        Recognized annotations
        A great many accelerators

This seems an efficient and lightweight representation. I still have that free slot `010`. I'm tempted to use it for zipper-objects representing 'open' blocks.

### Parallel Runtime

I intend to support **pipeline parallelism**, such that we may flexibly express computations that stream outputs from one task as inputs to another. Parallelism can be indicated by marking a block with `{&par}`, indicating that evaluation should proceed in parallel. To pipeline, we need to associate parallel evaluations such that the output of one process becomes input to another. This might be expressed by 'inlining' the parallel computations. For example:

        [A]{&par}i  [B]{&par}i

When connecting independent parallel computations, our parent can connect them directly so it no longer intervenes in their communications. This is a dynamic action, allowing for cases where computations are composed long after being constructed, or intermediate processes complete. 

Pipeline parallelism isn't the only option. For example, we can use `{&par}` with `cons` and `read` to model precisely controlled communications between processes in something closer to a rendezvous style. Parallelism is then achieved by performing other work between `cons` and `read`, or by buffering some reads.

See [Parallism.md](Parallelism.md) for more on context-local implementation.

### Program Representation

My primary "block" representation is a list of `(op, block)` cons cells, terminating with a *singleton block* (the empty block being modeled as singleton-identity). 

During evaluation, I'll likely need to represent an 'open' block with a Huet zipper to model where I'm working, e.g. `(left-code, right-code)`. Our left-code will mostly contain values, and the right contains the operations we're applying. This effectively serves as our 'data stack' during evaluation. 

Applying code results in 'block' values being inlined into current evaluation. We could inline these directly, i.e. `[A]i == A`. But doing so is O(len(A)), which is expensive if `A` is large. In general, we'll instead logically inline our code (via tagged action). At most, we'll need only deal with *one* inlined block at a time, with the call 'stack' being flattened into right-code.

            [[A]iB]i    ==  [A][iB]i    == [A]i[B]i

Evaluation isn't order restricted. An evaluator can work *anywhere* within the program. It quite feasible to open multiple zippers and split the program into 'interesting' chunks. This can prevent evaluation from getting 'stuck' on any one thing or failing to initiate `{&par}` threads and similarly important tasks.

Use of `{&par}` essentially enables programmers to control chunking of evaluation, specify the 'interesting' parts of the computation that deserve special focus. Every `{&par}` node would additionally be hooked into a big work pool and pipelines that carry values from one process to another. The "work pool" might be understood as simply a way to attach *very deeply* into within the program, in a thread-safe manner.

### Compilation

I plan eventually to support a 'compact' bytecode representation - a binary sequence where the contained binary (contained blocks, texts, binaries, big numbers etc.) can easily be sliced from it. 

Binaries in general could make for useful 'shared' objects to avoid multiple in-memory copies.

could make for useful 'shared' objects

Such representations may reserve sufficient memory for each 'checkpoint', and proceed through a series of checkpoints. A JIT representation of the compact bytecode will take this idea further.




I'm leaning towards use of linked list based 'stacks' and 'programs' as the normal mode. We might use some zipper concepts to preserve a 'cursor' to wherever evaluation is occurring.

Use of 'compact' bytecode representations, and eventual JIT representations, will be valuable for efficient evaluation, albeit mostly in cases where evaluation can proceed to completion. For incomplete evaluations, a lot of allocations will be needed.




### Performance Annotations

Evaluation Control

* `{&par}` - begin parallel evaluation of contained subprogram
* `{&seq}` - force partial evaluation of contained subprogram

It might be useful to include suggested evaluation strategies, e.g. `{&eval-wide}` vs. `{&eval-deep}`. 

I'm contemplating `{&static}` and `{&asap}`, but it isn't clear to me how to proceed with them.

Representation control

* `{&lit}` - force to a compact text format (or error)
* `{&nat}` - force to a natural number format (or error)


## The Minimal Bytecode


*Aside:* In context of parallelism and laziness, a convenient function might be `[X [A]] pop == [X][A]`, i.e. a sort of reflective action representing a demand-driven computation or partial evaluation. This is a rather tempting feature to include as a primitive in minimalist ABC.

## Numbers, Literals, and Command Sequences

An intriguing possibility is that *natural numbers* and *embedded texts* might be modeled as specific instances of command sequences. Command sequences are useful for concise data representation, DSLs, monadic effects, modeling cooperative multi-threading, etc.. If we can unify the two concepts, that would be convenient both didactically and for uniform utility, accelerators, etc..

With [claw command sequences](CommandLine.md) our command sequence is given a general meaning: `[foo,bar,baz] == [[bar,baz] after foo]`. I've also experimented with the variant `[foo,bar,baz] == [foo [bar,baz] yield]`. In both cases, `foo` is not wrapped as `[foo]`, which is convenient because it means we do not need to distinguish `foo` 
 Notably, `foo` is not wrapped up as `[foo]`, and we do not explicitly terminate the sequence.

, such as we don't need to think hard about an *empty* command sequence vs. 



A challenge for command sequences is distinguishing a sequence containing a single empty command (the identity function) vs. a sequence containing an empty command. 

The main difficulty with command sequences, of course, is distinguishing an empty command 


        "hello" == (#104, #101, #108, #108, #111)
        "" == ()
        #3 == (,,,)
        #0 == ()

            generalized behavior:

        [B][A](foo,bar,baz) ap == foo [[B][A](bar,baz) ap] A
        [B][A]() ap == B

A potential difficulty with this is distinguishing an empty sequence `()` from a singleton sequence with an empty command, e.g. `(baz)`. 




This isn't a bad model for command sequences. It can iterate without a fixpoint loop. It can't capture the continuation implicitly, but we can model continuatio passing explicitly if we so desire. At the bytecode level, such command sequences might be represented as `[[foo][[bar][[baz][e]c]c]c]` with appropriate `e` and `c`. 

But that isn't the only option. We could instead generalize as:

        "hello" == (#104 C, #101 C, #108 C, #108 C, #111 C)
        #3 == (t,t,t,)




A variant model producing `[[B][A](bar,baz) ap][A] foo` would also work well, more sensitive to the type of `foo` and `A` but also more expressive due to implicit continuation capture

a bit more expressive but also )


OTOH, this is not the only option. With a tweak to how we model texts, we could be using:

        [B][A](foo,bar,baz) ap = [[B][A](bar,baz) ap] foo A

This option requires `A` to be aware of the type of commands like `foo`, and for commands like `foo` to be aware of the continuation.

*Note:* ABC will not include a syntax for command sequences. They aren't essential for systems integration, and the plain bytecode representation will be efficient enough (especially with appropriate accelerators). Instead, command sequences are presented as *editable views* of bytecode, such as with [claw command sequences](CommandLine.md).