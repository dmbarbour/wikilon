
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
* lazy blocks by default reduces need for paired annotations
* pipeline processing implicit, much latent parallelism
* just one type simplifies compaction, stowage, sharing 
* with rewriting, easily model `[` and `]` as operators

This idea for a minimalist ABC seems very promising. 

I contemplated a few of my concerns below, including systems integration, control of behavior, error detection, and performance. But I'm satisfied minimalist ABC is better or at least no worse than the original flavor in each of those categories.

## The Bytecode

The proposed base is just four primitive combinators. 

        [A][B]x     == B[A]         (exec)
        [A][B]c     == [[A]B]       (cons)
        [A]d        == [A][A]       (dup)
        [A]k        ==              (kill)

This base is small, complete, and friendly to substructural types. The constructor action `c` is O(1) with simple list representations, which is convenient. Outside this base, we also have the `[]` square bracket characters for block structure, and whitespace characters (SP and LF - 32 and 10) for simple formatting.

For ease of integration with external systems, ABC supports natural numbers like `#42` or `#65535` and embedded literals. The syntax from the original ABC is unchanged.

        "embedded text literals
         may have multiple lines
         LF escaped by following SP
         terminates with LF ~
        ~

However, the *semantics* for numbers and literals are different. Numbers are a syntactic sugar for a Church encoding or variant thereof. The exact details are undecided. However, we shall at least be able to use numbers to iterate (e.g. `#42` can iterate up to 42 times). Similarly, texts will enable iteration through the associated utf-8 byte encoding.

ABC supports contextual extensions via tokens, short texts in curly braces like `{foo}`. Their function hasn't changed much. Annotation tokens like `{&par}`, `{&nat}`, or `{&trace}` support performance and debugging. Discretionary value sealing `{:s}` and `{.s}` supports lightweight labeling and safety. Linking of code and data might be expressed by `{%swap}` in an AO dictionary or `{'stowageId}` to reference external objects.

Because ABC is minimal, I'll be focusing early on an ABCD variant, providing an extended dictionary of opcodes that are defined in terms of our basic four.

*Aside:* The homage to [XKCD](xkcd.com), my favorite web comic, is intentional. I was only one operator away (`k` was originally `z`, for `zap`) and decided to go for it. But the language is still called Awelon Bytecode (ABC).

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

Scoping a computation doesn't improve expressiveness or power, but it does make for easier comprehension of code, simplified debugging and isolation of errors. A simple mechanism to control scope is to hide some specific content. For example, using the `dip` combinator, we can hide value `[A]` from our computation `B`:

        [A][B]dip   == B[A]

I assume our programmers will learn a useful set of standard combinators like `dip`. However, the left-scope of this computation, e.g. how many values it consumes or produces, is not immediately obvious. It requires knowledge about the type of `B`, which in general might be an argument from some distant location in the program. What we want is to control scope *structurally*, i.e. using local syntax and combinators that do not require knowledge of `B`. 
 
My proposal is to use blocks together with a structure assertion:

        [A][B]cons  == [[A]B]
        [A][B]cat   == [A B]
        [[A]]{&1}   == [[A]]

Blocks implicitly delimit scope of contained subprograms. 

Use of `cons` can feed a finite number of *inputs* to the scoped subprogram, while `cat` enables their ad-hoc dynamic construction. Annotation `{&1}` asserts that the computation produces a single value. That assertion is easily lifted to any finitary output (e.g. `[[]]cat{&1}` for zero outputs or `[[]cons cons cons]cat{&1}` for three outputs). For convenience and performance, a runtime might simply provide `{&0}`..`{&9}`. These size assertions can be removed upon verification by a static type checker or efficiently checked dynamically. 

Developers thus explicitly provide a finite set of inputs, assert a finite set of outputs, then run. They can be confident that the computation will fail fast rather than escape its scope. This is weaker than proper static types, of course, but it's still an effective basis for rapidly isolating errors in a codebase.

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

Dynamic error recognition needs to be efficient. One obvious candidate for dynamic error recognition is runtime type assertions about data that our runtime knows how to recognize efficiently. This can be expressed by annotation. For example:

        #42 {&nat}
        "hello" {&lit}
        [[A]]{&1}
        [[A][B][C]]{&3}

It's also easy to recognize substructure or value sealing errors:

        [A]{&aff} copy
        [A]{&rel} drop
        [A]{:s} op  
        [A]{.s} op

This is more or less the limit of efficient dynamic error recognition. We could feasibly introduce annotations to assert structural or behavioral equivalence, but those wouldn't be conditions we want to verify dynamically. (Even structural equivalence can be expensive to test in context of parallel computations and similar.)


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
        (that's all we need if we include identity opcode)

        (Common Opcodes)
        A very few ABC codes
        Recognized annotations
        A great many accelerators

This seems an efficient and lightweight representation. 

### Parallel Runtime

I intend to support **pipeline parallelism**, such that we may flexibly express computations that stream outputs from one task as inputs to another. Parallelism can be indicated by marking a block with `{&par}`, indicating that evaluation should proceed in parallel. Pipelining happens when multiple parallel computations are composed or inlined together. Consider:

        [A]{&par}i  [B]{&par}i

A runtime can dynamically recognize this case during evaluation, then directly attach output from `[A]` to input of `[B]` such that communication is no longer indirected through the parent. Establishing connections dynamically is expressive, enabling pipelines that shrink or grow over time (e.g. because `[]{&par}i` can be removed from a pipeline, while another computation may split into multiple paralle processes).

A runtime may use parallelism even if not indicated. It might be useful, for example, to divide `A [B]{&par}i C` into three parallel computations. However, it is feasible to model some sort of processor affinity groups so sequential bits remain sequential.

See [Parallism.md](Parallelism.md) for more on context-local implementation.

### Program Evaluation

The normal program representation a list of `(op, block)` cons cells, terminating with a singleton block (`[]` is singleton identity). During evaluation, we can use a Huet zipper as a basis to 'open' this list and work within it. This effectively gives us a `(stack, prog)` pair - i.e. the reversed program to our left can be understood as a stack, and the program is operating upon it. The main difference is that our 'stack' might contain actions, not just values. 

It is possible to divide a program into multiple subprograms, each with its own zipper, i.e. to effectively support processing of multiple expressions at once and moving data between them without unnecessary 'scanning' of the program. The ability to evaluate 'wide' in addition to 'deep' can resist getting stuck on any one problem.

Use of `{&par}` essentially enables programmers to control chunking of evaluation, specify the 'interesting' parts of the computation that deserve special focus. Every `{&par}` node would additionally be hooked into a big work pool and pipelines that carry values from one process to another. The "work pool" might be understood as simply a way to attach *very deeply* into within the program, in a thread-safe manner.

### Compilation

I'm not sure how to go about compilation of term rewrite programs. I'm certain it's been done (and a quick Google search confirms this). But even if I couldn't compile those programs, I can at least compile functions that are 'complete' in the sense of accepting a finite number of arguments and producing a finite number of results.

Aside from compilation, it would be convenient to at least support *compaction* of bytecode, such that I'm touching less memory during evaluation, copying less memory during iteration, etc..

### Performance Annotations

**Evaluation Control**

* `{&par}` - begin parallel evaluation of contained subprogram
* `{&seq}` - evaluate subprogram with same priority as parent
* `{&lazy}` - reduced priority for computation when applied
* `{&asap}` - compute a value with greater priority than parent 

The `{&asap}` annotation is a lightweight basis for staging, e.g. for forcing static computation of values. Use of `{&seq}{&asap}` would further allow for 'deep' evaluations (`{&asap}` is shallow). Interestingly, `{&seq}{&lazy}` is a possibility, indicating that we first want to evaluate the program as far as possible, then evaluate it by need when later applied.

**Representation Control**

* `{&lit}` - pack into a compact text format
* `{&nat}` - use runtime's natural number format
* `{&binary}` - represent a compact sequence of bytes
* `{&stow}` - move data out of working memory
* `{&compact}` - pack bytecode into tight compact format
* `{&compile}` - indicate compilation for performance
* `{&jit}` - heuristic compilation decision


## Numbers, Literals, and Command Sequences

Command sequences are useful for concise data representation, DSLs, monadic effects, modeling cooperative multi-threading, and more. Command sequences are a natural *iteration* concept in a language where we don't really have 'data', only commands (which might generate data). Given how proposed encodings for numbers and literals also act as iterators, an intriguing possibility is to unify the three ideas.

Desiderata for unification:

* Numbers and texts have a simple, uniform encoding.
* Encoding is compatible with [claw command sequences](CommandLine.md).
* Iteration may be aborted or saved and continued.
* Easy to compose and abstract command sequences.
* Can use iteration without a fixpoint function.



an intriguing possibility is to unify *natural numbers* and *embedded texts*.
An intriguing possibility is that *natural numbers* and *embedded texts* might be modeled as specific instances of command sequences.




With [claw command sequences](CommandLine.md) our command sequence is given a general meaning: `[foo,bar,baz] == [[bar,baz] after foo]`. I've also experimented with the variant `[foo,bar,baz] == [foo [bar,baz] yield]`. In both cases, I have a plain old block with some patterned convention. 

I would greatly prefer if my minimalist ABC command sequences can utilize to one of these two conventions, i.e. no *implicit* terminator, no wrapping `foo` into `[foo]`.


In both cases, `foo` is not wrapped as `[foo]`, which is convenient because it means we do not need to distinguish `foo` 
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