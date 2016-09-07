
# Minimalist ABC

A simpler ABC is (hopefully) a better ABC.

THE BIG (DIFFERENT) IDEAS: 

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

* evaluate without context, no 'input' setup
* no need to specialize a partial evaluator
* every definition in a codebase doubles as test
* evaluate many fragments of program in parallel
* can evaluate in presence of unrecognized tokens
* rendering may focus on only one type of value
* render evaluated output for every page/cell
* potential to animate evaluation by quota or gate
* debugging simplified, view of program under eval
* stowage simplified, just one type to handle

This idea for a minimalist ABC seems very promising. 

After contemplating my concerns below, I think minimalist ABC is better in many areas than normal ABC, and no worse in others. These areas include systems integration (e.g. data input and extraction), control and scoping of behavior, error detection, and performance. 

## The Bytecode

The proposed base is just four primitive combinators. 

        [B][A]a     == A[B]         (apply)
        [B][A]b     == [[B]A]       (bind)
           [A]c     == [A][A]       (copy)
           [A]d     ==              (drop)

This base is small, complete, and friendly to substructural types. The `[]` square brackets delimit first-class subprograms. The empty program is valid and has identity semantics. All programs and data in ABC have a formal semantics as a finite expansion to a sequence of just six characters, `[abcd]`.

For performance and convenience, ABC provides more than the primitives:

* Lightweight formatting: SP and LF equivalent to empty program. 
* Lightweight embeddings for data: natural numbers, literals.
* A standard dictionary of accelerated opcodes for performance. 
* Symbolic extensions for performance, safety, debugging, linking.

ABC is restricted to purely functional computation. However, ABC is easily used to construct effectful programs using [monadic effects](http://okmij.org/ftp/Haskell/extensible/more.pdf) or other [application models](ApplicationModel.md) that shift the actual performance of effects to an external agent. (I wonder if tuple spaces would be a good fit.)

### Numbers and Literals

Natural numbers are embedded as `#42` or `#108`. The operator `#` introduces a new zero value while the operators `0-9` each multiply by ten then add the digit. When applied, natural numbers would iterate a given program based on their specified count. 

Embedded literals have the format:

        "start with character `"`
         may have multiple lines
         LF is escaped by following SP
         text terminates with LF ~
        ~

Literals must be valid UTF-8 with a small blacklist (no control (C0, C1, DEL) except LF, no surrogates, and no replacement char). When applied as a program, literals will support iteration over every contained UTF-8 byte. (The assumption here is that byte-level ops are the common case.)

Semantically, these data embeddings have a Church encoding, one that unifies nicely with command sequences. ABC doesn't provide a concise representation for command sequences. However, an [editable view](CommandLine.md) may present something like: `(foo,bar,baz)`. In this case, our unification is:

        #0      == #
        #1      == ()
        #2      == (,)
        #5      == (,,,,)

        ""      == #
        "h"     == (#104)
        "hello" == (#104, #101, #108, #108, #111)

        [B][A](foo,bar,baz)i == foo [[B][A](bar,baz)i] A
        [B][A](baz) == baz [[B][A]#i] A
        [B][A]#i == B
        [A]i == A

The semantics is to follow each action in the sequence with a client-provided action. This is generic. Monadic sequences can be represented if our actions `(foo,bar,baz)` each includes another possibly empty command sequence, and `[A]` uses a fixpoint. But a lot of useful iteration can be expressed without fixpoints. Todo: Provide definitions for these semantics.

### The Dictionary

ABC includes a standard set of additional operators, each bound to a UTF-8 codepoint and defined by its expansion to an `[abcd]` sequence. The operators `#1234567890` for embedding number data are examples of ABCD operators. There are many potential motivations for adding an operator to the dictionary:

* ease of reading, rendering, editing code
* easier injection and extraction of data
* greatly improves interpreted performance
* simplifies optimization and compilation 

ABC doesn't need dictionaries just to compress code. For that, we'll use token-based linking. So the primary motivations are oriented around performance, HCI, and efficient integration with external systems. But ideally a candidate supports all these roles at once.

ABCD will likely get started with arithmetic and common data plumbing (like `i` for inline instead of `[][]baad`). Long term, I'd like to have operators that allow me to easily embed binaries within text, perhaps render images when rendering code, support for rich number models (rationals, decimals), GPGPU accelerated vector processing, or acceleration of monads for stuff like stateful programming (like Haskell's `runST`) or futures/promises.

### Tokens

ABC supports symbolic extensions by embedding tokens in code. Tokens are short texts wrapped by curly braces, e.g. `{foo}`. In addition to being valid literal texts, tokens are limited to 255 bytes utf-8, and exclude LF and the curly braces internally. ABC tokens must have pure, local semantics. However, there's great deal they can do within that limit. Examples:

* acyclic, static or dynamic linking of code
* stow large values to disk, save working memory
* evaluation strategy, parallelism, and staging
* enable remote evaluation in a cloud or mesh
* control compilation and optimization efforts
* manifest type assertions, static or dynamic
* control scope and interaction of computations
* mark values or computations as erroneous
* add debugging breakpoints or logpoints
* provide hints for rendering of results

I use tokens for annotations, gates, sealing, and linking.

Annotations are identified by a `&` prefix, and have miscellaneous use but always have *identity* semantics. For example, `{&par}` marks a block for parallel evaluation, `{&nat}` might assert we have a natural number, and `{&jit}` might tell our runtime to compile some code. 

Gates are identified by prefix `@`, e.g. `{@bp123}`. Gates provide a simple basis for active debugging. At the runtime level, gates may be configured by name to serve as breakpoints, logpoints, tracepoints, or to simply let data pass. They can easily be leveraged to animate program evaluation.

Sealing uses paired tokens, e.g. `{:foo}` and `{.foo}`. The semantics is simply that `{.s}{:s}` and `{:s}{.s}` are *identity behaviors*. With program rewriting, we can simply delete those sequences, but it's also feasible to use wrapper/unwrapper techniques on whole streams of values. One must use `[{:s}]` and `[{.s}]` to scope sealers to specific parts of a computation.

Linking replaces a token by a ABC program. For example, in [Awelon Object](AboutAO.md) our token `{%foo}` is replaced by definition of `foo` in the bound dictionary. Value stowage also produces links. Links must form a directed acyclic graph, and every linked program must at least be syntactically complete (e.g. `[]` blocks balanced, literals terminated).

## Ideas and Idioms

### Controlling Scope

Our primitive apply operation is `[A][B]a == B[A]`. Thus, it comes a built-in mechanism to scope the right side of our computation. However, we also want control of the *left* scope, i.e. how many items the computation will consume or leave. To achieve this control, we can use *blocks* to delimit scope. 

        [A][B]b     == [[A]B]
        [[A]]{&1}   == [[A]]

Our build operator `b` can control movement of input into the computation. If we expect three inputs, that becomes `bbb`. To control the number of outputs we can use an annotation, such that we can verify it statically or fail fast dynamically. The `{&1}` annotation can be generalized to any number of arguments (e.g. `[i[]]b{&1}` would assert an zero results while `[i[]bbb]b{&1}` would assert three results). But for convenience and performance, I could provide `{&0}`..`{&9}`.)

Developers thus explicitly provide a finite set of inputs and assert a finite set of outputs, and control scope.

### Data Encapsulation

The sealer tokens (e.g. `{:s}` and `{.s}`) can easily be used to model opaque data types and structures, e.g. by composing them into blocks.

### Weak Substructural Types

Substructural types are very expressive for structuring application behavior independently of syntax. For example, while the common resource pattern `open use* close` can be enforced by a structured syntax or RAII pattern, we could instead model this by constructing a linear handle (with sealed data) upon `open` and destroying it later, in the `close` action. This would give us a lot more flexibility to use the resource within the program.

However, I'm not convinced I won't want an escape for substructure, e.g. to model lossy networks or backtracking. So my proposal is to enforce these types only weakly. The proposal is as follows:

        [A]{&rel} == [A]    (mark relevant)
        [A]{&aff} == [A]    (mark affine)
        [A]{&nss} == [A]    (clear marks)

* a block marked relevant may not be dropped
* a block marked affine may not be copied
* a block both affine and relevant is called 'linear'
* block compositions preserve substructure

We can then model a general copy or drop function that ignores substructure by prefixing with `[] cons {&nss}` (and unwrapping our copies). But developers will be able to reason about substructure except where they explicitly choose to bypass it, and it will be a lot easier to search and find every relevant bypass by searching for clients of the `{&nss}` within a subprogram.

### Runtime Errors

Programmers might specify errors to indicate:

* incomplete programs (todo some day soon!)
* incorrect use (partial functions)
* assertion failures (implementation error)

I propose an `{&error}` annotation to indicate errors within code.

        "todo: implement foo"{&error}

The annotation creates an *error value*. It can be copied or dropped like any other value. An attempt to observe the error value (e.g. through application) becomes the actual error. This will cause computation to become 'stuck' at that point. Whenever our runtime dynamically recognizes an error, it should also use `{&error}`.

        [A]{&aff}c  ==  [[A]{&aff}c]{&error}i
        [A]{:s}i    ==  [[A]{:s}i]{&error}i

The runtime may additionally maintain a list of errors in the program, e.g. for efficient access.

### Static Analysis and Type Safety

ABC with type declarations is amenable to static typing. Tokens such as `{&nat}`, `{&aff}`, `{&1}`, and `{:s}` provide a foundation from which static type inference could proceed. 

Rich types are feasible. It's a bad idea to squeeze much logic into a token, but it is feasible to use programs or data to describe types. Given an [AO dictionary](AboutAO.md), types could be associated with each word via a simple convention like using `foo.type` to describe the type of `foo`. We could also use a pattern like `[foo][type]{&type}d` as a basis for inline declarations of types.

### Gates for Active Debugging

Program rewriting makes debugging relatively easy. We can render the whole program 'state' after something goes wrong. Use of breakpoints can augment this further by enabling developers to observe intermediate states and step through problematic parts of a computation. 

I propose to reify breakpoints by use of 'named gate' tokens. The behavior of a gate will be configured by name at the runtime level, e.g. as open, closed, logging, tracing.

        [A]{@foo} == [A]            (if open, logging, tracing)
        [A]{@foo} == [A]{@foo:123}  (if closed. `123` unique in output)

A logging gate copies data value to a log, perhaps with some profiling metadata. Tracing gates tag values as they pass through with simple markers, enabling us to better know their origins. Logging gates support the common printf-style of debugging, while tracing gates can provide extra context in a language without stacks.

A closed gate acts much like a breakpoint, and stalls computations depending on `[A]`. Other parts of the program will continue evaluation, so it is possible we'll stall on many gates, or many instances of the same gate. To continue computation, we may explicitly open arbitrary gates upon which we're stalled. Stalling gates are renamed to simplify precise identification.

Repeatedly evaluating with closed gates, taking a snapshot, then continuing can support program animation with a lot more structure than rendering on quota, and provides the same benefits of 'logging' but with a lot more available context.

## Runtime and Performance

### API

The API will be oriented around building a 'program' left to right in terms of injecting data (texts, numbers) and operations, evaluating it, then extracting data. We might also identify errors, extract log messages, or continue evaluations involving breakpoints. 

During evaluation, all program level errors are modeled via `{&error}`. The context level issue of 'running out of memory' shall be handled by a checkpoint-based evaluation mode so that, while we might lose a little work, our program state remains valid.

*Aside:* For work with AO dictionaries, it seems hugely useful to support injection of dictionary into our context. This would happen during 'program build' time, and we could evaluate even if we don't know a definition. E.g. if we forget to define `{%foo}`, we can fail. Injected definitions could be immutable after assignment.

*Note:* I may provide an option/mode to perform lightweight optimization and simplification during program entry. But I will want a version that exactly preserves a program as entered.

*Aside:* I won't introduce support for binaries yet - at least not before accelerators exist to simplify rendering within a program. 

### Linking in Context

With program rewriting, I can continue evaluation in the presence of unknown tokens, we'll just not be able to move data across that token. Thus, there is no reason to restrict against tokens we don't recognize. 

An interesting related point is that we can easily provide ABC definitions for these tokens, in a linker style.

Rather than repeating the work of injecting code into a context every time, ideally we can reify our dictionary just once then reuse it many times, perhaps integrated with value stowage. So this might be modeled by an API that introduces a special 'dictionary value' and enables constrained updates thereof, yet ensures this value is subject to stowage. I.e. runtime-layer data encapsulation. I think that, by this means, we could gain a great deal of performance.

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

I'm not sure how I'll use that `010` tag. Maybe for 'zipper' objects or finger-trees for open blocks during evaluation. Maybe for context-local shared objects (refcts, etc.).

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

*Todo:* I know the above evaluator will work well, but it would be nice to support something closer to a Forth-like stack. At least for cases where I know it can mostly complete. Alternatively, it might be feasible to use a contiguous stack in place of our list-based zipper stacks (maybe a tagged object?).

### Compilation

I'm not sure how to go about compilation of term rewrite programs. I'm certain it's been done (and a quick Google search confirms this). But let's just assume I won't succeed much at compilation in cases where I cannot completely apply a value.

That still leaves a useful subset: given a compiled subprogram, I can wait to evaluate until I have a sufficient amount of input for its *arity* before applying. I can also ensure the arguments are fully evaluated. Thus, the problem reduces to evaluation of a more conventional, purely functional program. And if need arises, I can easily flip back to a compiled program's source to perform some partial applications or whatever.

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
* `{&trash}` - recycle memory (error value), but preserve annotations

## Deprecating Symbolic Structure and Metaprogramming 

The idea of using tokens for structured data (polymorphic records and variants) would give me a jump start for turning ABC into a practical language. However, it comes with a large cost: I require *metaprogramming* to introduce new tokens - new field names or variants. I ultimately don't need metaprogramming for anything else. 

It seems feasible to shift the modeling of structured data primarily to ABCD, i.e. by developing appropriate accelerators to receive the data.

It will take time to develop the right ABCD extensions. Meanwhile, data entry at least can be simplified by use of an appropriate [claw](CommandLine.md) extension for JSON-like data, or perhaps use of an association sequence/list. And data extraction can always use active extraction. This should also give me time to find a *right* data model for my actual use cases, without committing early to a specification I might later regret.

And if it turns out I need symbol/token structured data... I can more easily add it later than remove it later.

Also, while metaprogramming may be convenient in other contexts, anything that involves searching the codebase would better be modeled explicitly - e.g. modeling indexed collections within the dictionary - because doing so simplifies solidifying results or testing with various different results, and also simplifies dependency management.

### Symbol Structured Data: Records and Variants (Deprecated)

ABC structured data leverages tokens to support row-polymorphic records and polymorphic variants. Nothing here increases expressiveness of ABC, i.e. the tokens involved could be modeled in terms of *linking* ABC subprograms. However, leaving the definitions and representations implicit simplifies:

* data entry and extraction, language interop
* plain text legibility and flexible rendering
* structural type safety, static or dynamic
* performance, be it interpreted or compiled

Usage:

        {#}[A]{/foo}[B]{/bar} {*foo}  ==  {#}[B]{/bar}  [A]
        [B][A]{#c}{?c} == {#c} A
        [B][A]{#d}{?c} == {#c} B

Tokens:

        {#}             empty anonymous record
        {/field}        write the record field
        {*field}        read field from record
        {#class}        empty variant record
        {?class}        match variant record

Record operations are linear. It is invalid to read a field that is not in the record, it is invalid to write a field that is already in the record, and reading removes a field from a record. Update may be expressed as a read-write action. 

Use of field operations on anything other than an anonymous or variant record is an error. Matching against anything that is not specifically a variant record is an error. Records may be copied or dropped (ops `c` and `d`) if and only if those operations are permitted for every contained member. Records are not usable as functions, i.e. use of `a` or `b` with a record as the primary argument is an error.

### ABC Metaprogramming (Deprecated)

An ABC program consists of `[abcd]` sequences with embedded `{tokens}`. Construction of `[abcd]` sequences requires no special attention. Thus a complete metaprogramming solution only needs to inject tokens. The challenge is to achieve this without compromising effective static reasoning or reusability of code. My best ideas so far involve modeling construction of tokens as an *effect*. 

        [program in Meta monad] {runMeta}

The simplest `{runMeta}` monad might answer every request with a wrapped token. We could request a token `"/foo"` and get back `[{/foo}]`. Note that `{runMeta}` might not exist as an explicit token, but rather be implicit to a programming environment. Within an [AO dictionary](AboutAO.md) we might use `foo.make = [program in Meta monad]` and implicitly reconstruct `foo` from `foo.make` whenever its recorded dependencies change. (A staged build system is an easy fit for Awelon project's [application models](ApplicationModel.md).)

Monadic metaprogramming is first class, subject to composition, abstraction, and flexible reuse. Assuming a free monad, we can intercept the requests, simulate and test how the metaprogram would behave under a variety of mockup environments and configurations. There is very clear staging between the metaprogram and its computed output, no risk of incomplete output, and it's easy to record requested dependencies. Any competing model for metaprogramming should do at least this well for serious consideration.

## Command Sequence Syntax? Maybe later.

I'm tempted to support `(foo,bar,baz)` as a syntactic sugar at the bytecode level. 

