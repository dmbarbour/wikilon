# Awelon Bytecode

Awelon Bytecode (ABC) provides a computational medium in which ideas, data, and actions can be precisely expressed, shared, and composed. This document describes ABC's idioms and design. For a short summary, see [ABC](ABC.md). 

## Why a New Bytecode?

Awelon project has a vision for human-computer interaction that places users within the environment rather than above or apart from it. This vision includes alternative [application models](ApplicationModel.md) where humans and software agents operate upon a codebase in a shared environment, and a [linked evaluation model](AboutAO.md) that preserves ad-hoc structure meaningful to humans and software agents.

Most bytecodes aren't designed with such applications in mind.

ABC has many unusual features that make it suitable for Awelon project:

* ABC is **easily serialized and rendered**. ABC has a plain-text encoding that directly embeds numbers and literals. Thus, even a simple text editor can provide a useful view for debugging and comprehending code. AO is designed for use with editable views, projectional editors. Sophisticated graphical renderings are feasible.

* ABC **embraces data**. Where most languages shift responsibility for data to an external filesystem or database, ABC prefers to internalize it. Doing so simplifies serialization, persistence, composition, static safety, partial evaluation and staging, transparent procedural generation, and modeling interactive objects. Further, this readily extends to larger-than-memory data via a stowage model and persistent data structures.

* ABC is **evaluated by local rewriting**. ABC subprogram may usefully be evaluated with very little context, e.g. no need to provide arguments, no need to reach a subprogram from a main function. The bytecode is effectively an active material: just place it and watch it flow together. Further, partially evaluated results are useful even if our quota times out. Local rewriting simplifies a lot of Awelon project's HCI visions for computation being continuous, omnipresent, easily observed, composed, and tested.

* ABC is **purely functional**. The rewriting semantics are simple, confluent, and context-free. Computations may be freely be cached or replicated. Modulo performance and quotas, ABC computations have deterministic behavior no matter which runtime evaluates them.

* ABC is **concatenative**. Composition of ABC functions is a simple concatenation of their programs. There is no sophisticated binding to perform, no headers or footers to manipulate. Conversely, decomposition is also straightforward, simply cutting programs apart at well defined fragments.

* ABC is **streamable**. Unbounded programs can processed iteratively. Old code is forgotten even as new code is introduced. This supports soft, fluid metaphors involving streams of code gradually modifying objects in a context. Human actions can also be modeled as streams of code.

* ABC is **incremental**. A consequence of serializability and purely functional semantics is that caching computations is safe and easy. Systematic caching enables incremental computation, which is necessary for Awelon project's application models.

* ABC is amenable to **type checking** and static analysis at the bytecode level. This simplifies safe sharing and linking, and reduces the need for a staged workflows. We can feel confident and secure in widely distributing ABC.

## The Bytecode

See also [ABC](ABC.md) and [minimalist ABC](ABC_Minimalist.md). 

### Primitives

ABC has only four primitive combinators `abcd`.

        [B][A]a == A[B]         (apply)
        [B][A]b == [[B]A]       (bind)
           [A]c == [A][A]       (copy)
           [A]d ==              (drop)

In addition to these four combinators, we have the `[]` block structure. A primitive ABC program consists of a stream of `[abcd]`. The blocks should be properly balanced, i.e. every `[` has a matching `]` and vice versa. 

Non-primitive ABC program must be reducible to a primitive ABC program by expanding data representations with their Church encodings, inlining linker tokens, eliminating other tokens. Doing so would hurt performance and hinder debugging, but would not affect the observable results. 

The potential reduction to just four primitives makes ABC relatively easy to comprehend. There aren't any surprises, no corner cases to handle. And these primitives are *useful* even for high level code, easy to understand and to implement efficiently.

### Data Embedding

ABC's data embedding simplifies data entry, extraction, and debugging by presenting data in forms that humans can comprehend and tools can easily access. Natural numbers use eleven operators `#1234567890`. These are designed such that `#42` will compute a Church encoded 42. Text literals use an embedded encoding:

        "literals are multi-line UTF-8
         they start with character " (32)
         blacklist characters:
            C0 (except LF), DEL, C1
            surrogate codepoints
            replacement character
         linefeed is special character:
            LF SP   new line, drop SP
            LF LF   same as LF SP LF
            LF ~    terminates text
         no other special characters
        ~

The chosen Church encoding is suitable to unify literals and numbers with more general iterators and coroutines. For example, if I assume `(foo,bar,baz)` represented a general sequence, then the following semantics would apply:

        #5      == (,,,,)
        #3      == (,,)
        #0      == #

        "hello" == (#104, #101, #108, #108, #111)
        ""      == #

        [B][A](foo,bar,baz)i == foo [[B][A](bar,baz)i] A
        [B][A]#i             == B

        [A]i == A;  i = [][]baad

From this, we might derive that `#` is equivalent to `[di]`. Sequences generally have a structure `(foo,bar,baz) = [[foo](bar,baz)s]`, terminating with `[[baz]#s]` for some `s`. (The elements might be flipped if that leads to a cleaner definition. I still need to derive the definition for `s`.)

*Note:* The syntax `(foo,bar,baz)` is not supported at the ABC layer, but a variation is available in the [claw](CommandLine.md) view.

### Whitespace

SP and LF are permitted in ABC. They have identity semantics, equivalent to the empty program. Essentially, this just permits some lightweight formatting of byte code for easier reading.

### Tokens

Tokens have form `{foo}`, a short text wrapped in curly braces. Tokens enable symbolic extensions to ABC. Semantically, tokens are restricted by the normal rules for ABC: it must be possible to reduce every token to a primitive, purely functional `[abcd]` sequence. Tokens in ABC fall primarily into two groups:

* tokens with *identity* semantics for performance, safety, debugging
* tokens with *linking* semantics for structured development

Tokens with *identity* semantics include seals, gates, and annotations. These are described more thoroughly later in this document. Seals support lightweight types and controlled scope. Gates support active debugging - breakpoints, logging, etc.. Annotations serve ad-hoc purpose but require explicit runtime support. Seals, gates, and annotations are discussed in later sections.

ABC's favored linking model is [Awelon Object (AO)](AboutAO.md), which introduces tokens of the form `{%word}` binding to an implicit dictionary. During evaluation, the token is substituted for the word's definition when doing so enables evaluation to proceed. 

### ABC Dictionary

ABC includes a standard dictionary of opcodes defined in terms of an `[abcd]` string. This dictionary will gradually develop with the motivation to improve:

* performance and optimization
* data entry and extraction

The initial standard dictionary consists only of opcodes `#1234567890`. These support both motivations, and were deemed essential for effective data entry. Eventually, ABC might include:

* natural number arithmetic
* binary and list processing
* floating point number models
* linear algebra, matrix math
* polymorphic records and variants
* a Haskell style `runST` monad
* a futures and promises monad

The ABC standard dictionary will be carefully curated and vetted, and thus moves very slowly. Fortunately, use of [Awelon Object (AO) dictionaries](AboutAO.md) supports similar benefits without rigorous standardization. Use of runtime built-in AO dictionaries can provide an empirical testbed for potential ABC dictionary extensions.

## ABC Evaluation and Performance

### Evaluation Strategy

The general evaluation strategy for ABC is:

* evaluate outer program
* evaluate within blocks
* prioritize annotations

Effectively, evaluation defaults to call-by-need input to a computation and strict outputs. This is unusual for most purely functional languages, but gives us opportunity to apply annotations or conditionally drop parts of the output rather than evaluate them. 

A runtime has discretion to deviate from this strategy.

### Big Step Accelerators

ABC performance is achieved primarily by big-step rewriting with known functions. For example, consider the useful inline combinator `i`:

        [A]i == A
        i = [][]baad

We can evaluate `i` much more efficiently than `[][]baad`.

Big-step rewriting becomes especially valuable when working with Church-encoded data structures. For example, if we know some function `+` is equivalent to addition when applied to numbers, then we can efficiently rewrite `#23 #19 +` to `#42`. The runtime/compiler can stick to *compact* representations rather than expanding the Church encoding. Efficient processing of massive texts, vectors, binaries, matrices, etc. is feasible.

Useful functions like `i` and `+` may eventually become part of the ABC standard dictionary, enabling use like bytecodes. However, the ABC standard dictionary moves very slowly and will have a slow vetting process. 

In the mean time, we can achieve similar (albeit less portable) benefits by having each runtime/compiler specify a *built-in* [AO dictionary](AboutAO.md). For example, wikilon runtime might provide `{%i@wikrt}` and `{%+@wikrt}`. This dictionary of built-ins should be subject to normal perusal and export.

*Aside:* Use of built-in functions avoids need for sophisticated recognition algorithms. It is possible than an optimizer could recognize `[][]baad` and replace it by `{%i@wikrt}`, but it is unnecessary to do so.

### Rewrite Optimizations

The basic rewrite rules admit some rewrites for performance. For example, `[]a` - applying identity - can be rewritten to the empty program. We get more useful rewrites when we start working at higher levels. For example, we can also eliminate `[i]b` or `[]bi`. Loop fusions for collections processing like `[F] map [G] map == [F G] compose map` are viable, assuming the `map` function is sufficiently restricted.

I intend that developers can propose rewrite rules, preferably together with a proof of correctness. Meanwhile, rewrite optimizations are at least available for built-in functions and perhaps a trusted subset of the dictionary.

### Compilation

A runtime can provide a `[function]{&jit}` annotation such that we construct a more efficient representation for evaluation of the function. Taken together with [AO dictionaries](AboutAO.md) and a little caching, we can effectively achieve staged compilation for important words. Effective use of JIT may be limited to cases where it's easy to determine static types.

Compiling an executable independent of an ABC runtime is feasible as a case of *program extraction*. Generally, program extraction might translate an ABC program to a Haskell module, JavaScript object, C function, and so on. Extraction requires a well understood program type to integrate with the context. In case of independent executables, the program type will likely be some variant on monadic IO.

Awelon project favors [application models](ApplicationModel.md) that do not rely on program extraction, so local `{&jit}` compilation more appropriate for basic performance concerns. However, I would like to support both techniques.

### Parallelism and Concurrency

ABC's purity supports a simple form of parallelism: we can evaluate many subexpressions at once. This particular form of parallelism is made explicit by the `[computation]{&par}` annotation. However, while `{&par}` is lightweight and has useful applications, it is not very expressive because parallel components cannot communicate.

Fortunately, it is feasible to achieve more expressive parallelism. In general:

* design a deterministic, concurrent computation model
* accelerate evaluation of this model with a built-in

Viable models include: 

* single-assignment futures monad
* [Kahn process networks](https://en.wikipedia.org/wiki/Kahn_process_networks)
* [flow-based programming](https://en.wikipedia.org/wiki/Flow-based_programming)
* [network stream transducers](https://github.com/ekmett/machines)

The single-assignment futures monad is problematic. We cannot ensure determinism without a sophisticated static analysis, to ensure that created futures don't escape the evaluation and that futures are resolved only once. Avoiding models that construct 'new' identifiers seems wise.

Support for other models hasn't been fully reviewed. KPN or a variant of FBP seem a promising balance between expressiveness and simplicity. The machines model seems much more sophisticated than I'd prefer to deal with.

In any case, the idea of supporting communication between parallel components via accelerated function evaluation seems simple, effective, and worth pursuing. It isn't high priority, but we can move beyond the limits of simple parallelism when need for greater scalability arises. 

Similarly, it is feasible to achieve GPGPU parallelism given suitable built-ins for linear algebra.

### Cached Computation

### Value Stowage

### Performance Annotations

Many annotations are used for performance:

* `{&seq}` - evaluate subprogram immediately 
* `{&par}` - parallelize evaluation of subprogram
* `{&lazy}` - call by need evaluation for subprogram
* `{&lit}` - force argument to text literal representation
* `{&nat}` - force argument to natural number representation
* `{&stow}` - move value to link layer, away from working memory
* `{&trash}` - drop value but preserve placeholder with metadata
* `{&cache}` - use cached result or add result to cache
* `{&opt}` - simplify and optimize a subprogram 
* `{&jit}` - compile a function for runtime internal use

Use of annotations to control staging and compilation has potential to be very effective in giving developers control of the performance of their code. In general, annotations on representation also support type checking and may be effectively used together with accelerators to squeeze the most performance from a representation.

*Note:* If you want something close to conventional laziness where we cache the result, use `{&cache}{&lazy}`.

## ABC Assumptions and Idioms

### Annotations

### Seals

### Gates

### Program Animation

### Runtime Error Reporting


### Conditional Behavior

We can Church encode true/false as values:

        [OnFalse][OnTrue] true i  == OnTrue
        [OnFalse][OnTrue] false i == OnFalse

        true  = [ad]
        false = [di]    (= #)

Usefully, we don't actually need an 'if' action. Applying true or false does the job. But this is probably *not* the best approach to conditional behavior - it suffers 'boolean blindness'. Instead, it might be better to model a sum type like: `(a + b)` or `Left a | Right b`. Perhaps something like:

        [A→C][B→C] [A]Left i  == A A→C
        [A→C][B→C] [B]Right i == B B→C

We'll need to see how these techniques work in practice. But as a general rule, the 'stack type' after a conditional behavior should be identical on both path. At least until we're ready to start dealing with dependent types.

### Iteration and Termination

ABC can support ad-hoc loops via a fixpoint combinator. However, for many use cases, fixpoint is unnecessary. Numbers, texts, and sequences enable iteration via simple application.

As a general rule, ABC assumes correct computations will terminate. ABC has no use for an infinite computation. Effects require returning with a request for an effect and a continuation. If a static analysis can prove non-termination for a computation, that's certainly an error.

### Type Safety Analysis



