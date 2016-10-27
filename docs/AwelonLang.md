
# Awelon Language

Awelon is a purely functional language based on concatenative combinators. Awelon is designed for an [RESTful application model](ApplicationModel.md) that maintains application state within a codebase and handles effects in context of a multi-agent system (for example, via [publish-subscribe](https://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern) and [tuple space](https://en.wikipedia.org/wiki/Tuple_space) patterns). Awelon systems support [incremental computing](https://en.wikipedia.org/wiki/Incremental_computing) with [hypermedia](https://en.wikipedia.org/wiki/Hypermedia) at a large scale.

Syntactically, Awelon is simple and minimalist (similar to Forth). Where more sophisticated or domain specific syntax is appropriate, Awelon systems shall leverage [editable views](http://martinfowler.com/bliki/ProjectionalEditing.html) rather than risk complicating parsers. These editable views will be defined within the Awelon system itself.

## Primitives

There are four primitive combinators:

            [B][A]a == A[B]         (apply)
            [B][A]b == [[B]A]       (bind)
               [A]c == [A][A]       (copy)
               [A]d ==              (drop)

Those square brackets `[]` enclose a 'block' of Awelon code, representing a function. With just this much, all computable functions can be defined. As a lightweight proof, I'll define the Curry-Schönfinkel SKI [combinators](https://en.wikipedia.org/wiki/Combinatory_logic).

            [B][A]w == [A][B]       (swap)           w = []b a
               [A]i == A            (inline)         i = []w a d
         [C][B][A]s == [[C]B][C]A   (S combinator)   s = [[c]a b w]a i
            [B][A]k == A            (K combinator)   k = a d

Awelon's primitive combinators are more convenient than SKI. Apply and bind provide structural control over scope, respectively hiding or capturing one value. Separation of 'copy' and 'drop' is convenient both for performance and [substructural type](https://en.wikipedia.org/wiki/Substructural_type_system) analysis. The value ignored by apply is accessible for further partial evaluations.

## Words

Words are identified by a sequence of UTF-8 characters with a few limitations. The blacklist is `@#[]<>(){},;|\=&"`, SP, and C0 control characters. Developers are encouraged to further limit symbols to those convenient in external contexts, such as URLs, natural language, or projectional editor views.

Words are not defined by the evaluator. Instead, words are defined in an external dictionary, described later. Valid definitions are acyclic definition. The four primitives `a`, `b`, `c`, and `d` may not be redefined.

## Annotations

Annotations help developers control, optimize, and debug computations. Annotations are represented as parentheticals (like this). Some useful examples of annotations include:

* `(/2)..(/9)` - arity annotations to defer computations
* `(0)..(9)` - tuple type assertions for scope control
* `(aff) (rel)` - support substructural type safety
* `(:foo) (.foo)` - lightweight type tag and assertions
* `(par)` - request parallel evaluation of computation
* `(jit)` - compile a function for use in future evaluations
* `(stow)` - move large values to disk, load on demand
* `(memo)` - memoize a computation for incremental computing
* `(error)` - mark a value as an error object
* `(@foo)` - gates for active debugging (logging, breakpoints)
* `(force)` - evaluate previously deferred computations

Any Awelon runtime may support arbitrary new annotations, or may freely drop and ignore unrecognized annotations. Support for popular annotations will propagate among runtimes, and hopefully we'll approach a global consensus on useful annotations. The annotations listed above are just a starting point, and are documented later.

Annotations, by nature, must have no observable effect on a correct computation. However, annotations may cause an incorrect computation to fail fast (perhaps even detect failure statically), and there may be external observations such as debug logs or different halting states.

## Numbers, Texts, Sequences

Awelon language has one primitive data type: the `[block of code]`, representing a function. However, arbitrary data may be [Church encoded](https://en.wikipedia.org/wiki/Church_encoding). To provide a convenient foundation, Awelon language provides syntactic sugar for encodings of: numbers, texts, and sequences.

A sequence will generally perform some work then yield to the caller with a continuation. We might represent these as syntactic sugar with some semantics:

        {foo, bar, baz} == [[foo] {bar, baz} y]
                  {baz} == [[baz]     ~      y]

        [B][A]~ i == B
        [B][A]{foo,bar} i == foo [[B][A]{bar}i] A

        ~ = [a d]

A text might then be understood as a sequence:







, and common representations may further be accelerated. Awelon provides a lightweight syntax for embedding of numbers, texts, and sequences.

A sequence...

        STATE ACTION STEP | STATE RETURN FINI

        ACTION STEP must have type STATE → STATE
        RETURN FINI must have type STATE → STATE-FINAL



A sequence is a function that either does some work then yields to the caller with a continuation, or performs no more work. Explicitly, there is no "final" return, because that would hinder

        {foo, bar, baz}         == [[foo] {bar, baz} y]
                  {baz}         == [[baz]     ~      y]

        [B][A]{foo, bar, baz}   == foo [[B][A]{bar,baz}i] A
        [B][A]~ i               == B

A text can be represented as a sequence of bytes:

        "hello"         ==  {#104, #101, #108, #108, #111}
        ""              ==  ~

A natural number can be represented as a sequence of NOPs:

        #7              ==  {,,,,,,}
        #0              ==  ~



A normal block of code in Awelon language effectively represents a normal function



Sequences, in general, represent functions that perform an operation then yield with a continuation until a final return.

        {foo, bar, baz} ==  [[foo] {bar, baz} yield]
                  {baz} ==  [[baz]   return   yield]




Natural numbers are embedded as pseudo-symbols starting with `#` as in `#12345`. Natural numbers are a syntactic sugar for a sequence with the given number of actions

Text may be embedded either inline or multi-line:

        "hello world!"

        "
         hello
         multi-line
        ~

Texts are generally limited to valid UTF-8 excluding C0 except LF, which is a special case. Other than LF, there are no escape characters. Inline text cannot contain LF or the double quote. Multi-line text may contain both LF and the double quote, but escapes contained LF by indenting the next line with SP. Multi-line text contains an additional LF at the start and end, which is removed by the parser. Texts desugar to a sequence of UTF-8 bytes, encoded as natural numbers.

        "hello" ==  {#104, #101, #108, #108, #111}
        "→"     ==  {#226, #134, #146}


Natural numbers should be encoded as:

Natural numbers, texts, and sequences are all unified on the definition of `~`



* with a result and a continuation like coroutines. 












The only primitive data type in Awelon is the `[block of code]`. However, Awelon does support a lightweight embedding of natural numbers and texts that can readily be leveraged.

Awelon does not have any primitive data type other than the `[]` block of code]. However, Awelon does offer some syntactic sugar for lightweight encoding of natural numbers and textual data. 

Natural numbers are embedded as `#42` or `#12345`, and correspond to the trivial expansions `# 4 2` and `# 1 2 3 4 5` where each of `#1234567890` are independently defined symbols. (A consequence is that `#4` cannot be defined as a symbol normally.) 

## Dictionary

A set of defined symbols forms an Awelon 'dictionary', and forms the codebase of an Awelon system. 

Awelon has two major requirements for a dictionary:

* definitions are acyclic and complete
* primitives `a b c d` remain undefined

Definitions of symbols form a 'dictionary'. 


## Evaluation Strategy


## Annotations

While Awelon only defines four primitive *computing* combinators, Awelon also introduces multiple symbols to help control computation. Th

Unlike most languages with first-class functions, Awelon supports serialization of those blocks, which represent functions. 




Unlike most languages, Awelon ensures that blocks can be serialized and rendered.

Awelon does not have any primitive data types other than the block. 




I believe ABC's primitive combinators are more convenient than the SKI primitives. Apply and bind (`a` and `b`) allow effective control of scope, respectively hiding or capturing part of the environment. Non-linear copy and drop operators (`c` and `d`) are separate, which allows precise control of substructure (checkable via `{&aff}` and `{&rel}`). ABC has multi-valued output, which supports flexible factoring and structural types. Apply immediately returns one value, which is useful for partial evaluations.

 some advantages over SKI. A

Symbols in Awelon do not need to be a single character, though that is a convention for common operations. Larger words are readily defined

Further, data may be [Church encoded](https://en.wikipedia.org/wiki/Church_encoding). 






Awelon defines additional symbols to improve performance, control computation, or support fail-fast static analysis or debugging. But all computation and even *data* is defined in terms of these four primitives.



Awelon also defines other symbols that do not compute but instead serve a role as annotations to improve performance, control evaluation, or support fail-fast static analysis or debugging. 

Data in Awelon language must be [Church encoded](https://en.wikipedia.org/wiki/Church_encoding) (or perhaps [Mogensen-Scott encoded](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding), or other ). However, Awelon language does provide some shorthand to simplify encoding of texts and natural numbers.

Beyond these, Awelon defines symbols that do not compute but instead serve a role as annotations to improve performance or simplify static analysis or debugging.

Beyond these, we have a series of non-computing combinators called annotations.


The square brackets `[]` wrap first-class functions, which simply contain more Awelon code. 



Awelon is extremely simple enough

 

 There are four primitive combinators:

 a simple language designed for incremental computing and sharing at a large scale. 

Awelon is a very simple language designed for incremental computing at a large scale. Word

 consisting primarily of words and `[]` blocks representing first class functions. 

