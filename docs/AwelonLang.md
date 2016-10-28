
# Awelon Language

Awelon is a purely functional language based on concatenative combinators. Awelon is designed for an [RESTful application model](ApplicationModel.md) that maintains application state within a codebase and handles effects in context of a multi-agent system (for example, via [publish-subscribe](https://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern) and [tuple space](https://en.wikipedia.org/wiki/Tuple_space) patterns). Awelon systems support [incremental computing](https://en.wikipedia.org/wiki/Incremental_computing) with [hypermedia](https://en.wikipedia.org/wiki/Hypermedia) at a large scale.

Syntactically, Awelon is simple and minimalist (similar to Forth). Where more sophisticated or domain specific syntax is appropriate, Awelon systems shall leverage [editable views](http://martinfowler.com/bliki/ProjectionalEditing.html) rather than risk complicating parsers. These editable views will be defined within the Awelon system itself.

## Primitives

There are four primitive combinators:

            [B][A] a == A[B]    (apply)
            [B][A] b == [[B]A]  (bind)
               [A] c == [A][A]  (copy)
               [A] d ==         (drop)

Those square brackets `[]` enclose a 'block' of Awelon code, representing a function. With just this much, all computable functions can be defined. As a lightweight proof, I'll define the Curry-Schönfinkel SKI [combinators](https://en.wikipedia.org/wiki/Combinatory_logic).

            [B][A]w == [A][B]       (swap)           w = [] b a
               [A]i == A            (inline)         i = [] w a d
         [C][B][A]s == [[C]B][C]A   (S combinator)   s = [[c] a b w] a i
            [B][A]k == A            (K combinator)   k = a d

Awelon's primitive combinators are more convenient than SKI. Apply and bind provide structural control over scope, respectively hiding or capturing one value. Separation of 'copy' and 'drop' is convenient both for performance and [substructural type](https://en.wikipedia.org/wiki/Substructural_type_system) analysis. The value ignored by apply is accessible for further partial evaluations.

## Words

Words are identified by a sequence of UTF-8 characters with a few limitations. The blacklist is `[]<>(){},;|\"`, SP, C0, and DEL. Developers are encouraged to further limit symbols to those convenient in external contexts, such as URLs, natural language, or projectional editor views.

Words are not defined by the evaluator. Instead, words are defined in an external dictionary, described later. There are some limitations on which words may be user-defined. Valid definitions are acyclic. The four primitives `a`, `b`, `c`, and `d` may not be redefined.

## Annotations

Annotations help developers control, optimize, and debug computations. Annotations are represented as parenthetical words like `(par)` or `(/3)`. Some useful examples of annotations include:

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

## Data

Awelon language optimizes representations for natural numbers and texts. Natural numbers use pseudo-words described by regex: `0 | [1-9][0-9]*`. Texts have two embeddings, inline like `"hello, world!"` or multi-line:

        "
         multi-line texts starts with `" LF` (34, 10)
         each line is indented by one space (32)
         terminate the text with `LF ~` (10, 126) 
        ~

Texts must be valid UTF-8, forbidding C0 (except LF) and DEL. LF is the only special case. There are no character escapes. For multi-line texts, the extra space at the start of each line, and the initial and final LF, are not considered part of the text.

Awelon language has exactly one primitive value type - the `[]` block of code, representing a function. Data is instead [Church encoded](https://en.wikipedia.org/wiki/Church_encoding) or uses alternatives like [Scott encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding), representing values as functions. The support for natural numbers and texts is close in nature to [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) for an encoding.

Before I jump to the encoding of numbers and texts, let us examine encodings for other useful data types. Booleans select from a pair of functions:

        [onT][onF] [T] i == onT                 T = d i
        [onT][onF] [F] i == onF                 F = a d

        Related Definitions:
           [A] i == A                           i = [] w a d 
        [B][A] w == [A][B]                      w = [] b a   

Booleans can be generalized to algebraic sum type `(A + B)`.

        [onL][onR] [[A]L] i  == [A] onL         L = w d w i
        [onL][onR] [[B]R] i  == [B] onR         R = w b a d

Constructors for left and right values are trivial, e.g. if `[B] mkR == [[B] R]` then `mkR = [R] b`. Constructors can always be modeled this way: simply use `b` to bind however many components into a partially constructed value. Functions with no constructors are trivial, e.g. `true = [T]`. 

Pairs, algebraic `(A * B)` product types, may also be Church encoded:

        [onP] [[B][A]P] i == [B][A]onP            P = [] b b a i
        [B][A]mkP == [[B][A]P]                  mkP = [P] b b

In context of Awelon language, it's convenient to encode a pair as `[[B][A]]`.

Between algebraic sums and products, we represent pretty much any data we might wish to model. The option type `(1 + A)` might be modeled as `[false]` or `[[A] R]`. Natural numbers can be encoded as `μNat.(1 + Nat)` - i.e. zero or successor. A list can be modeled as `μList.(1 + (A * List))`. Text can be encoded as a list of natural numbers. In that case, a list cons operation might be as simple as `Cons = mkP mkR`

An alternative is to model data as folding over its structure.

        Church Numbers:

        [X][F] 0 i == [X]
        [X][F] 1 i == [X] F         == [[X][F] 0 i] F
        [X][F] 2 i == [[X] F] F     == [[X][F] 1 i] F

        0 = [d i]
        1 = [0 Succ]
        2 = [1 Succ]
        Succ = [c] a [b b] b a i

This is essentially the Church encoding of natural numbers, adjusted for Awelon language. In this case we're folding over the recursive `μN.(1+N)` structure, so there is no other interesting data made available to `F`. But we can generalize easily to folds over list structures:

        [X][F] [[L][A]Cons] i == [[X][F]L] [A]  F
            Cons = [[c] a b [b] b a] a w i

However, specifying a deep fold over structure is awkward in the general case. With lists, we want both right folds and left folds. With tree structured data, we'll generally want multiple [tree traversals](https://en.wikipedia.org/wiki/Tree_traversal). For linear typed programming, it is most convenient if we can operate upon or [unzip](https://en.wikipedia.org/wiki/Zipper_%28data_structure%29) part of a structure without processing the whole thing. The algebraic representation may prove more convenient.

Return attention to natural numbers and texts in Awelon.

It is my intention that Awelon developers be given relatively free reign over the choice of encoding and tradeoffs. This is achieved as follows:

        0 is user definable (zero)
        S is user definable (succ)
        1 = [0 S]
        2 = [1 S]
        ...

        ~ is user definable (nil)
        : is user definable (cons)
        "" = ~
        "hello" = [104 "ello" :]
        "→" = [8594 ~ :]

In practice, the definitions will be influenced by runtime accelerators, as discussed later. But the formal representation of data is reasonably well separated from the syntactic layer.

## Coinductive Data

## Fixpoint

Fixpoint is a useful function for modeling loops. 

        [B][A]z == [B][[A]z]A 

        z = [[c]a[(/3)ci]bbwi](/3)ci




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

