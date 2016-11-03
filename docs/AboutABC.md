
## Static Type Safety for ABC

ABC's behavior does not depend on any type judgements - no type driven dispatch or overloading. Hence, ABC may be evaluated without static typing. However, static analysis of type safety can nonetheless offer significant benefits:

* a clean, robust, trustworthy codebase
* type-sensitive projectional editors
* verifiable documentation, informed programmers
* typeful rendering and program extraction
* JIT compilation without dynamic type checks

It is my intention that most ABC codebases be strongly, statically type safe as the default. Explicit bypass of the type system will be supported, cf. dynamic evaluation below.

### A Simple Static Type System

ABC programs can be understood as pure functions operating upon a stack value. For convenience, we can align type descriptions with program stack structure:

        a   : S A (S → S') → S' A
        b   : S A (E A → E') → S (E → E')
        c   : S A → S A A
        d   : S A → S
        [F] : S → S type(F)

Each type list `S B A` is left associative, representing a product type `((S * B) * A)` with the 'top' of our stack on the right so we align with a program of form `S [B] [A]`. The leftmost type `S` effectively represents the remainder of the stack. In ABC, all other values are Church encoded and thus have function types. So in `S B A` the types `B` and `A` must be functions.

These types for ABC's primitives together with simple techniques like type unification, can be leveraged for inference of much larger programs. However, there isn't much opportunity to detect inconsistencies from ABC primitives.

### Type Annotations and Declarations

Tokens provide convenient anchors for static type inference. For example:

* `{&nat}` - argument is embeddable as natural number
* `{&lit}` - argument is embeddable as text literal
* `{&bool}` - argument is a boolean value
* `{&t3}` - argument has form `[[A][B][C]]`
* `{&aff}` - argument may not be copied
* `{&rel}` - argument may not be dropped
* `{:foo}` - wrapped types, unwrap with `{.foo}`

Given these tokens, we can detect many inconsistencies statically or at runtime. For example, it is obvious that `[A]{&aff}c` is an error, as is `[A]{.foo}` or `#42{&lit}` or `[[A][B]]{&t3}`. However, tokens tend to be limited in their expressiveness, oriented on things that are easy to check at runtime.

To support rich, human meaningful type documentation, we should additionally use the [AO layer](AboutAO.md) to attribute type information to subprograms. For example, `word.type` may declare the type of `word` in a manner meaningful both to a type checker and a human. Type descriptions, in this case, would be first-class computable values, and may prove more flexible or extensible 

In any case, the presence of type annotations and declarations can provide a basis for detecting inconsistencies in the ABC program.

### Conditional Behavior

A major concern for any programming language and type system is conditional behavior. Most languages have a dedicated syntax and semantics, e.g. the `if then else` expression. In ABC, our conditional behaviors will instead be Church encoded. For example:

        type Bool = True | False
        [onTrue][onFalse] true  i => onTrue
        [onTrue][onFalse] false i => onFalse
        true  = [di]   
        false = [ad]

In practice, of course, computation of a boolean condition is frequently separated from expression of the `[onTrue][onFalse]` conditional paths. This makes it difficult to locally detect type errors or inconsistencies between `onTrue` or `onFalse` contingent on unification of environment. To detect type errors early, we need to inform our type system that our conditional paths will be evaluated in context of a boolean. We can leverage annotations in this role. For example:

        {&bool}[[onTrue][onFalse]]ai

This subprogram expresses that `[onTrue][onFalse]` will be applied in a context with a boolean at the top of the stack. Our static type checker knows about booleans, and thus can determine the types of `onTrue` and `onFalse` should unify in useful ways, enabling local detection of inconsistent type errors.

I would also like effective support for option and sum values:

        type Opt A = Some A | None
        [onSome][onNone] [A] some i     => [A] onSome
        [onSome][onNone]     none i     =>     onNone

        type Sum A B = Left A | Right B
        [onLeft][onRight] [A] left i    => [A] onLeft
        [onLeft][onRight] [B] right i   => [B] onRight

        left    = [wdwi]b
        right   = [wbad]b
        some    = left
        none    = false

Support for `{&bool}`, `{&opt}`, and `{&sum}` should be sufficient in practice. Generalizing to more than two paths isn't critical.

*Aside:* Conveniently, `{&bool}[[onTrue][onFalse]]ai` is structurally, semantically, and visually similar to an `if then else` expression. It is feasible to provide a more conventional if-then-else view through an editor.


### Value Sealing for Lightweight Types

ABC introduces two tokens `{:foo}` and `{.foo}` to support symbolic value sealing, to resist accidental access to data. This is supported by a simple rewrite rule: `{:foo}{.foo}` will rewrite to the empty program. The main goal is to force programs to fail fast if they aren't expected to access a particular data structure. In most cases, seals will be used as symbolic wrappers for individual values, e.g. using `[{:foo}]b`. During static type analysis, these type wrappers should serve as a useful type constraint.

### Dynamic Evaluation

Many useful programming styles are difficult to statically typecheck. Example:

        "abcx → ax^2 + bx + c" runPoly

Assume this constructs a program that takes four arguments - a, b, c, x - then computes the specified polynomial. The number of arguments we take depends on polynomial's text value. Providing a static type judgement for `runPoly` is feasible but non-trivial, generally requiring sophisticated dependent types and proofs.

Similar scenarios exist for print formatting and DSLs.

Fortunately, we do not need sophisticated types. The polynomial text is right there. Statically. So, even if we cannot easily provide a static type for `runPoly`, we might be able to provide a simple type for the larger program after we partially evaluate `runPoly`. We only need some means to defer static type analysis until after some partial evaluation. 

For ABC, I propose a `{&dyn}` annotation. Usage:

        "abcx → ax^2 + bx + c" runPoly =>
            "abcx → ax^2 + bx + c" compilePoly {&dyn} i =>
            [polynomial behavior]{&dyn} i =>
            [polynomial behavior]i

At this point we may halt evaluation and pass the program to our static type checker. Dynamic evaluation is considered complete after the `{&dyn}` annotations are eliminated (via `[A]{&dyn} => [A]`).

If a program contains `{&dyn}` annotations, we might call that program "dynamic". Developers are free to construct dynamic software. Where dynamic partial evaluation can complete statically, it can readily be used for macro-like metaprogramming. An ABC software system may consist of an ad-hoc mix of dynamic and static - fluid and rigid - software components.

## Miscellaneous

### Runtime Errors

While static type checking is optimal, runtime type errors are possible with `{&dyn}` or if we do not bother to type check. In addition, developers may perform dynamic assertions, or express partial functions, in ways that a type checker cannot readily handle.

To simplify error reporting and debugging, we'll want to record known errors in the generated program. To accomplish this, we can use an `{&error}` annotation around a bad subprogram. An evaluator can freely inject the error annotation, delimiting the bad code:

        #42{&lit}       =>  [#42{&lit}]{&error}i
        [A]{&aff}c      =>  [[A]{&aff}c]{&error}i
        {:s}d           =>  [[A]{:s}d]{&error}i
        [[A]]{&t2}      =>  [[A]]{&t2}{&error}

Developers may freely specify their own error values:

        "TODO: fix foo yesterday!"{&error}

Use of `{&error}` marks a *value* as erroneous. Observing that value, e.g. with `[A]{&error}i`, results in a stuck computation. Being stuck on an error will not generate further errors. A type checker may treat an error value as having any type.

### Comments

I generally recommend documentation be pushed represented as associative metadata at the AO layer via words like `foo.doc`, `foo.talk`, `foo.about`, `foo.author`, or `foo.example`. But it is not difficult to model comments in ABC. For example:

        "this is my boomstick!
        ~{&a2}{@rem}d

The arity annotation `{&a2}` and gate `{@rem}` make this ABC comment more useful and accessible compared to comments are in most programming languages. The gate enables logging of comments, conditional breakpoints on comments, etc.. The arity annotation resists premature processing and elimination of the comment, and enables runtime construction of commented values or programs.

### Gates for Active Debugging

Active debugging describes techniques that provide a view of a computation in progress: breakpoints, logging, animation, tracing, profiling.

* **breakpoints:** provide a frozen view of a computation in progress
* **logging:** provides a local view of a subprogram's dataflow
* **animation:** global logging via breakpoints, big space overhead
* **tracing:** prefix argument with comment to help trace data flows 
* **profiling:** view time/effort spent on specified subcomputations

I propose tokens of form `{@gate}` - called 'gates' - for active debugging. The symbol is user-defined. A gate operates on a single argument, e.g. `[A]{@foo}`. The behavior of a gate is configured at the evaluator. For example: 

* open gates just pass the argument (not debugging)
* closed gates do not evaluate further (breakpoint)
* logging gates record argument to configured log
* trace gates inject a comment into their argument
* profiling gates aggregate performance statistics

Gates may have de-facto standard configurations, e.g. with `{@log:xyzzy}` defaulting to log behavior. It is possible that a system could automatically inject gates for debugging purposes. 

#### Program Animation

Using breakpoints and a systematic animation strategy, we can animate a program's evaluation:

* When evaluation stalls on breakpoints, take a snapshot.
* Delete breakpoints as determined by animation strategy.
* Repeat until no active breakpoint gates are discovered.

This has a lot of benefits for debugging. We have the whole 'spatial' context of breakpoints, but we also preserve the 'temporal' context of logging. Though, logging and profiling can also be used with program animation, e.g. by recording a set of log messages and profiling statistics per frame.

An animation strategy can be specified many ways:

* always delete rightmost breakpoint (or leftmost)
* delete all current breakpoints to maximize frame size
* focus on breakpoints of given names, hierarchically
* interleave breakpoints of different names
* pseudo-random selection

Animating on breakpoint is much nicer than animating on quota. Individual frames will be deterministic, modulo profiling data. The structure is much more predictable, which simplifies both compression and stable rendering. The animation strategy enables evaluation to be precisely controlled and focused to just the parts we're interested in observing or rendering.

### Testing

For testing purposes, it is frequently useful to assert that two values match.

I'd rather avoid this sort of ad-hoc reflection at runtime. But it could be supported at the AO layer, e.g. by specifying test objects with expected and actual results. If necessary, however, we could introduce some annotations for the role:

* `{&eqv}` - assert two values are structurally equivalent
* `{&beqv}` - assert two values are behaviorally equivalent

Behavioral equivalence might be tested by some ad-hoc combination of static analysis or fuzz testing.

### Garbage Data

Similar to the `{&error}` annotation for error values, developers might want to recycle memory early while keeping the placeholder (especially if that placeholder might have substructural types or associated debug traces). This could be done by introducing annotation `{&trash}` which would replace the data with an error value but preserve substructure and debug data. 

### Logical Assertions?

Weaker than behavioral equivalence. Some way to say `∀X. F(X) → G(X)`. Some way to say `∃X. F(X)`. The quantification logics used by Alonzo Church seem relevant. `[F][G]{&imply}`, for example? 
