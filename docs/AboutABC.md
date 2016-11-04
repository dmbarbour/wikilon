
## Static Type Safety for ABC




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
