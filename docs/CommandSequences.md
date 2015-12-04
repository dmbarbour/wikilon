
# Command Sequences

A sequence in a command language is necessarily a sequence of commands. Even `42` is a command. Fortunately, sequences of commands can construct tables and other data structures. For example, a sequence of three commands like `[1,2,3]` could easily be processed to construct a list of three numbers. 

[Command Language for Awelon (Claw)](CommandLine.md) provides a simple Forth-like syntactic sugar above AO bytecode. However, I believe that effective support (an expansion rule) for command sequences could greatly extend the claw experience: embedded tables and lists, conventional block-structured programming, algebraic effects, and a useful variety of DSLs. 

Desiderata:

* compact representations for lists, tables, large volumes of data
* aesthetically and ergonomically pleasant syntax, easy to read and write 
* effective support for DSLs, effects, block-structured programming
* transparent and comprehensible: humans grasp operational semantics
* generic: sequence structure is independent of command or effect type
* easily factored: extract sub-sequences into separate words or methods
* efficiency; iterative, tail calls, simplification, partial evaluation

Easy factoring is essential for Awelon project's goals: given any subprogram (including a sub-sequence) we should be able to extract and abstract it. Associativity, identity element, composition, and parameterization are important properties for easy factoring. Ideally, we don't make a strong distinction between a single command vs. a sequence of commands. This allows us to refactor and abstract any sequence of commands into a single parameterized command.

Being *generic* is especially important for a syntactic sugar like claw. The type of a command is generally inaccessible until we get around to evaluating it. We cannot allow our sequence structure (representation, composition, associativity) to depend on the command type. Though, we can try to set things up for effective partial evaluation.

Monadic models support most of the desiderata. Free monads, in particular, support the generic structure independent of the command type. If we pick a specific free monad, we gain a consistent operational semantics that humans can grasp. My best plan, at this time, is to model some specific free monads then develop a simple syntactic sugar sufficient to construct and compose them from the claw view. 

## Free Monads

The notion of a 'command sequence' is pretty well captured by 'monads'. Many DSLs are easily expressed with monads. We achieve composition and decomposition via our monad laws. A simple list of numbers could be understood as a writer monad. However, there are issues with shoving Haskell monads into claw:

* sequence composition operators (`return`, `>>=`, and `>>`) are type specific
* humans find the monad abstraction relatively difficult to comprehend
* Haskell relies heavily on data-plumbing of results through variable names

The first two issues might be addressed by [free and freer monads](http://okmij.org/ftp/Computation/free-monad.html). We can develop a common, simple data structure that supports all necessary compositions. Human programmers then need comprehend only one concrete operational semantics. The last issue is addressable by matching AO's style. Instead of precisely capturing just the variables we need, we pass the entire program environment (e.g. the stack-hand model) from command to command.

Between these considerations, our commands and sequences have general shape `a → F m b`. Type `m` is our effects model. Type `F` is a good free monad. Types `a` and `b` model the program environment. Because our sequences are modeled by functions, Kleisli composition is a good match for sequence composition: `(a → F m b) → (b → F m c) → (a → F m c)`. 

Oleg has already done a lot of useful work with respect to modeling free monads. With a little hand-waving, I believe we'll end up with something like the following:

        Oleg's Model

        data F m a where
            Eff :: m a → Q m a b → F m b
            Val :: a → F m a
        data Q m a b where
            Leaf :: (a → F m b) → Q m a b
            Node :: Q m a b → Q m b c → Q m a c

        eta :: m a → F m a
        eta = \[Val] Leaf Eff

        foo :: a → m b
        foo eta :: a → F m b

        return :: a → F m a
        return = Val

        bar :: b → c
        bar return :: b → F m c

        seq :: (a → F m b) → (b → F m c) → (a → F m c)
        seq = (???) translation work to do here


There are simpler models. The following model has similar performance properties as tucking a queue behind Eff. But it doesn't structurally enforce that an interpreter fully construct Val before returning. This could be a useful feature, e.g. giving us some ability to model iterative pure computations and cooperative threading.

        data F m a where
            Eff :: m a → F m a
            Val :: a → F m a
            Bind :: F m x → (x → F m a) → F m a

Regardless of which model we use, we have sequences modeled as functions, and first-class sequences as blocks.

        [foo eff]                   :: (a → F m b)
        [bar pure]                  :: (b → F m c)
        [foo eff] [bar pure] seq    :: (a → F m c)

            where   eff  :: m a → F m a
                    pure :: a → F m a
                    seq  :: (a → F m b) → (b → F m c) → (a → F m c)
                    foo  :: a → m b
                    bar  :: b → c

We could probably include `eff` directly in the definition of `foo`. We generally know in advance that command words are commands. Pure data plumbing like `bar` doesn't have this advantage, so we'll occasionally need a `bar pure` (or `bar return`). The `seq` is where we benefit from syntactic sugar.

Ultimately, we'll have an interpreter that *runs* the sequence in small steps. The sequence returns to the interpreter for each `Eff`, or with a final `Val` when finished. In case of `Eff`, we also receive a continuation for where to push the updated environment. 

### Modeling Effects via Shared Program Environments

The default approach for modeling a GADT like `Val | Bind | Eff` in AO is to use a sum type, e.g. `(Ret + (Bind + (Eff + 0)))`. However, this is not the only option. Arguably, it is not even a very nice option, at least for IO effects:

* explicit `eff` vs. `pure` becomes semantic noise
* yielding for fine-grained effects is inefficient
* requires overly synchronous effects or polling
* invisible resource management, e.g. sockets, handles

A simple alternative is to model shared memory between our program environment and interpreter. 

For example, we could model an outbox and inbox for messages. Whenever we yield to our interpreter, it can drain the outbox and push some messages to the inbox. Our program could voluntarily examine the inbox, or we could model a triggered computation to process incoming messages. We can model a *publish-subscribe arena*. Whenever we yield to our interpreter, it could update the subscriptions and track updates to published data. We can model cooperative threads, background computations, [communicating machines](NetworkModel.md), and more.

The normal concurrency problems of shared memory still exist. However, they are greatly mitigated greatly because of the cooperative threading. Our program owns the memory until it yields to the interpreter. Then the interpreter owns it until it  it is active, and the interpreter owns the shared memory in between. Updates are atomic between explicitly yielding.

Our *current continuation* would also be modeled in this space, e.g. as a simple list of continuations. The `Bind` behavior will simply push a continuation onto this list. We can seal the continuation if we want to hide it from our program.

With this design, we simply yield our plain old program environment. 

* no syntatic or semantic noise from `eff` vs. `pure`
* yield less frequently, efficient batching of effects
* asynchronous by default, response upon future yield
* visible resource management: inbox, outbox, etc.

In general, only a fraction of our program environment should be shared this way. The rest of the environment is a private space for the program.

## The Sugar: Block-Delimited Sequences

AO doesn't do infix notation. However, a claw view can support infix notations within a clear, delimited region. Blocks provide clear, delimited regions. We can potentially model sequences blocks by having a command separator 'escape' each block and recombine it. Of course, this needs to work transparently with the existing sugar around blocks. 

        [foo, bar, baz, qux] desugars to:
        [foo \[bar \[baz \[qux] seq] seq] seq]

This is a right-associative binding. The continuation is hidden until just before we return. The continuation could easily be shoved into the program environment or returned to our interpreter. It isn't difficult to recognize that a block ends with `\[...] seq]`, so the desugaring is reversible in a straightforward way.

### Reviewing Use Case: Data Sequences

One of the motivating use-cases for command sequences is (syntactically) concise construction of data sequences (lists, streams, vectors, matrices, etc.). Assume we've committed to block-delimited sequences. Can we take `[1,2,3]` and turn it easily into a list of numbers? 

We can.

The program `[1,2,3]` desugars to `[1 \[2 \[3] seq] seq]`. Assume a program environment containing a data stack and a queue of continuations. Our `seq` function simply pushes the continuation onto the list. After each step, the interpreter pops one datum off the data stack, takes the entire queue of continuations, provides a fresh environment to the next step, and continues. Thus, our comma separated sequence computes a *stream* of data. Converting our (known finite) stream into a list is trivial. 

Further, this model of streaming data is *compositional*. We could rewrite `[1,2,3,4,5,6]` as `[1, [2,3,4] inline, 5,6]` or as `[1,2,3] [4,5,6] compseq`. We can abstract sequence fragments into separate words. We can also abstract ad-hoc stream generators.

We have a concise, composable, factorable, and friendly syntax for data sequences.
