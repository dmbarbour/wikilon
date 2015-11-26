
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

AO doesn't do infix notation. However, a claw view can support infix notations within a clear, delimited region. 

Blocks provide clear, delimited regions. We can potentially model sequences blocks by having a command separator 'escape' each block and recombine it. Of course, this needs to work transparently with the existing sugar around blocks. 

        [foo eff, bar ret]  desugars to
        \[foo eff] \[bar ret] bseq block

For almost any use-case (including monadic blocs), I'll want a right-associative structure. This is favorable for iterative processing, the ability to apply each block and discard it in the order provided by the programmer. We achieve this by sequencing from right to left. 

        [foo, bar, baz, qux] desugars to
        \[foo] \[bar] \[baz] \[qux] bseq bseq bseq block

With block-based sequences, we're forced to compose every pair of blocks into a larger block with each `bseq` step. There is no discriminating the first block, nor the last one. We must assume that the final block in the sequence has the right shape, the same 'kind' as our overall result. We cannot compute a list of blocks.

With monadic command sequences, we assume every command, first and last, describes a valid command sequence (effect, return, or bind). So, there is no need to discriminate. Also, a block is a natural way to describe monadic command sequences in AO because of how we use program environments for data plumbing. So, the above limits on expressiveness don't hurt for this use case.

*Aside:* Those `bseq bseq bseq` sequences will compress very easily, assuming definitions large enough to need it. I'm not particularly concerned about the overhead in the final AO encoding.

### Use Case: Concise List Construction

One of the motivating use-cases for command sequences is (syntactically) concise construction of data sequences (lists, streams, vectors, matrices, etc.). Assume we've committed to block-delimited sequences. Can we take `[1,2,3]` and turn it into a list of numbers? Or `[[1,2,3],[4,5,6],[7,8,9]]` into a matrix?

We can. Though, it may require a specialized namespace. 

Assume command sequences of type `cc → (datum * cc)`. Here, `cc` is a list of continuations. Our `bseq` behavior pushes a continuation onto this list first thing (before computing the datum). Every time we yield, our interpreter takes the `datum` value and shares it with a consumer then decides whether it can continue.

We generate a *stream* of numbers, blocks, or other data. This is an efficient, composable model for streams. We can easily abstract and inline sub-sequences (contributing continuations to `cc`). We can easily translate a stream to a list or other data structure. A matrix would require hierarchical stream processing.

This syntax is concise, composable, factorable, and friendly. 

Block-delimited sequences are at least as versatile as I hoped.

## Rejected Ideas

### (rejected) Multiple Command Separators

It would be easy to extend our syntactic sugar with support for multiple command separators. Consider:

        [foo , bar ; baz | qux] desugars to
        \[foo] \[bar] \[baz] \[qux] vertibar semicolon comma block

It is further feasible to support a simple precedence model, e.g. such that we sequence commas before semicolons before vertibars. Doing so greatly improves the potential utility of multiple separators by allowing them to apply to *groups* of commands. Aligning precedence with how we're likely to read the text as humans also improves the aesthetic, IMO.

        Precedence: smallest to largest ,;|

        [foo , bar ; baz | qux] desugars to
        \[foo] \[bar] comma \[baz] semicolon \[qux] vertibar

        [a,b,c;d,e,f;g,h,i|j,k,l;m,n,o;p,q,r|s,t,u;v,w,x;y,z]  desugars to

        \[a] \[b] \[c] comma comma
        \[d] \[e] \[f] comma comma
        \[g] \[h] \[i] comma comma
        semicolon semicolon
        \[j] \[k] \[l] comma comma
        \[m] \[n] \[o] comma comma
        \[p] \[q] \[r] comma comma
        semicolon semicolon
        \[s] \[t] \[u] comma comma
        \[v] \[w] \[x] comma comma
        \[y] \[z] comma
        semicolon semicolon
        vertibar vertibar
        block

*Aside:* I'd probably call these `cseq` `cseqs` and `cseqv` rather than specifying their visual representation. 

The *potential* motivation for multiple command separators is that they permit alternative compositions and interpretations of commands. For example, we could model alternatives or backtracking directly in our syntax. We can do this without special syntax, but it might be prettier with supporting syntax.

However, working with a single sequencing model is simpler. Our continuations handle one kind of bind. We make fewer assumptions about our interpreter. We don't gain any expressiveness. At best, we're flattening our code a bit and making it a little easier to read. Further, precedence hinders factoring. For example, if I extract the subprogram `q,r|s,t,u;v` and replace it by a word `qrstuv`, the behavior will be different because of of precedence. And this isn't an obvious error like refactoring across blocks (`foo] \[bar` is clearly not a valid definition).


### (rejected) Curly Brace Command Sequences

For greater expressiveness, we could instead favor a different region delimiter. This would allow construction of a list of blocks. For example:

        {foo, bar, baz, qux} desugars to
        \[foo] \[bar] \[baz] \[qux] cblock cseq cseq cseq

Does this offer any meaningful advantage? 

Stuffing commands into a list doesn't offer any processing advantages because commands are still opaque. Mostly, this allows treating the last command in a special way, e.g. we might interpret the above program as `foo >=> bar >=> baz >=> (return . qux)`. But special cases in our syntax will hinder composition and factoring, and injecting a `return` is easy to do by hand: `[foo, bar, baz, qux return]`.

There is an opportunity cost for using curly braces here. I might later discover a more effective use for them elsewhere, perhaps delimiting regional DSLs or a specialized view for data list construction. I'd prefer to reserve them for a more strongly motivated use-case, one that aligns with Awelon project goals. 
