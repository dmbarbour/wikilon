
# Command Sequences

A sequence in a command language is necessarily a sequence of commands. Even `42` is a command. Fortunately, sequences of commands can construct tables and other data structures. For example, a sequence of three commands like `{1,2,3}` could easily be processed to construct a list of three numbers. 

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

Anyhow, we'll end up with sequences modeled as functions, and first-class sequences as blocks.

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

## Simple Sequences of Blocks

AO doesn't do infix notation, but a claw view could support some simple infix within a clear, delimited region.

Blocks provide a clear, delimited region of code. We can feasibly model sequences with normal blocks by having a command separator 'escape' each block and recombine it. 

        [foo eff, bar ret]  desugars to
        \[foo eff] \[bar ret] bseq block

For constructing monadic command sequences, I'll want a right-associative structure. This is favorable for iterative processing. We achieve this by sequencing from right to left. A long string of `bseq bseq bseq` should compress nicely if we're writing very large programs.

        [foo, bar, baz, qux] desugars to
        \[foo] \[bar] \[baz] \[qux] bseq bseq bseq block

There is no discrimination on the first or last block. We're effectively forced to have a block after each step. This limits what we can express. Fortunately, these limits coincide nicely with our requirements for monadic command sequences.

If we need greater expressiveness, we could instead start with alternative delimiters. For example:

        {foo, bar, baz, qux} desugars to
        \[foo] \[bar] \[baz] \[qux] cmd cseq cseq cseq  

However, I'm interested in seeing how far we can get with sequencing blocks.
