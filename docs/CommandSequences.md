
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
* efficiency; tail calls, simplification, static partial evaluation
* iterative and incremental processing of the command sequence

Being *generic* is especially important for a syntactic sugar like claw. The type of a command is inaccessible at this layer. And easy factoring allows sequences to fit nicely with Awelon project, e.g. with streaming programs and abstraction of stream fragments. 

Some ideas:

1. uniform commands sequences: commands as blocks; sequence representation.
1. leverage [free monads](http://okmij.org/ftp/Computation/free-monad.html).
1. leverage [delimited continuations](https://en.wikipedia.org/wiki/Delimited_continuation). 
1. leverage [machines (networked stream transducers)](https://dl.dropboxusercontent.com/u/4588997/Machines.pdf)
1. leverage [arrows](https://en.wikipedia.org/wiki/Arrow_(computer_science))

I would prefer to not entangle my sequence structure with any particular effects model, i.e. the *sequence structure* should be independent of our *command type*.

## (rejected!) Uniform Command Sequences

Consider a design where `{foo,bar,baz}` becomes a sequence of blocks, e.g. `[foo]`, `[bar]`, and `[baz]` contained in some sequence representation like a list, stream, or larger block. This model is generic, transparent, comprehensible. Depending on the sequence representation, iterative processing and composition of sequences is feasible. However, this uniform representation has significant disadvantages for a other desiderata: 

* cannot extract a sub-sequence like `{foo,bar}` into a command.
* cannot simplify command sequences in any meaningful way.

## Regarding Generic Non-Uniform Commands

Claw is a syntactic sugar and editable view. Therefore:

* syntax must expand trivially to AO bytecode
* discrimination on commands must be syntactic

By convention, claw expansions favor words like `{%int}` and `{%ratio}`. Words are easy to recognize when computing our editable view. Further, words enable experimentation with alternative environments via namespaces. I will certainly continue with this convention. So we might consider something like `[foo]{%return}` vs. `[bar]{%action}`. 

Any syntactic discrimination will probably involve alternatives to commas. Fortunately, we have many characters we could use in this role: `,;|<>=`. We can also express compositions thereof, like `<*>` and `>>`.

## Free Monads

The notion of a 'command sequence' is pretty well captured by 'monads'. Many DSLs are easily expressed with monads. We achieve composition and decomposition via our monad laws. A simple list of numbers could be understood as a writer monad. However, there are issues with shoving Haskell monads into claw:

* sequence composition operators (`return`, `>>=`, and `>>`) are type specific
* humans find the monad abstraction relatively difficult to comprehend
* Haskell relies heavily on data-plumbing of results through variable names

The first two issues might be addressed by [free and freer monads](http://okmij.org/ftp/Computation/free-monad.html). We can develop a common, simple data structure that supports all necessary compositions. Human programmers then need comprehend only one concrete operational semantics. 

The last issue is addressable by fitting commands to AO's style: every command receives and returns our entire program environment. Instead of `action ()`, the simplest AO commands have identity type, `∀a. a → action a`. A proposed command language syntax for command sequences:

        {foo;bar;baz;qux}

This would expand to a Haskell equivalent of `foo >=> bar >=> baz >=> (return . qux)`. To fit AO's style, we're favoring Kleisli composition. Every effectful command is followed by a semicolon. Operationally, we might comprehend the semicolon as *yielding* to an external effects handler, from otherwise *pure* code. The space after the final semicolon, thus, is simply pure code. The empty command sequence `{}` corresponds to Haskell's `return`. 

What is a viable syntactic expansion for this sequence? We'll need to ensure that all our monadic composition properties are available, and that we can iteratively process each command. A first attempt:

        [foo \[bar \[baz \[qux rtn] cmd] cmd] cmd]

The command-sequence is a first-class block of type `(a → F m b)`. Here `m` is the effects model we handle, and `F` is our free monad wrapper. Our free monad wrapper essentially has two constructors: `cmd` and `rtn`. Each `cmd` receives an environment wrapped for our effects interpreter, together with a continuation - a following command sequence. 

In Haskell terms:

        data F m a where
            Rtn :: a → F m a
            Cmd :: m x → (x → F m a) → F m a

This naive representation corresponds directly to Oleg's initial construction of the Freer Monad. Oleg goes on to develop a more 'extensible' and 'optimal' variation of freer monads in his 2015 paper. I'd like to see if there is an easy adaptation of these Freer monads to claw, without complicating syntax. 

## Lightweight Sequence Punctuation

An option for lightweight sequences is to simply expand select punctuation into words. For example, if `,` expands to the word `comma` but is treated as whitespace (for separating commands), we gain the ability to express lightweight vectors as `[1,2,3]` and matrices as `[[1,2,3],[4,5,6],[7,8,9]]`. This doesn't generalize nicely, but it is probably sufficient for some specialized use-cases.

Between `[1,2,3]` and `{1;2;3;}`, we're only saving one character. Any potential benefit is aesthetic. This might be enough to motivate support for lightweight sequences. But it might be better to wait for a bit and see whether people are satisfied with the monadic sequences.
