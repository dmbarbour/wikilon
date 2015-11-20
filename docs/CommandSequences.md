
# Command Sequences

A sequence in a command language is necessarily a sequence of commands. Even `42` is a command. Fortunately, sequences of commands can construct tables and other data structures. For example, a sequence of three commands like `{1,2,3}` could easily be processed to construct a list of three numbers. 

[Command Language for Awelon (Claw)](CommandLine.md) provides a simple Forth-like syntactic sugar above AO bytecode. However, I believe that effective support (an expansion rule) for command sequences could greatly extend the claw experience: embedded tables and lists, conventional block-structured programming, algebraic effects, and a useful variety of DSLs. 

Relevant design goals:

* compact representations for lists, tables, large volumes of data
* effective support for DSLs, effects, block-structured programming
* transparent, comprehensible, uniformly composable, decomposable

By 'decomposable', I mean that I want trivial extraction of subprograms that are expressed as sub-sequences. Where we have `2,3` as a sub-sequence, we should be able to trivially extract this into a separate word as a subprogram. This may be semantic or syntactic. Ideally, both. Easy decomposition is valuable for refactoring and abstraction. By 'uniformly composable', I mean that I can build two sequences into a larger sequence with a short, universal string of bytecode. By 'transparent and comprehensible', I mean that users can easily internalize the sequence model and explain how it operates.

One promising idea is to leverage [free monads](http://okmij.org/ftp/Computation/free-monad.html). This would potentially enable us to compose commands without knowing much about the types of those commands.

## Uniform Command Sequences

One sequence semantics I've experimented with:

        {foo,bar,baz}   desugars to
        [\[foo] cmd \[bar] cmd \[baz] cmd] cmdseq

The commands are represented by individual blocks. The sequence is represented by a block that operates sequentially and uniformly upon each command. This has a nice property: composition of sequence blocks is equivalent to concatenation of sequences. The recommended definition is `cmd = \lw^z$`, which allows us to provide a command handler.

        handler  :: (command * st) → st
        sequence :: (st * (handler * 1)) → (st * (handler * 1))
        cmd      :: (command * (st * (handler * 1))) → (st * (handler * 1)))
        cmd = \lw^z$

For decompostion, we would need an additional sequence escape syntax for injection of arbitrary code into a sequence block. Potentially, this might be expressed by adding a `/` prefix to the command, for example:

        {/foobar, baz}  desugars to
        \[foobar \[baz] cmd] cmdseq

The motivation for sequence escapes is convenient syntactic abstraction, for example extracting subsequence `foo,bar` into a separate word `foobar`. Syntactic abstraction is an important property for Awelon project, though I encourage developers to favor composable semantic abstractions (such that `foo,bar` may be composed in a type-dependent way into a `foobar` command, no escapes required).

I don't like this. It's aesthetically unpleasant.

*Note:* `{}` is a sequence with one empty command. The empty command sequence is `{/}`.

## Non-Uniform Command Sequences 

Uniform command sequences have some weaknesses:

* we cannot easily join one sequence to another internally
* consequently, decomposition is syntactic, not semantic
* 

don't allow us to semantically decompose the sequence structure. We don't have any way to logically join one sequence to another from within. We cannot distinguish b

What I don't like about this semantics: 

* cannot semantically decompose; forced to use syntactic decomposition
* uniform structure, no ability to usefully discriminate command types









