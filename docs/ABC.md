# Awelon Bytecode

This document is a minimal summary of Awelon Bytecode (ABC). See [about ABC](AboutABC.md) for design details. This is a major revision from the prior ABC, towards a [minimalist](ABC_Minimalist.md) solution. 

## Primitives

Four primitive combinators `abcd`:

        [B][A]a     ==      A[B]            (apply)
        [B][A]b     ==      [[B]A]          (bind)
           [A]c     ==      [A][A]          (copy)
           [A]d     ==                      (drop)

A block is a subprogram delimited by square brackets `[]`. 

The `[abcd]` set is Turing complete and friendly for substructural types. ABC programs have a formal semantics as an expansion to a finite sequence of these six characters. ABC values always have a Church-encoding and semantics, though developers aren't forced to use them that way. 

## Data Embedding

Natural numbers may be embedded using the form `#42` or `#108`. Here `#` introduces a new zero value, while `1234567890` each have a 'multiply by ten, add digit' effect. 

Literals may be embedded as UTF-8 sequences:

        "start with character `"`
         may have multiple lines
         simple blacklist:
            C0 (except LF), DEL, C1
            surrogate codepoints
            replacement character
         LF gets special attention:
            LF SP   keep LF, drop SP
            LF LF   keep LF, continue
            LF ~    drop LF, terminate
         There are no other special chars.
        ~

*Aside:* While ABC does not support inline literals, some [editable views](CommandLine.md) will do so. For convenience in examples, a subprogram like `"hello"` is to be understood as an inline representation for the obvious literal of five characters, even though it would include a line break in ABC.

The formal semantics of natural numbers and literals is based on Church-encoded command sequences of NOPs or UTF-8 bytes respectively. ABC does not provide a convenient syntax for these sequences. However, assuming a presentation like `(foo,bar,baz)` for a sequence of three commands:

        #7          ==      (,,,,,,)
        #3          ==      (,,)
        #1          ==      ()
        #0          ==      #

        "hello"     ==      (#104, #101, #108, #108, #111)
        "→"         ==      (#226, #134, #146)
        "h"         ==      (#104)
        ""          ==      #

These sequences have the following behavioral semantics:
        
        [B][A](foo,bar,baz)i    ==      foo [[B][A](bar,baz)i] A
        [B][A](bar,baz)i        ==      bar [[B][A](baz)i] A
        [B][A](baz)i            ==      baz [[B][A]#i] A
        [B][A]#i                ==      B
        [A]i                    ==      A

This is a flexible sequence model, capable of finite iteration, short circuiting, and monadic programming depending on the types of the commands and `A`. It's also directly usable as a conditional behavior, e.g. `#` vs. `()` roughly correspond to `false` vs. `true`. The expansion semantics hasn't been fully worked out yet. 

        #               =   [di]
        i               =   [][]baad
        (foo,bar,baz)   =   [[foo](bar,baz)s]
        (baz)           =   [[baz]#s]

It's the definition of `s` that I haven't implemented quite yet.

## Formatting

ABC is designed to be *weakly legible*, though is intended for use together with editable views like [claw](CommandLine.md). To aide legibility, simple formatting characters - SP (32) and LF (10) - are freely permitted within ABC. These have identity semantics, equivalent to an empty program. 

Further, ABC will *commit to avoiding* certain characters, to simplify embedding of ABC in contexts like documentation, HTML, intelligent editors, editable views, or dictionary exports. In particular, the following characters shall be avoided at the ABC toplevel:

        <>@&:;,.=_`\(|)

These characters may be used within tokens and embedded literals, of course. Additionally, ABC shall never utilize a character that is forbidden from literal text (such as control characters). 

## Standard Accelerators

In addition to `[abcd]` primitives, ABC shall include a standard dictionary of accelerated operations aimed primarily at improving legibility, convenience, and performance. Each accelerator is formally defined by expansion into an `[abcd]` sequence, but in practice we'll tend to use hand-optimized code.

*Note:* The need for standard accelerators is greatly mitigated by use of [Awelon Object (AO)](AboutAO.md), which uses `{%word}` tokens for lightweight linking. A runtime can easily provide a standard dictionary of accelerated functions, e.g. `{%multiply@std2016}`. A consequence is that we may choose

For example, natural numbers are accelerators because embedding number data is important. The bulk of this document will be defining accelerators. Eventually. Choosing accelerators is a slow process with careful vetting. 

        Usage               Behavior            Definition
        (Natural Numbers)
        #                                       [di]                   
        0                                       {#10}*{#0}+
        1                                       {#10}*{#1}+
        2                                       {#10}*{#2}+
        ..
        9                                       {#10}*{#9}+



        (Tentative Accelerators)
        [A]i                A                   []wad
        [B][A]w             [A][B]              ua
        [A]u                [[A]]               []b
        [B][A]o             [B A]               
        [B][A]w             [A][B]              []ba

        (Intermediate Definitions)
        {#0}                                    #
        {#1}                                    #{S}
        {#2}                                    #{S}{S}
        ..
        {#9}                                    #{S}{S}{S}{S}{S}{S}{S}{S}{S}
        {#10}                                   #{S}{S}{S}{S}{S}{S}{S}{S}{S}{S}
        {S}                                     []{w}[{s}]bb


## Tokens

Tokens have the form of short text between curly braces, e.g. `{foo}`. Like accelerators, tokens must have a pure, formal semantics as a finite `[abcd]` expansion. 

Most tokens have *identity semantics*, i.e. removing them from the code does not impact its formal behavior. Tokens with identity semantics can broadly support performance, safety, security, debugging, and rendering. For example, `[A]{&jit}` is formally no different from `[A]`, but would tell a runtime to force just-in-time compilation.

Remaining tokens have *linking semantics*, inlining a named subprogram. For example, token `{%foo}` will formally inline the definition of word `foo` in place of the token. In a distributed system, linking might use secure hashes. Linking in ABC systems must always be acyclic.

Structurally, tokens are constrained more severely than embedded literals: they're limited to 64 bytes UTF-8 (including the curly braces) and may not contain LF or curly braces internally. 

For more information on tokens and the abundant idioms surrounding them, see the primary [ABC document](AboutABC.md).

*Aside:* Based on experimentation, tokens should not be used to introduce symbolic data types such as variant tags or record fields. Introducing a massive set of symbols creates a need for sophisticated metaprogramming. Instead, accelerators, numbers, and literals should be used for structured data, with some annotations to assert general structure.

## ABC CHANGE LOG

September 2016: 
* revision to [minimalist ABC](ABC_Minimalist.md)
* old content now deprecated and deleted
* tokens texts limited to 62 bytes UTF-8
