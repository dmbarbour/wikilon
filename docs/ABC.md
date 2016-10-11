# Awelon Bytecode

This document is a minimal summary of Awelon Bytecode (ABC). See [about ABC](AboutABC.md) for design details. 

## Primitives

Four primitive combinators `abcd`:

        [B][A]a     ==      A[B]            (apply)
        [B][A]b     ==      [[B]A]          (bind)
           [A]c     ==      [A][A]          (copy)
           [A]d     ==                      (drop)

A block is a subprogram delimited by square brackets `[]`. 

The `[abcd]` set is Turing complete and friendly for substructural types. ABC programs have a formal semantics as an expansion to a finite sequence of these six characters. ABC values always have a Church-encoding and semantics, though developers aren't forced to use them that way. 

## Tokens

Tokens have the form of short text between curly braces, e.g. `{foo}`. Like accelerators, tokens must have a pure local rewriting semantics. 

Most tokens have *identity semantics*, such that removing them from the code does not impact behavior of a correct program. Tokens with identity semantics can broadly support performance, safety, security, debugging, and rendering. For example, `[A]{&jit}` is formally no different from `[A]`, but would tell a runtime to perform just-in-time compilation.

Other tokens have *linking semantics*, which rewrite to more bytecode. ABC's primary link model is [Awelon Object (AO)](AboutAO.md). A link token `{%foo}` will rewrite to the definition of word `foo` from an implicit dictionary. Link dependencies in ABC must be acyclic, such that we could link everything up front without changing behavior of a program.

## Bytecode Extension

ABC is extensible in context of its link layer. In particular, we will recognize operators like `xyz` as a desugaring to `{%x}{%y}{%z}`. This effectively enables developers to define their own extensions to ABC.

## Data Embedding

Natural numbers will be embedded using the form `#42` or `#108`. Here `#` introduces a new zero value, while `1234567890` each have a 'multiply by ten, add digit' effect. These aren't primitives, but rather are eleven operations defined via extension.

Text data will be embedded as a valid UTF-8 byte sequence:

        "start with character `"`
         may have multiple lines
         simple blacklist:
            C0 (except LF), DEL, C1
            replacement character
         LF gets special attention:
            LF SP   keep LF, drop SP
            LF LF   keep LF, continue
            LF ~    drop LF, terminate
         There are no other special chars.
        ~

The desugaring semantics for text data has not been decided, but the most likely possibilities is `"hello"` desugaring to `[[#104] "ello" s]`, enabling a user-defined sequencing operator `s`. I would like to unify numbers, texts, and [claw](CommandLine.md) command sequences with a simple model of coroutines or iterators.

## Accelerated Operations and Representations

ABC achieves performance via its extension model. An interpreter is expected to optimize for a de-facto standard AO dictionary from which most users will derive new dictionaries. 

For example, our optimized dictionary might include operators `#1234567890` for representing numbers, and more operators or built-in words for arithmetic. By optimizing for common representations of numbers and arithmetic, a runtime could use a lightweight machine word under the hood for those operations or any code defined in terms thereof. This would only require a quick verification that we're using the accelerated versions.

The same idea can be applied to accelerating linear algebra and matrix representation to leverage GPGPU, or accelerating Kahn Process Networks to leverage cloud computing. These will be areas to explore.

*Aside:* Avoiding global standardization allows for a lot more ad-hoc experimentation and deprecation in the effort to support high performance computing from a language with just four operators.

## ABC CHANGE LOG

October 2016:
* reject global standard extensions to ABC
* extension and acceleration via AO layer

September 2016: 
* revision to minimalist ABC (`[abcd]`)
* old content now deprecated and deleted
* tokens texts limited to 62 bytes UTF-8

