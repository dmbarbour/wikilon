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

Tokens have the form `{foo}` - a short text between curly braces. Syntactically, tokens cannot 
, e.g. `{foo}`. Like accelerators, tokens must have a pure local rewriting semantics. 

Most tokens have *identity semantics*, such that removing them from the code does not impact behavior of a correct program. Tokens with identity semantics can broadly support performance, safety, security, debugging, and rendering. For example, `[A]{&jit}` is formally no different from `[A]`, but would tell a runtime to perform just-in-time compilation.

Other tokens have *linking semantics*, which rewrite to more bytecode. ABC's primary link model is [Awelon Object (AO)](AboutAO.md). A link token `{%foo}` will rewrite to the definition of word `foo` from an implicit dictionary. Link dependencies in ABC must be acyclic, such that we could link everything up front without changing behavior of a program.

## Bytecode Extension

ABC is extensible in via the link layer. In context of AO, we will recognize operators like `xyz` as desugaring to `{%x}{%y}{%z}`. This effectively enables users to define their own extensions to ABC.

ABC achieves performance via its extension model. An interpreter is expected to specify one or more AO dictionaries for which it specializes data representations and accelerates functions. At the very least, this should include the common data embeddings - natural numbers and texts. But the idea can extend to evaluating linear algebra on a GPGPU or Kahn Process Networks on a cloud.

There is no centralized standardization of extensions. Just a collaborative evolution between runtime developers and users.

## Data Embedding

All data in ABC is Church encoded, but compact embedding is viable via the link layer extensions. For example, natural numbers might be embedded using `#42` or `#108`. Here `#` introduces a value representing the number zero, while `1234567890` each have a 'multiply by ten, add digit' effect.

Text embedding requires special attention:

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

Text will have a simple desugaring via user-defined extension:

        "hello" => [[#104] "ello" y]    (y for 'yield')
        ""      => ~

The definitions of `y` and `~` must be provided through the AO dictionary. Text desugars as a UTF-8 binary, so a character like `→` (U+2192) will yield three bytes (226, 134, 146). *Aside:* It is possible to unify texts, natural numbers, and more generic sequences, in which case `~` and `#` will have the same meaning.

## ABC CHANGE LOG

October 2016:
* reject global standard extensions to ABC
* extension and acceleration via AO layer
* leverage extensions model for text data

September 2016: 
* revision to minimalist ABC (`[abcd]`)
* old content now deprecated and deleted
* tokens texts limited to 62 bytes UTF-8

