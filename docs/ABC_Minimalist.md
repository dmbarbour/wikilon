
# Minimalist ABC

A simpler ABC is (hopefully) a better ABC.

THE BIG IDEAS: 

* confluent, context free rewriting combinators
* minimal base plus early start on accelerators
* elimination of any non 'program' data types
* evaluation has uniform type program → program
* evaluation may proceed to fixpoint or quota

Consider some code:

        [A] swap [B] [C] swap [D] cons
            -- evaluates to...
        [A] swap [C] [[B] D]

This looks and acts *a lot* like a stack language. However, there is no stack per se, just a program under evaluation by rewriting. A significant difference is that there is no *stack underflow*. When we do not have enough arguments to the left, no rewrite occurs. Simultaneously, our runtime is free to perform deeper rewriting. 

A useful consequence is that we can 'grow' our programs at either edge. Adding to the left side provides extra inputs for our program to ingest. And adding to the right hand side enables processing of the program's outputs. In the common case, a function will only handle a finite stream of input. But in the general case, I might accept unbounded streams. (I might consider carefully how to statically type the latter case!)

Stream processing of inputs by sub programs offers a potential, simple basis for pipeline parallelism without requiring a model for parallel futures (nor for pipelines/channels/etc.). Additionally, constructs of the form `[[B] D]` would frequently admit further evaluation in parallel (though we might specifically indicate that intention by annotation). In general, we can achieve a great deal of parallelism without requiring the more expressive fork/join futures.

So far, this idea for a minimalist ABC seems very promising for flexibility and parallelism.

## Excellent for Application Model

Minimalist ABC is an *excellent* fit for the [application model](ApplicationModel.md) I've been developing since late 2014. There is no need to provide input or context to evaluate each word in a dictionary, and I can easily integrate a cache. And I need only a general view on 'programs', rather than separate views/renderings for both programs and values. (Though, if I rely on AO words for my view, I might need to track extra metadata or perform reverse lookups to produce cached views with words.)

## Systems Integration with Minimalist ABC

A challenge for any 'minimal' bytecode will be efficient integration with external systems. I need two things: to efficiently *inject* data into my computations, and to efficiently *extract* results from my computations. If input and extraction of data is complicated or expensive, ABC won't be a very practical language.

I posit that, like my original ABC, I should support:

* legible small numbers
* embedded text literals

Within the 'minimalist' goal of having only one data type, these values might be understood as compact representations of (first class) programs. I can easily eschew primitive support for negative integers. And if I'm just interested in natural numbers, I can easily give those *meaning as programs* by treating them as shorthand for a Church encoding.

        [B][A] #2 ap == [[A]#1 ap] A   == [[B]A]A
        [B][A] #1 ap == [[A]#0 ap] A   ==  [B]A
        [B][A] #0 ap == B

In retrospect, my decision to encode 'embedded texts' as 'lists' in the original ABC made for awkward composition, abstraction, rewriting, procedural generation, etc.. However, an interesting alternative is to simply reuse our structure already developed for number processing. Consider:

        [B][A] "hello" ap == #104 [[B][A] "ello" ap] A
        [B][A] "ello" ap  == #101 [[B][A] "llo" ap] A
        ...
        [B][A] "o" ap == #111 [[B][A] "" ap] A  == #111 [B] A
        [B][A] "" ap == B

This representation is expressive. I can abstract construction of texts. Composition can easily be accelerated, and might even use the same logical function as addition of naturals, though the type for `A` would generally be a little different.

Efficient extraction of these data types will rely on accelerators and runtime knowledge. E.g. when we add two integers with the appropriate accelerator, our runtime can very efficiently construct an integral representation for the result of the addition. 

*Aside:* I might explicitly use the UTF-8 expansion for text, e.g. such that `→` expands as three bytes rather than one larger codepoint. This would be more convenient for many use cases, especially those involving construction of maps, tries, hashes, etc.. It's generally more convenient for efficient processing and extraction.

## General Support for Command Sequences

The idea with a *command sequence* is that we iterate through a sequence of commands, but enable the client to perform some intermediate work after each command. Command sequences are convenient for concise data representations, DSLs, monadic effects models, etc.. I had previously developed support for [claw command sequences](CommandLine.md), as an editable view of code. 

An intriguing possibility is that *natural numbers* and *embedded texts* are specific instances of command sequences. Setting aside the specifics of representation or view, we might have the general form:

        #3 == (,,,)
        #0 == ()
        "hello" == (#104, #101, #108, #108, #111)

            generalized Church encodings:

        [B][A](foo,bar,baz) ap = foo [[B][A](bar,baz) ap] A
        [B][A]() ap = B

At the bytecode level, command sequences might be supported as simple views on a block, e.g. such that our 'comma' is simply a standard accelerator. Perhaps something like: `[[foo][[bar][[baz][%i]c]c]c]`. 

It may be worthwhile to compare alternative Church encodings. But, at the very least, avoiding need for *fixpoint* combinators for every little sequence seems a win. In many ways, actually, this is essentially a fit for J/K/APL style collections processing without loops.


## Efficient Runtime Representations



