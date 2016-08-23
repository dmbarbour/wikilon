
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

Potential benefits:

* great fit for application model, evaluate and cache abitrary words
* reduced need for explicitly 'paired' annotations, may capture in block 
* stream processing, chomp inputs on left, produce outputs to right
* flexibly provide more inputs or process outputs from any program
* latent parallelism, e.g. we could process `[[B] D]` immediately
* convenient value stowage, uniform value model (values are programs)

This idea for a minimalist ABC seems very promising.

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

### Generalizing Data to Command Sequences

A *command sequence* is a data structure that describes iteration through a sequence of 'commands'. Effective support for command sequences is good for concise data representation, DSLs, monadic effects, modeling cooperative multi-threading, etc.. An intriguing possibility is that *natural numbers* and *embedded texts* can be understood as specific instances of command sequences. Setting aside the specifics of representation and syntax, consider:

        #3 == (,,,)
        #0 == ()
        "hello" == (#104, #101, #108, #108, #111)
        "" == ()

            generalized Church encodings:

        [B][A](foo,bar,baz) ap = foo [[B][A](bar,baz) ap] A
        [B][A]() ap = B

Command sequences will be modeled as editable views, not a primitive feature of the bytecode. For example, `(foo,bar,baz)` above could be a view of `[[foo][[bar][[baz][e]c]c]c]` (for appropriate ops and accelerator `e` and `c`). But it's pretty cool if we can *understand* numbers and texts as command sequences.

*Aside:* The general idiom of using `[B][A]` with an iterator - a command sequence - seems a convenient basis for collections oriented programming without explicit loops or fixpoints (and hence no risk of non-termination).

### Effects Integration

Minimalist ABC will still favor monadic effects or any lightweight variant on that idea. We'll output a representation of a request (perhaps a command or query) together with a continuation. The halting condition could in general have the form `Continuation [Request]` or `[Continuation][Request]`. In either case, we can provide a response then continue.

## Evaluation Control

Use of `{&par}` and `{&seq}` could work pretty well in Minimalist ABC. The idea would be: we have a block containing a subprogram. We want to evaluate that subprogram as far as possible now, rather than later.

## API Thoughts

The modified API will operate on a context that contains a program. I'll probably favor a 'right biased' API, in the sense that stuff is usually injected in the RHS. However, a simple *move* function could let me move a value from the RHS of one context and inline it on the LHS of another (or the same) context. This would thus provide a very simple mechanism for injecting extra inputs on the left, and for stream processing in general.

An interesting possibility is to open a block and lay down bytecodes and such one at a time, perhaps perform some intermediate evaluations, then close the block. This would help separate the issue of injecting code from that of parsing it. 

## Efficient Runtime Representations

A pointer has spare bits that are convenient for tagging and embedding of values. For my original ABC, these bits were largely oriented around pairs, sums, and small integers. With minimalist ABC, any focus for tight representation would instead orient around natural numbers, opcodes, and block embeddings. In both cases, the general escape for 'tagged' objects, allowing me to handle arbitrary special cases (like stowage or texts) is essential.

Our context holds a 'program', potentially appends at both ends, and may extract data from the right hand side. In addition, we might want to perform ad-hoc evaluation internally to a block - i.e. open it up, iterate through for evaluation, then exit. Further, rather than reset to a canonical representation between quota steps, we might save some effort by essentially leaving a 'cursor' wherever we stopped evaluation. I imagine the Huet 'zipper' concept will prove a good fit for modeling this rather ad-hoc mix of program extension and evaluation context.

Compact bytecode will be important for performance with iterative code, just as it is for the original ABC. Shared objects would be similarly critical. I'll need to get an early start on these features.

Use of a JIT compiler seems a little bit awkward in context of program rewriting. But I believe it can work - at the very least, we can simply restrict a JIT to subprograms where we *know* we have all the inputs available.

## The Minimal Bytecode



