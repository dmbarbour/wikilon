
# Extensible Syntax

Syntax is just a structure that users manipulate in order to affect meaning or behavior of a program. Ideally, this structure is meaningful to the humans. The purpose of extensible syntax is to tune structures into something meaningful to humans, perhaps for a specific purpose.

## Evaluated Definitions

A viable basis for structure is to represent an intermediate data structure and interpret it. This structure can be extensible if we can control the interpretation, e.g. if the structure describes its own compilation into a function. We can render this structure for our user, perhaps aided by annotations or value sealers. We can manipulate the structure functionally. Applied to a dictionary, words provide a useful object identity that becomes a good fit for [embedded literal object](EmbeddedLiteralObject.md) ideas.

We could represent this with definitions of type:

        type Def a b = ∃v.∀e. e → ([v→[a→b]]*(v*e))

Here, the value `v` becomes the intermediate data structure, and the `[v→[a→b]]` function becomes the compiler that transforms said structure into a function. The *meaning* of the definition is the resulting `[a→b]` function computed when we apply the interpreter to the intermediate data structure. In the trivial case, our compiler can be the identity function `[]`, and the `v` value can simply be the block of code. For example, `[vrwlc][]` becomes a viable definition for a primitive swap function. 

In general, we can factor any compiler down to a single word, such as `[{%fooLang}]`. Thus, the idea that every definition carries its own compiler isn't a significant burden. Really, it reduces to specifying a purpose-specific language.

The explicit compilation step has several advantages. We can embed a lot of automatic testing and reasoning into our compilers. Compile-time annotations could feasibly intern data structures, assert properties of functions, and more. Relying on the `e` value could provide ad-hoc expression for incomplete definitions. 

The main disadvantage is this hinders direct interpretation of code in a REPL context, e.g. we'll evaluate a lot ahead of time. Also, this entire concept relies heavily on structure editors.

## Command Language

My work on [command language for awelon (claw)](CommandLine.md) offers a viable basis for extensible syntax: we expand a higher level source code into bytecode in a (more or less) reversible manner, i.e. such that we can parse the bytecode back into a sensible rendering of the higher level language. This can be driven by words such as `integer` and `ratio`, perhaps later extended with vectors and monads. We have escapes to raw bytecode to cover anything the extended syntax misses.

It is tempting to just take this route, but it misses some useful features. For example, we cannot functionally manipulate structures expressed this way. We lack compile-time computations and annotations (modulo partial evaluation, which isn't something I want to rely too much upon). We cannot express incomplete definitions. This command language approach is also relatively rigid, restricted to techniques with local expansion.

Command language can be used together with evaluated structure definitions, e.g. by using the command language within blocks and having basic command-language definitions use the form `[command language code][]`. 

