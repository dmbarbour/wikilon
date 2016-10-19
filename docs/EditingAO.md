
# Human Editable Views for AO

[Awelon Object (AO)](AboutAO.md) code can be directly read and manipulated by humans, but is not optimized for direct human use. 

Humans should instead manipulate AO through an editable view - an efficient and aesthetic representation of code that may be edited in place then converted back to bytecode for storage. This idea essentially an instance of [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html). The AO dictionary is our storage representation. I'm initially focusing on plain-text editable views. But more structured or graphical views (with tables, radio buttons, canvases, progressive disclosure, hypermedia, etc.) are certainly viable.

An editable view of code may be provided by a service - e.g. a wiki-inspired web service, or a [filesystem in userspace](https://en.wikipedia.org/wiki/Filesystem_in_Userspace) adapter.

This document starts with a concrete example of a useful editable view named Claw. Claw provides a convenient base from which we can motivate further enhancements. This document will conclude with a robust, portable, expressive, and extensible foundation for editable views.

## Command Language for Awelon (Claw)

Claw is a Forth-like editable view of AO designed for plain text input in a REPL-like or command line scenario. 

Like Forth, Claw doesn't scale nicely beyond about fifteen tokens for a definition, at least without an IDE that can print inferred types or examples in a connected pane (it's otherwise too easy to lose track of context). But, also like Forth, it's easy to factor large functions into small definitions.

Claw optimizes for:

* number data `42 -7 2/3 3.141 2.998e8`
* inline texts `"hello, world!"`
* AO dictionary words `foo swap bar`

Numbers in Claw receive the bulk of attention:

        42      ==   #42 int
        -7      ==   7 negate
        2/3     ==   2 #3 ratio
        95/100  ==   95 #100 ratio
        3.141   ==   3141 #3 decimal
        -2.7    ==   -27 #1 decimal
        0.001   ==   1 #3 decimal
        2.998e8 ==   2.998 8 exp10

Natural numbers can also be expressed easily, just use prefix `#` as in `#42`. This will result in code `#42` inlined into the output bytecode.

Inline texts have simple constraints. They may not contain double quote or LF, and we'll follow them with `lit`. Multi-line texts are the same as in ABC but require an additional blank line at the start to both simplify alignment and indicate they are not inline texts. As such, `"hello, world!"` shall desugar as:

        "
         hello, world!
        ~ lit

Words are trivially expanded into their token forms, i.e. `foo` to `{%foo}`. If a word would be ambiguous, it must be expressed in token form, e.g. `{%42}`. 

Claw also supports `[blocks]`, `{tokens}`, and command sequences `(foo,bar,baz)`. Blocks are just normal ABC blocks albeit containing Claw code, instead. Tokens are unmodified and inlined directly (and developers are encouraged to tuck a token behind a word if it's a common thing). Command sequences generalize on texts to model interactive streaming behavior or coroutines:

        (foo,bar,baz) == [[foo] (bar,baz) y]
        (baz)         == [[baz] ~ y]

Command sequences are potentially useful in a variety of scenarios, effectively providing continuation passing style and interactive streams that might be used to model problem specific command languages.

As an editable view, all Claw translations are *bidirectional*. We rewrite towards Claw representations on render, and rewrite to AO on store. A consequence is that Claw code is not always preserved exactly, e.g. if you input `2 #3 ratio` we'll store that as `#2{%int}#3{%ratio}` and later read that as `2/3`. 

Claw also ensures that a round-trip conversion starting from AO code is lossless. This is achieved by escaping AO whitespace by adding extra whitespace in Claw, i.e. `SP` in AO becomes `SP SP` in Claw. This can be useful for isolating Claw rewrites. 

## Evaluable Views

Claw was designed for an earlier version of AO and ABC, before rewriting semantics and preservation of link structure. AO now has a nice property where we can evaluate from `AO → AO`, with the same dictionary. Optimally, we should also view the evaluated AO code as Claw and treat output equally as `Claw → Claw`.

Unfortunately, use of words like `ratio` and `decimal` will not reliably be generated in program output. That depends very heavily on runtime accelerators. 

A dictionary should know about runtime accelerators. Presumably, users will derive from an accelerated dictionary. A reasonable conclusion is that *we should shove our definition of Claw into our dictionary*, such that it may be tuned for dictionary-specific conventions.

## Comments

I did not include comments in Claw because I favored the path of least resistance to be external documentation resources, such that developers define `foo.doc` and a few examples. However, representing comments is not difficult. Consider:

        /* this is a comment */   
            
            (desugars to)    

        "
         this is a comment
        ~ {&a2} {@rem} d

This is a sufficient model of comments. Importantly, by deferring destruction of the comment with arity annotation `{&a2}`, we gain the ability to construct and preserve comments in context of *Evaluable Views*. Further, by including a `{@rem}` gate and configuring the runtime, we might even do something useful with our comments (logging, profiling, ad-hoc progress indicators, conditional breakpoints, etc..).

Comments have greater merit in context of evaluable views and debugger gates. 

I would still encourage developers to focus primarily on dictionary-layer documentation resources. But it doesn't hurt to support conventional definition-layer source-level comments. We need only to add words starting with `/*` to the filter of words our view must escape.

## Editing With Errors

Humans will provide erroneous inputs. It's only a matter of when, how frequently, and how early do we catch the errors. In context of AO editable views, we can detect errors at many layers without too much effort:

* code that fails to parse
* code that fails to evaluate
* code that fails to typecheck
* code that causes clients to fail evaluation
* code that causes clients to fail typechecking

AO allows evaluation to occur before typechecking, which is potentially useful for macro-like staged static evaluations. And 'clients' here may include examples, tests, and what are essentially type assertions (i.e. that a word should be valid in a given context). 

Given erroneous input, we have an option: we can reject the input and ask the human to fix it *now*, or we can record the input and ask the human to fix it *later*. When I originally developed Claw, I favored the 'fix it now' option, so I didn't address the question of how to model erroneous inputs. 


When humans provide erroneous input - e.g. in the sense of code that fails to parse or type check - we have an option. We can reject the input and ask the human to correct it now. Or we can accept the input and ask the human to correct it later.

When I originally designed Claw, I was assuming the former. I would easily guarantee  However, accepting erroneous input and flagging 









While I did not attempt this with Claw, an interesting question is how we should handle *erroneous* human inputs. For example, assume an attempt to store `[foo`, leaving off the block terminator. We could translate this to AO code like:

        ["[foo"{~lang:claw}]{&error}i

If we later attempt to recover this back into Claw, we could recognize the error construct and replace it by `[foo` as we originally input. Thus, we've preserved our round-trip identity properties, but the resulting AO code is also clearly erroneous in a manner that is readily detected on evaluation, and readily flagged for human attention. 

Intriguingly, evaluation with partial errors is feasible. For example, we might instead capture this as:

        [{%foo} "missing-]"{&error}i]

Whether this is a 'good idea' is a separate question. But 






(and the `{&error}i`, but not in a way that resists partial evaluations.



- e.g. if someone attempts to store `[foo` leaving off the block terminal. 

A simple possibility is to present this as an AO-level *error object* - i.e. leveraging an `{&error}` annotation and enough context such that we can preserve and edit erroneous subprograms. This allows a useful separation of concerns, e.g. supporting editors that interactively help developers fix errors is separated from the storage concern.



## Word Fuzzing and Resolution

Unambiguous reference is very convenient for linking AO code, but is not so convenient for human use. We end up with bulky words that repeat information already available in their context of use - e.g `trie:insert` vs. `avl:insert`. A consequence is that the program is not nearly so concise as it could be. 

It is feasible to permit *local* ambiguity within an expression of a program so long as that information is recoverable in context. For example, we can replace `trie:insert` by `insert`, but only if we can later recover the `trie:` prefix.

To guard against ambiguous code, we can insist that, like Claw, a round-trip conversion (AO to View back to AO) must be lossless, an identity behavior. Even whitespace in the original AO input must be exactly preserved. Humans must never read an ambiguous view of the AO dictionary.

Humans may attempt to *write* ambiguous code. However, humans may attempt to write erroneous code in any number of ways. The specific possibility of ambiguous interpretation could easily be handled as an error that includes multiple possible interpretations (each of which might recursively have its own errors).



However, allowing an ambiguous view does introduce the possibility that humans will attempt to *write* ambiguous code into the dictionary, e.g. by failing to provide sufficient context. This might be guarded against 

Allowing ambiguity does introduce a possibility that the view will become ambiguous after some edits. In that case, we should be able to tell the human that the code is ambiguous, and present possible interpretations. This could be achieved as follows:




Designers of the view can partially guard against this, e.g. by including some medatadata in the view. 




 only one possible resolution, and that *must* be lossless identity behavior. Or put another way, humans should never *read* an ambiguous view from our dictionary.

 we may assert that translating from AO code to editable view back to AO is an identity behavior, preserving even whitespace. This 



We might insist on a fixpoint view, that reading the AO then writing it out again produces the exact same AO code. When writing Claw code in the first place, we might 

This might be recovered if we add some namespace information to our view. Or perhaps some type information. 

In context of editable views, we can heuristically introduce local ambiguities

In context of editable views, we should at least ensure our initial and final views a



For a more concise and human palatable programming environment, we may need to heuristically introduce some *local* ambiguity, so long as the overall meaning of our program i - e.g. replacing `trie:insert` by `insert` in a context where we know we can recover the `trie:`.

More broadly, we could potentially support concise expressions like `tins` for `trie:insert` if there is no other word in the dictionary for which `tins` could 'match' 

 This fuzzing could take many possible forms: namespaces, 



* including some type-based metadata in our view
* 

For editable views, 

we can permit a small amount of *local* ambiguity so long as it resolves

But we might need to *introduce* 

ambiguity at the human layer in favor of conciseness

However, at the human layer, we may benefit from allowing ambiguous use of words


At the AO layer, unambiguous name resolution is a good thing. 


## Patch Level Programming

Claw was defined at the level of AO strings, but could easily be applied to AO level updates, e.g. by use of:

        @word1 definition in Claw
        @word2 another word1
        ...

This is achieved by simply reserving use of the `@` on a new line for defining a new word. But in the general case - in context of word fuzzing, for example - this might not be the most efficient expression




## Adaptive Language

Further, while a Forth-like notation is broadly useful, it isn't optimal for all problems. For example, simple polynomial math becomes a mess of copies, adds, and multiplies.

Optimally, developers can adapt their language or view to each problem.

Adaptive language introduces some of its own challenges. We must minimize boiler-plate, avoid confusing users,  support fine-grained composition, limit impact on refactoring, guard against ambiguity, and permit an ad-hoc mix of multiple languages to solve a problem. With *evaluable* views in mind, it's also important that we don't lose adaptive language metadata to evaluation or data plumbing into a new context. 

A modest proposal:

A block of code can be labeled with a viewer hint, perhaps via `[foo code]{~lang:foo}`. A reader can recognize this situation and render the block appropriately, for example by looking up `foo.readAO` and `foo.writeAO`. We can guard against ambiguity by asserting that read/write actions have identity semantics.

This supports evaluable views. Labels may be controlled by the evalution, and default readers and writers can be provided to support generic Claw. Further, labels don't impose any contextual constraints on language, other than that changes in language align with blocks.

Naively, the reader might simply be `AO → Text` and vice versa. In practice, either our reader might invoke other readers (e.g. upon recognizing `[code]{~lang:bar}`) and we might choose to support something more than raw textual output. 

we'll need to perform a pass ahead of time and provide some partially parsed AO. Similarly, the writer may be complicated a little - e.g. to support namespaces (output any one of the following...). 

We can guard against ambiguity by ensuring a read/write action has identity behavior both for the initial read and after computing a proposed write. If either of these conditions fail, we'll fall back to the default reader/writer. 



Conversely, the writer might want to support namespaces such that, upon writing a word, we 







Our viewer knows to recognize this situation and, when rendering this block, may search the dictionary `foo.readAO` and `foo.writeAO`. Unlabeled blocks might use `readAO` and `writeAO` from the dictionary toplevel.




Our writer might

 reader is a function from `AO → Text` where the text is presented to the user. Conversely, `foo.writer` will translate 

view to look for an associated dictionary object like `foo.reader` and `foo.writer` where our reader processes AO → 

If a block is not labeled, we may use a default language, perhaps equivalent to `{~lang:default}`. 


Any editor 


* Language s
* We'll want to solve different sub-problems in different languages.
* 

 that different problems can be expressed in different sub-languages. The problem mustn't be overspecified for a given language.

To cover the aforementioned problems, we may want fine-graned use of DSLs, and perhaps namespaces. This would work 

In this case, we'll need some fine-grained use of DSLs.








Relevant desiderata:

* mix multiple contexts
* minimal boiler plate
* stable local control
* reusable languages
* language transparency


While ambiguity is problematic, we can at least support concise expression in cases where doing so is not ambiguous. 

Between these forces, it would be most convenient if we can adapt and tune our use of language to our problem in a fine-grained manner. 

Desiderata:


In context of AO, I propose a simple mechanism: that we recognize languages via labels. 

combined with goals for concise expression and readable code, all of this adds up to need for contextually adaptive interpretation - e.g. type driven overloading (traits, type classes), namespaces, or domain specific languages.

Related desiderata:


Ad-hoc mixing of contexts is essential for flexible expression. Controlling context and understanding it at a glance is valuable for human comprehension, refactoring, and editable views. Constraining boiler plate overhead is valuable for concise expression. Adapting symbols is a specialized case of tuning syntax.

General observations:

Type driven approaches are excellent for mixing topics and minimizing boiler plate, but tend to fail at goals for stability, locality, and visual comprehension of context. Type driven techniques also aren't readily applicable in context of AO, where static typing remains optional.

Namespaces are a familiar but mediocre option. Import lists for managing multiple namespaces easily becomes boiler-plate. It can be difficult to know at a glance to which namespace a word belongs. Fortunately, an editable view *does* ameliorate one problem of namespaces in conventional PLs: adding words to a namespace does not risk introducing ambiguity to non-local source code.

Domain specific languages (DSLs) are a general option for tuning syntax to each problem, whether high level or low level. DSLs also generalize nicely to structure editors with non-textual views. However, use of DSLs must be restricted for easy composition and minimal boiler-plate.

I favor DSLs as a foundation for adaptive language in AO.

In context of editable views for AO, DSLs might be scoped to blocks or values, as in `[block of code]{


a DSL might be understood as a pair of functions much those defining Claw: a reader function that parses AO code into the editable DSL presentation and a writer function that expands DSL code back into the AO storage representation. Further,


This leaves the challenge of specifying *scope*. 


One simple, viable option is to use annotations like `[AO code]{&lang:foo}`. 



In context of AO editable views, a 'DSL' might be defined by a pair of functions - one to rw AO into the DSL and one to parse the DS


Boiler plate for DSLs 

Some difficulty with DSLs is potential for boiler plate and 

Consequently, I see them as the most desirable option for editing AO. However, conventionally DSLs are difficult to compose.







Tuning syntax to a problem is the most general option, and also generalizes nicely to non-textual views.

 generalizes very nicely to problems that may otherwise be difficult to express in concatenative forms. 

In the general case, mixing languages is notoriously difficult. But we could potentially use simple techniques like XML-style 'language boxes'. 

, composing languages is notoriously difficult. 

Adapting language to the problem is an interesting option and generalizes nicely to non-textual views. 

Adapting language

Adapting a language to a specific problem 

Problem oriented languages (aka domain specific languages) 

DSLs or dialects

XML style techniques are viable, but may have a relatively large overhead.




(However, because we aren't storing AO does avoid potential update ambiguity issues where words are added to a namespace.) 

 The meaning of code under namespaces can change non-locally due to adding words to a dictionary.

Namespaces have a moderate amount of boiler plate




Related goals are that it should be easy to mix contexts within a computation, and that context should be stable and local such that a local change to code does not require reconstructing the whole view.



rather than rigidly use a single context. 

Type driven approaches are pretty good except insofar as they require static typing. In context of editable views, 


they do not fit easily with AO where static typing is optional. However, perhaps the idea could be adapted somewhat.





 where static type checking is optional. 

They enable an adaptive interpretation of words based on a program stack. 

In context of editable views for AO, we have some implicit limitations. 

Typeful approaches would be most convenient, but seem difficult to apply. 




In context of ABC and AO, we have some implicit limits

Without that, developers are forced to use 



or some other means to use words in a context specific manner. Use of typeclasses or traits could potentially work, for a typeful context. 

All of these tendencies are bad news for a language without some means to scope discussion. 



Full qualification of words becomes bulky. which may encompass a broad array of subjects or heterogeneous variants within a subject. 

This is largely a consequen

A weakness of Claw is that words are so trivially expanded. In practice, this doesn't scale nicely to large dictionaries that may cover many subjects. Words such as  





## Visual Claw

