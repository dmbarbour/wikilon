
# Human Editable Views for AO

[Awelon Object (AO)](AboutAO.md) code can be directly read and manipulated by humans, but is not optimized for direct human use. Humans should instead manipulate AO through an editable view - an efficient and aesthetic representation of code that may be edited in place then converted back to bytecode for storage. 

This idea is essentially an instance of [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html). The AO dictionary is our storage representation. The editable view of code would be provided by a service - e.g. a web service or a [filesystem in userspace](https://en.wikipedia.org/wiki/Filesystem_in_Userspace) adapter.

This document starts with a concrete example of a useful, purely textual, editable view named Claw. From there I generalize to more robust, expressive, portable, optionally graphical, user-defined views.

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
        0.0010  ==   10 #4 decimal
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

## Feature Request: Evaluable Views

Claw was designed for an earlier version of AO and ABC, before rewriting semantics and preservation of link structure. AO now has a nice property where we can evaluate from `AO → AO`, with the same dictionary. Optimally, we should also view the evaluated AO code as Claw and treat output equally as `Claw → Claw`.

Unfortunately, use of words like `ratio` and `decimal` will not reliably be generated in program output. That depends very heavily on runtime accelerators. 

Claw must be tuned to provided runtime accelerators. Runtime accelerators are most directly expressed by an accelerated dictionary. Hence, *Claw should be tuned to the dictionary*. The easiest way to achieve this is to define Claw within the dictionary, which has a bunch of extra benefits besides - like portable views.

## Comments

I did not include comments in Claw because I favor the path of least resistance to be external documentation resources, such that developers define `foo.doc` and a few examples. However, representing comments is not difficult and may be useful. Consider:

        /* this is a comment */   
            
            (desugars to)    

        "
         this is a comment
        ~ {&a2} {@rem} d

This model of comments is compatible with the *Evaluable Views* goal because we can construct comments at runtime, comment values, etc.. and can leverage comments for active debugging (e.g. conditional breakpoints or logging). 

## Ambiguity and Resolution

It grates to use `trie:insert` versus `avl:insert` when the choice is clear in context. At least to humans, the `trie:` prefix just makes the code more verbose, more difficult both to read and write. Our editable views should allow some controlled introduction of local ambiguity, at least insofar as we can resolve that ambiguity in the greater context. 

It is feasible to support some ambiguity resolutions via lightweight reflection on the dictionary to support namespaces or peeking at declared types. It is also feasible to make ambiguity externally visible for resolution. But that complicates use of editable views, so I'd prefer to focus on use of lightweight reflection.

To guard against ambiguity in generated views, we can assert that a round-trip (AO → View → AO) is not just unambiguous, it must also be lossless - an identity behavior preserving even whitespace. 

## Editing With Errors

Humans will provide erroneous input. Not just at the parser layer, either - we can have code that fails to evaluate or typecheck, or causes tests to fail. So a question is how to handle edit-time errors. One option is to reject the code at edit time, forcing immediate resolution. Another is to allow the erroneous code, perhaps recording it as an error object, something like:

                ["[foo" {%viewToAO}]{&error}i

When we view this again, we might recognize the error construct and replace it by `[foo` as we originally input. This option seems useful, and potentially permits parsing and evaluation with only partial errors. More importantly, it unifies error handling. Parse errors are handled the same as other errors.

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

