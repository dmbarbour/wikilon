
# Human Editable Views for AO

[Awelon Object (AO)](AboutAO.md) code can be directly read and manipulated by humans, but is not optimized for direct human use. Humans should instead manipulate AO through an editable view - an efficient and aesthetic representation of code that may be edited in place then converted back to bytecode for storage. 

This is essentially an instance of [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html). The AO dictionary serves as the storage representation. The editable view of code would be provided through a service - likely a web service or a [filesystem in userspace (FUSE)](https://en.wikipedia.org/wiki/Filesystem_in_Userspace) adapter. FUSE would simplify integration with conventional text editors.

## Command Language for Awelon (Claw)

I present Claw as a useful example of an editable view - a concrete, Forth-like view designed for plain-text input in a REPL or command line scenario. I've used variants of Claw in practice to define a few thousand words. Claw shares a lot of strengths and weaknesses with Forth. For example, Claw becomes unreadable between ten and twenty tokens, but it's easy to factor large definitions into smaller ones.

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

Natural numbers use prefix `#` as in `#42`, and generate identical bytecode.

Inline texts have a simple constraint: they may not contain double quote or LF. Multi-line texts are almost the same as in ABC but include an extra LF just after the quote to distinguish them from inline texts. As such, `"hello, world!"` would desugar as:

        "
         hello, world!
        ~

Words are trivially expanded into their token forms, i.e. `foo` to `{%foo}`. If a word would be ambiguous, it must be expressed in token form, e.g. `{%42}`. 

Claw also supports `[blocks]`, `{tokens}`, and command sequences `(foo,bar,baz)`. Blocks are just normal ABC blocks albeit containing Claw code, instead. Tokens are unmodified and inlined directly (and developers are encouraged to tuck a token behind a word if it's a common thing). Command sequences generalize on texts to model interactive streaming behavior or coroutines:

        (foo,bar,baz) == [[foo] (bar,baz) y]
        (baz)         == [[baz] ~ y]

Command sequences are potentially useful in a variety of scenarios, effectively providing continuation passing style and interactive streams that might be used to model problem specific command languages.

As an editable view, all Claw translations are *bidirectional*. We rewrite towards Claw representations on render, and rewrite to AO on store. A consequence is that Claw code is not always preserved exactly, e.g. if you input `2 #3 ratio` we'll store that as `#2{%int}#3{%ratio}` and later read that as `2/3`. 

Claw ensures a round-trip from AO to Claw to AO is lossless. This is achieved by escaping AO whitespace by adding extra whitespace in Claw, i.e. `SP` in AO becomes `SP SP` in Claw. This can be useful for isolating Claw rewrites. 

### Evaluable Views

With the rewrite semantics and preservation of link structure, `AO → AO` is our basic evaluation. Ideally, we also want a corresponding `Claw → Claw` evaluation. Ensuring an evaluable view is sensitive to features such as accelerators and rewrite optimizations. To account for these, our Claw view should really be defined within the dictionary. For example, we might adjust use of `ratio` and `decimal` based on external features.

Not all views will be usefully evaluable. It can be difficult to ensure data inputs are effectively in normal form. But we can make a reasonable effort to ensure we can usefully view code after evaluation.

### Source Comments

I did not originally include comments in Claw because I favor external documentation like `foo.doc` plus a few examples. However, representing comments is not difficult, and they may serve a useful role for large values. Consider:

        /* this is a comment */   
            
            (desugars to)    

        "
          this is a comment 
        ~ {&a2} {@rem} d

A model of comments including arity annotations model of comments is compatible with the *Evaluable Views* goal because we can construct comments at runtime, comment values, etc.. and can leverage comments for active debugging (e.g. conditional breakpoints or logging).

## Editable View Functions

An editable view will be modeled as a pair of functions:

        view  : AO → View
        store : View → AO

Here AO is actually the UTF-8 text representation of the bytecode. The View might be purely textual like Claw, or may be structured in some ad-hoc manner. A round trip from AO to View to AO (`view store`) should be lossless, an identity function - an invariant that is easy to enforce dynamically. There is no guarantee for a round trip from View to AO to View. For example, it is possible that if a human writes `2 3 ratio` we'll see it as `2/3` when next we render.

Modeling views as a pair of functions helps keep it simple. In context of an AO, a view might be specified by a word that constructs a first-class `[[view] [store]]` pair.

*Note:* Reflection on the dictionary has been explicitly rejected. It's a complicating factor that hinders sharing, testing, versioning, and caching of views. However, restricting reflection has consequences. For example, we cannot resolve an ambiguous name by peeking at the dictionary.

*Aside:* It is possible to model lens-like focused structured when a render of the view may partially hidden from the human - e.g. leveraging `input type="hidden"` in HTML.

## Domain Specific Views (DSVs)

It is possible for views to leverage little languages specific to a problem, e.g. for representing mathematical formula or a table. All of these little languages must be already included within the `view` function. However, views may derive from a registry where we provide a list of DSVs.

Integrating domain specific views can be achieved by introducing a 'language' comment for a delimited subprogram, e.g. something like `["foo"{@lang}{&a2}d subprogram]` at the AO layer. Importantly, a comment-like mechanism remains accessible in context of *Evaluable Views* so DSVs could also be used in the output. In turn, this view might be presented to a user as `<foo>subprogram</foo>` or `[foo> subprogram]` or whatever the view developer deems appropriate.

Domain-specific views serve similar roles as domain-specific languages, and may serve as an alternative to macros. Unlike DSLs, we can retroactively add or deprecate features from a DSV. 

## Qualified Namespaces

It is feasible for a view to support qualified namespaces, i.e. such that `t/insert` expands to a larger word `trie:insert`. In a projectional editor, we have even more freedom - e.g. a word `insert` might include a hidden reference to the `trie:` prefix, and words might be given colors or iconography.

Namespaces might be understood as a specialized form of DSV. Handling namespaces in a similar manner to DSVs may help with namespace management boiler-plate, i.e. so we don't have a bunch of namespace declarations for a small function.

## Graphical Views

I'd very much like to pursue graphical views, where code includes such things as radio buttons, drop down lists, text areas. Enabling form-like presentation is useful for hypermedia, both editing code in place and treating the form as a template for a new command.

## Latent Error Handling

Humans will provide erroneous input. Not just at the parser layer, either - we can have code that fails to evaluate, fails to typecheck, or causes clients or tests to fail. Rather than treat edit-time errors as special, I propose that we store AO code that makes the errors explicit. Something like:

                "[foo" {:parse-error} 

Ideally, parse errors should be obvious at edit time, and should result in corresponding errors at runtime. The idea is that we should be able to handle parse errors through an external policy for handling erroneous code in general, whether that means rejecting erroneous code from entering a dictionary or allowing it in and flagging it for later correction. Separating error handling also simplifies the view and store functions, which do not need a conditional output for errors.
