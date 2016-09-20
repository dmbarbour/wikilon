# Awelon Object (AO)

Awelon Object (AO) is a useful set of conventions for [Awelon Bytecode (ABC)](AboutABC.md). AO defines tokens for linking and how they interact with evaluation in context of dictionaries.

A dictionary consists of a set of words and a definition for each word. Definitions are encoded in ABC. Linking a `{%word}` token logically substitutes that word's evaluated definition in place of the token. Dependencies between definitions must form a directed acyclic graph, such that all link tokens could be eliminated in by a finite expansion to inlined code. Hence, linking has no effect on ABC's expressiveness.

Dictionaries serve as a unit of evaluation, development, linking, and distribution in AO, and as a foundation for Awelon project's various [application models](ApplicationModel.md). In general, if there are meta-level features like security or immutability, they must be specified on a per-dictionary basis.

## AO Evaluation and Linking

Dictionaries are the basic unit of AO evaluation. 

        eval :: Dictionary → Dictionary

Evaluation operates on each definition in a dictionary. Linking is interleaved with evaluation, performed only when it enables evaluation to proceed beyond a trivial inlining of code. Ultimately, our evaluated dictionary will preserve much link structure, which simplifies debugging and rendering. Human-meaningful symbols like `{%word}` may remain in the evaluated code, as do associative attributes like `{%word.doc}` or `{%word.type}`.

Evaluation may proceed with undefined words. An undefined word will simply not be linked, i.e. it will be treated symbolically. 

### Arity Annotations

Arity annotations offer a simple means to control evaluation progress, and hence to control linking and preservation of link structure. Each annotation has the given arity. These are defined by a set of rewrite rules of form:

        [A]{&/1}            ==      [A]
        [A][B]{&/2}         ==      [A][B]
        [A][B][C]{&/3}      ==      [A][B][C]
        ...

For AO, arity annotations should be supported up to at least `{&/8}`. 

How this works is that, in a `{%producer}{%consumer}` scenario, our consumer may contain an arity annotation such that we know it's waiting on at least K more inputs before we have any need to link it. Upon evaluating `{%producer}` we can easily see how many outputs it makes available. If that number isn't at least K, we can avoid linking.

### Multi-Dictionary Evaluation Contexts

AO can be evaluated in context of multiple named dictionaries. Tokens of the form `{%word@dict}` name a word in another dictionary. Each dictionary is an implicit namespace, i.e. unadorned tokens like `{%multiply}` in context of dictionary `myMath` are equivalent to `{%multiply@myMath}`. 

For all AO-layer purposes, we can consider the pool of available dictionaries to just be one much larger dictionary with a simplistic namespace. Where multiple dictionaries can make a useful difference is meta-level system concerns: ownership, mutability, group connectivity, sharing, security, etc.. Dictionaries are readily identified as a named group.

### Large Value Stowage

For big data in a small working memory, it is useful to reverse the linking process: move data to disk, and replace the in-memory representation by a small token that may be used to reload the data from disk. I call this pattern 'stowage'. Stowage is most efficient with log-structured merge trees and similar persistent data structures that buffer and batch writes. Careful use of value stowage can enable entire databases as first-class values, and greatly reduces need for IO effects surrounding storage.

In context of AO, stowage involves creating new word tokens during evaluation.
        
        [large value]{&stow}  == [{%resourceId}]
        [small value]{&stow}  == [small value]

Here `{%resourceId}` is a word whose definition is equivalent to `large value`. We might stow to a separate dictionary, e.g. `{%resourceId@stowage}`. For a shared stowage dictionary, we may need to address security concerns (perhaps by including a small HMAC in the resource ID). Stowage has overhead, so an evaluator should make a heuristic decision about whether to stow depending on the value size.

How resource IDs are named is left to the evaluator and runtime. Stowage doesn't need deterministic naming, though at least having stable names would be convenient for humans, rendering tools, caching, etc. that interact with the results in contexts like incremental computation. Structure sharing could also be useful.

### Incremental Computing and Caching 

Incremental computing is essential for Awelon's application models, and effective use of cache is critical for incremental computing. A typical change to a codebase will do some mix of adding new code and updating old code. (Even a simple command pattern, for example, will add a new 'command' then update the 'head'.) 

Thus, incremental computing requires caching at both the dictionary level (so we can compute new objects) and potentially at the value level (such that a minor change in application state requires only minor recomputation of a view). 

 will add some new code (e.g. monotonic command pattern) and update some old code (e.g. spreadsheet style, perhaps update 'head'). Updates must propagate in a spreadsheet style). It will be important to react in near real-time. This has the following requirements:

* we store enough of our evaluated dictionaries

This means we'll be *storing* our evaluated dictionaries. Additionally, we'll need to track which words are relevant to a given computation, in the sense that an evaluator observed their value during evaluation (even if the decision was to not link the value). Thus, for a given dictionary `foo`, our runtime will 

 a change to the definition



Support for incremental computing is essential for applications in Awelon project. A change to a dictionary will take one of the following forms are either adding new code or updating old code.

We can break this down into two broad classes of computation:

* add some new code to compute it
* change 

we add a new definition, we want to compute
* we update an old definoi

Dictionaries apps have a very 'spreadsheet-like' aspect to them, so incremental computing will be highly valuable for performance. Incremental computing means minimizing computation after each update. Consider a scenario involving a database object and a useful view of that object:

        ...
        @myDB.v99 {%myDB.v98}(update99)
        @myDB     {%myDB.v99}

        @myDB:usefulView   {%myDB}{%usefulView}

If we assume a cached version of `myDB.v98` then it seems obvious that we can incrementally compute and cache `myDB.v99`, so at least that respect for our incremental computing is straightforward. The challenge, then, regards incremental computing of the useful view of our database.

One feasible solution involves caching. Assumptions:

* the view is compositional
* the database object has multiple 'component' partitions
* the updates are isolated to just a few components
* annotation `{&cache}` creates caching functions

By *view is compositional*, I mean that the view of the composite is a function of the views of the components. Formally, `∃F.∀x,*,y. V(x*y) = F(V(x), quote(*), V(y))` for our view `V`. I believe that a lot of useful views fit this criteria, or at least can be made to fit with a little wrapping and tuning. Time-series data processing, for example, is almost always compositional in this sense.

Annotation `[V]{&cache}` will create the caching version of function `V`. This must load the cache if it already exists, so our runtime will use a serialization of `[V]` (perhaps indirectly, via secure hash or stowage address) to load the appropriate cache. In general, we'll also need to serialize *arguments* to `[V]` to access cached results or save new results. With a cached view function, we can directly apply it within composition view function to access deep cache structure.

Efficient use of cache can be achieved by heuristic decisions about what to cache. At the runtime layer, we may observe effort vs. space tradeoffs, and avoid caching if recomputing is relatively cheap. At the development layer, we might make explicit, conditional decisions about which views to try caching, so we aren't serializing minor or unstable components.

It seems that explicit caching could serve effectively as a basis for incremental computation in dictionary apps, even in the presence of state, for a widely useful subset of views and related processes. 



## Sharing and Composing AO

To share any AO program requires sharing the dictionary. This context will frequently be implicit in the communication medium. For example, in a web service, the dictionary would be held by our server. Composing AO programs encounters challenges when there are name conflicts. In that case, we may need to translate, renaming words with conflicting definitions before integration. A multi-dictionary evaluation context shifts the concern to conflict between dictionary names.

To simplify sharing, one goal is to reduce renaming. This can be achieved by de-facto standardization of names, a centralized name registry (like a code wiki), simple naming schema (e.g. use domain name in dictionary name), secure hashes, random GUIDs, etc..

Another challenge surrounding sharing is update propagation. As much as possible, we'll want to share immutable or monotonic dictionaries that simplify caching. But ultimately a lot of [application models](ApplicationModel.md) rely on mutable dictionaries.



## Representing AO

AO does not make strong assumptions about how dictionaries are represented. One might use a hashtable, database, filesystem, first-class ABC values, etc.. However, AO does define a simple **.ao** file format for import and export purposes. 

        @swap []ba
        @swapd [{%swap}]a
        @swapd.doc "[C][B][A] -- [B][C][A]
         swap just under the top stack element
         assuming typical (stack*ext) environment
        ~

Each definition has the form `@word definition`, starting at the beginning of a line. It's a simple SP (32) between the word and definition. A definition continues until the end of a file or until the next definition starts. There is no risk of ambiguity: ABC does not and will not use `@` for anything. There are no constraints on the order of definitions, and a later definition of a word will overwrite an earlier definition.

A multi-dictionary context might be exported as an directory or archive, containing one file per dictionary.

Use file suffix **.ao**, or `text/vnd.org.awelon.aodict` in context of an HTTP transfer.

## Developing AO

Editing dictionary files by hand is feasible. But it's also a chore. Outside of early development (e.g. bootstrapping), I wouldn't recommend it to anyone. Dictionaries are optimally manipulated through projectional editor systems. The Forth-like [claw](CommandLine.md) view is suitable for a minimal text-only input, and is a fair bit more legible than AO. But Awelon project's [application models](ApplicationModel.md) are based on richer development environments. 

An interesting possibility for filesystem integration is to use a *FUSE* (Filesystem in Userspace) view, perhaps operating via the web service. If done properly, this could simplify integration with emacs, vim, and other nice text editors.

## Futures and Promises?

Would introducing words, a bit like stowage, be a good basis for concurrent futures and promises? This seems an interesting possibility, at least.


## Development Idioms

### Dictionary Inheritance

A useful technique is to say: "this dictionary is the same as that one, but with the following tweaks". A prototype inheritance or patching model would be useful in this context. For the **.ao** format, this might be represented by an extension like `@@PARENT source`. When a `{%word}` is not defined locally, the linker would search the parent source for the same word, as if it were locally defined. Then the grandparent, and so on.

### Leveraging Undefined Words

Undefined words can serve a useful role in a development context. They serve as 'holes', and an ABC system might help developers discover their definitions based on inferred type and usage. With ABC rewriting based evaluation, these holes also support lightweight symbolic partial evaluations. 

### Regarding Large Definitions

Definitions can potentially grow very large, especially when containing embedded texts or with dictionary applications. However, huge definitions are not recommended, as they may hinder incremental computation, reuse, memory management, and developer comprehension.

A very large text might be better broken into smaller texts - e.g. per chapter, paragraph, or other meaningful fragment. A large binary modeled via text might better be divided into 'pages', such that persistent structure and edits can be modeled can reusing most pages. Sophisticated definitions consisting of many components (e.g. more than ten to twenty elements) might be better factored into smaller fragments that can be documented and understood incrementally.

At the moment, I'm not suggesting hard caps for definition sizes. 

## Constraints on Words and Definitions

Words are minimally constrained to be relatively friendly in context of URLs, English text delimiters, HTML, [claw code](CommandLine.md), and AO dictionaries/streams. 

Summary of constraints on words:

* ASCII if alphabetical, numeral, or in -._~!$'*+:
* UTF-8 excepting C1, surrogates, replacement char
* must not start with numeral-like regex `[+-.]*[0-9]` 
* must not start or end with . or : (period or colon)
* no empty or enormous words, 1..30 bytes UTF-8.
* dictionary names use the same constraints

Words may be further limited in context of a given system or application model, e.g. unicode normalization and case folding to reduce ambiguity, forbid unicode spaces and separators. However, I'd prefer to avoid more sophisticated rules at this layer.
