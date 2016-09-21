# Awelon Object (AO)

Awelon Object (AO) is a useful set of conventions for [Awelon Bytecode (ABC)](AboutABC.md). AO defines tokens for linking and how they interact with evaluation in context of dictionaries.

A dictionary consists of a set of words and a definition for each word. Definitions are encoded in ABC. Linking a `{%word}` token logically substitutes that word's evaluated definition in place of the token. Dependencies between definitions must form a directed acyclic graph, such that all link tokens could be eliminated in by a finite expansion to inlined code. Hence, linking has no effect on ABC's expressiveness.

Dictionaries serve as a unit of evaluation, development, linking, and distribution in AO, and as a foundation for Awelon project's various [application models](ApplicationModel.md). In general, if there are meta-level features like security or immutability, they must be specified on a per-dictionary basis.

## AO Evaluation and Linking

Dictionaries are the basic unit of AO evaluation. 

        eval :: Dictionary → Dictionary

Evaluation operates on each definition in a dictionary. Linking is performed dynamically, only when it enables evaluation to proceed beyond trivial inlining of code. Ultimately, our evaluated dictionary will preserve much link structure. Human-meaningful symbols like `{%word}` will remain in the evaluated code, as do associative attributes like `word.doc` and `word.type`.

### Arity Annotations

Arity annotations offer a simple mechanism to control evaluation progress and hence to control runtime linking. This can be useful to limit degeneration of program structure to overly fine-grained partial evaluation. These are defined by a set of rewrite rules of form:

        [A][B]{&/2}                         == [A][B]
        [A][B][C]{&/3}                      == [A][B][C]
           ...
        [A][B][C][D][E][F][G][H][I]{&/9}    == [A][B][C][D][E][F][G][H][I]

Each annotation effectively has the given arity. I propose that AO should support arity annotations 2..9, a total eight options. Lower arities are unnecessary because all primitives have arity at least one. And higher arities should be unnecessary in practice.

In a `{%producer}{%consumer}` scenario, our consumer may contain an arity annotation so that we know it's waiting on at least K more inputs before evaluation may progress. We can also compute how many outputs our evaluated producer makes available. If the count is sufficient, we link both and proceed with evaluation. Otherwise, evaluation will halt and we'll preserve the link structure.

There is no strong constraint that our arity match the entire input requirement for a computation. It may be useful to use arity annotations to control partial evaluations.

### Large Value Stowage

For big data in a small working memory, it is useful to reverse the linking process: move data to disk, and replace the in-memory representation by a small token that may be used to reload the data from disk. I call this pattern 'stowage'. Stowage is most efficient with log-structured merge trees and similar persistent data structures that buffer and batch writes. Careful use of value stowage can enable entire databases as first-class values, and greatly reduces need for IO effects surrounding storage.

In context of AO, stowage involves creating new word tokens during evaluation.
        
        [large value]{&stow}  == [{%resourceId}]
        [small value]{&stow}  == [small value]

Here `{%resourceId}` is a word whose definition is equivalent to `large value`. Stowage has overhead, so an evaluator should make a heuristic decision about whether to stow depending on the value size.

How resource IDs are named is left to the evaluator and runtime. Stowage doesn't need deterministic naming, though at least having stable names would be convenient for humans, rendering tools, caching, etc. that interact with the results in contexts like incremental computation. Structure sharing could also be useful.

### Incremental Computing and Caching 

Consider two common [application patterns](ApplicationModel.md):

* **command pattern** - We model an application as an initial state followed by a long sequence of update action, potentially resulting in massive databases. It is possible that we'll tweak/undo recent actions and recompute state, but that's mostly near the head of the application. Older actions may gradually be made immutable, composed, and garbage collected. 

* **view pattern** - We model reactive views that query and summarize the state of one or more applications. Application states, in general, are massive databases, but we can assume most updates are small - affecting only a small part of that database. It's important that we can compute the update for our view without recomputing the entire database. 

The command pattern suggests that we should be caching near the head of our app, such that adding new words does not require recomputing the entire application state. The view problem cannot be solved for the general case, so I assume two light constraints:

* large applications have persistent, compositional structure
* views are compositional: `∃f.∀x*y. V(x*y) = f(V(x),*,V(y))`

Our application state is modeled by a compositional structure `x*y`. With persistence, updates will tend to involve either the the `x` or `y` but not both. Hence, it becomes feasible to leverage a cached `V(x)` or `V(y)` from a prior computation. This applies hierarchically, i.e. `x` might equal `v*w`. Many systems will fit this pattern or can be made to fit with a little consideration.

A question, then, is how do we know what to cache? I propose the following:

* we *implicitly* cache evaluation over words
* we *explicitly* cache any other computation

Cached evaluation over words is a natural fit for AO. It makes no difference for our evaluated dictionary whether we substitute token `{%word}` with its original definition or its cached, evaluated definition. Because `{%word}` may be used more than once in a dictionary, we will frequently benefit from caching rather than recomputing. In context of incremental computation, our cached result would additionally be used over time.

Explicit caching can be expressed by annotation, i.e. `[computation]{&cache}`.

The `{&cache}` annotation does not force evaluation. When evaluation does occur, our evaluator will first search its cache. If the cached value is not available, we'll perform the computation to completion then decide whether or not to add it to our cache. 

*Note:* An evaluator may make heuristic decisions about whether to cache based on time/space tradeoffs. This applies for both implicit and explicit caching.




 of dictionary words effectively gives us lazy evaluation on our dictionary. In this case, caching is mostly important because a `{%word}` may be used more than once. Rather than link and evaluate its definition every time, we might choose to evaluate the word's definition once and use the 

 It makes no difference whether we substitute `{%word}` by its definition or its *evaluated* definition, so in general we might as well cache our evaluations and save some work in cases where a word is used more than once.



This is straightforward enough: we want caching of words anyway because it makes no difference whether we subsitute `{%word}` by a dfi

 - i.e. evaluate just what is needed for your current computation, keep a record so we don't repeat evaluation unnecessarily. But it's a bit less stringent. We might perform some recomputations, might choose not to preserve cache for cheap computations.

suggests that our `eval :: Dictionary → Dictionary` is augmented with a cache, such that we don't rec

 is more or less equivalent to *lazy* evaluation on a dictionary, though 


Evaluation may be lazy. That is, we don't need to evaluate a word before there is cause to do so, and we can usefully cache our evaluated results. If we aren't exporting our resulting dictionary, there is no need to evaluate the whole thing.



we could cache view `V` on our application state `x*y`

These assumptions seem a good fit for a lot of applications and databases. Anyhow, this means we might cache `V(x)` and `V(y)`. 

For the view pattern, we might need more general caching of computations. 


The former


To do this, we'll try for *compositional*


 that we don't need to review the entire database if there are only a few small, relevant changes.

When an application is updated, it's important that we efficiently compute the changes in the view rather than reprocessing the entire application state.

 In any case, this pattern suggests we must be caching near the 'head' of our dictionary.


* **spreadsheet pattern** - 

Older actions tend to become immutable,

This leads to long chains of computations to c

 - i.e. the command . This leads to long chains of computations within our dictionary. 

* we update an application state via a 

Large data structures are incrementally updated via command patterns. 

With stowage, we'll be constructing large data structures. and incrementally updating them through command patterns. We can recompute these structures via 

 Effective use of cache is critical for incremental computing. I assume, to start with, that evaluations on *words* will generally be cached.

In addition to caching evaluations of words, we must cache evaluation on more ad-hoc computations, i.e. such that small changes to large data structures (trees and similar) result in incremental computations for computed views. 


, and effective use of cache is critical for incremental computing. A typical change to a codebase will do some mix of adding new code and updating old code. (Even a simple command pattern, for example, will add a new 'command' then update the 'head'.) 

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

## Dictionary Representation

I assume a common pattern during development will involve forking a dictionary, performing a few simple edits, then observing how those changes will affect our evaluated results. It's convenient if our AO dictionary representation supports this behavior efficiently.




### Multi-Dictionary Contexts

As a simple convention, we can take `{%word@dict}` to refer to a word in a named dictionary. Every dictionary acts implicitly as a namespace, such that within dictionary `myMath` the token `{%multiply}` may logically be rewritten to `{%multiply@myMath}`. Our evaluation context then becomes a set of named dictionaries. 

For AO layer purposes, the set of named dictionaries is effectively one composite dictionary. But modeling multiple dictionaries can be convenient in other layers - e.g. security, versioning, representation. For example, in a filesystem, we can use one file or directory named dictionary. We can support features like prototypal inheritance of dictionaries. We can name dictionaries with a secure hash to guard immutability. We can have some dictionaries that are more heavily curated and trusted than others.

### AO Import and Export

AO does not make strong assumptions about how dictionaries are represented. One might use a hashtable, database, filesystem, first-class ABC values, etc.. However, AO does define a simple **.ao** filesystem representation for import and export purposes. 

        @swap []ba
        @swapd [{%swap}]a
        @swapd.doc "[C][B][A] -- [B][C][A]
         swap just under the top stack element
         assuming typical (stack*ext) environment
        ~

Each definition has the form `@word definition`, starting at the beginning of a line. It's a simple SP (32) between the word and definition. A definition continues until the end of a file or until the next definition starts. There is no risk of ambiguity: ABC does not and will not use `@` for anything. There are no constraints on the order of definitions, and a later definition of a word will overwrite an earlier definition. 

For multi-dictionary contexts, we'll use one file per named dictionary, and we leverage directory structure just a little such that dictionary `foo.bar` uses file `foo/bar.ao`. 



## Development of AO

### Editable Views

Editing dictionary files by hand is feasible. But it's also a chore. Outside of early development (e.g. bootstrapping), I wouldn't recommend it to anyone. Dictionaries are optimally manipulated through projectional editor systems. The Forth-like [claw](CommandLine.md) view is suitable for a minimal text-only input, and is a fair bit more legible than AO. But Awelon project's [application models](ApplicationModel.md) are based on richer development environments. 

An interesting possibility for filesystem integration is to use a *FUSE* (Filesystem in Userspace) view, perhaps operating via the web service. If done properly, this could simplify integration with emacs, vim, and other nice text editors.


### Sharing and Composing AO

The dictionary is the unit of sharing in AO. This 

If there is need to do so, it is not difficult to extract a minimum useful dictionary for a given application. We take some initial set of words, then compute a transitive closure including attributive metadata (such that for `word` we also include `word.doc` and `word.type`). A copy of a dictionary may be disconnected from any further updates to the origin.


 it should be sufficient to share a URL or similar.





To share an AO program requires sharing a dictionary, or at least part of one. It is not difficult to compute a transitive closure of all the relevant words for a given program, and export just that subset.

. This context will frequently be implicit in the communication medium. For example, in a web service, the dictionary would be held by our server. Composing AO programs encounters challenges when there are name conflicts. In that case, we may need to translate, renaming words with conflicting definitions before integration. A multi-dictionary evaluation context shifts the concern to conflict between dictionary names.

To simplify sharing, one goal is to reduce renaming. This can be achieved by de-facto standardization of names, a centralized name registry (like a code wiki), simple naming schema (e.g. use domain name in dictionary name), secure hashes, random GUIDs, etc..

Another challenge surrounding sharing is update propagation. As much as possible, we'll want to share immutable or monotonic dictionaries that simplify caching. But ultimately a lot of [application models](ApplicationModel.md) rely on mutable dictionaries.




## Securing AO

Potentially, we might stow to a named dictionary, e.g. `{%resourceId@stowage}`. In this case, we may wish to include an HMAC in our resource IDs.

We might stow to a separate dictionary, e.g. `{%resourceId@stowage}`. For a shared stowage dictionary, we may need to address security concerns (perhaps by including a small HMAC in the resource ID). 


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
