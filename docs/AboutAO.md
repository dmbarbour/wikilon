# Awelon Object (AO)

Awelon Object (AO) is a useful set of conventions for [Awelon Bytecode (ABC)](AboutABC.md). AO defines tokens for linking and how they interact with evaluation in context of dictionaries.

A dictionary consists of a set of words and a definition for each word. Definitions are encoded in ABC. Linking a `{%word}` token substitutes that word's evaluated definition in place of the token. Dependencies between definitions must form a directed acyclic graph, such that all link tokens could be eliminated in by a finite expansion to inlined code. Use of linking has no effect on ABC's expressiveness.

Dictionaries serve as a unit of evaluation, development, linking, and distribution in AO, and as a foundation for Awelon project's various [application models](ApplicationModel.md). In general, if there are meta-level features like security or immutability, they must be specified on a per-dictionary basis.

## AO Evaluation and Linking

Dictionaries are the basic unit of AO evaluation. 

        eval :: Dictionary → Dictionary

Evaluation operates on each definition in a dictionary. This may be a lazy process, evaluating and linking only insofar as necessary for a particular observation on the dictionary. It is possible to evaluate an anonymous AO string in context of a dictionary. Doing so is equivalent to adding a new word and observing its evaluation.

### Preservation of Link Structure

Linking - the mechanical step of substituting a `{%word}` token by its evaluated definition - is performed only insofar as it enables evaluation to proceed. AO will not link if it just results in a trivial inlining of code.

Preserving link structure ensures human-meaningful symbols remain in our evaluated results. Further, these symbols will frequently have ad-hoc associative structure like `word.doc` and `word.type` useful for both humans and software agents that might render, extract, or otherwise interact with a definition. Preserving link structure is essential for Awelon project's application models.

### Lightweight Staged Evaluation

AO will link the *evaluated* definition for a given word. If we did link the original definition, we'd end up recomputing that fragment, so there is no semantic impact of this decision. But it does have a number of advantages:

* evaluated result is necessary for effective linking decisions
* evaluations cached for multiple use and incremental computing
* predictable basis for staged computations; no deep evaluation
* quota for each word's evaluation; can tune using `word.quota`

An interesting feature (IMO) is that, when lightweight staging and caching is combined with `{&jit}` compilation, we effectively get staged compilation without ever leaving our programming language. Also, static type inference can be applied to the evaluated definition. Doing so allows for very flexible staged construction.

### Arity Annotations

Arity annotations are defined by a set of rewrite rules of form:

        [A][B]{&arity2}     == [A][B]
        [A][B][C]{&arity3}  == [A][B][C]
        ...

Each annotation simply has the given arity. Arity annotations don't say anything about the surrounding computation. An AO runtime should support at least the practical range `{&arity2}`..`{&arity7}`. Less than two is unnecessary, and working with more than seven arguments is impractical.

Arity annotations offer a simple way to control evaluation. In context of AO, use of arity annotations to control evaluation will also control *linking*, by ensuring sufficient arguments are available for computation to complete and thus (modulo type error) avoiding a mess of incomplete evaluations in a rendered result.

An AO runtime can easily track metadata about each word's arity and number of available outputs to simplify fast linking. Thus, in a `{%producer}{%consumer}` scenario, we can quickly determine whether the producer has enough data to evaluate the consumer.

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

Cached evaluation over words is a natural fit for AO. We evaluate before linking anyway, and we might use a word many times. Even without mutation, we're likely to evaluate a word many times due to external observations (e.g. an HTTP web service). Incremental computing is just one more reason among many to cache evaluation of words.

Explicit caching can be expressed by annotation, i.e. `[computation]{&cache}`. The `{&cache}` annotation does not force evaluation. But when evaluation does occur, the evaluator will first search its cache. If the cached result is not available, we'll perform the computation then decide heuristically whether to add it to the cache (based on time/space tradeoffs). 

Serializing computations has overhead, and caching is wasted if the computation is cheap. Developers can improve cache efficiency by careful use of stowage (to reduce serialization overheads) and by making suitable 'cache points' more explicit in their data structures. That can generally be achieved by buffering of recent updates into larger batches, such that we cache on the older data and recompute from the buffer.

### Gate Configuration and Debugging

ABC uses a concept of configurable `{@gate}` tokens for debug output. An interesting possibility is to model this output as part of the AO dictionary. But I'm not convinced it's a good idea. It might be better to have debug views for evaluations on a specific word.

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
