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

Serializing computations has overhead, and caching is wasted if the computation is cheap. Developers can improve cache efficiency first by careful use of stowage to reduce serialization overheads, and second by making suitable 'cache points' more explicit in their data structures. That can generally be achieved by buffering of recent updates into larger batches, such that we cache on the older data and recompute from the buffer.

### Cached Evaluation Strategy

Caching in AO is complicated by:

* preservation of link structure 
* mutability of word definitions 
* forked or versioned dictionary
* application refactoring and GC
* definition not always observed
 * outputs like `[{%foo} #42]`
 * link fails on arity, outputs
 * redirects like `@foo {%foo.v99}`
* time vs. space trade for cache

Optimally, I want a cache that supports minimal evaluation on update, and that does not propagate updates any further than necessary, such that irrelevant changes (due to refactoring, added redirects, conditional paths not taken, data plumbing of changed definitions) does not cause recomputation. 

Further, the cache must support multiple versions of code, such that we can continue to use old versions of code after an update.


the following features:

* minimal re-evaluation on update


must preserve `{%foo}` in output for debugging, AO apps
* tokens such as `{%foo}` may refer to mutable definitions
* 


. We have tokens such as `{%foo}` in our code whose definition is, in the general case, mutable. We must preserve the token `{%foo}` because it's valuable for comprehension, debugging, and associative data.

 Important points:

* evaluation containing `{%foo}` may observe the *value* of `{%foo}`.
* it 

 Evaluation of an expression involving `{%foo}` potentially depends on the *value* of `{%foo}`. 

This section is a simple proposal that should work pretty well:

* in runtime `{%foo}` token has hidden data:
 * result of `[foo evaluated def]{&stow}`
 * fast linker metadata - arity, outputs
 * if foo is redirect, bind redirect target
 * deep link structure preserved in stowage

* we maintain cache of AO words to metadata
 * cache is incomplete, lazily reconstructed
 * rebuild using `[word original def]{&cache}` 
 * used to bind `{%foo}` to hidden data
 * tracks failed evaluations (errors, cycles, etc.)
 * may track time/space evaluation efforts

* also have reverse lookup (word → clients) table
 * enable transitive invalidation of word cache
 * transitively invalidate on definition update
 * may transitively invalidate to recover space
  * requires some good usage metrics

Explicit `[computation]{&cache}` is simple, leveraging that contained `{%foo}` tokens contain a deeply immutable stowage identifiers. So we don't need to perform any lookups outside the cache table.

* computation is serialized as if for stowage
* use secure hash on serialization (if large)
* map serialization to stowed evaluation

The disadvantage of this design is that AO word evaluations are cached even in cases where recomputation would be cheaper than the space cost. The AO word cache must also be maintained explicitly. 

OTOH, our tokens will frequently bind small definitions directly (since `{&stow}` doesn't always go to disk), and lookups will be efficient and simple. I suspect the simplicity benefits will outweigh the potential space hit from storing an extra representation for every definition.

## Dictionary Representation

I assume a common pattern during development will involve forking a dictionary, performing a few simple edits, observing how those changes will affect our evaluated results, and occasionally merging updates back. It's convenient if our AO dictionary representation supports this behavior efficiently.

### Multi-Dictionary Contexts

As a simple convention, we can take `{%word@dict}` to refer to a word in a named dictionary. Every dictionary acts implicitly as a namespace, such that within dictionary `myMath` the token `{%multiply}` may logically be rewritten to `{%multiply@myMath}`. Our evaluation context then becomes a set of named dictionaries. 

For AO layer purposes, the set of named dictionaries is effectively one composite dictionary. But modeling multiple dictionaries can be convenient in other layers - e.g. security, versioning, representation. For example, in a filesystem, we might use one file per named dictionary. We can support features like prototypal inheritance of dictionaries. We can name component dictionaries with a secure hash to guard immutability while leaving the rest mutable. 

### Dictionary Inheritance

A useful technique is to say: "this dictionary is the same as that one, but with the following tweaks".


*Aside:* I think formal semantics should not depend on ad-hoc conventions. I've been tempted to support dictionary inheritance via ad-hoc conventions like defining an `inherit` word to a value with the dictname.

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

### Active Debugging

Developers should be able to evaluate at least a single word or expression in 'debug' modes using `{@gate}` tokens and a provided configuration. It would be nice if we can automatically produce 'animated' evaluations for any word, and cache debug outputs review.

### AO Type Safety

The rich structure of AO can greatly improve type safety analysis. We can declare or constraint types using `word.type` attributes. A word linked in many places enables constraint unification. Heuristic techniques like SHErrLoc can effectively wield a multi-use context.

Strong, static type safety with a simple algorithm makes a good default. But for the general case of metaprogramming (e.g. turning a text into a program) we will need either dependent types or dynamic types.

As a general rule, we could perform type safety analysis *after* evaluation has the potential to eliminate unnecessary elements. This would allow us

Interestingly, we might defa

Type declarations could tweak this:

 Type declarations can potentially expand this further: 

Words should default to strong static types with a de-facto standard type inference. However, 

, an interesting possibility is to leave some words explicitly dynamic (or dependent) in their effect on program structure to simplify ad-hoc metaprogramming. This intention could readily be declared via attribute. 

### Sharing and Composing AO

Topics:
* extraction
* merging
* forking
* security
* connectivity
* immutability

I wonder how much security can be achieved by constraining connectivity, how much by unforgeable capabilities.



### AO Layer Security

Potentially, we might stow to a named dictionary, e.g. `{%resourceId@stowage}`. In this case, we may wish to include an HMAC in our resource IDs.

We might stow to a separate dictionary, e.g. `{%resourceId@stowage}`. For a shared stowage dictionary, we may need to address security concerns (perhaps by including a small HMAC in the resource ID). 


## Development Idioms

### Leveraging Undefined Words

Undefined words can serve a useful role in a development context. They serve as 'holes'. An ABC system might help developers discover definitions based on inferred type and usage. Or we might just use the words as undefined symbols for some symbolic partial evaluation.



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

