# Awelon Object (AO)

Awelon Object (AO) is a useful set of conventions for [Awelon Bytecode (ABC)](AboutABC.md). AO defines tokens for linking and how they interact with evaluation in context of dictionaries.

A dictionary consists of a set of words and a definition for each word. Definitions are encoded in ABC. Linking a `{%word}` token substitutes that word's evaluated definition in place of the token. Dependencies between definitions must form a directed acyclic graph, such that all link tokens could be eliminated in by a finite expansion to inlined code. Use of linking has no effect on ABC's expressiveness.

Dictionaries serve as a unit of evaluation, development, linking, and distribution in AO, and as a foundation for Awelon project's various [application models](ApplicationModel.md). In general, if there are meta-level features like security or immutability, they must be specified on a per-dictionary basis.

## AO Dictionary Representation

A dictionary is an association of words to definitions, but in practice I want many other features: history and versioning, lightweight forks, simple merge, structure sharing, cache sharing, fine grained security, efficient import and export, verifiable provider-independent distribution, history compaction, and human readable and writeable with simple tools.

My general proposal is as follows:

* a dictionary is described by a series of patches
* most patches are immutable, named by secure hash
* a user named patch serves as the dictionary head
* forking a dictionary requires copying head patch
* forks are named, use `{%word@fork}` to reference

As a concerete filesystem format, a patch is a **.ao** file of form:

        secureHashOfHistory
        @word1 definition1
        @word2 definition2
        ...

Forks use human-level names, and appropriate files - e.g. `math.ao` or `myApp.ao`. Within each file, an undecorated token of form `{%word}` refers to the word as defined within that dictionary. Explicit references to a forks are possible with `{%word@math}`. 

Histories are named by secure hash. This ensures our history is deeply immutable, verifiable, provider-independent. We can search for history files anywhere then verify the result. However, token references directly to history are not supported because they would hinder compaction of history into fewer patches (e.g. an exponential decay model).

Patches operate on the word level. Each `@word` action defines the specified word, overriding the prior definition. Deletion of a word can be modeled by adding a definition, e.g. `@word {%word}`. 

The **.ao** format is not intended primarily for use directly by humans with a text editor, though it is designed to can serve in a pinch. See *Developing AO*, below.

## AO Evaluation and Linking

Dictionaries are the basic unit of AO evaluation. 

        eval :: Dictionary → Dictionary

Evaluation operates on each definition in a dictionary. This may be a lazy process, evaluating and linking only insofar as necessary for a particular observation on the dictionary. It is possible to evaluate an anonymous AO string in context of a dictionary. Doing so is equivalent to adding a new word and observing its evaluation.

### Preservation of Link Structure

Linking - the mechanical step of substituting a `{%word}` token by its evaluated definition - is performed only insofar as it enables evaluation to proceed. AO should not link if it just results in a trivial inlining of code.

When linking a `{%word@fork}`, we additionally rewrite undecorated tokens to add the `@fork` namespace. This rewrite is performed even when `@fork` names the current fork. Explicit link structure is transitively preserved.

Preserving link structure ensures human-meaningful symbols remain in our evaluated results. Further, these symbols will frequently have ad-hoc associative structure like `word.doc` and `word.type` useful for both humans and software agents that might render, extract, or otherwise interact with a definition. Preserving link structure is essential for Awelon project's application models.

### Arity Annotations

Arity annotations are defined by a set of rewrite rules of form:

        [A][B]{&arity2}     == [A][B]
        [A][B][C]{&arity3}  == [A][B][C]
        ...

Each annotation simply has the given arity. Arity annotations don't say anything about the surrounding computation. An AO runtime should support at least the practical range `{&arity1}`..`{&arity7}`. More than seven is generally impractical without a parameter object.

Arity annotations offer a simple way to control evaluation. In context of AO, use of arity annotations to control evaluation will also control *linking*, by ensuring sufficient arguments are available for computation to complete and thus (modulo type error) avoiding a mess of incomplete evaluations in a rendered result.

An AO runtime can easily track metadata about each word's arity and number of available outputs to simplify fast linking. Thus, in a `{%producer}{%consumer}` scenario, we can quickly determine whether the producer has enough data to evaluate the consumer.

### Redirects and Linker Objects

A redirect is a word that simply indicates another word. Consider:

        @foo {%foo.v99}

By the goal to preserve link structure, we'll not evaluate further. A `{%foo.v99}` result is useful for both human observers and software agents because of associative metadata. And, in the general case, our redirect might be the computed result of a conditional decision. 

However, for any context in which we link `{%foo}` we'll also want to link `{%foo.v99}`, and so on down the redirect chain. Thus, working directly with the evaluated definition is *inefficient*. It wastes our evaluator's runtime effort. Instead, we optimally want to jump straight from `{%foo}` to a directly useful result.

This concept can be generalized beyond redirects. Consider:

        @bar {&arity3} {%baz}{%qux} {&arity2}

Independent of context in which `{%bar}` is linked, we can determine that *at least three* inputs will be available to `{%baz}`. With a little static analysis, this might propagate this further to `{%qux}`. We can make some useful link decisions statically, so we aren't looking at arities at runtime. Further, we might be able to usefully inline `{%baz}` or `{%qux}` into a cached representation - a linked object used by the evaluator.

Thus, AO implies at least three useful representations per word:

* definition, provided by the programmer or external software agent
* evaluation, generated by our evaluator, preserves link structure
* linker object, via context-free static analysis and heuristics

The main heuristics for linking would regard potential inlining of words. Inlining small link objects can offer some performance advantages compared to dynamic linking at runtime. For large objects, these benefits are eventually outweighed by overheads surrounding redundant duplication. We might support extra attributes, e.g. `word.inline`, to guide link behavior.

We can support evaluation time linking via an `[program]{&link}` annotation. 

### Lightweight Staging and Compilation

A runtime can compute and bind a *linked object* to every `{%word}` token before evaluation of a program even starts, excepting for undefined words (which won't rewrite). By doing so, we can avoid ad-hoc runtime lookups once evaluation starts, and we can also bind some useful runtime metadata per word like arity.

By caching computations on words, we effectively get staged computing implicitly per word.

Staged compilation of words is similarly possible, e.g. generating an LLVM representation. This is more convenient than use of annotations like `{&jit}` because we can use a word like `word.compile` to provide ad-hoc extra directives (e.g. recommended optimization passes). When we lookup our `{%word}` token before evaluation, we can also bind the compiled representation.

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

Cached evaluation over words is a natural fit for AO. We evaluate before linking anyway, we use a word many times within a dictionary. And in contexts like Wikilon, we'll frequently look up evaluations of words based on external HTTP requests. Incremental computing is one more reason among many. 

Explicit caching will be expressed by annotation:

        [computation]{&cache}

This annotation tells our runtime to try using the cache if/when we later decide to evaluate the object. Caching doesn't force immediate evaluation. Caching may be heuristic, based on observed time/space tradeoffs. 

#### Cache Implementation

Caching can be implemented by taking a *secure hash* of the representation and performing a lookup. In case of `{%word}` tokens, we do not know whether those words would be linked during evaluation or preserved as symbols. So we must conservatively include both the `{%word}` symbol and a reference to the word's linker object. 

        [{%foo}{%bar}{%baz}]{&cache}

        cacheID = REDUCE {%foo}(foo){%bar}(bar){%baz}(baz)
            REDUCE heuristically uses inline or secure hash.
            (X) represents the cacheID for X's linked object.

Taking these constraints overall, we might assume four tables of form:

* **dictionary:** word → def. Developed by humans and software agents. 
* **clients:** word → words. Computed directly from the dictionary. 
* **word cache:** word → (eval, link, metadata). Computed by evaluator. 
* **data cache:** secure hash → def. Where we keep cached computations.

We can shove large definitions, evaluations, and linker objects into the data cache to improve structure sharing. The clients table is used to incrementally invalidate the word cache, and also is valuable for reverse lookup, renaming of words. Metadata may generally include other useful properties: inferred types, arity, date and time of evaluation, evaluation resources, debug outputs, and so on.

Cache and stowage interact in a useful way to help developers control serialization overheads when caching, e.g. in cases where a function may be used for many different cached computations. To further control serialization, developers should avoid explicitly requesting frivolous caching of cheap computations, e.g. by batching things predictably and caching computations on full batches.

*Aside:* References of form `{%word@fork}` don't need any special rules for caching. However, to improve sharing of cache between multiple similar forks, we should use the equivalent to `[word def]{&cache}`.

## Development of AO


*Note:* While humans can reasonably view and edit this code, AO is not optimized for use with a conventional filesystem and text editor. The expectation is to use a web service, FUSE view, or projectional editor. See *Developing AO*, below.

*Note:* Development metadata - bug trackers, todo lists, commit messages, edit sessions, etc. - should be modeled within a dictionary, albeit perhaps a different fork. Making development data accessible ensures development itself is a first-class application, subject to the same programmable views and extensible structure as everything else. Thus, 


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

