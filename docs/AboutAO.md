# Awelon Object (AO)

Awelon Object (AO) is a set of conventions for structured use of [Awelon Bytecode (ABC)](AboutABC.md). AO defines tokens for linking and how they interact with evaluation in context of dictionaries.

A dictionary consists of a set of words and a definition for each word. Definitions are encoded in ABC. Tokens of form `{%word}` token will logically substitute that word's definition in place of the token. Dependencies between definitions must form a directed acyclic graph, such that all link tokens could be eliminated in by a finite expansion of inlined code.

Dictionaries serve as a unit of development, evaluation, linking, communication, and distribution. They provide a foundation for Awelon project's [application models](ApplicationModel.md). 

## AO Representation

A dictionary is an association of words to definitions, and there are many merely adequate ways of representing such a thing. But I want a variety of related features: history and versioning, lightweight forks, simple merge, structure sharing, cache sharing, fine grained security, efficient import and export, communication, distribution, and compaction. Additionally, AO must preserve ABC's properties of being weakly human readable and writeable with a text editor. To achieve these features, AO has received some careful attention to representation.

The general proposal is as follows:

* dictionary is described by a series of patches
* history patches are immutable, via secure hash
* named dictionary references via `{%word@dict}`

Concretely, a patch is a string or file with format:

        secureHashOfOrigin
        @word1 definition1
        @word2 definition2
        ...

On our first line, we have opportunity to specify a secure hash specifying the prior patch. Naming origin by secure gives us a verifiable, deeply immutable, content-addressable linked list history. To keep it simple, each patch is limited to a single origin, and origin is always immutable. 

The body of any patch is a sequence of `@word def` actions, each updating a word's definition. Last update wins. To logically delete a word, we may define it as a trivial cycle `@word {%word}`. (Non-trivial cycles should be reported as errors.)

### Anonymous Dictionaries

In the general case, dictionaries are anonymous, in the sense that they cannot be referenced via `{%word@dict}`. Secure hashes are *explicitly* considered anonymous in this sense. This ensures we can compact and garbage collect large histories, filter spam or confidential data, etc.. 

### Named Dictionaries

Named dictionaries may be referenced from other dictionaries via `{%word@dict}` and are generally mutable. AO evaluation occurs in *context* of a set of named dictionaries. It is possible for multiple names to refer to the same dictionary. However, which name is used will be preserved during evaluations. 

Depending on [application model](ApplicationModel.md), relationships between dictionaries may be constrained structurally by the update system. For example, dictionary `foo` may be permitted to reference `bar` but not vice versa.

### Secure Hash

I propose use of BLAKE2b 360 bit secure hash, encoded in Crockford's Base32 (favoring lower case). BLAKE2b is a very efficient secure hash, and the limit of 72 characters seems an acceptable balance between goals for global uniqueness and aesthetics.

### Filesystem 

At the filesystem layer, we'll encode a context of named dictionaries as a directory containing `myApp.ao` and the like. The name of the dictionary matches the filename (minus the **.ao** suffix). 

History patch files use `secureHash.ao`, but we move them into an archive or separate directory. Archiving history files together could prove convenient for lightweight indexed access - i.e. we can just `mmap` the entire archive and index it as a whole, and we don't need to mess around unnecessarily with file handles and so on.

(TODO: review `.zip`, `.tar`, and `.dar` archive formats as candidates.)

### Forking and Merging

Forking a named dictionary is trivial: simply copy the file to a new name. A three-way merge is possible by finding a common history between two dictionaries.

### Communication

An intriguing option is use of anonymous dictionaries to model communication.

One option is message-passing. We create a dictionary per message, encoding the message as an object within the dictionary with a standard name. For example, we might reserve `$`, `$.head`. `$.body`, and so on for this use. The message's secure hash origin then encodes a common vocabulary that can be reused for many messages. The origin may also provide useful 'default' attributes - i.e. a prototype object. The remote system can easily download, cache, and compile the standard vocabulary.

A related option is streaming 'updates'. This could be understood as a series `PUT` messages, with each message simply overwriting the prior and thus modeling a time-varying object. A common case would be we want to reference the current dictionary as our 'origin' then provide only the delta. To simplify this, we could permit use of `~` in place of a secure hash to represent a reference to the current dictionary. Conveniently, the result of PUT with `~` doubles as a general append/patch behavior due to AO semantics.

Outside of a streaming context, use of `~` as a stand in for a secure hash would not be permitted.

### Distribution

Anonymous dictionaries are distributed easily by secure hash. 

Given a secure hash for a dictionary we do not possess, we can usually ask whomever provided the secure hash to provide the dictionary. This is the primary distribution model. But content-addressed networking techniques are possible, and may be useful in distributing a network burden. A viable technique for provider independence is to just use a fraction of the secure hash for the lookup (e.g. the first 120 bits) then use the rest as a symmetric encryption key. 

Named dictionaries are generally mutable. It is feasible to distribute development of local dictionaries by use of DVCS techniques (highly recommended!) or to share common names for dictionaries in a global system. 

In the latter case, global names need some strategy to resist conflict (otherwise we'll be forced to rename some word). This might be achieved by deriving from an existing registry (e.g. ICANN), or a more informal registry (e.g. someone maintains a webpage), or taking a secure hash of a public key. Etc.. 

### Caveats

This AO representation has many nice properties, but it is missing a few.

First, there is no support for *rename*. To rename a word, developers must explicitly rewrite every reference to that word. Explicit rename is cheap enough until widely used. But the general case for rename is greatly complicated by potential for external references (like `{%foo@dict}`, human knowledge, bookmarks).

Second, there is no support for modifying part of a definition. Fortunately, there are easy work arounds to this one. To patch a definition, first factor it into small named pieces then update a few specific pieces. To append a definition, consider a command pattern of form:

        @foo.v98 ...
        @foo.v99 {%foo.v98}(update98)
        @foo {%foo.v99}

Third, there is no support for *metadata*. There is no place for commit messages, blame, etc.. Developers are instead encouraged to model metadata within the dictionary or an auxiliary. For example, we might create a dictionary named `tracker` for our bug tracking, commit messages, edit sessions, and so on. Making development metadata explicit is necessary for development to become a first-class application.

Finally, while raw AO can be viewed and edited in small doses, it's still a bytecode. It isn't a convenient view for humans. The curly braces grow annoying, and too much file hopping is needed. The expectation is humans will work with AO primarily through editable views like [claw](CommandLine.md) or an [application model](ApplicationModel.md). 

## AO Development

AO development is based on ABC development, but the use of words augments this in many ways:

* implicit targets for debugging, program animations
* metadata and declarations, e.g. via `word.type`
* embed tests and examples for each word in dictionary

More broadly, use of symbolic structure via words provides a convenient platform for both stateful updates and computed views. The [application model](ApplicationModel.md) represents 'applications' within a stateful codebase. This is integrated with the real-world through publish-subscribe models and other RESTful techniques.

AO does introduce an interesting new 'effect' that could be tracked for type safety: a context of named dictionaries upon which a computation might depend. This is important for understanding mutability. However, this context might be restricted at an AO security layer.

## AO Evaluation and Linking

Dictionaries are the basic unit of AO evaluation. 

        eval :: Dictionary → Dictionary

Evaluation operates on each definition in a dictionary. This may be a lazy process, evaluating and linking only insofar as necessary for a particular observation on the dictionary. It is possible to evaluate an anonymous AO string in context of a dictionary. Doing so is equivalent to adding a new word and observing its evaluation.

### Preservation of Link Structure

Linking - the mechanical step of substituting a `{%word}` token by its evaluated definition - is performed only insofar as it enables evaluation to proceed. AO should not link if it just results in a trivial inlining of code.

When linking a `{%word@dict}`, we must additionally rewrite undecorated tokens to add the `@dict` namespace. This rewrite is performed even when `@dict` names the dictionary we are currently evaluating. Explicit link structure is transitively preserved.

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

#### Cache Design

Caching can be implemented by taking a *secure hash* of the representation and performing a lookup. In case of `{%word}` tokens, we do not know whether those words would be linked during evaluation or preserved as symbols. So we must conservatively include both the `{%word}` symbol and a reference to the word's linker object. 

        [{%foo}{%bar}{%baz}]{&cache}

        cacheID = REDUCE {%foo}(foo){%bar}(bar){%baz}(baz)
            REDUCE uses inline or secure hash.
            (X) represents X's linked object.

Taking these constraints overall, we might assume four tables of form:

* **dictionary:** word → def. Developed by humans and software agents. 
* **clients:** word → words. Computed directly from the dictionary. 
* **word cache:** word → (eval, link, metadata). Computed by evaluator. 
* **data cache:** secure hash → def. Where we keep cached computations.

We can shove large definitions, evaluations, and linker objects into the data cache to improve structure sharing. The clients table is used to incrementally invalidate the word cache, and also is valuable for reverse lookup, renaming of words. Metadata may generally include other useful properties: inferred types, arity, date and time of evaluation, evaluation resources, debug outputs, and so on.

Cache and stowage interact in a useful way to help developers control serialization overheads when caching, e.g. in cases where a function may be used for many different cached computations. To further control serialization, developers should avoid explicitly requesting frivolous caching of cheap computations, e.g. by batching things predictably and caching computations on full batches.

*Aside:* References of form `{%word@fork}` don't need any special rules for caching. However, to improve sharing of cache between multiple similar forks, we should use the equivalent to `[word def]{&cache}`.


## Constraints on Words and Definitions

Words are constrained to be friendly in context of URLs, English text delimiters, HTML, [claw code](CommandLine.md), and AO dictionaries/streams. 

Summary of constraints on words:

* ASCII if alphabetical, numeral, or in -._~!$'*+:
* UTF-8 excepting C1, surrogates, replacement char
* must not start with numeral-like regex `[+-.]*[0-9]` 
* must not start or end with . or : (period or colon)
* no empty or enormous words, 1..30 bytes UTF-8.
* dictionary names use the same constraints

Words may be further limited in context of a given system, e.g. unicode normalization and case folding to reduce ambiguity, forbid unicode spaces and separators. 

