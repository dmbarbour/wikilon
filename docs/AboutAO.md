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

Concretely, a patch is a UTF-8 text with format:

        secureHashOfOrigin
        @word1 definition1
        @word2 definition2
        ...

Prior to the first word, we may list a single secure hash that identifies a prior patch from which we inherit words. Naming a single origin by secure hash gives us a verifiable, linear, deeply immutable, content-addressable linked list history. The body of the patch is a sequence of `@word def` updates, each overwriting a word's prior definition. In case of multiple updates to a word, the last update wins. A word may be logically deleted by writing a trivial cycle like `@foo {%foo}`. 

### Secure Hash

For the specific secure hash, I propose use of BLAKE2b 360 bit secure hash, encoded in Crockford's Base32 (favoring lower case). BLAKE2b is a very efficient secure hash. This results in 72 characters, which seems an acceptable compromise between goals for uniqueness and aesthetics. If this is ever found to be unsuitable, we can change to another secure hash easily enough.

### Anonymous and Named Dictionaries

Named dictionaries may be referenced from other dictionaries via `{%word@dict}` and are generally mutable. AO evaluation occurs in context of a set of named dictionaries - frequently an empty context. In the more general case (including patch histories) dictionaries are anonymous, and may only be referenced internally via `{%word}` tokens.

Internally, words within a dictionary are referenced by `{%word}` without the dictionary name. When we link a word from another dictionary with `{%word@dict}`, the `@dict` decorator must be appended to any undecorated words in the linked code.

### Filesystem Layer

In a filesystem layer, we'll encode a context of named dictionaries as a directory containing `myApp.ao` and the like, one file per head. The name of the dictionary matches the filename (minus the `.ao` suffix). History patchfiles may additionally use `secureHash.ao`. To avoid clutter, history files might be moved into another directory or a composite history archive. (Use of an archive simplifies memory mapping of multiple history objects that won't be mutated.)

### Forking and Merging

Forking a named dictionary is trivial: simply copy the file to a new name. A three-way merge is possible by finding a common history between two dictionaries, but is not explicitly supported. It must be represented by copying updates from one branch into the other.

### Communication

An intriguing option is use of anonymous dictionaries to model communication.

One option is message-passing. We create a *dictionary per message*. The main message content is encoded with standard words, e.g. we might use `$.head` and `$.body`. This secure hash origin can encode the *entire vocabulary* used by each message, but in a cache-friendly manner so we can reuse the vocabulary for many messages and only need to download and compile the vocabulary once. Conveniently, we might also encode defaults, e.g. so `$.head` has a value even if not specified per message. This gives us lightweight prototype or template based communications.

A related option is streaming 'updates'. This could be understood as a series `PUT` messages, with each message simply overwriting a contextually implicit or named dictionary. This is a decent fit for publish-subscribe data and views. For streaming updates, a common case is that we want to model just the patch/append/update. This behavior might be represented by use of `~` in place of the secure hash, indicating "whatever was there before". Use of `~` may generally be permitted in contexts where we update an existing dictionary.

Outside of streaming contexts, use of `~` as a stand in for a secure hash is not permitted.

*Aside:* Conveniently, AO's patch model is idempotent, and `~` patches are commutative if operating on independent subsets of the dictionary. This might be leveraged to simplify communication properties.

### Distribution

Anonymous dictionaries are distributed easily by secure hash. 

Given a secure hash for a dictionary we do not possess, we can usually ask whomever provided the secure hash to provide the dictionary. This is the primary distribution model. But content-addressed networking techniques are possible, and may be useful in distributing a network burden. A viable technique for provider independence is to just use a fraction of the secure hash for the lookup (e.g. the first 120 bits) then use the rest as a symmetric encryption key. 

Named dictionaries are generally mutable. It is feasible to distribute development of local dictionaries by use of DVCS techniques (highly recommended!) or to share common names for dictionaries in a global system. 

In the latter case, global names need some strategy to resist conflict (otherwise we'll be forced to rename some word). This might be achieved by deriving from an existing registry (e.g. ICANN), or a more informal registry (e.g. someone maintains a webpage), or taking a secure hash of a public key. Etc.. 

## AO Evaluation and Linking

Dictionaries are the basic unit of AO evaluation. 

        eval :: Dictionary → Dictionary

Evaluation operates on each definition in a dictionary. This may be a lazy process, evaluating and linking only insofar as necessary for a particular observation on the dictionary. It is possible to evaluate an anonymous AO string in context of a dictionary. Doing so is equivalent to adding a new word and observing its evaluation.

### Preservation of Link Structure

Linking - the mechanical step of substituting a `{%word}` token by its evaluated definition - is performed only insofar as it enables evaluation to proceed. AO should not link if it just results in a trivial inlining of code.

Preserving link structure ensures human-meaningful symbols remain in our evaluated results. Further, these symbols will frequently have ad-hoc associative structure like `word.doc` and `word.type` useful for both humans and software agents that might render, extract, or otherwise interact with a definition. Preserving link structure is essential for Awelon project's application models.

### Linking Between Dictionaries

AO evaluation may occur in a context of named dictionaries, which may be accessed via `{%word@dict}`. 

When linking `{%word@dict}`, we additionally rewrite undecorated tokens to include the `@dict` namespace. This is the case even when `@dict` names the current dictionary. Explicit link structure always preserves the requested dictionary name, even when multiple names refer to the same dictionary.

Rewriting is not limited to link dependencies like `{%foo} => {%foo@dict}`. We will also rewrite value sealing tokens and active debugging gates - any user defined symbols from that namespace.

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

Minimally, our linker object should flatten simple redirect chains for purpose of caching. Heuristically, we may also choose to inline words that we know will link in place of their tokens, e.g. based on total size so we don't waste effort linking smaller objects at runtime but also can share structure for large objects. 

We might support evaluation time linking via `[program]{&link}` annotation. 

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

Caching doesn't force immediate evaluation. Instead, this annotation tells our runtime to use the cache if/when we later choose to evaluate the object. Caching may be heuristic, based on observed time/space tradeoffs. 

Developers can use simple techniques such as modeling suitable 'cache points' and batches in data structures to mitigate potential efficiency issues of fine-grained caching. Careful use of stowage can also help, by reducing the serialization overhead for stowed fragments of the computation.

#### Cache Design

Caching can be implemented by taking a *secure hash* of the representation and performing a lookup. In case of `{%word}` tokens, we do not know whether those words would be linked during evaluation or preserved as symbols. So we conservatively include both the `{%word}` symbol and a reference to the word's linker object. 

        [{%foo}{%bar}{%baz}]{&cache}

        cacheID = SecureHash {%foo}{%bar}{%baz} (foo)(bar)(baz)
            where (X) is cacheID of X's linker object

Taking these constraints overall, we might assume four tables of form:

* **dictionary:** word → def. Developed by humans and software agents. 
* **clients:** word → words. Computed directly from the dictionary. 
* **data cache:** secure hash → def. Where we keep cached computations.
* **word cache:** word → (eval, link, metadata). Computed by evaluator. 

Our dictionary has a set of definitions. We maintain an index of immediate clients for each word for reverse lookup, rename, incremental cache invalidation, etc.. Our data cache is updated by the `{&cache}` annotation or a local equivalent, may be shared by multiple dictionaries, and may be maintained heuristically (e.g. random deletion is okay). 

Challenges surround maintenance of the *word cache*. Upon updating a word's definition, its linker object may be invalid, and transitively any clients of that word. 

An easy maintenance technique is perhaps to conservatively clear the word cache then rebuild it lazily. Assuming the *data cache* is used to evaluate words and linker objects, we might avoid the bulk of unnecessary recomputation. However, it might be advantageous to more precisely invalidate the word cache. This requires evaluation *during* invalidation, which has its own challenges. 

A heuristic balance of precise and lazy cache maintenance may prove effective in the general case, e.g. based on an effort quota upon performing each dictionary update. 


## AO Development

AO development is based on ABC development, but the use of words augments this in many ways:

* implicit targets for debugging, program animations
* metadata and declarations, e.g. via `word.type`
* embed tests and examples for each word in dictionary

More broadly, use of symbolic structure via words provides a convenient platform for both stateful updates and computed views. The [application model](ApplicationModel.md) represents 'applications' within a stateful codebase. This is integrated with the real-world through publish-subscribe models and other RESTful techniques.

Type declarations are something that must still be considered carefully with AO. Potential to support named types or human-level documentation for types could be very useful, as is potential to integrate additional type constraints with static type checking.

AO does introduce an interesting new 'effect' that could be tracked for type safety: a context of named dictionaries upon which a computation might depend. This is important for understanding mutability. However, this context might be restricted at an AO security layer.

### Caveats

This AO representation has many nice properties, but it isn't 100% great. Here are some weaknesses:

1. AO files and ABC aren't very convenient for direct use by humans. They can work in a pinch. But AO is intended to be manipulated primarily through editable views like [claw](CommandLine.md) or an [application model](ApplicationModel.md).
1. There is no support for *metadata* such as timestamps or commit messages. Developers are instead encouraged to represent metadata within an AO dictionary. This ensures metadata is accessible to Awelon application models or views.
1. To rename a word, developers must explicitly rewrite every reference to that word. It may be necessary to update external `{%word@dict}` references, or retrain humans, when a word is very widely used.
1. Updates apply only to whole definitions. Fortunately, it is easy to factor most AO code into smaller parts that can be updated independently, or to model a command pattern for append-only updates (cf. [application model](ApplicationModel.md).

## Constraints on Words

Words are constrained to be friendly in context of URLs, English text delimiters, HTML, [claw code](CommandLine.md), and AO dictionaries/streams. 

Summary of constraints on words:

* ASCII if alphabetical, numeral, or in -._~!$'*+:
* UTF-8 excepting C1, surrogates, replacement char
* must not start with numeral-like regex `[+-.]*[0-9]` 
* must not start or end with . or : (period or colon)
* no empty or enormous words, 1..30 bytes UTF-8.

Dictionary names must be valid words. Further, most tokens (annotations, gates, sealers, unsealers) should be valid words modulo the prefix character.
* dictionary names use the same constraints

Words may be further limited in context of a given system, e.g. unicode normalization and case folding to reduce ambiguity, forbid unicode spaces and separators. 

