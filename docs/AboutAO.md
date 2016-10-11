# Awelon Object (AO)

Awelon Object (AO) extends [Awelon Bytecode (ABC)](AboutABC.md) with a link model and representation for for symbol-structured code. AO defines tokens for linking and how they interact with evaluation in context of dictionaries.

A dictionary consists of a set of words with an ABC definition for each word. These definitions are lazily linked via ABC tokens of form `{%word}`. Additionally, AO will treat single character words as effectively extending ABC's bytecode - i.e. `xyz` is implicitly `{%x}{%y}{%z}`. Dependencies between definitions must form a directed acyclic graph, such that all defined link tokens could be eliminated, resulting in a finite (albeit exponential) expansion of code. 

AO dictionaries serve as a basis for development, evaluation, modularity, communication, and distribution. They provide a foundation for Awelon project's [application models](ApplicationModel.md). Lazy linking, in particular, is critical for many application models - it enables preservation of link structure which in turn enables AO to serve as hypermedia. 

## AO Representation

A dictionary is an association of words to definitions, and there are many merely adequate ways of representing such a thing. But I want a variety of related features: history and versioning, lightweight forks, simple merge, structure sharing, efficient communication, distribution, and compaction. Additionally, AO must preserve ABC's properties of being weakly human readable and writeable with a text editor. To achieve these features, AO has received some careful attention to representation.

The general proposal is as follows:

* dictionary is represented by tree of patches
* patches logically immutable, via secure hash
* named dictionary references via `{%word@dict}`

Concretely, a patch is a UTF-8 text with format:

        secureHashOfPatch1
        secureHashOfPatch2
        @word1 definition1
        @word2 definition2
        ...

Each patch consists of a header and a body. The header is a simple sequence of secure hashes, each identifying another patch (hence forming a tree structure). The body is a sequence of `@word def` actions, each overwriting a prior definition for the specifified word. A word may be logically deleted by defining a trivial cycle, `@foo {%foo}`. The last update for a word's definition 'wins', whether that update occurs within the header or body. We might interpret each secure hash as logically inlining the identified patch. 

This representation allows for flexible dictionary structure and merge or update models, with a fair amount of structure sharing. Secure hashes implicitly have a global namespace, so sharing of dictionaries is easy, only requiring transmission of unknown hashes. Sharing of a cache between patches is feasible.

### Secure Hash

For the specific secure hash, I propose use of BLAKE2b, 360 bits encoded in Crockford's Base32 (lower case). BLAKE2b is a very efficient secure hash, and the resulting size (72 characters) seems a reasonable compromise between uniqueness and aesthetics. If this is ever found to be unsuitable, we can change to another secure hash easily enough (with a small header if necessary).

### Anonymous and Named Dictionaries

AO evaluation may occur in a context of named dictionaries. In that case, each dictionary serves as an implicit namespace, with `{%word}` accessing the word as defined in the current dictionary and `{%word@dict}` indicating a word in an external dictionary. (Even if you name the current dictionary, it is treated as external with the `@dict` modifier.) However, in the general case dictionaries are anonymous. Patches cannot be named directly, i.e. the `@dict` cannot be a secure hash.

Best practices for named dictionaries have yet to be developed. However, the intention is to use named dictionaries *not* as named libraries or modules (we can always glue those together anonymously), but rather at an [application layer](ApplicationModel.md) where security and access control become concerns.

### Communication

An intriguing option is use of anonymous dictionaries to model communication.

One option is message-passing. We create a *dictionary per message*. The main message content is encoded with standard words, e.g. we might use `$.head` and `$.body`. This secure hash origin can encode the *entire vocabulary* used by each message, but in a cache-friendly manner so we can reuse the vocabulary for many messages and only need to download and compile the vocabulary once. Conveniently, we might also encode defaults, e.g. so `$.head` has a value even if not specified per message. This gives us lightweight prototype or template based communications.

A related option is streaming 'updates'. This could be understood as a series `PUT` messages, with each message simply overwriting a contextually implicit or named dictionary. This could be used to model time-varying objects, for example, and is suitable for publish-subscribe data and spreadsheet-like computations. Conveniently, it is possible to transition as needed between rewriting large objects and updating just the essential fragments.

### Filesystem Layer

In a filesystem, we might encode a context of named dictionaries as a directory containing patch files with suffix **.ao**. For example, `myApp.ao` allowing for `{%word@myApp}`. A named dictionary patch might consist of just the secure hash for an anonymous patch, but the easiest way to hand-update a dictionary in the filesystem is certainly just to addend the primary patch file.

Anonymous patch files will use `secureHash.ao`, but may be held in a separate directory or archive files to control clutter. The exact filesystem structure will depend on the tooling. Internally, patches themselves are independent of the filesystem layer.

*Aside:* In addition to **.ao** files, a runtime might treat a subset of the filesystem itself as a named dictionary. For example, `{%foo@fs/bar}` might refer the file `foo` in directory `bar`. This is potentially convenient for processing large binary and text data.

### Forking and Merging

Forking a named dictionary is trivial: simply copy the dictionary then begin updating. A two-way merge between dictionaries is always possible, identifying word-level differences. A three-way merge is possible only if there is a readily identified common patch history between two dictionaries, which depends on the update model.

### Distribution

Anonymous dictionaries are distributed easily by secure hash. Given a secure hash for a dictionary we do not possess, we can ask whomever provided the secure hash to provide the patch. This is the primary distribution model of AO. But content-addressed networking techniques are possible, and may be useful in distributing a network burden. A viable technique for provider independent transport is to just use a fraction of the secure hash for lookup (e.g. the first 120 bits) then use the rest as a symmetric encryption key. 

Named dictionaries are generally mutable. It is feasible to distribute development of named dictionaries by use of DVCS techniques or to share names for dictionaries globally. In the latter case, global names may need some strategy to resist conflict. This might be achieved by deriving from an existing registry (e.g. ICANN or Namecoin), or a more informal registry (e.g. someone maintains a webpage), or taking a secure hash of a public key. Etc.. 

## AO Evaluation and Linking

Dictionaries are the basic unit of AO evaluation. AO evaluation rewrites an AO dictionary into a different representation of the same AO dictionary. 

        eval :: Dictionary → Dictionary

In practice, this evaluation may be lazy, i.e. such that we don't evaluate a subset of a dictionary unless relevant to an external agent, an incoming or anticipated query. Such queries might consist of evaluating an ABC program within the dictionary.

### Preservation of Link Structure

Linking - the mechanical step of substituting a `{%word}` token by its evaluated definition - is performed only insofar as it enables evaluation to proceed. AO should not link if it just results in a trivial inlining of code.

Preserving link structure ensures human-meaningful symbols remain in our evaluated results. Further, these symbols will frequently have ad-hoc associative structure like `word.doc` and `word.type` useful for both humans and software agents that might render, extract, or otherwise interact with a definition. Preserving link structure is essential for Awelon project's application models.

### Distributed Linking and Localization

AO evaluation may be understood as occurring in a system of 'named' dictionaries, or alternatively in one much larger dictionary with namespaces. Either way, tokens of form `{%word@dict}` effectively resolve to an external definition for a word. 

Assume we resolve `{%word@dict}` to ABC code `x{%bar@baz}{%foo}`. We must generally rewrite that code to specify its namespace, adding the `@dict` qualifier to every undecorated link token, including implicit link tokens). Logically, we have `{%x@dict}{%bar@baz}{%foo@dict}`. 

Localization is separate from linking.

When a runtime system knows `{%foo@dict}` is defined and equivalent to the local `{%foo}`, it may *localize* by rewriting the former to the latter. Localization sacrifices potentially useful link structure. For example, there is no assurance that `{%foo.doc@dict}` is equivalent to `{%foo.doc}`. But we can assume that, if documentation is necessary for `{%foo}`, we probably have our own. Hence, we localize metadata while preserving semantics. 

Localization enables more efficient and aesthetic relationships between AO systems to the extent vocabulary is shared. 

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

For big data in a small working memory, it is useful to reverse the linking process: move data to disk, and replace the in-memory representation by a small token that may be used to dynamically link the data. I call this pattern 'stowage'. 

In context of AO, stowage involves creating new word tokens during evaluation.
        
        [large value]{&stow}  == [{%resourceId}]
        [small value]{&stow}  == [small value]

Here `{%resourceId}` is a word whose definition is equivalent to `large value`. Stowage has overhead, so an evaluator must make a heuristic decision about whether to stow depending on value size. Thus, smaller values should not be stowed. 

Stowage works best with persistent data structures, where updates to the structure only require updates to a small subset of nodes. Stowage works even better with log-structured merge trees and similar structures where updates are implicitly batched and most updates are shallow.

The naming of stowed resources is left to the runtime, but stable names should be favored to simplify caching and structure sharing.

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

*Aside:* When computing cacheID of a linker object, we might hide symbols for tokens that we know will be linked (i.e. anything we could have inlined). Doing so might improve reuse of cache independent of names. 

### Accelerated Dictionary

An AO runtime will generally include an anonymous, accelerated dictionary - a set of words with definitions whose implementations may be hand-optimized. Other dictionaries may then derive from the accelerated dictionary. Recognizing accelerated definitions is most readily performed via the word cache, e.g. we can recognize whenever a word's cacheID matches the same from our accelerated dictionary, and if so use the accelerated implementation.

## AO Development

AO development is based on ABC development, but the use of words augments this in many ways:

* implicit targets for debugging, program animations
* metadata and declarations, e.g. via `word.type`
* embed tests and examples for each word in dictionary

More broadly, use of symbolic structure via words provides a convenient platform for both stateful updates and computed views. The [application model](ApplicationModel.md) represents 'applications' within a stateful codebase. This is integrated with the real-world through publish-subscribe models and other RESTful techniques.

Type declarations are something that must still be considered carefully with AO. Potential to support named types or human-level documentation for types could be very useful, as is potential to integrate additional type constraints with static type checking.

AO does introduce an interesting new 'effect' that could be tracked for type safety: a context of named dictionaries upon which a computation might depend. This is important for understanding mutability. However, this context might be restricted at an AO security layer.

### Caveats

This AO representation has many nice properties, but has some weaknesses:

1. AO files and ABC aren't very convenient for direct use by humans. They can work in a pinch, but AO is intended to be manipulated primarily through editable views like [claw](CommandLine.md) or an [application model](ApplicationModel.md).
1. There is no support for *metadata* such as timestamps, commit messages, or bug tracking. Instead, the intention is that AO development environments should represent such metadata within an AO dictionary (perhaps an auxiliary named dictionary). This ensures metadata is accessible to Awelon application models or views.
1. To rename a word requires rewriting every reference to that word. This might be performed by a development environment, but is not a first-class feature of the AO representation.
1. Updates apply only to whole definitions. Fortunately, it is easy to factor large AO code into small words that can be updated independently when need arises. Append only updates can be modeled by a command pattern (cf. [application model](ApplicationModel.md)).

## Constraints on Words and Tokens

Words are weakly constrained to fit tokens, control size, and support wrappers or extensions.

* no empty or enormous words; 1..30 bytes valid UTF-8
* no C0, SP, `@[]{}<>(|),;"`, DEL, C1, replacement char

However, these constraints do not ensure a clean embedding in URLs, HTML, English text, and so on. So there may be other limitations in context. For example, a [claw view](CommandLine.md) would require the `{%word}` token wrapper for any word that might be confused with a number when parsed.

Words `a`, `b`, `c`, and `d` are valid, but are implicitly defined as the four ABC primitives and may not be redefined. That is, `{%a}` is equivalent to `a`.

Dictionary names must also be valid words. And ABC tokens (annotations, gates, sealers, unsealers) should be valid words modulo the prefix character. 


