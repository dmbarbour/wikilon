# Awelon Object (AO)

Awelon Object (AO) extends [Awelon Bytecode (ABC)](AboutABC.md) with a link model and representation for for symbol-structured code. AO defines tokens for linking and how they interact with evaluation in context of dictionaries.

A dictionary consists of a set of words with an ABC definition for each word. These definitions are lazily linked via ABC tokens of form `{%word}`. Additionally, AO treats bytecodes as single character words - i.e. `xyz` is implicitly `{%x}{%y}{%z}`. Dependencies between definitions must form a directed acyclic graph, such that all defined link tokens could be inlined, resulting in a finite (albeit exponential) expansion into raw ABC.

AO dictionaries serve as a basis for development, evaluation, modularity, communication, and distribution. They provide a foundation for Awelon project's [application models](ApplicationModel.md). Lazy linking, in particular, is critical for many application models. It enables preservation of link structure which, in addition to being important for performance of large scale computing, enables AO to serve a foundation for hypermedia. 

## AO Words

Words are weakly constrained (to support AO wrappers, present and future). In addition to the normal limits on tokens (valid UTF-8, no curly braces, control characters, or replacement character), AO forbids characters SP, `@[]<>(),;|"` within words. Also, the empty word is prohbited.

Words `a`, `b`, `c`, and `d` are valid, but refer always to the four corresponding ABC primitives and may not be redefined. These are effectively the four 'keywords' of AO.

Developers are encouraged to favor words that are convenient in external contexts like URLs, natural language documentation, [editable views](EditingAO.md), and so on.

## AO Development

AO development consists of dictionary construction, sharing, and maintenance. 

Humans will directly develop AO primarily through [editable views](EditingAO.md), which must be defined within a dictionary and rendered through an intermediate service. A web service is likely, but a [filesystem in userspace](https://en.wikipedia.org/wiki/Filesystem_in_Userspace) adapter could enable developers to leverage emacs, vim, and so on. This is essentially a [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html) pattern, with the AO dictionary as the storage layer.

Indirectly, dictionaries are also developed through the Awelon project [application model](ApplicationModel.md). Software agents and humans both systematically manipulate the dictionary via RESTful patterns, modeling databases, work orders, and publish-subscribe patterns as a basis for real-time systems with real-world effects. In this context, a dictionary serves as a substrate for applications much like filesystems do conventionally, except with linking, computation, composition, and spreadsheet-like characteristics built in.

See linked documents on editable views and the application model for details. 

## AO Representation

A dictionary is an association of words to definitions, and there are many merely adequate ways of representing such a thing. But I want a variety of related features: history and versioning, lightweight forks, simple merge, structure sharing, efficient communication, distribution, and compaction. Additionally, AO must preserve ABC's properties of being weakly human readable and writeable with a text editor. To achieve these features, AO has received some careful attention to representation.

The general proposal is as follows:

* dictionary is represented by tree of patches
* patches logically immutable, via secure hash

Concretely, a patch is a UTF-8 text with format:

        secureHashOfPatch1
        secureHashOfPatch2
        @word1 definition1
        @word2 definition2
        ...

Each patch consists of a header and a body. The header is a simple sequence of secure hashes, each identifying another patch (hence forming a tree structure). The body is a sequence of `@word def` actions, each overwriting a prior definition for the indicated word. A word may be logically deleted by defining a trivial cycle, `@foo {%foo}`. The last update for a word's definition 'wins', whether that update occurs within the header or body. We might interpret each secure hash as logically inlining the identified patch. 

This representation allows for flexible dictionary structure and merge or update models, with a fair amount of structure sharing, and potential index sharing (depending on the nature of the updates). Secure hashes implicitly have a global namespace, so sharing of dictionaries is easy, only requiring transmission of unknown hashes. 

### Secure Hash

For the specific secure hash, I propose use of BLAKE2b, 360 bits encoded in Crockford's Base32 (lower case). BLAKE2b is a very efficient secure hash, and the resulting size (72 characters) seems a reasonable compromise between uniqueness and aesthetics. If this is ever found to be unsuitable, we can change to another secure hash easily enough (with a small header if necessary).

### Filesystem Layer

AO doesn't rely on a filesystem but is easily represented within it. I propose use of the **.ao** suffix for AO patch files. For a dictionary head, we can simply use a human provided name - e.g. `myApp.ao`. Optimally, this head would simply consist of a single secure hash. Hand editing is okay, but might require rehashing, building indices and so on.

Anonymous, immutable patch files would be named by `secureHash.ao`. Due to their immutable nature and easy verifiability by name, it doesn't really matter if they're stored in the same directory, a subdirectory, or even an archive, so long as we know where to find them. 

*Aside:* I am interested in developing an archive format for AO, in part to simplify memory mapping and potential indexing of large volumes of files together. In this case, I might simply use the ZIP file format, but use a .pao (packaged AO) file extension, and maybe add some conventions like a manifest file and certificates (per JAR and APK conventions).

*Note:* If a filesystem ever becomes a primary storage layer, we'll need to also include indices, caches, and optimized representations (like JIT code) in a manner that supports incremental processing.

### Forking and Merging

Forking a named dictionary is trivial: simply copy the dictionary then begin updating. A two-way merge between dictionaries is always possible, identifying word-level differences. A three-way merge is possible only if there is a readily identified common patch history between two dictionaries, which depends on the update model.

### Provider Independent Distribution

If we receive a secure hash for a dictionary/patch we do not possess, we should be able to ask whomever provided the secure hash to provide the definition. We can download it as needed, or cache it for future use. But we also have an option to download the same patch from an external system - a content distribution network. Doing so would reduce network burden on a central provider.

With a content distribution network, privacy and security becomes a concern. A useful technique is to use only a fragment of the secure hash to identify it on the network, and the rest as a symmetric encryption key.

### Limitations and Workarounds

There is no support built in to the AO representation for development metadata like commit messages or timestamps. This metadata, if desired at all, should instead be made explicit in the dictionary by some recognizable convention. The intention is that this metadata should be accessible to the [Awelon application model](ApplicationModel.md).

There is no support to update only part of a definition. This limitation is easy to address by factoring fragments that change independently into separate words. ABC's concatenative structure makes this easy. In case of regularly appending a definition, the application model command pattern is applicable.

There is no support for external references. All `{%word}` tokens must be part of the same dictionary. Communicating with the outside world, including other dictionaries, must be performed indirectly via the application model effects layer. *Aside:* I originally pursued `{%word@dict}` for binding external resources, but it's ugly, complicated, and doesn't generalize nicely.

There is no special support to rename words. Renaming can be expressed by explicitly updating every relevant word. Fortunately, this process is easily automated. *Aside:* Avoiding rename simplifies indexing and idempotence of AO patches.

## AO Evaluation and Linking

Dictionaries are the basic unit of AO evaluation. AO evaluation rewrites an AO dictionary into a different representation of the same AO dictionary. 

        eval :: Dictionary → Dictionary

In practice, this evaluation may be lazy, i.e. such that we don't evaluate a subset of a dictionary unless relevant to an external agent, an incoming or anticipated query. Such queries might consist of evaluating an ABC program within the dictionary.

### Preservation of Link Structure

Linking - the mechanical step of substituting a `{%word}` token by its evaluated definition - is performed only insofar as it enables evaluation to proceed. AO should not link if it just results in a trivial inlining of code.

Preserving link structure ensures human-meaningful symbols remain in our evaluated results. Further, these symbols will frequently have ad-hoc associative structure like `word.doc` and `word.type` useful for both humans and software agents that might render, extract, or otherwise interact with a definition. Preserving link structure is essential for Awelon project's application models.

Developers may freely leverage *arity annotations* to control preservation of link structure.

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

Here `{%resourceId}` is a word whose definition is equivalent to `large value`. Stowage has overhead, so an evaluator must make a heuristic decision about whether to stow depending on value size. Smaller values should not be stowed. 

Stowage works best with persistent data structures, where updates to the structure require updates only to a small subset of nodes. Stowage works even better with log-structured merge trees and similar structures where updates are implicitly batched and most updates are shallow. Stowage can also be used optimize caching, even if the stowage is not itself optimal.

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

Explicit caching will be expressed by memoization annotation:

        [computation]{&memo}

Memoization doesn't force immediate evaluation. Instead, this annotation tells our runtime to use a memoization cache if we later evaluate. Caching may be heuristic, based on observed time/space tradeoffs. 

Developers can use simple techniques such as modeling suitable memoization points and batches in data structures to mitigate potential efficiency issues of fine-grained caching. Careful use of stowage can also help, by reducing the serialization overhead for stowed fragments of the computation.

#### Cache Design

Caching can be implemented by taking a *secure hash* of the representation and performing a lookup. In case of `{%word}` tokens, we do not know whether those words would be linked during evaluation or preserved as symbols. So conservatively we might include both the `{%word}` symbol and a reference to the word's linker object. 

        [{%foo}{%bar}{%baz}]{&memo}

        cacheID = SecureHash {%foo}{%bar}{%baz} (foo)(bar)(baz)
            where (X) is cacheID of X's linker object

Taking these constraints overall, we might assume four tables of form:

* **dictionary:** word → def. Developed by humans and software agents. 
* **clients:** word → words. Computed directly from the dictionary. 
* **data cache:** secure hash → def. Where we keep cached computations.
* **word cache:** word → (eval, link, metadata). Computed by evaluator. 

Our dictionary has a set of definitions. We maintain an index of immediate clients for each word for reverse lookup, rename, incremental cache invalidation, etc.. Our data cache is updated by the `{&memo}` annotation or a local equivalent, may be shared by multiple dictionaries, and may be maintained heuristically (e.g. random deletion is okay). 

Challenges surround maintenance of the *word cache*. Upon updating a word's definition, its linker object may be invalid, and transitively any clients of that word. 

An easy maintenance technique is perhaps to conservatively clear the word cache then rebuild it lazily. Assuming the *data cache* is used to evaluate words and linker objects, we might avoid the bulk of unnecessary recomputation. However, it might be advantageous to more precisely invalidate the word cache. This requires evaluation *during* invalidation, which has its own challenges. 

A heuristic balance of precise and lazy cache maintenance may prove effective in the general case, e.g. based on an effort quota upon performing each dictionary update. 

*Aside:* We can potentially improve cache precision further by use of an inline analysis to determine which symbols would not be part of our output. Use of the [VerSum SeqHash](https://people.csail.mit.edu/nickolai/papers/vandenhooff-versum.pdf) concept might be useful for logically inlining subprograms for purpose of hashing and caching.

### Accelerated Dictionary

An AO runtime will generally specify one or more anonymous, accelerated dictionaries - words with definitions whose implementations may be hand-optimized. Other dictionaries may then derive from the accelerated dictionary. Recognizing accelerated definitions is most readily performed via the word cache, e.g. we can recognize whenever a word's cacheID matches the same from our accelerated dictionary, and if so use the accelerated implementation.

This is discussed more under [ABC](AboutABC.md). 
