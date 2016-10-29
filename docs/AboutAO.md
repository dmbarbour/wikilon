# Awelon Object (AO)

Awelon Object (AO) extends [Awelon Bytecode (ABC)](AboutABC.md) with a link model and representation for for symbol-structured code. AO defines tokens for linking and how they interact with evaluation in context of dictionaries.

A dictionary consists of a set of words with an ABC definition for each word. These definitions are lazily linked via ABC tokens of form `{%word}`. Additionally, AO treats bytecodes as single character words - i.e. `xyz` is implicitly `{%x}{%y}{%z}`. Dependencies between definitions must form a directed acyclic graph, such that all defined link tokens could be inlined, resulting in a finite (albeit exponential) expansion into raw ABC.

AO dictionaries serve as a basis for development, evaluation, modularity, communication, and distribution. They provide a foundation for Awelon project's [application models](ApplicationModel.md). Lazy linking, in particular, is critical for many application models. It enables preservation of link structure which, in addition to being important for performance of large scale computing, enables AO to serve a foundation for hypermedia. 

## AO Words

Words are weakly constrained to support AO/ABC wrappers. In addition to the normal limits on tokens (valid UTF-8, no curly braces, control characters, or replacement character), AO forbids characters SP, `@[]<>(),;|="` within words. Also, the empty word is prohibited.

Words `a`, `b`, `c`, and `d` are valid, but refer always to the four corresponding ABC primitives and may not be redefined. These are effectively the four 'keywords' of AO. Other words may be reserved by the runtime, e.g. to support large value stowage.

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

        secureHashOfPatch1
        secureHashOfPatch2
        @word1 definition1
        @word2 definition2
        ...

        

Each patch consists of a header and a body. The header is a simple sequence of secure hashes, each identifying another patch (hence forming a tree structure). The body is a sequence of `@word def` actions, each overwriting a prior definition for the indicated word. A word may be logically deleted by defining a trivial cycle, `@foo {%foo}`. The last update for a word's definition 'wins', whether that update occurs within the header or body. We might interpret each secure hash as logically inlining the identified patch. 

This representation allows for flexible dictionary structure - e.g. an append-only log, or an LSM tree. It can be indexed externally, and the immutability of secure hashes simplify indexing. Applying patches is conveniently idempotent. It's easy to fork, and three-way merge is feasible. It is not difficult to replicate the dictionary and to maintain it incrementally over time - to download and cache unrecognized secure hashes while preserving the rest for structure sharing.

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

There is no support to update only part of a definition. This limitation is easy to address by factoring fragments that change independently into separate words. ABC's concatenative structure makes this easy. In case of regularly appending a definition, the application model command pattern is applicable.

There is no support built in to the AO representation for development metadata like commit messages or timestamps. This metadata, if desired at all, should instead be made explicit in the dictionary by some recognizable convention. The intention is that this metadata should be accessible to the [Awelon application model](ApplicationModel.md).

Patches are not transactions. While patches can be merged in DVCS style, they offer no ability to detect read-write conflicts, perform application-layer updates like command pattern, or rename dictionary objects. We could feasibly extend AO patches for multi-agent transactions by supporting `@@COMMAND` actions. But the basic AO patches should be favored for simple structure sharing and transport of dictionaries.

AO rejects the concept of foreign functions. All `{%word}` tokens must be defined within the same dictionary. Communicating with the outside world must be performed via the application model effects layer (work orders, publish subscribe, etc.).

## AO Evaluation and Linking

Dictionaries are the basic unit of AO evaluation. AO evaluation logically rewrites an AO dictionary into a different representation of the same AO dictionary. 

        eval :: Dictionary → Dictionary

In practice, this evaluation will frequently be lazy. Instead, we evaluate only a subset of the dictionary sufficient a particular observation - i.e. to evaluate a word's definition or a specific AO query string.

### Preservation of Link Structure

Linking - the mechanical step of substituting a `{%word}` token by its evaluated definition - is performed only insofar as it enables evaluation to proceed. AO should not link if it just results in a trivial inlining of code.

Preserving link structure ensures human-meaningful symbols remain in our evaluated results. Further, these symbols will frequently have ad-hoc associative structure like `word.doc` and `word.type` useful for both humans and software agents that might render, extract, or otherwise interact with a definition. Preserving link structure is essential for Awelon project's application models.

Developers may freely leverage *arity annotations* to control preservation of link structure.

### Value Words

### Redirects and Static Linking

A redirect is a word that simply indicates another word. Consider:

        @foo {%bar}

By the goal to preserve link structure, we'll not evaluate further. A `{%bar}` result is useful for both human observers and software agents because of associative metadata like `{%bar.doc}`. In the general case, our redirect might be the computed result of a conditional decision.

However, when we link `{%foo}`, we'll certainly link `{%bar}`. It would be wasteful to follow these chains at runtime. So we might benefit from flattening this link chain statically, computing a context free static link object per word in addition to each word's evaluated definition.

The idea generalizes beyond redirects. With static analysis, and especially in context of arity annotations, we might be able to inline many words into static link objects. Inlining in the general case would need to be heuristic, based on tradeoffs.

### Lightweight Staging and Compilation

A runtime can compute and bind a *linked object* to every `{%word}` token before evaluation of a program even starts, excepting for undefined words (which won't rewrite). By doing so, we can avoid ad-hoc runtime lookups once evaluation starts, and we can also bind some useful runtime metadata per word like arity.

By caching computations on words, we effectively get staged computing implicitly per word.

Staged compilation of words is similarly possible, e.g. generating an LLVM representation. This is more convenient than use of annotations like `{&jit}` because we can use a word like `word.compile` to provide ad-hoc extra directives (e.g. recommended optimization passes). When we lookup our `{%word}` token before evaluation, we can also bind the compiled representation.

### Large Value Stowage

For big data in a small working memory, it is useful to reverse the linking process: move data to disk, and replace the in-memory representation by a small token that may be used to dynamically link the data. I call this pattern 'stowage'. 

In context of AO, stowage involves creating new word tokens during evaluation.
        
        [large value]{&stow}  == [{%resourceId}]
        [small value]{&stow}  == [small value]

Here `{%resourceId}` is a new word in the output dictionary whose definition is equivalent to `large value`. Stowage has overheads so a simple heuristic decision must be made. Small values, for example, should not be stowed.

Stowage works most efficiently with persistent, tree-based data structures where there is much implicit structure sharing after updates. Even better are trees like LSM that implicitly batch updates so deep modifications are only performed when there are sufficient changes to warrant the effort.

The naming of stowed values is at discretion of the runtime. Use of `{%_secureHashOfBytecode}` is a good option - stable for caching and structure sharing, and reproducible unlike allocated names. A runtime may raise an error when a user-defined word might conflict with stowage.

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

Caching can be implemented by taking a *secure hash* of the representation and performing a lookup. In case of `{%word}` tokens, we do not know whether those words would be linked during evaluation or preserved as symbols. So conservatively we might include both the `{%word}` symbol and a reference to the word's static link object. 

        [{%foo}{%bar}{%baz}]{&memo}

        cacheID = SecureHash {%foo}{%bar}{%baz} (foo)(bar)(baz)
            where (X) is cacheID of X's static link object

Taking these constraints overall, we might assume four tables of form:

* **dictionary:** word → def. Developed by humans and software agents. 
* **clients:** word → words. Computed directly from the dictionary. 
* **data cache:** secure hash → def. Where we keep cached computations.
* **word cache:** word → (eval, link, metadata). Computed by evaluator. 

Our dictionary has a set of definitions. We maintain an index of immediate clients for each word for reverse lookup, rename, incremental cache invalidation, etc.. Our data cache is updated by the `{&memo}` annotation or a local equivalent, may be shared by multiple dictionaries, and may be maintained heuristically (e.g. random deletion is okay). 

Challenges surround maintenance of the *word cache*. Upon updating a word's definition, its static link object may be invalid, and transitively any clients of that word. 

An easy maintenance technique is perhaps to conservatively clear the word cache then rebuild it lazily. Assuming the *data cache* is used to evaluate words and static link objects, we might avoid the bulk of unnecessary recomputation. However, it might be advantageous to more precisely invalidate the word cache. This requires evaluation *during* invalidation, which has its own challenges. 

A heuristic balance of precise and lazy cache maintenance may prove effective in the general case, e.g. based on an effort quota upon performing each dictionary update. 

*Aside:* We can potentially improve cache precision for words by removing symbols we know will be inlined. Use of the [VerSum SeqHash](https://people.csail.mit.edu/nickolai/papers/vandenhooff-versum.pdf) concept could even logically inline subprograms for purpose of hashing and caching. It seems worth looking into.

### Accelerated Dictionary

Acceleration is a primary approach to achieving performance in ABC systems. An AO/ABC runtime will specify accelerated dictionaries, e.g. with definitions like `@i [][]baad` (inline) that it recognizes and hand-optimizes. More broadly, accelerators can also leverage specialized representations like for natural number construction and arithmetic, or accelerating evaluation of process networks to leverage multiple CPUs.

This is discussed more under [ABC](AboutABC.md). 
