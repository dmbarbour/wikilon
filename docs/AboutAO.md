# Awelon Object (AO)

Awelon Object (AO) extends [Awelon Bytecode (ABC)](AboutABC.md) with a link model and representation for for symbol-structured code. AO defines tokens for linking and how they interact with evaluation in context of dictionaries.

A dictionary consists of a set of words with an ABC definition for each word. These definitions are lazily linked via ABC tokens of form `{%word}`. Additionally, AO treats bytecodes as single character words - i.e. `xyz` is implicitly `{%x}{%y}{%z}`. Dependencies between definitions must form a directed acyclic graph, such that all defined link tokens could be inlined, resulting in a finite (albeit exponential) expansion into raw ABC.

AO dictionaries serve as a basis for development, evaluation, modularity, communication, and distribution. They provide a foundation for Awelon project's [application models](ApplicationModel.md). Lazy linking, in particular, is critical for many application models. It enables preservation of link structure which, in addition to being important for performance of large scale computing, enables AO to serve a foundation for hypermedia. 

## AO Words

Words are weakly constrained to support AO wrappers, present and future. In addition to the normal limits on tokens (no curly braces, control characters, or replacement character), words prohibit characters SP, `@[]<>(),;|"`. Also, the empty word is prohibited.

Words `a`, `b`, `c`, and `d` are valid, but refer to the four corresponding ABC primitives - apply, bind, copy, and drop - and may not be redefined. These are effectively the four 'keywords' of AO.

Developers are encouraged to further limit themselves to relatively short words that have convenient embeddings in URLs, natural language documentation, [editable views](EditingAO.md), and so on.

## AO Development

AO development consists of dictionary construction, sharing, and maintenance. 

Working with bytecode and dictionary patches directly is painful. Humans will work instead through [editable views](EditingAO.md) of AO, which may be provided through an intermediate service - e.g. a web service, or potentially a [filesystem in userspace](https://en.wikipedia.org/wiki/Filesystem_in_Userspace) adapter (to leverage emacs, vim, or whatever). This is essentially a [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html) pattern with the AO dictionary as our storage layer.

The Awelon project [application model](ApplicationModel.md) is oriented around software agents (bots) working together with humans in development of AO dictionaries. We can model databases, work orders, and publish-subscribe patterns as a basis for real-time systems with real-world effects. Curated dictionaries seed transparently persistent applications and rich communicating systems.

See documents on editable views and the application model for details. 

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

Named dictionaries serve as the foundation for scalability and security, and are discussed in a later section. 

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

*Aside:* In addition to **.ao** files, a runtime could be configured for limited access to external resources. File `./foo/bar.txt` might be accessible via `{%bar.txt@/foo}`. If supported, the focus should be binary data, covering for a weakness of ABC.

### Forking and Merging

Forking a named dictionary is trivial: simply copy the dictionary then begin updating. A two-way merge between dictionaries is always possible, identifying word-level differences. A three-way merge is possible only if there is a readily identified common patch history between two dictionaries, which depends on the update model.

### Provider Independent Distribution

Anonymous dictionaries are distributed easily by secure hash. Given a secure hash for a dictionary we do not possess, we can ask whomever provided the secure hash to provide the patch. This is the primary distribution model of AO. But content-addressed networking techniques are possible, and may be useful in distributing a network burden. A viable technique for provider independent transport is to just use a fraction of the secure hash for lookup (e.g. the first 120 bits) then use the rest as a symmetric encryption key. 

Named dictionaries are generally mutable. It is feasible to distribute development of named dictionaries by use of DVCS techniques or to share names for dictionaries globally. In the latter case, global names may need some strategy to resist conflict. This might be achieved by deriving from an existing registry (e.g. ICANN or Namecoin), or a more informal registry (e.g. someone maintains a webpage), or taking a secure hash of a public key. Etc.. 

### Caveats

There is no support in the AO representation for development metadata (commit messages, timestamps). This metadata should instead be explicit in the dictionary or an auxiliary named dictionary. That way, it is accessible to normal [AO applications](ApplicationModel.md).

There is no support to update only part of a definition. This isn't a big problem because it's easy at the AO layer to factor a large definition into smaller parts that can be updated independently. Even a definition can be represented using a simple command pattern:

        ...
        @word.v99 {%word.v98}(update)
        @word {%word.v99}

There is no support for renaming words or objects. Despite 'rename' being perhaps the most common example in proposals for language-specific semantic patch models, renaming introduces a lot of problems like not being idempotent, having a non-local effect, being rather ad-hoc in a distributed development scenario, and complicating patch-level indices. Developers are asked to perform renaming the old fashioned way - by explicitly redefining every relevant word in the dictionary, and avoiding rename for established words.

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

## AO Scalability and Security

For open distributed systems, the *named dictionary* is the natural unit for independent development, asynchronous update, and security. Each named dictionary has its patch, secure hash, head. Each writer operates on its own authority.

To clarify, partitioning into named dictionaries is unnecessary at the scale of normal applications. A single dictionary can feasibly scale to hundreds of concurrent applications, a hundred thousand writes per second. Dictionary patches might be organized as an [LSM tree](https://en.wikipedia.org/wiki/Log-structured_merge-tree) or [hitchhiker tree](https://github.com/datacrypt-project/hitchhiker-tree/blob/master/doc/hitchhiker.adoc) with an external index. The dictionary can be replicated for efficient reads. Non-conflicting updates can be composed, batched, distributed across replicas. 

Named dictionaries are nonetheless be essential if we scale to thousands of users, each of whom maintain hundreds of applications. Further, in context of mutually distrustful agents, we must precisely control access and decentalize write authority.

### Distributed Consistency

In context of continuous update and observation on a dictionary, an AO runtime system should support three useful properties for reasoning about system consistency:

* *snapshot consistency* - evaluations are consistent with instant in time.
* *eventual consistency* - if no new updates, eventually see head snapshot.
* *real-time capability* - controllable latency between views and head.

On the third point, I emphasize control. Achieving strong real-time guarantees requires control of factors outside the runtime: the nature and frequency of updates, access to computation resources, network connectivity. Runtime features that might cause difficulty for reasoning about real-time properties - such as heuristic memoization and JIT compilation - should be under control of programmers. It should be able to reason about real-time properties and what must change to achieve them.

In context of AO representation, a 'snapshot' is essentially a patch, or a secure hash thereof.

When we scale to multiple dictionaries, *snapshot consistency* will be weakened because each dictionary may operate asynchronously. Instead, we have *pairwise snspahot consistency*. Pairwise snapshot consistency is not difficult to achieve in practice, needing only some means to perform atomic batch queries or push atomic batch updates.

Pairwise snapshot consistency allows for inconsistent views. Consider:

        info@foo ----→ view@baz
             ↘_ data@bar _↗

On update to `info@foo`, we can propagate a batch of updates to dictionaries `baz` and `bar`. Later, the update to `bar` is propagated. If we evaluate `view@baz` between these update events, we will observe inconsistent information - a 'glitch' in the jargon of reactive computing. 

Fortunately, dictionaries are implicitly units of modularity and implementation hiding. The developer of `view@baz` should not be making any strong assumptions that `data@bar` is related to or updated synchronously with `info@foo`. Without consistency requirements, the glitch is unlikely to cause problems.

*Note:* In case of replicated dictionaries, we might weaken consistency a little more to permit different replicas to process merge non-conflicting updates in different orders. In that case, snapshot consistency is weakened to a snapshot of a particular replica, with eventual consistency between replicas.

### Distributed Linking and Localization

Linking remote tokens `{%word@dict}` is much the same as linking local token `{%word}` but we will need an extra pass to add the `@dict` decorator to any undecorated link structure from the remote word. For example, if the remote linker returns `x{%bar@baz}{%foo}` then we must (logically) rewrite this to `{%x@dict}{%bar@baz}{%foo@dict}`.

However, dictionaries will share a common vocabulary due to de-facto standardization of accelerators, shared utilities via forks and merges, and so on. Because of this, the `@dict` qualifier is mostly wasted - it just bulks up our code, hinders memoization and structure sharing, and so on.

To ameliorate this, AO permits a process called *localization*. Upon conservatively recognizing `{%word@dict}` has the same meaning as `{%word}`, a runtime should rewrite the former to the latter. Localization occurs independently of linking.

Localization is potentially lossy for associative metadata. For example, we do not test whether `{%word.doc@dict}` is the same as `{%word.doc}`. However, this isn't a problem. If `{%word}` needs documentation, we probably already have our own. Effectively, we localize metadata while preserving semantics.

### Secure Linking

`{%word@dict}` should be at least a bearer token to read the evaluated definition. 


### Secure Update



