
**NOTE (May 2017):** I feel I've been tangled in the weeds when re-implementing the runtime for the simplified Awelon language developed circa November 2016. It's GC, linking, stowage, and such that are causing much heartache. So I'm moving to a dynamic runtime for now, and might move some parts to C (or LLVM?) later.

# Awelon Runtime Design

## CLR

To leverage .NET JIT, I'll start by reimplementing in F#. This should enable access to runtime code generation mechanisms (e.g. CodeDOM), assuming a suitable 'accelerated' intermediate language. I've selected the `dotnet` core, which unfortunately excludes some nice libraries like WebSharper but has a relatively clean and portable toolset.

I'll need to implement ByteStrings, since CLR's UTF-16 strings aren't very appropriate for Awelon.

## Wikilon Database

As a RESTful system, all persistent state should be kept in a database, excepting only regenerable, cached computations and some weak session-like features for real-time server push (which must be easily restarted). 

### The Dictionary

Our primary database will simply be one big dictionary. This is an important center of Wikilon, and so must not be coupled with other data - excepting, perhaps, a long-running history. We can leverage some features of Awelon dictionaries:

* hierarchical dictionaries can simplify user-level or project-level sandboxes
* dictionaries can use `meta-*` fields to track geneology, origin, and so on
* dictionaries can keep tagged snapshots via further hierarchical dictionaries

We can feasibly model subscriptions, such that some dictionaries are copied from a different branch automatically, but it isn't a critical feature for now. We can also have a convention to model web-services that allow external clients to post/put/get.

### Search?

I'm uncertain about how to approach search. Complicating factors include the hierarchical dictionary structure, security concerns, and secure hash resources.

Minimally, I can provide an index to find all clients of any given word. But full-text search that might cross dictionaries or require concept analysis is quite beyond the level I can support for now. (Reverse lookup for clients, however, might be useful indirectly, e.g. for embedding keywords.)

### The Users

Additionally, I need to keep user data - sessions, clipboards, authorities, etc.. It might prove convenient (for import, export, history, etc.) to represent each user via dictionary, perhaps even within the global dictionary. User activities might be generally modeled similar to inventory tracking, of sorts.

Each user should perhaps have a dedicated sandbox dictionary, or at least the ability to create one.

## Caching

Design for cached computation is cause for some frustrations at the moment. 

My current proposed design is this:

* compute a global reverse-lookup index
* compute a deep behavior version index
* cache from version to evaluation, etc

A global reverse-lookup index is useful for search, discovery, and other purposes. Further, it can be maintained in real-time with updates to a dictionary. Stowage resources will requires some special attention.

A behavior version hash is useful. It can map to evaluated definitions, which might have their own version hashes (to further improve sharing). The main issue with version hashes is that I cannot compute them in real-time: a change to the dictionary can result in a cascading change to version hashes. Fortunately, I can compute version hashes lazily without too much difficulty, and it not difficult to model a queue of versions to invalidate incrementally.

This design seems very promising, but it's still rather expensive in nature. And the implementation is more complicated than I'd prefer for just getting started swiftly.

For a fast start, I propose a simplified ephemeral cache model:

* Develop an evaluation environment with caching and invalidation.
* Memoize recently evaluated (compiled) definitions, only. 

In this case, we can use ad-hoc stateful means for invalidation. We don't need durability. Instead, we update the source and we'll automatically invalidate all definitions with changed code.

## Web Services

Just a general set of goals for Wikilon.

* ability to view and edit definitions
* ability to store binary data, like text files (via `%secureHash`)
* ability to work with basic sessions, view and edit multiple definitions
* ability to access, download, and browse resources, given full secure hash
* import and export of dictionaries + resources (via .tgz/.tar, perhaps?)
* ability to access historical definitions

## Paths

For URL paths, we can treat dictionary names as a directory structure (such that `/foo/` refers to the foo dictionary) and words as files (such as `/foo/word`). But I'm uncertain this will work, we might need a suffix like `word.w` for a word or perhaps `foo/Dict` as a suffix for the dictionary-as-a-resource. 

Our root page should enable logging in, finding the dictionaries for which the user is authorized, and also provide some news and other metadata. Dictionary pages should support import-export of the dictionary, access to history, etc.. Since we don't want the root to be a dictionary page.

It should be feasible to view definitions as pages, in many cases. And to support stateful definitions that can receive POST requests.

I'm tempted to simply use an Awelon *dictionary* as the database. This would give me scalability, hierarchical structure, etc.. A benefit is that the implementation is simple, import/export is simplified, and even histories can be maintained easily. A cost is that updates may become overly synchronized. This cost could be ameliorated via a transactional update model, i.e. so concurrent updates can be analyzed and merged if they do not conflict (conflicts should be very rare).

I could create a dedicated data structure, e.g. such that we can improve prefix sharing near the tree root and have dedicated space for indices, but it's a lot of extra work for marginal benefits and would certainly hinder full-system import/export and legibility.

An important consideration is how indices should be maintained. Many indices should be maintained asynchronously, such that they don't interfere with the commit/update operations. We might explicitly manage a collection of "recent updates" separately from our primary dictionary with indices. 

## Cache Management

I need a lot of caching for my goals with Wikilon. Consequently, these caches must be managed and updated effectively.

Each dictionary might be associated with multiple cached observations. Examples:

* reverse lookup index (word to client set)
* word definition version hashes (word to version hash)
* version hashes to evaluated definitions
* version hashes to type descriptors
* version hashes to link-optimized definitions
* version hashes to intermediate compiled code
* fuzzy find indices - find words by suffix, initialisms, types

Ideally, I want to share a lot of cache across versions and forks of a dictionary. To achieve this, indirection via 'version hashes' seems appropriate. A version hash would be a secure hash that uniquely summarizes a word and its transitive dependencies, e.g. like a minimal dictionary.

However, when dealing with transitive dependencies, maintaining these indices atomically becomes infeasible. Even updating all the version hashes could involve millions of writes, and ideally should be handled lazily or asynchronously. I think we need asynchronous and background computations. We'll also want the ability to extend the system with new caches as required for the development environment. A reasonable paradigm for cached observations is perhaps to model it in terms of multiple external but interdependent agents asynchronously indexing the dictionary. An agent's cache should be robust, regenerable, reproducible even if the agent misses some intermediate update events. Hence, cache should be computed from RESTful states, although diffs might be used against a remembered state.

Ideally, we should leverage a topographical sort on inter-cache dependencies to avoid unnecessary rework. It's feasible to achieve this schedule implicitly using appropriate asynchronous await methods of .Net. But for snapshot consistency, I believe a more explicit schedule may be required. Perhaps we can model the appropriate "dependencies" in terms of "watching" Stowage TVars?

If I focus cache on TVars, then perhaps a viable option is to extend the `DB` type with watching variables and automatic transactions. We can perform a transaction once, remember its dependencies and outputs, and add it to a topographical sort. We could leverage a return value to determine whether we continue watching. In some cases, perhaps, we might want to abort the transaction but retry automatically when dependencies change. Cached data would then be accessible via computed TVars. Durability is optional. This seems like a very promising direction!

A related concern is real-time processing. Awelon application models depend on efficient dictionary updates and low-latency observations for multiple agents, along with explicit memoization for large data. It seems to me that this could easily be associated with multi-agent cache management models.

## Spike Solution

Goals:

* efficient import and export of dictionaries (.tar?)
 * primarily secure hashes, stowage




Initial requirements:

* data and resource input, access, export
* dictionary lookups, evaluations
* editable views
* command line interface or widget app
* simple application models, server and client side
* cross-compilation of programs from Awelon to JavaScript
 * accelerated, too

I'd like to support some flexible binary resources, such that I can render JPEG or GIF images or produce some music or movies via computations. 

An interesting point with Awelon's current definition is that I don't really need more than one dictionary, as I may implicitly use hierarchical dictionaries. That said, I might want to separate synchronization of these implicit dictionaries from synchronization of the root.

For uploads, it might be best to immediately root every upload by binding it to a word or dictionary.

I'm not going to worry about security or user tracking quite yet.


## Accelerators and Intermediate Language

Desiderata:

* unboxed static types where feasible
* numbers, tuples, vectors, records
* lightweight uniqueness tracking
 * statically typed uniqueness
 * static in-place mutation
 * dynamic uniqueness tracking
* fine-grained parallelism
* lightweight data plumbing
* well behaved failure model
* labeled jumps, conditions, branching

A special challenge with Awelon is that code can have variable input-output arities. This might be mitigated by specializing the code for the different arities in use, e.g. replace `a` by `a/dyn` then wherever possible replace `a/dyn` by a specific aritied `a`. 

Requiring our code is statically typed before compilation shouldn't be a problem in practice, and would simplify compilation to an intermediate language that has *more* types than the source language.

Most internal functions should use unboxed static types without any dynamic checks. But it should also be feasible to "box" a value where needed with just enough type information for gatekeepers and conversion work.

Ideally, we can easily track uniqueness where needed. Static typing of uniqueness would mean we don't need to check dynamically. We might benefit from dynamic uniqueness tracking in some cases, however, so we can 



unboxed, statically typed code

For uniqueness, 99% of the time we should be able to assume uniqueness. But 

I think most code can be compiled to use unboxed data types, meaning that our compiler should *know* when it is looking at a natural number, tuple, vector, record, or whatever. But it should also be possible to work with dynamic types at the edges of our system, to model the gatekeepers at the edges of compiled functions. We should know statically where we have a dynamic type.




The intermediate language should support a lightweight f with conversions back into Awelon language. 




For static types, I want the compiler to *know* when it's looking at a natural number or vector or whatever, 

 to *know* what type I'm looking at


It might be I'll want multiple layers of intermediate languages during compilation. 

One idea is that I could borrow Haskell's STG machine for my intermediate language, but I don't feel it is optimally suitable for Awelon.




Some features are made more complicated, such as linear references and in-place mutations of arrays. It isn't clear how to perform JIT compilation, especially not at a large scale. GHC's plugins don't seem to offer what I want here.



Short-term, it seems feasible to hand develop many accelerators by hand. Further, I can feasibly compile one or more 'standard library' dictionaries directly into the Wikilon codebase, leveraging template Haskell or a staged cabal `Setup.hs`. Both the hand accelerators and compiler could be developed for ongoing improvement.

This short-term solution can be extended to mid-term via "hot swap" features that let normal users update an accelerated dictionary and recompile it into the Haskell runtime. This could be achieved by exporting reference dictionaries into a DVCS repository (e.g. `git`), then automatically building and re-deploying from DVCS. I can potentially develop space-optimizing passes to work with large dictionaries, as needed.

Long term, I'd like to properly bootstrap Awelon, and have it self-compile with its own compiler and runtime. Hopefully, the short and mid term solutions can provide a scaffolding here.

*Notes:* It seems feasible to leverage `plugins` as a lightweight approach for hot swapping the reference dictionaries. We might also use `compact` to reduce GC overheads for static indexes.

## API Concepts

The runtime API should be oriented around an agent's view of the system.

Some operations we might want to perform:

* read or update a word's definition
* perform a set of updates atomically
* input a program, binary, or dictionary
* output a program, binary, or dictionary
* partial program output, background parallel
* process program as text (for editable views)
* evaluate, type, optimize, compile a program
* profile computation; line item accounting
* subscribe, alerts upon change (callbacks?)
* rename dictionary object, special edits
* find clients of a word, reverse lookup
* find words with a common prefix or suffix

It seems to me that most input and output can occur as binary streams or chunked lists. Fine grained program input (e.g. at the level of open/close block and add word) seems unnecessary, though I could support it anyway. Whether it's worth introducing depends on how much it complicates the runtime API. I may need to track missing secure hash resources, however. 

For export, we could export dictionaries by secure hash or by named transaction. An interesting point is that we could export ALL dictionaries as a single, hierarchical dictionary. Outputting all dictionaries as a massive .tar file would be an interesting import/export mechanism.

*Aside:* By 'optimized' output, I want *Awelon* code after all the rewrite optimizations, static linking, staging, etc.. By 'compiled' output, I would like relocatable JIT code (Clang or LLVM?) or similar.

*Note:* Security models (HTTP authentication, HMAC bearer tokens, etc.), collaborative conflict avoidance patterns (discretionary locks, [behavioral programming](http://www.wisdom.weizmann.ac.il/~bprogram/more.html)), and accounting (quotas, policy for costs shared via memoization or stowage), are not part of this C runtime API. I believe they can be implemented more or less independently, and I don't want to commit to any particular model.

## Streaming and Futures

I could support single-assignment futures when inputting data into a computation. This would be trivial, in terms of undefined words. But is doing so worthwhile? Without streaming IO, the C API is a lot simpler.

## Evaluations

We have at least a few evaluation statuses: evaluated, stalled, unevaluated. I might want an additional option for profiling evaluations on a block, i.e. so I can allocate a unique counter to track the evaluation.

## Extraction

Compilation of Awelon code for an external system is an interesting option, but is only feasible if we either limit the IO types at that layer (e.g. to integers, rationals, decimals, texts, labels, lists) or ensure that first-class function objects in the target language are coupled to Awelon representations.

## Computations

A computation should be snapshot consistent, at least. Like a lightweight transaction. 

Ideally, we should be able to take any computation and set callbacks recording invalidation with whatever precision we can afford.

## Memoization, Stowage, and Accounting

Memoization and stowage accounting should support potential sharing not just within a transaction, but between many dictionaries or many forks of a dictionary. 

Line item accounting should makes this feasible. We might track on a global scale that 'yup, that computation was also performed by Alice and Bob today' and appropriately divide compute and storage costs. This gives us 'economies of scale' for sharing objects.

## Value Sealers

Sealers have a trivial definition:

        (:foo) (.foo)   ==

The normal usage pattern is only a little more sophisticated:

        [(:foo)]b ...  i(.foo)

It doesn't seem these really need any special support, other than to associate `(.foo)` to `(:foo)` when initially interning so it's a pointer-level operation.

## Value Stowage

        [large value](stow) => [$secureHash]

I might want latent stowage, performed based on memory pressure, when promoting values during compaction. Latent stowage isn't too critical if I model batch data structures (like the log-structured merge tree). But it's still convenient.

## Numbers and Arrays

Numbers generally use a boxed representation, excepting small naturals.

I intend to eventually support 'unboxed' fixed-width numbers in context of typed arrays and matrices. This is necessary to accelerate linear algebra, leverage the GPGPU, or enter a domain of high performance computing in general. But I won't bother trying to optimize basic numeric processing for most Awelon code, not beyond simple acceleration of arithmetic.

## Optimizations

For visible optimizations, we can at least perform:

* staged partial evaluation up to arity
* static linking for words
* rewrite optimization

Some invisible performance enhancers:

* determine up-fun-arg vs. down-fun-arg types
* acceleration for common multi-word actions
* JIT compilation of code

## Memory Profiling

I don't currently propose to directly support memory profiling. 

Developers can at least take intermediate snapshots during evaluation, e.g. at breakpoints or stopping on effort quota. This would give developers a reasonable view of where memory would be consumed in an implementation of Awelon code without logical copies.

It would be feasible to count heap references to individual words and types of data (numbers, etc.), however.

How should I offer insight about where memory is being consumed? Ideally, non-invasively?

It seems feasible to track each block/closure to a syntactic origin. But it also seems expensive to record this information in memory, to add metadata about origin to each block or closure and to trace down the object while profiling. 

Alternatively, I could count references to words or annotations in the current program. This would offer an efficient (albeit imprecise) metric of memory usage. Small values (naturals, etc.) could also be grouped by type, as could be texts and binary arrays. This wouldn't give a good view of where memory was allocated.

A third option is to simply record intermediate evaluation states and report on them. This wouldn't provide much information about shared values via logical copies, but applying a compression algorithm might help highlight where most of our memory is used.

