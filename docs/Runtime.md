
**NOTE (May 2017):** I feel I've been tangled in the weeds when re-implementing the runtime for the simplified Awelon language developed circa November 2016. It's GC, linking, stowage, and such that are causing much heartache. So I'm moving to a dynamic runtime for now, and might move some parts to C (or LLVM?) later.

# Awelon Runtime Design

So what is my performance path? I'd like potential for state of the art performance, without too many difficulties.

## A viable (but looong) Haskell path

Haskell simplifies many problems surrounding GC, parallelism, parsing, serialization, stowage, linking, and indexing. OTOH, my Haskell interpreters for prior Awelon definitions have consistently been at least 10x worse than C interpreters, sometimes 100x worse.

First, accelerators. I can and should get started much earlier on a widely useful "standard library" of accelerated functions and data types. If every time I see a significant performance bottleneck, I accelerate, I can consistently ratchet up performance and parallelism both, even if I'm assuming simple interpreters.

Second, compilation from Awelon to an intermediate language designed for fast naive interpretation or further JIT. I've already discussed under [Awelon Language](AwelonLang.md) how it isn't too difficult to rewrite a lot of Awelon code to an intermediate language where we have call-return, labeled jumps, more conventional loops and branching, perhaps even a few registers for data plumbing. Hand crafted accelerators and types would become primitives at this layer. 

Third, compilation from this intermediate language into Haskell, something we can link directly into our runtimes. We can leverage Template Haskell to represent a "standard library" Awelon dictionary in a subdirectory of the Wikilon project, and compile everything fully to well-typed Haskell, with an option for gate-keeper code where appropriate. This would make it a lot easier to extend the set of accelerators. And I believe I can achieve excellent performance this way.

Fourth, we can dynamially compile Haskell code at runtime. This is feasible either by use of the GHC `plugins` system or by constructing independent Haskell processes to which we may delegate computations. I can feasibly leverage type-indexed products to represent a shared context API without use of imports. I imagine this would make for very high-latency JIT. But it should be sufficient for the more stable of software components.

Fifth, compilation from the same intermediate language via LibClang, OpenCL, or LLVM. This would provide a basis for true JIT compilation. I could compile to use a memory-mapped region for the stack and heap, or compile for external processes or virtual machines to distribute computation. Compiling to binaries for external use could provide integration with conventional systems.

I believe this is a viable and scalable long-term performance path for Awelon.

The first two ideas might be combined, since every accelerated type and function must also be represented in our intermediate language. The intermediate language requires much attention. As will developing a useful set of "primitive" accelerators.

## A Shorter Path? Leverage JVM or CLR

With JVM or CLR we can leverage JIT and dynamic code generation. Leveraging this would allow me to take advantage of existing and ongoing optimization efforts. Code generation may also simplify construction or plug-ins for server-side agents to support various application models.

CLR has a few advantages - a cleaner CodeDOM, full abstraction. OTOH, the .Net ecosystem is a mess with all the different frameworks. The .Net core seems to clean things up a fair bit, but a lot of stuff hasn't been ported to .Net core yet. WebSharper is interesting, too, but is not currently usable from the .Net core. JVM is also usable. But I'll give CLR with F# a fair try first.

Strings are an issue. Awelon uses UTF-8 exclusively, but JVM and CLR strings are UTF-16 by default. I'll just need to translate everything, I suppose.

## Spike Solution

First, get a web server running with some basic APIs for data entry and processing. Then the database. Hopefully I can interact with my code from the start, so I'll want to get started 

 every point, I should be able to interact usefully with my code.



My first attempt used Wai directly, but it was difficult to get what I wanted from that. So I'm trying something richer this time around. I'd also like to get started early with web-sockets to support rich and reactive web applications.

Anyhow, I need to get several APIs working ASAP:

* basic import/export, upload/download of codebases.
* 


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

## Indexing of Dictionaries?

I need a simple data structure suitable for a few different operations:

* efficient insertion and deletion, obviously
* efficient diff and merge for database-level ops
* efficient search with common suffix is convenient
* structure sharing: data determines representation
* easy batching of updates, avoid temporary stowage

Tries and crit-bit trees are the most promising options here. 

With crit-bit trees, however, we do must occasionally peek at a sample key when performing diffs and merges so we can detect differences *prior* to the first crit-bit. It is feasible to shift the key from one of the two child nodes upwards into the parent, e.g. such that the least key is always represented in the parent node. This results in a structure akin to:

        data CBT  = CBT (Maybe Root)
        data Root = Root Eph Key Node   -- 'Eph' resists GC of stowage
        data Node = Inner Int Node Key Node
                  | Leaf Data

This ensures we always have immediate access to the least key in the tree. The Inner node includes the least key from the right-hand tree, assuming we have received our least key for the child key from the parent.

An intriguing property of crit-bit trees is that we could easily reorder visitation of bits based on priority for flattening the tree. This is the [prioritized crit-bit trees (PCBT)](http://unisonweb.org/2015-12-22/data-api-implementation.html) developed by Paul Chiusano for the Unison project. Such trees are not readily merged, but they could offer superior read-only performance.


It may be we can modify a crit-bit tree a little, i.e. such that we record a range of keys in each node.

I'm looking mostly at tries and crit-bit trees as my options.

Crit-bit trees have a lot of simplicity advantages. I guess my main concern is efficient 'diff' on trees, convenient for comparing full databases. Can we efficiently `diff` 


Crit-bit trees

I can use crit-bit key-value trees, adapted for stowage, for most of my data. 

The root of a CBT is maybe a node, otherwise empty. It may need to hold some ephemeron roots, so we'll also keep a root ephemeron holster. A node is either inner or a leaf. A leaf has a key-value pair, while a node has an integer then left and right children. With *stowage*, our key and value may be represented inline or using an external reference.



When serializing these nodes, we must be careful to avoid hindering recognition of secure hash resources within our key-value data. We may also benefit from supporting lightweight inlining of nodes, such that an Inner node may be inlined or not. It seems feasible to combine the Leaf and Inner types in their serialized forms, simply using 0 for our Int value within the leaf-node.

If we do this carefully, it should also be feasible to process the index with minimal parsing of it, e.g. just skip the first node to read the second, or skip the key to read the data. This would require one extra size field in the serialized form.

index our data using binary representations directly, without parsing. This would enable a quick search without copying.

Intriguingly, it is feasible to reuse crit-bit tree representation with very little alteration for a , reordering visit bits to ensure .

 as a batch process. Doing so would provide a more optimal index, but would hinder dynamism since we cannot easily add, remove, or update elements from a PCBT.



The real trick here will be ensuring our node sizes inline enough to be worthwhile. 

It is feasible to use a fixed-width node size to represent several branches. But it might be wiser to simply 




 node in a CBT is either inner or leaf

assuming we don't need to merge our trees.

A related performance concern regards how I should go about indexing of dictionaries and the various related structures. 

Desiderata:

* index multiple versions of dictionaries
* structure sharing between indices
* composable indices (monoid class)
* integrated data stowage, batch updates 

The simplest index that guarantees structure sharing regardless of construction history is the trie. A critical-bit tree is also a potential basis, though it may prove difficult to integrate with stowage. It's essentially a bit-level trie. 

An interesting idea from the Unison Web project is the , which would correspond to a trie where we're not forced to discriminate on the *first* character difference. Instead we can discriminate over the Nth bit or character in the path, selected to optimize information with each branch. The PCBT has some nice properties, but we can't readily model composition of indices or insertion of new data.

For now, a simple bytestring trie should do the job well enough, especially with a little support for batched input and composition at the stowage layers. Importantly, trie's are a simple, predictable, composable, and comprehensible data structure. 

Later, new index structures may be introduced on an as-needed basis. Indexing, fortunately, is a feature that doesn't need to be perfect up front.

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

## Dictionary Indexing

I must efficiently index a dictionary to:

* find definition, given a word
* find references to a word or annotation
* find words with common suffix (or prefix)

For efficient import/export of dictionaries, I want want to preserve structure of dictionary resources.

But it is feasible to operate mostly off the indexes, and to generate a new dictionary resource that represents the same dictionaries more efficiently. Importantly, such indices must be incremental and composable, such that a composition of patches can be indexed by a function composing their indices. Structure sharing, so updates aren't scattered throughout the dictionary, may also be valuable.

All of this suggests use of trees or tries to me. Tries, unlike most search trees, have a nice property of fully deterministic structure based on the *elements* they contain rather than their update order. I lean in favor of tries to improve structure sharing between independently developed dictionaries.

* to find a definition, use trie from word to definition
* to find clients, use trie from word to a set of words
* one trie encodes words backwards for suffix lookups
* set of words is encoded as a trie, using keys only

During evaluation, I'll probably need to maintain another index for cached evaluations, localization of words, etc. - again, having a lot of structure sharing would be convenient.

Tries require a lot of references between tree nodes, which seems a problem given 60-byte resource IDs. Use of stowage size heuristics - e.g. don't stow anything smaller than 256 or 512 bytes - might help by collapsing smaller nodes.

All of this involves a lot of references between index nodes. Of course, when references are large secure hashes, having lots of small nodes is problematic. This can be mitigated using stowage style heuristics, deciding whether to collapse a node based on its apparent size. 

Definitions could be included directly, again via stowage, instead of a `(resource, offset)` indirection. This would be important if we wish to share structure of indices independtly of dictionary update order or patch structure. 

Anyhow, the runtime will need to be relatively good at working with tries and stowage.

## Concurrent Update

A context shall represent a transaction on the dictionary, in the sense that it may conflict with updates from other contexts on the same dictionary. 

It seems feasible to model discretionary locks within a dictionary. Or to keep some form of write journal, to support DVCS-style workspaces with fork and ad-hoc merge. But such features won't be supported by the runtime.

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

