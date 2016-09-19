# Awelon Object (AO)

Awelon Object (AO) is a useful set of conventions for [Awelon Bytecode (ABC)](AboutABC.md). AO defines tokens for linking and how they interact with dictionary objects.

A dictionary consists of a set of words and a definition for each word. Definitions are encoded in ABC. Linking a `{%word}` token logically substitutes that word's evaluated definition in place of the token. Dependencies between definitions must form a directed acyclic graph, such that all link tokens could be eliminated in by a finite expansion to inlined code. Hence, linking has no effect on ABC's expressiveness.

Dictionaries serve as a unit of evaluation, development, linking, and distribution in AO, and as a foundation for Awelon project's various [application models](ApplicationModel.md). In general, if there are meta-level features like security or immutability, they must be specified on a per-dictionary basis.

## Evaluation of AO

Dictionaries are the unit of AO evaluation. To evaluate a specific ABC string, we can certainly create a dictionary containing just that string. But the normal case is to evaluate every word in the dictionary. 

Critically, the intermediate and final output of evaluation is a dictionary. We can preserve a lot of link structure and meaning. Words that cannot be usefully linked will remain named by human-comprehensible symbols. With each `{%word}` we may have attributes like `{%word.doc}` or `{%word.type}` to guide help rendering and debugging. Further, we might introduce new words via stowage, modeling large constructs.

To preserve predictable link structure, AO uses two rules for evaluation:

* Do not evaluate within block unless requested by annotation. 
* Do not link tokens unless it results in evaluation progress.

What 'progress' means is that linking leads to something other than a trivial inlining of code. In general, this suggests some form of `{%producer}{%consumer}` scenario, but linking may also occur if simplifications are possible. Annotations such as `{&seq}` are necessary to force evaluation within a block value.

### Arity Annotations

To precisely control linking requires control over partial evaluations. A simple means to achieve this is arity annotations, defined by a set of rewrite rules of form:

        [A]{&/1}            ==      [A]
        [A][B]{&/2}         ==      [A][B]
        [A][B][C]{&/3}      ==      [A][B][C]
        ...

Effectively, each annotation has the given arity. An arity annotation acts as an input buffer, requiring sufficient inputs be produced before evaluation proceeds. If there aren't enough inputs, there is no progress. 

### AO Value Stowage

When working with larger-than-memory data, it is useful to shove some values to a backing store in favor of lightweight placeholders, then later load the data into memory as needed. I call this pattern 'stowage'. In context of AO evaluation, it is most natural that our placeholder should be a word, part of our evaluated dictionary output.

        [larger than memory value]{&stow}  == [{%resourceId}]

Stowed resources are readily be named by hashing their definitions. This gives us structure sharing and stable names across evaluations, which is useful for incremental computing and caching. A secure hash isn't necessary so long as we have stable conflict resolution techniques.

Anyhow, stowage is ultimately part of our generated dictionary. This isn't particularly useful until we're dealing with very large dictionaries, or large groups of named dictionaries.

### Multi-Dictionary Evaluation Contexts

AO can be evaluated in context of multiple named dictionaries. Tokens of the form `{%word@dict}` allow naming a word in another dictionary. Each dictionary is an implicit namespace, so every use of `{%multiply}` within dictionary `myMath` is equivalent to `{%multiply@myMath}`. 

Support for multiple dictionaries is motivated primarily by external system concerns. For example, we might want immutable (versioned) dictionaries to simplify sharing and caching. We might want curated dictionaries that we can trust. We might want precise ownership and security properties over different dictionaries.

*Aside:* For most use cases, dependencies between dictionaries should be acyclic. However, there may be exceptions to this, e.g. for modeling multi-agent systems and communications between them. The only strong requirement is that *definitions* form a directed acyclic graph.

### Lazy, Parallel, Staged, Incremental Evaluations

Evaluation of AO has a lot of flexibility for strategy. Laziness is implicit for blocks (modulo annotation), and may be present at the dictionary level. Parallelism may be explicit for blocks (via annotation), and is implicit for evaluating many words in a dictionary. Staging is inherent to the interleave of linking and evaluation - i.e. we only link *evaluated* definitions. Incremental computing is feasible, spreadsheet style, by reprocessing every transitive client of a definition after any change. Explicit incremental computing is rather more challenging, but feasible via use of cache annotations.

### Incremental and Cached Evaluation Strategy


## Sharing AO

To communicate an AO program while preserving its behavior requires communicating its linker context. Similarly, to compose independent AO computations requires composing their linker contexts. 

Composition of linker contexts may, in the general case, require renaming of dictionaries that share a name but refer to different resources. To simplify sharing, we want to minimize need for renaming. Thus, developers should favor dictionary names that are unique at a global scope. Fortunately, this is not a difficult feature to achieve. A common name registry, simple naming schema, GUIDs or secure hashes, etc. can be used in naming of things with fair confidence of avoiding conflicts. 

To further simplify sharing, it is useful to favor immutable or monotonic dictionaries to mitigate concerns surrounding update propagation and cache invalidation.

An important feature for shared AO is that we can easily include metadata. From `word` we might look up `word.doc`, `word.type`, `word.example`, `word.author`. This extra data will simplify search, rendering, projectional editing, testing, and human comprehension. It's an important part of the proposed [application model](ApplicationModel.md).


## Representing AO

AO does not make strong assumptions about how dictionaries are represented. One might use a hashtable, database, filesystem, first-class ABC values, etc.. However, AO does define a standard **.ao** file format for import and export purposes. 

        @swap []ba
        @swapd [{%swap}]a
        @swapd.doc "[C][B][A] -- [B][C][A]
         swap just under the top stack element
         assuming typical (stack*ext) environment
        ~

Each definition has the form `@word definition`, starting at the beginning of a line. It's a simple SP (32) between the word and definition. A definition continues until the end of a file or until the next definition starts. There is no risk of ambiguity: ABC does not and will not use `@` for anything. There are no constraints on the order of definitions, and a later definition of a word will overwrite an earlier definition.

Use file suffix **.ao**, or `text/vnd.org.awelon.aodict` in context of an HTTP transfer.

## Developing AO

Editing dictionary files by hand is feasible. But it's also a chore. Outside of early development (e.g. bootstrapping), I wouldn't recommend it to anyone. Dictionaries are optimally manipulated through projectional editor systems. The Forth-like [claw](CommandLine.md) view is suitable for a minimal text-only input, and is a fair bit more legible than AO. But Awelon project's [application models](ApplicationModel.md) are based on richer development environments. 

An interesting possibility for filesystem integration is to use a *FUSE* (Filesystem in Userspace) view, perhaps operating via the web service. If done properly, this could simplify integration with emacs, vim, and other nice text editors.


## Development Idioms

### Dictionary Inheritance

A useful technique is to say: "this dictionary is the same as that one, but with the following tweaks". A prototype inheritance or patching model would be useful in this context. For the **.ao** format, this might be represented by an extension like `@@PARENT source`. When a `{%word}` is not defined locally, the linker would search the parent source for the same word, as if it were locally defined. Then the grandparent, and so on.

### Leveraging Undefined Words

Undefined words can serve a useful role in a development context. They serve as 'holes', and an ABC system might help developers discover their definitions based on inferred type and usage. With ABC rewriting based evaluation, these holes also support lightweight symbolic partial evaluations. 

### Regarding Large Definitions

Definitions can potentially grow very large, especially when containing embedded texts or with dictionary applications. However, huge definitions are not recommended, as they may hinder incremental computation, reuse, memory management, and developer comprehension.

A very large text might be better broken into smaller texts - e.g. per chapter, paragraph, or other meaningful fragment. A large binary modeled via text might better be divided into 'pages', such that persistent structure and edits can be modeled can reusing most pages. Sophisticated definitions consisting of many components (e.g. more than ten to twenty elements) might be better factored into smaller fragments that can be documented and understood incrementally.

At the moment, I'm not suggesting hard caps for definition sizes. 

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
