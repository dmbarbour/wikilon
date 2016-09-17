# Awelon Object (AO)

Awelon Object (AO) is a linking model for [Awelon Bytecode (ABC)](AboutABC.md). AO defines tokens for linking and how they interact with dictionary objects. Dictionaries are the basic unit of linking and sharing in AO. 

A dictionary consists of a set of words and a definition for each word. Definitions are encoded in ABC. Use of a `{%word}` token logically substitutes that word's definition in place of the token. For valid AO, dependencies between definitions must form a directed acyclic graph, such that statically inlining all link tokens has a finite expansion. Hence, linking has no affect on program semantics. 

Use of `{%word@dict}` tokens enables linking between named dictionaries. A linker operates in a context of named dictionaries, each of which is an implicit namespace. Dictionaries in a linker context must also have acyclic relationships.

## Dictionary Representation

AO defines a standard **.ao** file format, suitable for import and export. 

A trivial example dictionary:

        @swap []ba
        @swapd [{%swap}]a
        @swapd.doc "[C][B][A] -- [B][C][A]
         swap just under the top stack element
         assuming typical (stack*ext) environment
        ~

Each definition has the form `@word definition`, starting at the beginning of a line. It's a simple SP (32) between the word and definition. A definition continues until the end of a file or until the next definition starts. There is no risk of ambiguity: ABC does not and will not use `@` for anything. There are no constraints on the order of definitions, and later definitions will overwrite earlier ones.

It is feasible to extend this AO with actions for streaming updates, such as `@@RENAME foo bar` and `@@PARENT dictName`, or even `@@TARGET dictName` to enable modeling multiple dictionaries in one file.

Use file suffix **.ao**, or `text/vnd.org.awelon.aodict` in context of an HTTP transfer.

## Editing AO

Dictionaries are optimally manipulated through projectional editor systems. The Forth-like [claw](CommandLine.md) view is suitable for a minimal text-only input, and is a fair bit more legible than AO. But Awelon project's [application models](ApplicationModel.md) needs richer environments. 

An interesting possibility for filesystem integration is to use a *FUSE* (Filesystem in Userspace) view, perhaps operating via the web service. If done properly, this could simplify integration with emacs, vim, and other nice text editors.

Editing a **.ao** file by hand, or even a set of linked files, is feasible. But it's also a chore. Outside of early development (e.g. bootstrapping), I wouldn't recommend it to anyone.

## Linking AO

A linker will operate in context of:

* a collection of named dictionaries
* the name of the current dictionary
* the program - ABC with link tokens

The name of the current dictionary is necessary for any `{%word}` tokens in the initial program. The use of `{%word@dict}` tokens will use the dictionary named `dict`. In general, a linker should work with dictionary *values* or a consistent transactional snapshot, such that evaluation is not affected by concurrent edits. 

Our linker may generally test for cyclic dependencies or undefined words, and do a little static work to load dependencies into working memory and bind tokens to the appropriate targets. In case of compiled subprograms, linking may also bind those together.

## Sharing AO

To communicate an AO program, while preserving its behavior, requires communicating its linker context. Similarly, to compose independent AO computations requires composing their linker contexts. Composition of linker contexts may, in the general case, require logically renaming dictionaries that share a name but refer to different resources.

To simplify sharing, one goal is to minimize the need for renaming. Thus, we choose dictionary names that are unique at the global scope. Fortunately, this is not a difficult problem. A common registry, simple naming schema, version identifiers, GUIDs or secure hashes, etc. can be used in naming of things. And while they're not *proof* against collision, they can ensure renaming dictionaries is a rare event in practice.

To further simplify sharing, we may favor *immutable* dictionaries, perhaps including a secure hash or version identifier in the name. Doing so mitigates concerns surrounding update propagation and cache invalidation.

*Aside:* A valuable feature for sharing AO is that it may include a lot of metadata. From `word` we might also look up `word.doc`, `word.type`, `word.example`, `word.author`. All this extra data can simplify search, rendering, projectional editing, testing, and human comprehension.

## Dictionary Inheritance

AO doesn't make strong assumptions about how a dictionary is constructed. A useful technique is to say: "this dictionary is the same as that one, but with some tweaks". A prototype inheritance or patching model seems appropriate. 

With a **.ao** format, this might require some extensions like adding `@@PARENT dictName` metadata. When a `{%word}` is not defined locally, the linker would search the parent dictionary for the same word as if it were locally defined. Then the grandparent, and so on.

## Value Stowage and AO Dictionaries

ABC supports taking large values and replacing them with a lightweight link token.

        [BIG VALUE]{&stow}      =>      [{%resourceId$HMAC@stowage}]

I assume our linker context is extended to provide a default stowage dictionary. Stowage dictionaries support monotonic updates. Additionally, they may be secured against casual reading to protect sensitive data from computations sharing the same resource. Thus, we might use an HMAC to validate read access to each stowed resource.

An interesting possibility is to permit multiple stowage dictionaries in a linker context, and specify which one we're using, e.g. `{&stow@foo}` to tell the evaluator to use a specific stowage dictionary `foo`. 

## Development Idioms

### DVCS Based Distribution

PLs today widely use package based distribution models. A package of code includes data types and functions, and may depend on other packages of code, all independently developed and versioned. Package based distribution introduces many complications: version conflicts, configuration management, external configuration languages. These complications hinder efforts to refactor, abstract, test, live-code, model application state in code, etc. across multiple packages. 

AO favors a far more simple and robust technique: a dictionary contains all dependencies, all words defined. DVCS-based forks and merges are readily applied: developers can fork public dictionaries, update as needed, validate changes, and cherry-pick functions or updates to push back upstream atomically. Instead of packaged libraries, every dictionary is a wholistic ecosystem. 

*Aside:* An interesting possibility is genetic programming. Words serve as genes, definitions as alleles. Fitness can be based on typechecking, unit tests, and score annotations within each dictionary. Take a population of dictionaries and use word-level random merges (and rare mutations) to model sexual recombination. This would be expensive, but reasonably straightforward.

### Leveraging Undefined Words

A dictionary in a transitory state of development will frequently have a few undefined words. These can serve a useful role in the development context: a development environment can recognize undefined words as 'holes'. The type for a hole may be inferred from usage. A good developoment environment should help developers find implementations of this type. The number of undefined words should generally be limited based on the amount of active development, and only certain approaches to development will use them (e.g. top-down).

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

## Security

