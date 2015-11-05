
# Wikilon's Application Models

Wikilon is intended as both a software platform and development environment. 

As a software platform, Wikilon should be able to directly host wikis, forums, blogs, image canvases, document editors, REPL sessions, spreadsheets, chatrooms, interactive fictions or multi-user dungeons, and other applications or web services. As a development environment, Wikilon must support edit sessions and layout, debugging sessions, and compilation of applications for execution elsewhere. Development environments might be considered an important use-case of hosted applications.

Primary concerns as a software platform include *state* and *security*. 

I have decided to focus Wikilon exclusively on *dictionary applications*. 

Wikilon will support an ad-hoc variety of dictionary applications. I'd like to eventually 'generalize' the idea, i.e. by constructing one or more application models that can express the other applications. This is feasible, but the details will need to wait until dictionaries are more mature.

## Modeling State with Dictionary Applications

A **dictionary application** represents all of its state within the AO dictionary. This representation of state must be conveniently structured, well typed, and meaningfully executable. Structure includes both bytecode and relationships between words. Dictionary applications offer many powerful benefits: 

* application state is persistent, portable, versioned
* live coding and continuous testing become implicit
* dictionary compression of state is uniformly available
* everything is visible and indexable for debugging

The disadvantage is that our applications cannot directly *push* information. This can be mitigated in many cases by using some simple strategy to pull information (polling, long-polling, Comet, subscription, etc.) and by relying on external agents to push updates occasionally. But it does force us to favor a more RESTful architectural style. 

The *command pattern* or append-only log is widely useful for dictionary applications. Expressed within a dictionary, this might look something like:

        @foo.v0 (init foo)
        @foo.v1 {%foo.v0}(command to update foo)
        @foo.v2 {%foo.v1}(another update command)
        @foo.v3 {%foo.v2}(yet another command)
        @foo {%foo.v3}{%fooType}

With this pattern, we gain several features: unlimited undo, uniform cloning, back-in-time debugging, ability to tweak old commands and replay, visible command histories for abstraction and refactoring, cache-friendly computations for common update patterns (i.e. assuming we mostly add commands and undo or tweak recent commands). Taken together with an annotation to stow large values into VRefs, it is feasible to construct truly massive objects (e.g. filesystems, databases, game worlds) within a single dictionary object without massive RAM overheads.

For many applications, like mailing lists and web forums, it is entirely acceptable to keep a complete history or leave maintenance to humans. Space is cheap enough that we could easily keep complete histories for anything that involves discrete human inputs. But in other cases we may need to gradually stabilize and compact our history. It is feasible to automate processes that will systematically inline, simplify, refactor, and garbage collect words at the dictionary layer to recover space. However, to avoid upsetting users and breaking applications, we must describe which optimizations are permitted. See *Hidden, Opaque, and Frozen Words*, below.

## Representing Databases, Filesystems, and other Big Data

Many PL runtimes keep everything in volatile memory by default. In my experience, this creates a discontinuity in the programming experience. Developers must use a very different design if they want larger-than-memory data structures or data persistence. Awelon project and Wikilon favor a different approach: 

* the `{&stow}` annotation tells our runtime to serialize a value to disk
* the runtime will load and cache stowed values from disk when necessary
* track affine, relevant properties for for fast copy, drop without load
* structure and storage sharing for stowed values is frequently implicit

With this feature, we can model larger-than-memory filesystems or databases as normal data structures - e.g. tries, finger trees, log structured merge trees. Granted, we must still design our data structures for performance, i.e. so we stow chunks of acceptable size.

Persistence of the root filesystem or database value is a separate concern. 

In context of *dictionary applications*, persistence is addressed by a cache of compiled words. We can rebuild our databases from the dictionary definitions if we must. But we'll try to design dictionary applications to use cache-friendly update patterns so recomputing is rarely necessary.

*Aside:* In a distributed system, stowed values correspond to ABC value resources, i.e. referenced by `{#resourceId'kf}` where the `'kf` suffix indicates the resource is a quoted value with relevant and affine substructural properties. The resourceId is just a secure hash of some bytecode. However, the `{&stow}` annotation is generally local to a runtime. Locality simplifies the issues of loading values and garbage collection.

## Providing Security for Dictionary Applications

As a security use case, I imagine modeling a game (perhaps a multi-user dungeon) where each player should:

1. control their own character, not world state or other characters
2. observe the state of the game world only near their character

Modeling the game world state is feasible with dictionary applications. The greater challenge, then, is controlling the players. A viable option would be to constrain access to the dictionary through an application interface, at least for some subset of users who are not administrators of that dictionary.

## Extraction of Dictionary Applications

As a development environment, Wikilon must support compilation of applications to run on a desktop, android phone, an independent web service, and so on. Ideally, these are the same applications that we debug within Wikilon, but we want to extract a minimal amount of logic and state from our dictionary to make it work. Dictionary objects are a good fit here, as are applications that operate only on a well-defined subset of the dictionary (e.g. some finite set of named objects). 

Extraction of dictionary applications *in media res* supports an implicit debug mode, automatic records and testing of application debug sessions, and a possibility for interactive construction of applications from a fork of a prototypes. In some ways, this is much nicer than conventional application compilers.

## Automatic Optimization of Dictionaries: Hidden, Opaque, and Frozen Words

Words in an AO dictionary provide a basis for mutable meaningful structure, modularity, and structure sharing. The mutable meaningful structure allows AO dictionaries to serve as a platform for live coding and dictionary applications. 

Unfortunately, the presence of mutable meaningful structure interferes with direct optimization at the dictionary layer. This isn't a major problem: a compiler could maintain a cache of optimized definitions separate from the dictionary. But there are some opportunity costs with respect to separating the optimizer, redistributing optimized code, and generalizing automatic  management of dictionary applications. 

To recover the lost opportunities, we can enable developers and dictionary applications to *declaratively relax* the constraints on an assumed dictionary optimizer for useful subsets of the dictionary. I propose the following:

* A word may be declared **hidden** to relax the requirement for stable external reference. An optimizer is free to delete a hidden word if it has no clients. This enables garbage collection of dictionaries.
* A word may be declared **opaque** to relax the requirement for stable structure of its definition. An optimizer is free to rearrange, refactor, or reorganize the definition of an opaque word in ways that preserve its behavior. 
* A word may be declared **frozen** to relax the requirement for mutable behavior. An optimizer is free to inline definition of a *frozen* word into the definition of an *opaque* word. A frozen word is *deep-frozen* if all transitive dependencies are also frozen.

To declare a list of attributes, I propose prefixing any definition as follows:

        [{&hidden}{&opaque}{&frozen}]%

This structure is preserved on export, easy for an optimizer to recognize and handle, and trivially eliminated by simplifiers. It is extensible with ad-hoc new attributes (categories, relations, etc.) with no need for runtime semantics. A reverse lookup can find all uses of a token. It's voluminous, but space is cheap. The main disadvantage is that most other dictionary applications will need to recognize and handle this structure, too.

Note: These attributes only affect a dictionary optimizer. The *frozen* attribute is not a security attribute. If a developer wants to modify the definition for a frozen word, he can do so. Though, a development environment might require explicitly un-freezing the word to modify its behavior.

## Robotic Reflection, Metaprogramming, and Maintenance

Awelon Object (AO) lacks any *direct* mechanism for reflection on the dictionary.

However, it is feasible to model reflection indirectly, e.g. by automatically maintaining dictionary words depending on other words in the dictionary. In the general case, we could maintain a word that contains a complete copy of our dictionary as an association list. However, fine-grained reflection is far more likely to be cache-friendly, allowing us to track which queries we depend upon.

This *robotic reflection* is itself a dictionary application, and may be guided by the dictionary. Consider one potential representation of this guidance:

        @foo.def [{&auto}]%(computed foo definition here)
        @foo [[{%foo.make}]{&make}%]%{%foo.def}

The *make* attribute receives an argument, a make rule. The definition must be a simple redirect to the make target. Our host applies the rule to construct the target. The target carries the *auto* attribute, to support trivial filtering from an AODict export and represents implicit permission to recompute. The separation of `foo` and `foo.def` conveniently leaves an obvious hole in the dictionary when the target is filtered (e.g. so we can infer the type of `foo.def` from usage, even if we haven't computed it). 

The above is a reasonable representation by several heuristics. But I'm not committed to it. It seems worthwhile to explore alternatives that, for example, easily maintain multiple words from a single declaration.

## Purity of Dictionary Applications

I have contemplated introducing an effects models for dictionary applications. Presumably, effects could provide capabilities for fine-grained error-handling, explicit reflection, performance shortcuts, and even first-class shared resource models within the limits of causal commutativity and spatial idempotence (e.g. unification variables).

However, explicit effects are *complicated*. Interacting with an implicit environment hinders the gradual compaction of history and compression of state for long-running dictionary applications. We must specify the environment. We must deal with competing environment models and versions.

Further, explicit effects are *unnecessary*. For high level error handling, it is sufficient to reject a command that would lead to a bad application state or generate an error report. For reflection, we can model software agents that maintain and metaprogram our dictionary. For performance, we can push for ABCD-like accelerators and better annotations. Any model of shared resources can be modeled functionally and we can mitigate performance by accelerating common patterns. External communications is modeled in terms of RESTful interactions with external agents.

The main costs of purity is that the rest of the world isn't designed for it. Retained mode rendering, for example, favors a stream of update events. We'll need to work with this when designing applications for extraction, e.g. explicitly computing update logs from subsequent representations of state (like Facebook's React).
