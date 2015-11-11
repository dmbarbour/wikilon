
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

Dictionary applications are free to present editable views of bytecode. Such views are potentially convenient for representing form or document based user inputs.

However, dictionary applications are additionally free to *evaluate* code. This enables operation on structured values. An application might render a view of a value, or represent application state as a value. This allows us to push update logic from our views into our dictionaries, representing update commands as pure functions of type `AppState → AppState`.

The *command pattern* is especially useful in context. It provides several useful features: cache-friendly updates, universal undo, back-in-time debugging, visible command histories, trivial cloning. Expressed within a dictionary, this pattern might look something like:

        @foo.v0 (init foo)
        @foo.v1 {%foo.v0}(command to update foo)
        @foo.v2 {%foo.v1}(another update command)
        @foo.v3 {%foo.v2}(yet another command)
        @foo    {%foo.v3}

For many applications, like mailing lists and web forums, it is entirely acceptable to preserve the complete history or leave maintenance to humans. But in other cases we might prefer to gradually stabilize and compact our histories; for these, see *Managed Dictionaries*, below. 

Dictionary applications can potentially grow larger than memory. This is acceptable, so long as developers leverage [stowage](LargeValueStowage.md). With stowage, it is not difficult for a dictionary application to model a database or ftp server. 

## Extraction of Applications from the Dictionary

As a development environment, Wikilon must support compilation of applications to run on a desktop, android phone, an independent web service, and so on. Ideally, these are the same applications that we debug within Wikilon, but we want to extract a minimal amount of logic and state from our dictionary to make it work. Dictionary objects are a good fit here, as are applications that operate only on a well-defined subset of the dictionary (e.g. some finite set of named objects). 

Extraction of dictionary applications *in media res* supports an implicit debug mode, automatic records and testing of application debug sessions, and a possibility for interactive construction of applications from a fork of a prototypes. In some ways, this is much nicer than conventional application compilers.

## Managed Dictionaries and Attributes

Words in an AO dictionary provide a basis for mutable meaningful structure, modularity, and structure sharing. The mutable meaningful structure allows AO dictionaries to serve as a platform for live coding and dictionary applications. 

Unfortunately, the presence of mutable meaningful structure interferes with direct optimization at the dictionary layer. This isn't a major problem: a compiler could maintain a cache of optimized definitions separate from the dictionary. But there are some opportunity costs with respect to separating the optimizer, redistributing optimized code, and generalizing automatic  management of dictionary applications. 

To recover the lost opportunities, we can enable developers and dictionary applications to *declaratively relax* the constraints on an assumed dictionary optimizer for useful subsets of the dictionary. I propose the following:

* A word may be declared **hidden** to relax the requirement for stable external reference. An optimizer is free to delete a hidden word if it has no clients. This enables garbage collection of dictionaries.
* A word may be declared **opaque** to relax the requirement for stable structure of its definition. An optimizer is free to rearrange, refactor, or reorganize the definition of an opaque word in ways that preserve its behavior. 
* A word may be declared **frozen** to relax the requirement for mutable behavior. An optimizer is free to inline definition of a *frozen* word into the definition of an *opaque* word. A frozen word is *deep-frozen* if all transitive dependencies are also frozen.

To declare a list of attributes, I propose prefixing any definition as follows:

        [{&hidden}{&opaque}{&frozen}]%

This structure is preserved on import/export but trivially eliminated by simplifiers. It is extensible with ad-hoc new attributes (categories, relations, deprecation, licensing, decay models, etc.) and has no need for runtime semantics. A reverse lookup can find all uses of a token. It's voluminous, but space is cheap. The main disadvantage is that most other dictionary applications will need to recognize and handle this structure, too.

Note: These attributes only affect a dictionary optimizer. The *frozen* attribute is not a security attribute. If a developer wants to modify the definition for a frozen word, he can do so. Though, a development environment might require explicitly un-freezing the word to modify its behavior.

## Robotic Reflection, Metaprogramming, and Maintenance

Reflection entangles. With reflection, a function's behavior and meaning will change based on relationships that are subtle or implicit. Often, changes won't happen until the future, while integrating what probably should be unrelated code. Unit testing, modularity, separate compilation, and portability of behaviors into a new codebase are resisted by reflection. 

By design, Awelon Object (AO) lacks any direct mechanism for reflection.

However, it is possible to *model* reflection. For example, we can model objects that carry some metadata about their purpose and type. We can model a database with pages of code. We can model an entire AO dictionary as an object within an AO dictionary, and reflect on that. By explicitly modeling collections of code and the compilers, we gain a useful ability to isolate, test, and port specific configurations.

It is also feasible to implement reflection as part of an IDE or robot. It is not unusual for people to develop independent autonomous software agents to maintain some aspect of natural language wikis. For a dictionary of structured code, developing a bot should be easier. Also, it is feasible to push more reflection logic into the dictionary itself. Though, ensuring nice properties (e.g. that the resulting codebase is stable and deterministic, that updates are incremental and cache-friendly) remains a challenge.

## Purity of Dictionary Applications

I have contemplated introducing an effects models for dictionary applications. Presumably, effects could provide capabilities for fine-grained error-handling, explicit reflection, performance shortcuts, and even first-class shared resource models within the limits of causal commutativity and spatial idempotence (e.g. unification variables).

However, explicit effects are *complicated*. Interacting with an implicit environment hinders the gradual compaction of history and compression of state for long-running dictionary applications. We must specify the environment. We must deal with competing environment models and versions.

Further, explicit effects are *unnecessary*. For high level error handling, it is sufficient to reject a command that would lead to a bad application state or generate an error report. For reflection, we can model software agents that maintain and metaprogram our dictionary. For performance, we can push for ABCD-like accelerators and better annotations. Any model of shared resources can be modeled functionally and we can mitigate performance by accelerating common patterns. External communications is modeled in terms of RESTful interactions with external agents.

The main costs of purity is that the rest of the world isn't designed for it. Retained mode rendering, for example, favors a stream of update events. We'll need to work with this when designing applications for extraction, e.g. explicitly computing update logs from subsequent representations of state (like Facebook's React).

## Security for Dictionary Applications

Imagine our goal is to model a game where each player is limited to viewing and manipulating a shared game world through a separate avatar. The security requirement here involves limiting the players, as opposed to asking them to police themselves.

To implement this requires we cripple player access to the dictionary. Even read-only access is sufficient for players to 'view' the game world through the eyes of every other player. The AO programming experience is designed for the holistic dictionary. We try and undo, we fork and simulate, we edit and observe. We fork whole dictionaries, use DVCS-style pulls and commits and cherry-picking. We search and refactor whole dictionaries. None of this is compatible with fine-grained security constraints.

But if we're willing to accept that this remains a 'dictionary application' in only the eyes of our game administrators, we can proceed easily enough. We constrain player access to a whitelisted set of dictionary applications that we know to be compatible with our security policy.

*Aside:* If the number of players is small, asking them to police themselves is reasonable. Shared storytelling, quests, roleplaying, and their like are frequently represented on conventional forums. Doing so within a programming medium like AO has potential to create a far more real-time interactive multi-media experience than natural language. It will also automate the administrative burdens. And if a storyteller or player wants to keep a surprise, they can just keep it to themselves, perhaps using a private fork of the dictionary if the surprise needs development and debugging.
