
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

For many applications, like mailing lists and web forums, it is entirely acceptable to keep a complete history or leave maintenance to humans. Space is cheap enough that we could easily keep complete histories for anything that involves discrete human inputs. But in other cases we may need to gradually stabilize and compact our history. It is feasible to automate processes that will systematically inline, simplify, refactor, and garbage collect words at the dictionary layer to recover space. However, to avoid upsetting users and breaking applications, we must describe which optimizations are permitted. One viable possibility is discussed in [About AO](AboutAO.md) under *Hidden, Opaque, and Frozen Words*. 

Applications may potentially manage very large amounts of state, e.g. modeling databases and filesystems that are much larger than RAM. This is feasible with a `{&stow}` annotation, allowing us to move large values out of RAM and into disk storage. However, this only impacts cached computations.

Between these various techniques, dictionaries can truly serve as general software platforms and ecosystems.

## Providing Security for Dictionary Applications

As a security use case, I imagine modeling a game (perhaps a multi-user dungeon) where each player should:

1. control their own character, not world state or other characters
2. observe the state of the game world only near their character

Modeling the game world state is feasible with dictionary applications. The greater challenge, then, is controlling the players. A viable option would be to constrain access to the dictionary through an application interface, at least for some subset of users who are not administrators of that dictionary.

## Extraction of Dictionary Applications

As a development environment, Wikilon must support compilation of applications to run on a desktop, android phone, an independent web service, and so on. Ideally, these are the same applications that we debug within Wikilon, but they are not entangled with the dictionary after extraction. Dictionary objects are a good fit here. An interesting possibility is to extract an object in media res. We must cross-compile this object into something we may run on another machine, and perhaps optimize for the target environment.

Usefully, the idea of extracting an application fits nicely with other 'views' of the dictionary. It's just that we're 'viewing behavior' this time, e.g. extracting by cross-compiling to a language like JavaScript or C or x86. In terms of User Interface, a compiler might trivially be a URL that we HTTP GET from Wikilon (with the normal caching).

The idea of extraction might also apply to non-interactive targets, such as PDF documents and JPEG images and MP3 music files.

## (Rejected) Effects Models for Dictionary Applications

Dictionary applications restrict developers in how they model effects. However, there are a lot of effects that fit within the constraints of dictionary applications. Some possibilities: 

* try a computation and handle type errors
* constrain a computation to so many steps
* support distributed or parallel computation
* cryptographic value sealing and unsealing
* uniqueness resources and design patterns
* reflect on a block and view its content
* accelerate graphics, physics, collections
* model constraint or unification variables

In context of AO, our environment is naturally represented by a value provided to our application. Access to effects is provided together with this value, e.g. perhaps an association list that includes values like `[{try}]` and `[{reflBlock}]`. Or perhaps the environment is modeled by a powerblock or something else entirely. 

I expect to experiment with many different environment models. To indicate which environment our application is expecting, we could apply an unsealer like `{.env:std.v1}`. This would indicate a name and version, imply an initial value. It is feasible to develop configurable environment models that further receive an executable specification. Thus, even if we want to shift specification into the dictionary, it seems sufficient to start with just a name to infer or inject an environment value.

If we implement environments, Wikilon would recognize and implement at least a few environment models. 

However, I'm concerned that a command-pattern of interactions with an environment won't compress very nicely. Naturally, if we depend on the environment value, then we must allow that our behavior will change when the environment changes. Hence, we're forced to record the entire history of commands, unless we have some way to combine and compress those sequences of commands.

Without effects, we can still accelerate computations via ABCD-like accelerators and annotations. We can annotate constraints on computations, quotas and similar. We can functionally model constraint or unification variables. The main items we cannot handle without effects are error handling and reflection on blocks. But error handling isn't critical, since we can always push that up to the dictionary layer (report a type error) and effectively forbid erroneous interactions with dictionary apps. And reflection on blocks generally isn't critical because blocks are opaque massive structures anyway.

So, my current resolution is to eschew modeling effects within dictionary apps. Maybe I'll revisit this issue later.
