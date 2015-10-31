
# Wikilon's Application Models

Wikilon is intended as both a software platform and development environment. 

As a software platform, Wikilon should be able to directly host wikis, forums, blogs, image canvases, document editors, REPL sessions, spreadsheets, chatrooms, interactive fictions or multi-user dungeons, and other applications or web services. As a development environment, Wikilon must support edit sessions and layout, debugging sessions, and compilation of applications for execution elsewhere. Development environments might be considered an important use-case of hosted applications.

Primary concerns as a software platform include *state* and *security*. 

I have decided to focus Wikilon exclusively on *dictionary applications*. 

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

## Cross-Compilation of Applications

Two levels to consider:

* extracting applications from the dictionary
* cross-compilation of dictionary applications








One basic mode for Wikilon as a development platform is to *compile* applications for use elsewhere. In this case, we specify the compiler and application - each of them functions - in a URL for HTTP GET. Typically, these links will be easily constructed. We may also need to specify a dictionary and version number. The 'build' process is implicit, highly cacheable, and performed by the Wikilon server.

For these apps, nothing is directly run on Wikilon other than the compiler function, which is pure or close enough (maybe leveraging memoization or ABC's linker model). There are no stateful requirements other than maintenance of the dictionary. There is no entanglement between the dictionary and the application after compilation. The application is downloaded for use elsewhere.

The encoding of functions in a URL can be pretty straightforward, perhaps using base64 if we need more than a single word. Any user-defined syntax is feasible in a base64 encoding. By encoding ad-hoc functions in URLs, it becomes easier to share or bookmark content up to about the size of an AO module or command line, without always being asked to generate a new wiki page.

"Application" here doesn't need to be executable. It might be documentation, music, video, etc.. It's just something that requires an additional compilation pass after we hit the ABC bedrock.

