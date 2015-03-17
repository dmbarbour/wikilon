
# State Models

Last week, I developed a vision for a unified resource model based on:

* manipulating a filesystem-like trie of named resources
* cryptographic capabilities to control access to files and directories
* 'files' as purely functional objects allowing query and update messages
* network or UI via files modeling inboxes, outboxes, queues, etc.

I do like the idea of modeling a filesystem with a trie. But does every AVM need a filesystem? Using cryptographic capabilities internally is rather heavy-weight. Could we use something more lightweight? Using the effects model for local state resources seems awkward, especially since state is purely functional. Could we find something easier?

Fortunately, within Wikilon, we can leverage VCache with ABC to model very large values and functions. Even a filesystem scale trie containing purely functional objects may be implemented as a plain old ABC value. Similar patterns should not be too difficult to reimplement in other contexts, such as a unikernel with a block device or Amazon S3 external storage. ABC can even support this in an open distributed system, using `{#resourceId}` to name ABC resources, adding a few suffixes like `'k` to indicate the resource constructs a relevant value. (Though, use of names does make GC more difficult.) Ultimately, at the very least, we can leave state structure to the user. 

Let's go with that: **AVM state is just an ABC value**. 

An important constraint for Awelon project, however, is that the machine state should be cleanly separated from the machine's update function. The separation might be modeled by treating state as both parameter and return value. This separation serves a dual purpose:

* independently update or live-program our machine's behavior
* visualize, inspect, debug, and directly modify machine state

Usefully, we should be able to leverage the same [structure editors being developed for dictionaries](ExtensibleSyntax.md) for visualizing and manipulating both our AVM state. And we'll probably just bind our AVM behavior to a dictionary. So a single editor model can cover all these aspects, greatly simplified by immutable structure and purely functional behaviors.

Remaining Questions:

* how to model cryptographic capabilities?
* should the AVM support substructural types?
* how to model communication between machines?

For crypto-caps, it shouldn't be a significant problem to simply inject e.g. `[{:hmac$key}]` or `[{:aes$key}]` capabilities into our AVM's initial state, or perhaps a secure-random source that may fork new keys. And maybe a few secure random models if we want them. Injecting cryptographic caps that are purely functional by nature shouldn't be a problem, we only need to be clear regarding which protocols a given AVM platform supports. Usefully, including crypto caps internally supports a clean separation of concerns of internal vs. external identity, i.e. where internal identity is modeled by sealers and unsealers, and external by the network relationships.

Regarding substructural types within AVM state: I plan to allow them. Developers must provide their own typeful control regarding which fragments of state must be copyable, droppable, and so on. If they want fast parallel queries, they can model this directly in terms of copying state then applying a query function in parallel with returning the original state. Because AVMs are substructural, this gives them a stronger notion of identity - i.e. they generally not be copied, destroyed, forked (though debuggers could easily violate these conditions).

A communication model requires detailed attention. Since I cannot have 'standard' structure for state, state can no longer be used to directly communicate effects (no inboxes, for example). A lot of old ideas - capabilities, effects, revocation, etc. might need to be shifted into the [network layer](NetworkModel.md).

Note: A lot of older content has been deleted, but should be available via github (2015-03-16).
