
# General Overview

A recent decision is to actually separate this auxiliary state from Wikilon proper. Instead, we model *abstract virtual machines* (AVMs), each supporting a set of [hosted applications](ApplicationModel.md). But these AVMs must still support some explicit state models. This document describes state models and the resource model in general.

## Transactions and Batches

State is updated using ACId transactions. Durability won't be the default (since this is VCache), but may be requested (in which case all outgoing messages would wait for sync). Naturally, outgoing messages or effects from an AVM must be delayed until the transaction commits. Consequently, we will have 'batches' of generated messages moving towards multiple targets. 

I will preserve this batching in the receiver. Thus, the primary unit of communication between abstract machines becomes *batches of messages*. Messages between AVMs will also preserve serialization order - i.e. an earlier batch will always run before a later batch. Messages are also ordered within each batch. This provides a useful 'snapshot consistency' when viewing other AVMs.

Each AVM is, more or less, a 'vat' in the E parlance. But plus many nice atomicity and consistency properties. These properties will be preserved if we compile AVMs to a separate process or unikernel.

*Note:* I may need to model logical connections to handle disruption and latency effectively. Further, it might be useful to separate batches from stability such that we have a rollback window between AVMs (i.e. Time Warp).

## Resource Structure

Each AVM will have a file system inspired trie of resources. 

The resource trie structure provides many useful properties:

* stable identifiers for live programming or extension
* securely partition tree structure between subprograms
* trivial to delete, copy, or move full subtrees
* structure sharing properties for logarithmic history
* suitable for purely functional AVM implementations

Resource identifiers will more or less correspond to URLs or filepaths. Some characters (LF, SP, C0, C1, DEL) will be forbidden, but otherwise arbitrary UTF-8 is accepted. The character `/` has special meaning, indicating a directory. Objects and directories may share the same name modulo `/`, i.e. `/foo/bar/` and `/foo/bar` may be distinct objects. Note also that `/foo//` is a valid directory distinct from `/foo/`. 

Resources and directories are accessed by *capabilities*. 

A capability is a first-class, unforgeable token that identifies a resource and provides some authority to manipulate it. Taking this literally, here's what our resource capability will look like (at the bytecode layer):

         "RW /foo/bar yqpbfhydyqhpznjm..."{$/}
          ↑  ↑        ↑                    ↑ 
          ↑  ↑        unforgeable HMAC     hides structure from ABC 
          ↑  to the resource named here    (& keeps out of dictionary)
          provides read-write authority

The `/foo/bar` path string and permissions remain visible for convenient debugging. The HMAC here is the primary protection. The `{$/}` seal doesn't provide much security, though it does resist covert channels. Mostly, `{$/}` prevents dependencies on the capability representation, and resists capabilities from being hard-coded into the Wikilon dictionary, and simplifies recognition of capabilities if we want to rewrite them during a deep copy.

Basic mechanisms for resource security include:

1. secure partitioning
2. revocable capabilities
3. value sealing resources
4. permissions attenuation
5. transparent scripting

These impact expressiveness in addition to security.

### Secure Partitions

The idea with secure partitioning is that different subprograms can be isolated to different subdirectories in the trie. By dividing subprograms between subdirectories, developers can comprehend and control the shared state interaction and entanglement of subprograms. Using shared state carefully can be useful for *extension*, e.g. adding new features to an application without deeply modifying application code. But secure partitioning can guard against accidental sharing. 

The current plan is to use capabilities to model partitions. E.g. if you have directory `/foo/`, you can partition this into `/foo/bar/` and `/foo/baz/` for the different subprograms. You certainly won't be *forced* to partition the resources, but we can try to make this a convenient convention to follow. 

Partitioning state then allows us to model 'objects' that can talk to each other by more fine-grained capabilities. For example, a subprogram with full read-write access to `/foo/` might have much more limited access to `/bar/inbox`, e.g. able only to send messages or query whether the inbox overfloweth (to support discretionary pushback). Usefully, even anonymous objects can be modeled by partitioning, e.g. using `anon1/` then `anon2/` then `anon3/`. Though, with anonymous objects you'll need manual collection (modeling regions with subdirectories might help). 

### Revocable Capabilities and Redirects

Revoking a granted authority should be as easy as granting it, or maybe easier. 

Awelon's AVMs will support revocation by cleverly leveraging links within the filesystem. Links allow a many-to-one relationship between paths and resources. The idea, then, is that we can express revocation by destroying a particular path, removing a specific link. All capabilities including that link will be broken.

To use links effectively, conventions and best practices are necessary. For example, if we create a link for each user of a resource, then we can cut out individual users. However, links also simplify the fallback approach: just move the original object then provide individual links to all the authorized users. We might also transparently link the object for a transition phase if security isn't a major concern.

To avoid the entanglement and cycle problems created by links in common file systems, I'm going to add a *confinement* constraint. A directory `/foo/` may only contain links to elements under `/foo/`. A directory `/foo/bar/` may only contain redirects to other elements under of `/foo/bar/`. Confinement makes it easy to copy a directory without breaking any links. I might also reject transactions that would commit a link cycle.

A related concern is *stability of revocation*. For example:

* Bob Foo works with our system and for a while uses path `/xyzzy/bob`.
* Bob Foo retires. The redirect at `/xyzzy/bob` is deleted, revoking Bob's caps.
* Years later, Bob Bar joins the team. The new Admin Alice creates `/xyzzy/bob`.
* Bob Foo tries his old `/xyzzy/bob` capability, and is surprised it works!

This happens because we lose visibility of old links after breaking them. The fix is probably to add a salt, a little extra entropy, such that the HMAC will be different after we include the salt. 

### Resources for Value Sealing

Long term, I'd like some first-class cryptographic value sealers and unsealers. The plan in ABC is to use `{:fmt$key}` tokens for cryptographic value sealing, with the result being represented more or less like `["cryptoText"]kf{$fmt}`. Here `{$fmt}` indicates a sealed value, and the block is to capture substructural attributes. However, choosing an appropriate format and implementing it is a lot of hassle, and only wortwhile after we have real distribution. In the short term, I'm thinking to utilize *second-class* value sealing, by modeling the sealer and unsealer as a pair of resources (maybe together with a manager, altogether acting a lot like a subdirectory). 

Second-class value sealing has a lot of potential advantages:

* *expiration*, model values that can only be unsealed for so many cycles
* *large linear values*, fast storage-based sealing for linear objects!
* *optimize local*, no encryption between AVMs; discretionary up to network

Non-linear objects could use storage-based sealing IF they also expire (to avoid GC issues). Cryptographic value sealing only becomes necessary to avoid GC pitfalls when working with remote machines. Thus, this technique can be very efficient in a lot of common cases, while still offering a level of security that cannot be matched within a dictionary.

Meanwhile, first-class capabilities for second-class sealers give us roughly the same expressiveness. What we lose is performance in the case where a sealer and unsealer would ideally be on different machines. We can come back to first-class sealers and unsealers later, when need arises.

### Permissions Attenuation

A capability has a few associated permissions that are uniformly understood, similar to the read-write-execute permissions in Unix. An important difference is that these permissions are bound to the capability, part of the capability string. 

The set of meaningful permissions depends on resource types. In Unix, we have "dumb" files that cannot hide information or protect their own invariants. For Awelon's AVMs I favor purely functional objects, which may hide information and protect invariants if manipulated through query and update messages. Here are a few scenarios or levels I'd like to support:

* directly modify bytecode for resources in a directory; full ownership
* read bytecode of resources in a directory; deep-copy a directory
* browse a directory, query some resources; allows information hiding
* send update messages to resources; objects guard their invariants
* create and destroy objects through a developer controlled script

A potential set of permissions is:

* *write*  - create, destroy, and replace objects
* *read*   - read resource structure, read bytecode as block
* *update* - send update messages to object
* *query*  - send query messages to object, hear result

These permissions are *transitive* across directories and redirects. If you have read-write permissions to a particular directory, then you have read-write permissions to every subdirectory. Attenuation is achieved by simply taking a capability and requesting one with permissions. To support the 'create and destroy objects through a script', we'll additionally need a means for a script to receive an override authority from the creator.

Directories may be enumerated with a simple query.

### Transparent Scripts

Without scripting, data can be very rigid. For example, when modeling a book you must make a choice: do you use one file per chapter, or one file for all chapters. You could try to keep both up-to-date, but statefully managing data in two places is very error prone. With scripts, we could make one 'canonical' choice then provide an ad-hoc *view* for the other choice. We can transparently model capabilities to individual chapters from the larger book object.

Scripts can also support ad-hoc attenuations, e.g. by wrapping messages or unsealing values. And scripts may have an 'override' feature that gives them read-write authority regardless of the caller's authority. Override is formally justified because you could always have embedded it: it takes read-write authority to create the script. But actually embedding the capability would unnecessarily entangle the script with its location.

A query operation on a script will be limited to querying other objects. An update operation on a script may query and update arbitrary objects. Scripts usually don't update themselves (if they must, one might explicitly use a write capability).

## Resource Types

Most resources are *state objects*. 

State objects are specified either for RDP or Transactional. The queries are the same in both cases. The main internal difference is the update message: RDP receives a *set* of messages and computes to a fixpoint (within limits). Transactional resources receive individual update messages one at a time in batch order. Communication between these modes is limited to queries.

Beyond that, we might have:

* directories
* scripts and redirects
* value sealer resources
* demand monitors for RDP
 * on-demand behaviors (single instance)
* clock resources
* network resources

* random numbers?
* uniqueness sources?

Modeling *network resources* as essentially the same as state resources is feasible based on the idea that we're really modeling output *queues*, and an implicit step is later draining these queues and shipping them off to other machines. Network resources may be restricted to a particular subdirectories near root, but capabilities to them could end up just about anywhere.

## Performance Notes

I don't anticipate any major performance pitfalls right now.

Well... other than the limits of ABC itself.

VCache already addresses sub-value sharing and lazy loading concerns, i.e. so there is no need to break a large object into a lot of different resources. (Instead, we annotate to intern parts of it.)

## Monadic Effects

This resource model does use capabilities, but it doesn't use `{tokens}` for side-effects directly. Instead, the capabilities are used as arguments to a monadic update mathod. Ideally, all [effects](EffectsModel.md) will be monadic and purely functional. I'll try very hard to ensure the monad model exhibits useful properties - flexibility, composability, extensibility, specialization via staging or partial-evaluation, etc.. 

Delimited continuations and free monads will be worth exploring. 











OLD STUFF
=========


# Major Design Points

These are important conclusions that took me some time to reach...

4. **transactional & RDP updates don't mix** The concurrency control models are completely incompatible. RDP is long running, leverages anticipation, retroactive correction, generally allows multiple agents to collaboratively influence future state, prohibits observation of 'instantaneous' events. Transactions assume an authoritative view, a single writer, are logically instantaneous, and transactions are simply serialized with the different writers taking turns. The main consequence of this incompatibility is simply that we must distinguish 'resources updated via RDP' from 'resources updated via imperative'. Fortunately, cross-model *queries* don't seem to cause any problems.


# Persistence of Structure

        {&stow} :: (a*e) → (a'*e)    tuck value into cold store

When a stowed value is observed or accessed, it must automatically be loaded. After all, annotations don't affect observable semantics. To reduce loading latencies, we might wish to explicitly annotate this, e.g. with `{&load}`, so we begin asynchronously loading a value before we'll need it. To reduce risk of accidentally loading a stowed value, developers might be encouraged to wrap stowed values with discretionary sealers.

Usefully, stowed values would be strictly internal, and largely orthogonal to ABC resource model. ABC resources could still benefit from stowed values with regards to persistent, separate compilation. Stowage might also be disabled for read-only computations, such that they're also load-only.


# CRDTs

[Commutative Replicated Data Types](http://hal.upmc.fr/file/index/docid/555588/filename/techreport.pdf) seem a very simple and very promising approach to support coordination in a distributed system. Further, they may also offer some nicer integration properties between RDP and imperative programming, at least where I can add idempotence.

It seems to me that I should be able to model CRDTs as a special object type in the auxiliary state model as it is currently defined. But the question would be how CRDTs are to be shared. I suppose a read authority would be necessary for replication.



# Rejected Ideas

It sounded good at the time.

## Ad-Hoc Attributes Attenuation (Rejected!)

The idea of *ad-hoc attributes attenuation* is users might want to define their own permissions-like attributes that may be attenuated. The implementation of this idea is pretty straightforward: we add some more text to our capabilities, such as:

        "RW /xyzzy/foo/ +x +y -z"

Here, I have three attributes "+x" "+y" and "-z", separated by whitespace. I'm going to keep it simple by forbidding whitespace in my path and attributes. These attributes are meaningless at the resource layer, but they might be meaningful to user code. So we'll simply pass them together with our *query* and *update* messages. 

I listed 'positive' vs. 'negative' attributes. The difference would impact attenuation via simple monotonicity rule: *we may remove positive attributes or add negative attributes*. With read-write permissions, however, we could modify any attributes. They only become monotonic for query or post.

However, there are many weaknesses for this design:

* hurts caching because we don't know which attributes affect query results
* implicit transitivity isn't visible for reasoning about covert channels
* attributes difficult to abstract, painful to parse, easy to forget
* more complicated type signatures for our file objects
* requires extending APIs for manipulating attributes

A good alternative to attributes attenuation is to leverage value sealing together with the scripting support. The script could unseal the attributes then pass them on. Anyone with access to the sealer could create a new set of attributes.

