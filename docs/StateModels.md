
# General Overview

In addition to the dictionary, Wikilon needs an auxiliary state model to support user workspaces, issue trackers, feature requests, new web applications, multi-user services, and so on. 

Unlike the dictionary, users will not typically have a 'frozen' view of the auxiliary state model. That said, it might be useful on occasion to experiment on a temporary fork or historical view of state resources, e.g. when initially developing and debugging. I will be keeping a fair amount of historical state for various reasons - browsing, debugging, recovery from vandalism, archaeological digs, etc.. 

Given historical views, I would like some ability to track which objects were updated by a recent transaction or tick event. This could prove very useful. It might require keeping a timestamp or event stamp on every resource.

## Major design points

These are important conclusions that took me some time to reach...

First, it seems **simple object models** are a better option than dumb byte strings or plain old data. Objects can protect their internal structure, provide their own logic and model, support extension, and perhaps even be self-explaining (if the support the right queries). This simplifies collaboration between agents sharing the resource.

Which model of objects to use is still an open question. For example, `µP.[a→(P*b)]` is too simplistic; it fails to distinguish updates from reads, which would be a nice property for attenuation and for optimization. It would be useful if HTTP GET only requires read-only access to the acid-state resources. [Embedded literal objects](EmbeddedLiteralObjects.md) would be an example of a simple object model, which might be suitable for imperative resources. RDP resources must be updated by a set (sorted list) of demands, but we might be able to treat that as a specialization.

Second, **state resources must be weakly isolated**, by which I mean they do not keep `{tokens}` other than annotations, sealers, unsealers, or links to isolated ABC resources. This level of isolation is necessary for imperative resources to protect causal commutativity and spatial idempotence, because we must prevent the case where we alias a resource (i.e. opportunity for an 'already in use' error). Isolation is also useful for RDP resources due to logically continuous expiration of token capabilities. I suspect purity of resources might also be a good idea.

This level of isolation isn't a bad thing. It makes for a simple model with low entanglement and easy reasoning. That said, there is a valuable a workaround. See **Capability URLs** below.

Third, **imperative and RDP resources should be separate**. A resource should be declared for one interface or the other. The problem is that the concurrency control models are very different (serializable transactions vs. concurrent demands with anticipation and retroactive correction), and trying to mix the two creates a hairy mess that I'd much rather avoid. That said, imperative and RDP computations can interact in useful ways. For example, so long as we avoid cross-model updates, the cross-model queries are okay.

# Design Ideas

I've attempted a couple designs from whole cloth, but they've fallen apart on me leading to various 'major design points'. For now, I'm going to float a bunch of different ideas and see which sink, which rise, and which can be composed into something whole and valuable.

## Capability URLs

As a major design point, we don't keep `{tokens}` in state resources. However, nothing prevents us from creating an intermediate value, such as capability URLs, to access resources. This would allow much richer resources, similar to hypertext. And since I expect to actually support access to resources via URL, this would also ensure internal programmability remains at least as expressive as external programmability.

The idea, then, is that we take our capability URL, and we "open" it.

Open might fail for dynamic reasons. For example, open might fail if the resource has been destroyed and is no longer available. For transactions, open might also fail because the resource is already in use, i.e. such that opening it would risk a violation of linearity properties. Though, even for transactional imperative code, it might be acceptable to open resources multiple times so long as all are read-only.

Usefully, we'll never have a permissions failure. 

The authority to a resource is determined by the capability URL itself. It is possible that we attempt to use a URL in a manner not authorized by it - e.g. open a read-only capability for read-write - and that will result in failure. But this is closer in nature to attempting to divide by zero; it's failing due to a property of the URL, not a property of the user. Identity and permission simply never get involved. 

Opening our URL will typically return a `[{token}]`. The interface to this token will depend on the mode with which we opened the URL. But the important aspect is that: 

* Token is volatile, restricted to a transaction or an RDP behavior.
* Token is neither forgeable nor reusable. Perhaps via HMAC or GUID.
* Token is opaque. Original URL cannot be recovered from token text.

In a sense, our new token serves a similar role to a file handle. In some modes, the token may permit explicit release, returning a sealed value that may be explicitly 'closed' to recover the resource. However, this is likely to be a rare requirement. RDP doesn't need it, nor will most transactions. So it might involve an extra mode flag or something like that.

## Organization of State

### Flat Structure?

Convention is a file belongs to a directory. 

An alternative is a file has a canonical GUID, that is not tied to any particular directory structure. This would be similar to the 'inode' of ext filesystems, except that the GUID is also independent of the storage medium. Many directories may then refer to the same file, perhaps with associated metadata. Directories themselves may be plain old files, themselves having a GUID and a conventional structure (e.g. a table).

I like this idea, at least as a conventional approach for sharing resources.

However, assuming the flat structure also applies to resource allocation, it may prove difficult to attenuate and delegate authority to create new objects, manage quotas, and similar. It may also be too easy to "leak" objects, i.e. resulting in dead objects that cannot readily be rediscovered.

### Tree Based Directory Structure?

The conventional tree-based (or forest-based) structure where every file belongs to a directory is very convenient from an administrative standpoint - i.e. an administrator can measure quotas, track activity, delegate subdirectories, and so on. 

A tree structure is convenient for creating stable names; essential for RDP.

Many benefits of the flat namespace might still be acquired, so long as we can easily share capability URLs to objects and subdirectories deep within our trees.

The directory structure does mean we need to deal with a new type of object, the directory. 

### Overlapping Regions?


## Resource Life Cycle


Question: How do we create new objects?

The difficulty, likely, will be proving authority over an object.


Concerns: 

* Creation of new identifiers for new resources.
* We need to share IDs without sharing authority.
* How to eventually clean up unnecessary objects.

Well, we could create new GUIDs by secure hash. But the problem is proving authority over a GUID.


It isn't difficult to create new GUIDs via secure hash. We must then prove *authority* over these GUIDs, so let's say this requires a secret value.



This would actually allow some arbitrary 'public' roots, such as hash("foo"), but would also enable secret resources that use a secret value in their construction.

The question, then, is how we decide authority over "foo". One option, perhaps, is to require an HMAC that again uses the 'secret' that is "foo". 



So authority for "foo" is a result of hmac("foo","ReadWrite:"++hash("foo")), where 



Actually... that might work pretty well. 


compose an existing identifier with some other value and take a secure hash or compute an HMAC. 

Though, this does create a relevant question, of how we should go about deciding who gets authority over this new ID.


We can even compose multiple GUIDs to create new GUIDs.

Well, the problem of creating new identifiers for new resources is not difficult: just use a secure hash, perhaps combining with a value we can rebuild.


Well, one way to create a new GUID is to use a secure hash of an existing ID, or perhaps an HMAC. Presumably, we could create whole directories of arbitrary new names. But  






## Representation of Capability URLs

I would like to keep this concrete. What does an actual, serialized capability URL look like in my ABC? In AO? In an HTTP reference? This ties into some other questions, such as:

* location
* attenuation
* discovery
* dependencies



How do we *attenuate* this URL?




 






While (query)

`read` authority 


 messages to the object (or influence demands

That is, we shouldn't need to use much external authority to turn the string into a capability.

An advantage of this 





The feature gap might be covered by a careful application of sealed values modeling secure paths that can (via powerblock) be translated back to linear capabilities on something like a first-come first-serve basis. The resulting indirection might serve us in other ways, e.g. supporting revocation.

## Token Scrubbing (this idea is poison)

I had this idea, so I thought I'd write it down. Rather than *fail* when tokens are injected into persistent state, we could simply scrub the tokens, e.g. by replacing them with `{☠}` (skull and crossbones, U+2620) or `{☢}` (radioactive, U+2622). The effect is that we could accept more flexible interactions with our resources. The disadvantage? We've poisoned the well. In retrospect, it seems much wiser to me to fail fast, fail early, rather than inject material that will hurt users later.

## Attenuation of Objects

Consider the following authority levels:

1. «source»: view, rewrite, or replace the bytecode for the object.
2. «update»: update or query the object through its normal interface.
3. «query»: query the object through its normal interface.

This contrasts with conventional «read», «write» authorities of filesystems. The «update» authority allows writes but in a manner that allows an object to protect itself. The «query» authority allows reads that can hide information or require unsealers to access.

Is this good? Can we add flexibility?

A common way to attenuate authority in Awelon project is to wrap access to a `[{capability}]` in some ad-hoc code, guarding the input or transforming the output, i.e. `[transformInput{capability}transformOutput]`. This can be very flexible. Obviously, we can't do this directly due to the limitations on tokens, but there are several ways we could achieve this design.


 I think we could achieve something like this via a variation on redirection objects. However, that does come with the risk of complicating any notion of object identity.

*redirection/wrapper objects*, i.e. redirection with some added logic, albeit at the risk of complicating any notion of object identity.



Assuming ad-hoc transformations, we could even take a simple *read-write* authority on values and translate it back into objects, e.g. on write, our transformer might need to read the current value, transform it, and write it back.

 arbitrary *simple objects* above


 Obviously, if we naively presented this bytecode to a client who can introspect the bytecode, there would be nothing preventing them from 


 It seems feasib



Within Awelon, this works because users cannot poke inside the bytecode and lift the capability back out of it. However, a similar approach could work 

It would not be difficult to model all three of the aforementioned *authority levels* in terms of wrapping access to a 


There is a lot of interesting atte

 that allows an object to protect its own authority. The «query» authority allows reads.
And these objects might even implement their own authority models via use of value sealing.

ow will we attenuate authority to directory structures? 

Well, let's put that aside for now. I still need to decide what directory structures should look like, and whether resources should belong to more than one directory, and so on.




My intuition is that directories should not be modeled as normal objects. We don't want directories as a chokepoint to reference the resources created within them, so it's important that resources within a directory be separate from the directory itself. That said, 

Further, *by default*, it might take a «source» authority to query, create, or destroy resources owned by a directory.



I think they'll need to interact with special authorities to create new children that can be referenced without the chokepoint of the directory. However

## Redirection? (bad idea)

It is feasible to implement invisible redirection as a first class feature of directory structures. A potential advantage of doing so is support for *revocation*: we can easily destroy the redirect. It also might be feasible to add logic to the redirect, or other features.

In retrospect, I think this is a bad idea. For two reasons:

* It hides aliasing, which hinders reasoning, e.g. linearity of transactions.
* Creates entanglements at the directory layer, rather than the object layer. 

We might model redirects at the object layer, perhaps even as a specific object type. 

## Managing Relationships with Region Tags (too complicated?)

If we have many agents, services, objects, etc. we'll inevitably build some sophisticated relationships between them, and we'll want to model those. So we start keeping data about these relationships between other objects, and these relationships essentially become objects themselves. A question, then, is how to clean up.



This results in data kept by many objects, tucked away where it's difficult to keep track of and access. Then the relationship reaches its natural end, and our systems are littered with garbage.

Existing software systems have a number of ways to deal with this issue: [ephemeron tables](http://en.wikipedia.org/wiki/Ephemeron), weak references, regions, and so on. However, ephemerons and weak references aren't appropriate for Awelon project because we don't have reference comparisons. And regions, at least as conventionally applied, are extremely constraining on the developer.

My proposal here is a simple variation on regions:

* an object is created or named with a *set* of regions
* when a region is destroyed or reset, so are objects associated with it

In Awelon project, a region tag would be represented by a special sealed value. 

Usefully, we might not need to explicitly create region tags. Regions would instead be associated with other objects - e.g. if a specific directory in the filesystem were to act as a region, then deleting that file would trigger a cascading destruction of other files and directories.

The main difference from conventional regions is that we can use an ad-hoc set of region tags, and thus our objects are effectively part of multiple regions at once. This should make associations a lot easier to build.


# Organization of State

A tree-like directory structure has many advantages:

1. paths in a tree provide stable names for state resources
2. a capability may access a subdirectory without parent or siblings
3. tree structure simplifies divide-and-conquer computations 
4. tree structure can be easy to process wholistically 
5. familiar; comparable to file systems, URLs, etc.

So we'll be using this structure unless we run into any major issues with it. One weakness is that a tree is not so great for modeling relationships. I'll return to this concern later.

Some questions:

* resource life cycle?
* how to track resource updates?
* how to model relations, associated states?
* how to describe, secure, and share capabilities?
* reasonable whole-directory operations?


## Resource Discovery, Initialization, and Reset

In the normal course of operation, our resources have stable names and might stick around for hours, days, or years. However, even stable names aren't eternal. There is always a first user of a name. There is possibly a final user, after which we reset a resource back to its pre-initialized state to recover resources. 


## 

* how to track resource updates?


## Describing and Sharing Capabilities

What is the *internal structure* of a state resource capability? I.e. what does it look like, when serialized? 

One simple option is a triplet of (locator,facets,HMAC). 

: a locator, a list of facets, and an HMAC. Or, alternatively, we could provide a different capability for each facet, and simply bundle the capabilities, so long as they have common locators.



## Wholistic Directory Operations

copy/clone, swap, destroy, etc.



Older Content
===============

## Distinguishing Leaf and Node? Best of both?

In a typical filesystem, a name exclusively either is a file, is a directory, or does not exist. But there is a related possibility, that every name is both a file and a directory, e.g. such that `/foo/bar` may both have some content and some children. 

(Meta Thought: I might be better to stick with a `bar.foo` style, like I use for words, with context on the right and specific object on the left.)

The ability to add children to any object does seem potentially useful. We could easily create 'associated' objects, metadata and such. OTOH, I think name collisions are likely, permissions may be problematic (e.g. permission to write metadata vs. permission to read file). Worse, this might introduce covert channels that are difficult to reason about, e.g. with different apps sharing information via metadata (similar to how they manipulate cookies today). 

So it seems there is a good argument for providing capabilities to a specific object separately from providing capabilities to a space of objects. That said, there might still be some strong relationship such that every leaf `/foo/bar` has a corresponding space `/foo/bar/`.

## Rumpelstiltskin Tree Hash

Rob Meijer wrote a couple articles describing Rumpelstiltskin Trees for minorfs [1](http://minorfs.wordpress.com/2014/02/20/rumpelstiltskin-and-his-children/)[2](http://minorfs.wordpress.com/2014/03/21/rumpelstiltskin-and-his-children-part-2/).

It seems attenuation in Rumpelstiltskin design is linear. Rob Meijer uses only one attenuation: read-write to read. I might use two levels: ownership to message to query-only. Here, ownership allows direct read-write access to the bytecode, and ability to create new objects or destroy existing ones. Message limits to passing messages (query or update), and thus allows the object to secure itself. 




