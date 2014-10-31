
# General Overview

In addition to the dictionary, Wikilon needs an auxiliary state model to support user workspaces, issue trackers, feature requests, new web applications, multi-user services, and so on. 

Unlike the dictionary, users will not typically have a 'frozen' view of the auxiliary state model. That said, it might be useful on occasion to experiment on a temporary fork or historical view of state resources, e.g. when initially developing and debugging. I will be keeping a fair amount of historical state for various reasons - browsing, debugging, recovery from vandalism, archaeological digs, etc.. 

Given historical views, I would like some ability to track which objects were updated by a recent transaction or tick event. This could prove very useful. It might require keeping a timestamp or event stamp on every resource.

## Major design points

These are important conclusions that took me some time to reach...

First, it seems **simple object models** are a better option than dumb byte strings or plain old data. Objects can protect their internal structure, provide their own logic and model, support extension, and perhaps even be self-explaining (if the support the right queries). This simplifies collaboration between agents sharing the resource.

Which model of objects to use is still an open question. For example, `µP.[a→(P*b)]` is too simplistic; it fails to distinguish updates from reads, which would be a nice property for attenuation and for optimization. It would be useful if HTTP GET only requires read-only access to the acid-state resources. [Embedded literal objects](EmbeddedLiteralObjects.md) would be an example of a simple object model, which might be suitable for imperative resources. RDP resources must be updated by a set (sorted list) of demands, but we might be able to treat that as a specialization.

Second, **state resources must be isolated**, by which I mean they do not contain `{tokens}` other than annotations, sealers, unsealers, or links to similarly isolated ABC resources. Isolation is necessary for imperative resources to protect causal commutativity and spatial idempotence (rather, access to resources must be linearized). Isolation is necessary for RDP resources due to continuous expiration of capabilities.

Isolation isn't a bad thing. It certainly makes for a simple model with low entanglement and easy updates. But developers may feel the need to work around isolation to model hypertext-like behaviors. The feature gap might be covered by a careful application of sealed values modeling secure paths that can (via powerblock) be translated back to linear capabilities on something like a first-come first-serve basis. The resulting indirection might serve us in other ways, e.g. supporting revocation.

Third, **imperative and RDP resources should be separate**. A resource should be declared for one interface or the other. The problem is that the concurrency control models are very different (serializable transactions vs. concurrent demands with anticipation and retroactive correction), and trying to mix the two creates a hairy mess that I'd much rather avoid. That said, imperative and RDP computations can interact in useful ways. For example, so long as we avoid cross-model updates, the cross-model queries are okay.


Brainstorming
=============

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


## Relationships and Associated States: Region Sets

When we have many agents, services, objects, etc. we'll inevitably have some sophisticated relationships between them, and we'll want to model those. This typically results in data kept by many objects, tucked away where it's difficult to keep track of and access. Then the relationship reaches its natural end, and our systems are littered with garbage.

Existing software systems have a number of ways to deal with this issue: [ephemeron tables](http://en.wikipedia.org/wiki/Ephemeron), weak references, regions, and so on. However, ephemerons and weak references aren't appropriate for Awelon project because we don't have reference comparisons. And regions, at least as conventionally applied, are extremely constraining on the developer.

My proposal here is a simple variation on regions:

* an object is created or named with a *set* of regions
* when a region is destroyed or reset, so are objects associated with it

In Awelon project, a region tag would be represented by a special sealed value. 

Usefully, we might not need to explicitly create region tags. Regions would instead be associated with other objects - e.g. if a specific directory in the filesystem were to act as a region, then deleting that file would trigger a cascading destruction of other files and directories.

The main difference from conventional regions is that we can use an ad-hoc set of region tags, and thus our objects are effectively part of multiple regions at once. This should make associations a lot easier to build.

## Describing and Sharing Capabilities

What is the *internal structure* of a state resource capability? I.e. what does it look like, when serialized? 

One simple option is a triplet of (locator,facets,HMAC). 

: a locator, a list of facets, and an HMAC. Or, alternatively, we could provide a different capability for each facet, and simply bundle the capabilities, so long as they have common locators.



## Wholistic Directory Operations

copy, swap, destroy, etc.



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




