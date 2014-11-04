
# General Overview

In addition to the dictionary, Wikilon needs an auxiliary state model to support user workspaces, issue trackers, feature requests, new web applications, multi-user services, and so on. This will likely form the foundation for all resource models in Wikilon, much as filesystems do for conventional operating systems.

Unlike the dictionary, users will not typically have a 'frozen' view of the auxiliary state model. That said, it might be useful on occasion to experiment on a temporary fork or historical view of state resources, e.g. when initially developing and debugging. (Or even to run the same code on *all* the historical samples, as a quick form of regression testing.) I will be keeping a fair amount of historical state for various reasons - browsing, debugging, recovery from vandalism, archaeological digs, etc.. 

Given historical views, I would like some ability to track which objects were updated by a recent transaction or tick event. This could prove very useful. It might require keeping a timestamp or event stamp on every resource.

# Major Design Points

These are important conclusions that took me some time to reach...

1. **purely functional objects** are excellent for collaboration between mutually distrustful agents because they protect their own state, guard information, and leverage value sealing, yet don't have the entanglement issues of OO systems. A trivial example of a purely functional object model is `µObject.[Message → (Object * Answer)]`. However, I'll likely want to separate queries from updates (i.e. so I can optimize HTTP GET to use read-only queries), and simplify consistent views. Conveniently, [embedded literal objects](EmbeddedLiteralObjects.md) seem to be a pretty good fit for the role of state resources, ensuring consistent views for both readers and writers. Of course, to fit RDP, the update argument might be a a sorted list of structurally distinct demands (a set of demands) instead of a message.

2. **serialize imperative through a powerblock** AO/ABC can operate in imperative modes only by ensuring operations on any specific resource are linearized (otherwise we lose causal commutativity and spatial idempotence). However, precisely tracking which resources are 'in use' is difficult, especially if we want redirection, adapters, or attenuation features. As a programmer, I don't want to be burdened with 'already in use' errors. So, my plan is to logically serialize everything through "the powerblock". That said, we should be able to get a lot of asynchronous computation, e.g. returning the powerblock immediately while the query result is processed in another thread. 

3. **transactionalize imperative operations** If we model logical time as advancing between operations on a powerblock, we can observe writes by other imperative behaviors. This is a problem because it makes for difficult reasoning about code correctness, about partial failure and cleanup, and about consistency of stateful resources. It's also a problem because it would allow for productive polling loops, which is something I wish to strongly discourage. RDP should be the only option for long-running behavior in Wikilon. Console apps should at least transactionalize the commands; each command must terminate successfully to be productive.

4. **transactional & RDP updates don't mix** The concurrency control models are completely incompatible. RDP is long running, leverages anticipation, retroactive correction, generally allows multiple agents to collaboratively influence future state, prohibits observation of 'instantaneous' events. Transactions assume an authoritative view, a single writer, are logically instantaneous, and transactions are simply serialized with the different writers taking turns. The main consequence of this incompatibility is simply that we must distinguish 'resources updated via RDP' from 'resources updated via imperative'. Fortunately, cross-model *queries* don't seem to cause any problems.

5. **stateful resources are weakly isolated** Specifically, no `{tokens}` allowed, with a few machine-independent exceptions like annotations, sealers, unsealers. This is necessary to protect linearization of resources for imperative code, and to ensure compatible cross-model data queries between RDP and imperative, and potentially for logically continuous expiration of volatile capabilities via RDP behaviors. These 'live' tokens shall be treated as volatile capabilities within Wikilon. If we also assume purely functional objects, then we can reject tokens even in query or update messages. (Aside: in type system terms, location of code corresponds to a modality; isolation corresponds to a universal or unconstrained location.)

6. **capability-string based hyperlinking** We can't store live tokens, but we'll still want to model hypertext, directories, registries, relationships, etc.. So we'll need some form of capability values which may be passed to a powerblock to access resources without accidentally aliasing them. Also, we'll want to use *the exact same strings* for external programmability of Wikilon, e.g. via web APIs (PUT, POST, GET, etc.). These must be true capability strings, providing authority independent of user identity, no need for permissions (thus one less failure mode).

# Brainstorm Mode!

Supposing I have a space for stateful resources in Wikilon, what's next? 

The step that's obvious to me is to divide this ball into different resources for both public services (issue trackers, etc.) and individual users. I expect that, later, individual users will want to divide their own little spaces in a similar manner - public spaces, spaces for each significant application they use, etc.. Applications might again do the same.

In context of RDP, this division process must be stable, idempotent, and commutative. These are also nice default attributes for imperative code, e.g. offering stability across rewind and replay of a transaction, though we certainly could model a counter, a flat directory, and `new`-like behavior using imperative code.

A natural fit for this spatial organization is the simple tree data structure. We would delegate subdirectories to each agent or application. A critical constraint, then, is that authority to a child directory offers no inherent authority to the parent, i.e. no `..` path unless it is granted explicitly. 

Though, one property I dislike about trees is how easily applications become dependent upon the relatively arbitrary structures. We might mitigate this by offering an illusion of each agent or application having its own root authority. Usefully, we can potentially implement stable but opaque names by use of secure hash or HMAC. 

For now, let's take as a given: **tree structure semantics, but opaque IDs**

Besides directories, our tree structure will have many 'leaf objects', representing the user-defined resources. As mentioned earlier, I currently favor pure functional objects in this role, with a clean separation of read-only queries and write-only updates, e.g. based on embedded literal objects. But we may find some use for other objects, such as:

* **read-only access** to a directory should give us both the ability to browse a directory and query the objects within it, including read-only access to its subdirectories and their objects. This suggests every object would have a read-only variant on the capability. 

* **transparent redirects** would offer an interesting ability to 'mount' objects or directories within other directories. Interestingly, this could give an inverted view compared to conventional databases: my home is root, but I have access to public or shared resources in subdirectories. Redirects and mounts may also serve as a useful foundation for visible, *transitively revocable* attenuation.

* **scripts** allow transparent scatter-gather of data, lenses and data model transforms, ad-hoc attenuation, etc.. Usefully, we can have a clean separation of responsibilities: scripts are stateless, but instead are encoded as simple transactions or RDP behaviors. Scripts would be extremely beneficial for expressiveness and extensibility of the resource model. May have opportunity to attenuate authorities based on caller.

* **identities** are also necessary for demand monitors, constraint models, and other stateless resources. They might also be useful for secure random number generators and other, related concepts.

I've already filtered this list down to what I think are great ideas, worth implementing. 

So, what shall our capabilities look like?

* **authority descriptor** e.g. query only
* **stable identifier** specific to the object
* **path tracker** for transitive revocation
* **hmac of the above** to secure the capability
* **compacted into a string** suitable for use in a URL

Based on the idea that we'll mostly be working with objects and scripts (with their own built-in logic), I feel we need two primary layers of authority: one to update or query through the message interface (which allows objects to protect themselves), and another to read or write source code for administrative and maintenance purposes. So I propose the following:

        CODE    CLASS           AUTHORITIES         MNEMONIC
         N      name            name                Name
         P      update          update              Post, Push, uPdate
         Q      query           query               Query
         M      message         query+update        Message
         K      destroy         kill                Kill
         R      read            read                Read
         S      inspect         read+update         inSpect
         W      ownership       read+write          Write, oWn

            read implies query
            read+write implies update
            query or update imply name
            write implies kill

Write authority on a directory is needed to create objects, query to enumerate children. Name authority allows confirming an object's existence. Write-only is not supported because I consider it unwise to blindly write without giving a model some opportunity to defend itself. Update-only is probably sufficient for a job where write-only might be considered, or you could use a script. Kill is an attenuation of write, and allows deleting an object (resetting it back to initial state). More codes can be added as needed to cover new use cases, so long as there's a nice lattice involved. E.g. it might be neat to disable an object, but it isn't really clear how this fits the current lattice, except maybe by cutting a redirect.

These authorities also attenuate objects discovered through enumeration of directories or by naming children. I'd like to support similar 'discovery attenuation' for capabilities held within scripts, at least as an optional transform.

Regarding stable identifiers, I'm inclined to leverage HMAC with the parent's unique identifier and the child's pet name (e.g. "foo"). This would provide stability regardless of whether an object is destroyed and created many times. Since HMAC is mixed with a secret, it's also infeasible to reverse, e.g. in case someone with a reference wonders where the object sits in the tree or forest.

The path tracker seems a challenging element to model. 

Any given capability is discovered through a finite sequence of steps, e.g. from parent to child, and via transparent redirects. If any of these steps are broken, the capability should be disabled. This supports *transitive revocation*, which is a useful property when delegating various authorities. 


There should exists an 'authoritative path' through our resource model, e.g. from parent to child or via transparent redirects. This path leads us to our current capability. 

 via redirects, that allowed us to discover a particular capability. This path is distinct from conventional parent to child relationships. We must validate that such a path still exists in order to continue use of our capability, thereby allowing transitive revocation. But it isn't very clear how to track this path without our capability strings growing arbitrarily large.

A more explicit variation is to add a 'liveness dependency' to an existing capability - albeit, in a functional way, i.e. `withDependency(X,Y)` may create a new capability that is just the same as `X` except that it is disabled if `Y` is destroyed or disabled. Usefully, such an expression would be idempotent. Unfortunately, I still don't know how to represent this relationship without capabilities growing very large.



While this idea is interesting, a simpler approach might be to explicitly attenuate a capability relative to some other objects, returning a new capability that will be revoked

What is the shape of this path? Well, normal paths are modeled as a list of vertices. But in this case, we might need to deal with named edges, e.g. representing specific redirects. 

Something like: object redirect object redirect object redirect?


. Obviously, we can't start at root, since that could access everything. Nor can we start at , especially in context of possible cycles.





Mostly, this path would consist of 

besides the obvious parent to child relationship, that allows discovery of a resource. For the most part, this path will involve transparent redirection. 

If any step in that path is disabled, then so should be the capability in question. 

might work on a very simple principle: if any object identified by the tracker is disabled, then so is our capability. 

This simple technique protects principles of both revocability and visibility. To effectively leverage a tracker will take some more sophisticated logic at the endpoints, but the idea is that most trackers will be associated  

The HMAC shouldn't 

The 




 Tracker IDs are also stable, so it's feasible to temporarily disable a subset of capabilities and enable them again later; developers are simply encouraged to not reuse trackers. 




Trackers are intended to support *revocable* access to a directory or object. The main issue is directories: if you give Alice authority to a subdirectory, then later take that authority away, ideally we want the revocation to disable Alice's access to the directory *as if you had deleted it*. Essentially, revocability should also be transitive. 


The final HMAC to protect the capability needs no explanation.

I'm thinking our capability strings should look a lot like this:

        Mdpxgznkbmxftkjdphhgqsmbszdzmtnkypxqhzxqmzstsqmszqybtqskdpbxfhkbkzhngghqmgnftkbxshmpxhpszxpzqndjt

Unlike conventional filenames or URLs, this resource identifier is almost devoid of human-meaningful semantic content and provides all the authority necessary to use it. So, while it's somewhat large, it's also combining all the roles of user (via tracker) and password (via HMAC), and may be used in ways that normal URLs cannot. The first character is a code for the authority we have, the rest include 192 bit GUID, 64 bit Tracker, 128 bit HMAC. No type descriptors. Embedding in bytecode would generally take the form:

        "Mdpxgznkbmxftkjdphhgqsmbszdzmtnkypxqhzxqmzstsqmszqybtqskdpbxfhkbkzhngghqmgnftkbxshmpxhpszxpzqndjt
        ~{&rsc}[{:rsc}]vr$c

The annotation `{&rsc}` would assert that this is a valid identifier. This little step would mitigate many issues that arise from embedding strings and links in code! For example, we could easily scan for broken links. It may also enhance performance if we plan to use the resource many times. The `{:rsc}` discretionary sealer then prevents us from directly touching the string (at least by accident). 


GUIDs and HMACs don't need any explanation.


Unlike GUIDs, which must be stable, we want our 

there is little need for stable trackers. Just the opposite, we *don't* want stable trackers, because reusing a tracker ID for the same resource is a problem. 






 authorities to the children of that directory are also revoked. 

Scripts might have some authority to attenuate names relative to some other name.

The trigger for this revocation is essentially the destruction of an object. 

The idea here is that tracker identifiers are determined entirely by the sequence of *explicitly redirection objects*.

 sequence of transparent redirects. 


Whenever a transparent redirect is added to the model, we'll select a random, unused tracker ID.





Support for *transparent redirects*




The *redirection trackers* are a rather important idea here. We're  important idea


## Unifying Objects and Directories?

Uniformity is a nice property. It might be nice to unify objects and directories, i.e. presenting a similar interface such that directories don't require any special consideration in application logic, or that objects may simulate directories to some degree. But it certainly isn't clear to me how, without monadic objects or similar, we could create new names. The separation might be more useful to keep explicit.

It is possible to model transparent redirects as primitive objects, and perhaps even a few other structures. 





I previously mentioned that I'm favoring embedded literal objects as the basis for state resources, i.e. pure functional objects with read-only queries and write-only updates. That said, I could modify these objects to be monadic in nature (and still purely functional), such that an object can act as a redirect based on its own internal logic. Alternatively, I could treat redirects as a special 

An interesting possibility is t 








An interesting question, then, is what other features we might offer above our directory structure. 



A valuable feature for a system is uniformity. At the moment, we essentially have a system with at least two types of elements - a directory tree structure, and leaf objects. At the moment, I'm assuming leaf objects will be purely functional objects that cleanly distinguish read-only queries from write-only update messages. It would be nice if directories presented a similar interface and thus may be treated, transparently, the same as any other object. I think this is feasible, so long as we're a little careful about it.












Rather than flat files, the objects at the leaves of this tree will be purely functional objects, i.e. persisting the bytecode. Likely, they'll have the same basic self-modifying form as [embedded literal objects](EmbeddedLiteralObjects.md), distinguishing read-only queries from write-only updates. : directories would, ideally, present a similar interface as these other objects. Directories w


A strong desiderata, then, is that 


At this point, we essentially have:

        data ResourceTree a =








Stable names also seems like a useful feature for imperative code, especially since imperative code is transactional and thus requires stable names to find the same resources every time. 

Usefully, we could avoid the concept of 'new' objects

A tree-based directory structure would be a good fit.




Essentially, this division process 


## Source Stable Unique Names

RDP requires a spatial idempotence property. This forbids use of `new Object()` because - to be idempotent - that expression

http://awelonblue.wordpress.com/2013/08/26/source-stable-uniqueness/





So, we must somehow express this act of division. But we can potentially 



The concept, then, is of a root space that can be divided into more spaces. 



As an additional constraint on this model, we want to avoid routing everything through the root space.
The typical filesystem directory structure seems a decent example of such behavior.












## Visibility and Revocability





## Capability URLs

Note: original idea of 'opening' a capability URL might still be applicable. But an alternative might be to use something like a `{&rsc}` annotation to validate the resource ID and capture it within a container.

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



## Distinguishing Leaf and Node? Best of both?

In a typical filesystem, a name exclusively either is a file, is a directory, or does not exist. But there is a related possibility, that every name is both a file and a directory, e.g. such that `/foo/bar` may both have some content and some children. 

(Meta Thought: I might be better to stick with a `bar.foo` style, like I use for words, with context on the right and specific object on the left.)

The ability to add children to any object does seem potentially useful. We could easily create 'associated' objects, metadata and such. OTOH, I think name collisions are likely, permissions may be problematic (e.g. permission to write metadata vs. permission to read file). Worse, this might introduce covert channels that are difficult to reason about, e.g. with different apps sharing information via metadata (similar to how they manipulate cookies today). 

So it seems there is a good argument for providing capabilities to a specific object separately from providing capabilities to a space of objects. That said, there might still be some strong relationship such that every leaf `/foo/bar` has a corresponding space `/foo/bar/`.

## Rumpelstiltskin Tree Hash

Rob Meijer wrote a couple articles describing Rumpelstiltskin Trees for minorfs [1](http://minorfs.wordpress.com/2014/02/20/rumpelstiltskin-and-his-children/)[2](http://minorfs.wordpress.com/2014/03/21/rumpelstiltskin-and-his-children-part-2/).

It seems attenuation in Rumpelstiltskin design is linear. Rob Meijer uses only one attenuation: read-write to read. I might use two levels: ownership to message to query-only. Here, ownership allows direct read-write access to the bytecode, and ability to create new objects or destroy existing ones. Message limits to passing messages (query or update), and thus allows the object to secure itself. 





## Should State Resources be Pure? Yes.

I reject `{tokens}` from being part of state. But this doesn't forbid tokens from becoming arguments to queries or updates. Indeed, it might be convenient to have side-effects driven by internal state of objects. But the difficulty with this is, again, that readers won't be able to make the same observations on state as writers (who might observe by passing some tokens into the object), and also we end up exposing internal state of objects by accident.



