
# Organization of State

Each user will be given a personal space for state, e.g. for workspaces and mailbox and whatever else. In addition, wikis may have a public space, e.g. for advertising or sharing data, that any user or application may access. Further, each user might have a few conventional spaces.

A tree-like directory for state could be useful for many reasons.

1. paths in a tree provide stable names for state resources
2. a capability may access a subdirectory without parent or siblings
3. tree structure simplifies divide-and-conquer computations 
4. tree structure can be easy to process wholistically (e.g. reset)
5. potentially splice or mount subtrees as a basis for sharing

Though, a tree structure does have some weaknesses, e.g. for representing relationships and regions of overlapping interest. We might instead consider something like a Venn diagram, or a clustering model. I'll come back to this below.

Some questions:

* nature of state - bytes, values, objects?
* how to model relations, associated states?
* can we limit entanglement of the state space?
* how to integrate both transactions and RDP?
* how to describe, secure, and share capabilities?

## Bytes, Values, or Objects? Objects.

In a typical filesystem, the content of a file is just a string of bytes. This pushes all the logic into the users of the file. If there is more than one user, they must all agree on the structure of a file and how to update it without stepping on each others toes. If there are relationships, the agent might need special authorities to follow them. Overall, I think it's a awful, inflexible system. 

Ad-hoc value structures - e.g. with text, numbers, pairs, sums, functions - are a fair bit more interesting. They are easily updated by applying ad-hoc functions. Developers can model objects explicitly, if that's what they wish. OTOH, different agents must still agree on the data structure, and risk clobbering one another.

Compared to values or byte streams, objects can control over their own evolution, guard their own consistency, hide information, and even model useful features such as privilege escalation.

*Note:* I mean objects in the sense that `µP.[a→(P*b)]` is potentially a pure fixpoint function that models a simple process object. A rather interesting possibility is to duplicate some interface from embedded literal objects, such that objects can be directly be utilized as applications.

### Structure for RDP Resources? Objects.

Transactions can deal with any of the aforementioned types. For RDP, it is obvious that RDP could *observe* value or string resources, but isn't very obvious how RDP might *influence* them. Well, I suppose we could use a reactive term rewriting model (though that's a bit of a hack). 

With objects, we might allow that not all objects support RDP influence, but those that do could provide a simple update method that receives a *set* of demand values (e.g. represented as a sorted list), thus entering a new state until the set of demands change. If queries are cleanly separated from updates, then observation could be separated almost entirely from influence. 

Potentially, RDP update and transactional update might also be cleanly separated, such that transactions cannot use the RDP update facet, and vice versa.

An interesting possibility is *animated* objects, whose behavior is time-varying with demand in a more continuous fashion, e.g. leveraging an energy model or timeout model or similar. However, I'll leave this idea alone until I have a clean, simple, generic approach to it.

## Limiting Entanglement

In many object-oriented software systems, objects become deeply connected by references. And while this does allow some very rich expressions of behavior, it also leads to frustration, spaghetti code, and the ever popular 'big ball of mud' architecture.

I wish to limit entanglement. Ideally:

* prevent cyclic relationships, which hinder identification of subsystems
* limit cross authority relationships, which limit practical extraction

However, within these constraints, I would like to preserve some rich interactions between objects. We could do a lot with completely disconnected objects or tree structures. But direct routing of information is a significant performance and expressiveness advantage of conventional object-oriented programming.

A viable path forward involves:

* allow parent objects in tree structure to access children
* shared spaces, acyclic, that multiple objects may access

The idea here is that, for example, sibling objects could not directly call one another (as that would allow a call cycle), but each might create a mailbox object and somehow make it visible to the other. Then, indirect communication (with direct routing) would be possible via shared state. One object could add to the mailbox and the other could pull from it.

The question is first how to avoid cycles, and second how to make these objects visible to one another in a simple way that won't entangle their state.

(TODO!)

## Relationships and Associated States: Region Sets

When we have many agents, services, objects, etc. we'll inevitably have some sophisticated relationships between them, and we'll want to model those. This typically results in data kept by many objects, tucked away where it's difficult to keep track of and access. Then the relationship reaches its natural end, and our systems are littered with garbage.

Existing software systems have a number of ways to deal with this issue: [ephemeron tables](http://en.wikipedia.org/wiki/Ephemeron), weak references, regions, and so on. However, ephemerons and weak references aren't appropriate for Awelon project because we don't have reference comparisons. And regions, at least as conventionally applied, are extremely constraining on the developer.

My proposal here is a simple variation on regions:

* an object is created or named with a *set* of regions
* when a region is destroyed, so are all objects associated with it

In Awelon project, a region tag would be represented by a special sealed value. 

Usefully, we might not need to explicitly create region tags. Regions would instead be associated with other objects - e.g. if a specific directory in the filesystem were to act as a region, then deleting that file would trigger a cascading destruction of other files and directories.

The main difference from conventional regions is that we can use an ad-hoc set of region tags, and thus our objects are effectively part of multiple regions at once. This should make associations a lot easier to build.



## Distinguishing Leaf and Node? Best of both?

In a typical filesystem, a name exclusively either is a file, is a directory, or does not exist. But there is a related possibility, that every name is both a file and a directory, e.g. such that `/foo/bar` may both have some content and some children. 

The ability to add children to any object does seem potentially useful. We could easily create 'associated' objects, metadata and such. OTOH, I think name collisions are likely, permissions may be problematic (e.g. permission to write metadata vs. permission to read file). Worse, this might introduce covert channels that are difficult to reason about, e.g. with different apps sharing information via metadata (similar to how they manipulate cookies today). 

So it seems there is a good argument for providing capabilities to a specific object separately from providing capabilities to a space of objects. That said, there might still be some strong relationship such that every leaf `/foo/bar` has a corresponding space `/foo/bar/`.




## Rumpelstiltskin Tree Hash

Rob Meijer wrote a couple articles describing Rumpelstiltskin Trees for minorfs [1](http://minorfs.wordpress.com/2014/02/20/rumpelstiltskin-and-his-children/)[2](http://minorfs.wordpress.com/2014/03/21/rumpelstiltskin-and-his-children-part-2/).

It seems attenuation, in Rumpelstiltskin design, must be linear. I.e. if we have three different facets (foo+bar+baz), we can only remove them one at a time, in a specific order (foo+bar+baz)→(foo+bar)→(foo)→(), where the empty aspect presumably gives us the ability to locate the object but not do anything with it (useful for storage). We can generally simplify this expression to (baz)→(bar)→(foo)→(locate). For minorfs, Rob's attenuation function is likely from (write)→(read)→(locate), with the conventional 'file is a sequence of bytes' concept. 

For an object system, we might instead try something like: (source)→(update)→(query)→(locate), e.g. where (own) gives us the ability to directly view and modify the code, and (update+query) gives us all the normal messages for an object.

For an RDP-based resource system, we might extend this slightly to: (own)→(update)→(influence)→(query)→(locate), preventing RDP capabilities from supporting transactional updates. OTOH, I'm not confident this would offer any significant benefits. 






The tree structure of Rumpelstiltskin is providing access to an extensible set of resources - i.e. the subdirectories of minorfs. Similar would hold true for 




If we step away from Rumpelstiltskin trees, we can easily find other techniques that are more flexible in their attenuation. Our capabilities might instead consisted of three parts: a locator, a list of facets, and an HMAC. Or, alternatively, we could provide a different capability for each facet, and simply bundle the capabilities, so long as they have common locators.



To attenuate, we would send a message to the object, asking for a further attenuated capability.

It is, of course, feasible to attenuate at a fine granularity if we use some other techniques, such as using a larger object name that includes three elements: a locator, a list of privileges, and an HMAC.


I'd like to explore a bit. How much of a limitation will linearity be, and is there is a way to avoid it?

Here, (owner) can directly observe and modify object state and source code, while (message) allows you to update it only through the normal messaging interface, and (query) gives access to read-only observations of the object. There might also be some utility in  adding another layer, e.g. (protected/friend) vs. (public) levels of access for messaging. 

Or we could favor value sealing to model different rights within the .

The 'tree' in minorfs is really offering an extensible set of resources, orthogonal to the content - i.e. you can access all objects in that tree, and you can grant access to a specific subtree without granting access to the parent or siblings.

















# General Thoughts

I currently envision two state repositories besides dictionary:

* wiki state, modeled as part of the wiki object
* user state, modeled as part of a user or session

These might instead be combined into one large repo, with users occupying spaces within it. Doing so would have significant advantages, such as users being able to publish capabilities to state resources they've modeled internally.

Difference between auxiliary state and the dictionary is:

* users usually have a frozen (transactional) view of the dictionary.
* user state tracks workspaces, consoles, preferences, activity, etc.
* wiki state supports multi-user interaction, behaviors, shared databases.
* feasibly, auxiliary state might be more entangled than the dictionary.

*Aside:* [Embedded literal objects](EmbeddedLiteralObjects.md) can serve an interesting intermediate role, similar to wiki state objects but tied irrevocably to the dictionary state. Conversely, it should eventually be feasible to reflect the dictionary within state. 

Much like dictionary state, Wikilon auxiliary state will support a large amount of history, again using an exponential decay model. It might be possible to build the decay function into each state resource, but point decay (losing sample information) should probably be the most common.

In this case the history isn't for a transactional view of state, but is rather to support debugging, easy recovery from errors, etc.. Though, we can certainly copy snapshots of the past into the present.

## Relationship between dictionary and auxiliary state?

If there is a relationship between the dictionary and auxiliary state, it should be based on the most recent transactional values in the wiki, rather than the current view of the individual user. A potential relationship is between state and the state model (how that state receives updates). E.g. the state model for a given state resource could be named by a word. 

However, it isn't clear to me that this is the best way to do things.

It might be better to cleanly separate dictionary and state, such that neither depends directly on the other. This separation would certainly be easier to reason about, yet would still offer some interesting interactions.

## State Observation and Influence: Transactions and RDP

To support conventional web applications, state may support some transactional operations - e.g. read-only for GET, or read-write for POST or PUT. However, my long term goal is to shift towards RDP based continuous observation and influence, and to cast most state in those terms.

I do have some generic concepts for declarative state that allow for both [1](http://awelonblue.wordpress.com/2013/03/23/ad-hoc-external-state-models/). There may also be some way to leverage linearity.

Support for read-only GET applications could be a major performance boon. And if I can enforce idempotence of PUT, e.g. by forbidding you to read and write the same states or restricting apps to write-only, that would also be interesting.

### Sharing Capabilities

Developers MUST install a long-running RDP behaviors in their environment to represent long-lived policies. This is a *design principle* of Awelon project (described in my earlier post [personal programming environment as extension of self](https://groups.google.com/d/msg/reactive-demand/gazxhLLXscQ/_2YpJ6b3-6sJ)), and a natural property of RDP (most capabilities cannot be persisted; continuous grant instead of revocation; protects visibility, etc.). 

Transactions cannot share or persist capabilities. By nature, a transactional update doesn't have a duration, so we can't share state even within the zero duration. But we might allow transactional operations on shared state to act as a short-lived demand, in order to unify transactional state with RDP state. (I'll need to think about this.)

Long-running behaviors might always be tied to a user agent, e.g. to a particular session, or to an administrative session. If that user is cut off, their active behaviors can be disabled.

