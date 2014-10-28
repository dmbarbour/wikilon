
I currently envision two state repositories besides dictionary:

* wiki state, modeled as part of the wiki object
* user state, modeled as part of a user or session

These might instead be combined into one large repo, with users occupying spaces within it. Doing so would have significant advantages, such as users being able to publish capabilities to state resources they've modeled internally.

Difference between auxiliary state and the dictionary is:

* users usually have a frozen (transactional) view of the dictionary.
* user state tracks workspaces, consoles, preferences, activity, etc.
* wiki state supports multi-user interaction, behaviors, shared databases.

*Aside:* [Embedded literal objects](EmbeddedLiteralObjects.md) can serve an interesting intermediate role, similar to wiki state objects but tied irrevocably to the dictionary state. Conversely, it should eventually be feasible to reflect the dictionary within state. 

Much like dictionary state, Wikilon auxiliary state WILL support a large amount of history, again using an exponential decay model. And while users cannot obtain a 'transactional view' of user state, they will at least be able to recover information from their history. It might be especially useful if users can have multiple active sessions, each with distinct state, such that we can easily 'fork' an older state into an active session (and perhaps switch between many passive sessions). The ideas here are pretty wide open. But keeping history available seems like a good idea in almost any case, makes Wikilon much more resilient to vandalism, etc.

There is NO direct relationship between auxiliary state and the dictionary. That is, auxiliary state cannot contain capabilities to reference the dictionary, and the dictionary cannot directly reference the auxiliary state. This separation is important because:

* different users view different dictionaries, and users step through time
* protects capability security properties of codebase
* simplifies code - no need for observer patterns between state and dict

Ideally, most of these state resources are subject to both transactional updates/views (for conventional web apps) and RDP-style continuous influence/observation. I do have some generic concepts for declarative state that allow for both [1](http://awelonblue.wordpress.com/2013/03/23/ad-hoc-external-state-models/). In addition, we might leverage linearity to model something very close to local state.

Developers MUST install a long-running RDP behaviors in their environment to represent long-lived policies. This is a *design principle* of Awelon project (described in my earlier post [personal programming environment as extension of self](https://groups.google.com/d/msg/reactive-demand/gazxhLLXscQ/_2YpJ6b3-6sJ)), and a natural property of RDP (most capabilities cannot be persisted; continuous grant instead of revocation; protects visibility, etc.). 

Transactions cannot share or persist capabilities. By nature, a transactional update doesn't have a duration, so we can't share state even within the zero duration. But we might allow transactional operations on shared state to act as a short-lived demand, in order to unify transactional state with RDP state. (I'll need to think about this.)

Long-running behaviors might always be tied to a user agent, e.g. to a particular session, or to an administrative session. If that user is cut off, their active behaviors can be disabled.



For web applications, I think it might be useful to specialize the 'read only' applications, i.e. which do not ask for a write capability. These might allow a much more efficient computation for lots of readers.
