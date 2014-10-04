
Okay, so I now have a few ideas on how to address multiple users.

# Sessions (highest priority)

A **session** exists as a layer between users and the dictionary.

1. Typically, each user has a session. In some cases, a user might have more than one session, e.g. for editing different branches, or for testing certain multi-user interactions. It is also possible for multiple users to share a session, though this is less common.

2. A session is identified by a long, pseudo-random string. For HTTP access, this string will usually be placed in a cookie, but Wikilon will also support special capability URLs whose only purpose is to read the URL, set the cookie, then forward the user into whatever remains of the URL. All web-applications are defined by the dictionary, then observed within a session.

3. A session is associated with a specific *branch* in the dictionary. A session may also have a set of local edits that have not been committed. A given session can pretend isolation, i.e. edits by other people won't change the observed behavior of Wikilon from the perspective of a given session. However, when you go to commit, you may find yourself in conflict with a commit from another session. To help with this, we'll keep sessions aware of potential conflicts with 'sibling sessions' operating on the same branch.

4. A session has state resources. State resources are for imperative or RDP programs, and must respect causal commutativity and spatial idempotence. The dictionary, with its specialized transactional editing model, is not considered a state resource in the same way. (It could be used as one, but that would hinder cross-compilation of stand-alone applications.) A parent session may observe states of any of its child sessions. The expectation is that conflicts will be rare.

5. A session has a set of associated authorities. A session can create new session identifiers, with equivalent or reduced authority. A session also has uniform ability to revoke authorities granted to any specific child, or disable or destroy the child. Naturally, all such effects are transitive. Sessions may also have other limitations, such as quotas.

So, we have these sessions. The issue two special edge cases are the *master session* and the *guest session*. The master session is a root, a source of all authority for a given Wikilon instance. Every session is a child, directly or indirectly, of its master. A *guest* session, OTOH, is a session that can be created for a prospective new user who lacks a session. 

The master session starts on the master branch. It may be feasible to create new 'root' transactions, as a special authority. Essentially the same as the master session (perhaps minus a few capabilities). Multiplicity at the transaction layer is certainly a possibility.

Masters will have a lot of authorities that are dangerous and not available to guests by default. For example, support to compile and install plugins would be disabled.

# Welcome your Guests! (high priority)

Especially for a budding language like AO, it's important to keep the barrier for entry very low. Guests need a warm welcome, an easy start, rich interactions and visualizations. 

Relevant points:

* Guests should see progress, and start on an active public branch.
* A guest may already have a session, just forgot to log in with it.
* Guests are likely to 'lose' sessions regularly and just create new ones.
* Explicit act of creating a session can help teach users how the system works.
* Guests may be reasonably angered if people use content they haven't committed

* So offer *volatile* session to a new user; dies a few days after disconnect 
* Volatile sessions DO NOT support URL-based access. They are cookie-based. 
* Volatile session can be renamed into a permanent one through explicit act.
* At that point, permanent session has URL that can be bookmarked, delegated
* Still not quite the same as *registering* (no association to user identity)

A permanent session might have a password associated with it, too. 

So every guest gets a new session, which lasts for the lifetime of a cookie, but is able to easily create a new session or join an old one, or even explicitly delete their current session.

The question is how guest sessions should be created in the first place. One option is to have the master control them directly. Another option is to have the master create a prototype guest, whose permissions can be edited to affect all guest sessions in real time. There may be special permissions associated with accomplishments, including registry.

# Stabilizing the Important Words (medium priority)

Assumptions:

* *most* users want to contribute and help
* *most* users are smart and capable of learning
* *a few* users are the type to piss in the pot

It's important to keep the system working smoothly *despite* the small handful of assholes that are out to ruin it for everyone. So a question is: how do we do this?

Well, a viable possibility is that 'important' words - those upon which many other words depend - probably should have a longer gestation before committing to change. But I'd rather not SLOOOOWWW DOOOWWWWN the editing process, especially when what we really need is for *other* people to think and act on it right away.

Another viable possibility is that we can 'undo' the commit. 

What does that mean? Well, after the commit, the information is public and should stay that way. So we can't just push it back into the original session. Similarly, the requirement is that every session provides the user an isolated view of the dictionary state. So we cannot change what the user's session experiences. Finally, we don't want to branch the wiki any more than necessary!

So I think what 'undo' means is that we create another transaction that undoes changes by a prior transaction, or at least for some specific words, i.e. rewriting and renaming them back to an earlier definition. It's a new transaction, properly. The users will ultimately need to figure out how to regulate themselves.

We can potentially leverage various heuristics, as well, to prevent committing when it's likely to break things:

* do the automatic tests still pass?
* does the software still typecheck?

If a commit breaks things, we might not permit it (especially not for the main public branch).

# User Registration and Authentication (low priority)

Normally, a session is just a number, not bound to any particular user. 

However, we may support a feature of binding a user proof-of-identity to a given session, such that said identity must be utilized whenever accessing the session (with some time-space constraints, e.g. a particular computer for so many hours). Proof-of-identity could be many things: password, separate identity (e-mail, SMS, phone); biometric challenge (voice, face); etc.. 

We might also associate registered sessions with donations/accounts/bitcoin, and perhaps namecoins. 

Optional password support wouldn't be a bad idea to get started. A password would be provided together with the capability URL, mostly to resist accidental leaks. However, I'd favor a password-less system by default.

# Session State and Workspaces

The dictionary generally does not keep *volatile* information, e.g. about users, user preferences, workspaces, and so on. All that information is associated instead with sessions. Wikilon web applications may also interact with session states. We're still a big step removed from my vision of a personal programming environment, i.e. we aren't yet directly modeling user actions as a command stream.


Large Scale Multi-User (Across Wikis)
=====================================

# Hyperlinking Content: No.

While the main workflow for a wiki is definitely transactional and 'linear' within a wiki, an interesting possibility is to leverage cross-wiki links to make it easy to share up-to-date content between wikis.

I'm not entirely sure about this, though. If we have `foo` and `foo@elsewhere`, we'll certainly be tempted to start editing `foo@elsewhere` just as easily as we edit all the other words. But while such editing might seem acceptable from our limited POV, it would be relatively difficult for us to know whom else is using `foo@elsewhere`, and to account for their tests and such. (And, conversely, nobody else would readily when they break our stuff.)

No... half the reason for a wiki is to gather all the code you'll need, to have a whole dictionary at your fingertips, reusable pieces cobbled together for thousands of projects. And, importantly, Wikilon instances will often run offline (e.g. instance per user).

We need instead to share code between wikis by means that preserve this property: don't *link* someone else's vocabulary. Reproduce or merge it into your own private language. But we do need to address the issue of tracking when the remote vocabulary is updated.

# Cherry Picking and Merging Remote Content: Maybe.

The main workflow I'm envisioning is transactional. One is operating on a mostly-linear branch of the wiki, occasionally creating new branches when necessary for some purpose. However, we'll occasionally want content on our branch that is only found on another branch (which might even be hosted on an entirely different Wikilon instance). My initial proposed approach to this: 

* create a merge session for two branches
* pick the content you want 
* leverage renaming and refactoring tools as needed
* add the (modified) remote vocabulary to your own

An interesting possibility is to automatically rewrite the content you want in terms of your own vocabulary. We can start with the assumption that there is much shared vocabulary, or at least equivalent words, especially for lower level concepts. Rewriting the imported concept in your own words might help you grasp it. Further, we can have the concept of a user and (personal) wiki instance maturing together, i.e. when you're near to grasping a concept, it won't take many words to describe it.

But I'd still like good support for continuing updates. 

I suppose the github-style explicit pull, push, and pull-request relationships could work well enough, assuming we track where each word came from initially. We could also automate a subscription, e.g. a long-running check to see if any relevant updates have been applied. This will require exposing an API that allows us to review a history of transactions for words of interest.

Anyhow, I certainly want this property: cherry-picking of *content*, not of *changes/patches*. Take the content you want, not the entire context around it. AO, at least, should make it a lot easier to extract content due to the acyclic structure and composability of dependencies.


Misc
====

# THOUGHTS

One question is whether I should even bother with cookies. Why not just use session-specific URLs at all times? Well, I suppose I don't want users to share that URL by accident, and thus share direct access to their own session. But I will need to provide users easy access to administrative web-apps for obtaining session URLs, creating child-sessions (with a subdirectory for state; pet names; and various ad-hoc authorities), revoking sessions, and so on.

# SESSION SECRET?

Originally, I was envisioning a single 'secret' for a full wikilon instance. But another viable option is a per-session secret. If I plan to generate secrets deterministically for each session, I'll probably want secrets the same size as my secure hash function (i.e. 384 bits). But if I want all randomness to stem from one source, then there is less need to persist random information; I can compute every session ID from the inital secret and a simple counter (perhaps including the parent ID).

older content
==================

With multiple users, I should be concerned with:

* User Display Preferences
* Private User Data
* Sessions and Workspaces
* Password Recovery and Two Factor Authentication
* Delegations and Revocations
* Authorities and Privileges

.. and perhaps more.

A curious issue is how users and their state should interact with transactions, multi-user transactions. 

