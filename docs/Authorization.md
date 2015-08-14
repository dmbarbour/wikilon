
# Authorization in Wikilon

I would like for Wikilon to supply object-capability based authority at a fine granularity. However, I'm amenable to having more conventional authorization for interaction with browsers - e.g. tracking users, groups, passwords.

To integrate user-based authorization: an interesting idea is to provide a monad transformer for the WikilonModel. If we do this, I can integrate flexible, fine-grained authorization models. Object capabilities, meanwhile, could simply bypass said authorization mechanism... something like: `Capability a b → a → m b`, with capabilities serializable into strings (perhaps via encryption).
