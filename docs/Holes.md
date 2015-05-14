Holes
=====

An interesting possibility is to have Wikilon help with development by creating 'holes' that Wikilon can try to fill automatically. These might be modeled using tokens like `{_holeName}`, with a constraint that all instances of the named hole within a dictionary will use the same definition.

Potentially, I could build holes upon annotations. I'm not sure whether that would be better or worse, though. Annotations are supposed to have identity semantics. A hole has unknown semantics, so might be better separated.

For now, I'll not implement holes. I can't leverage them without a good type system and code search models. But it might be a worthy idea to return to at a later date.



