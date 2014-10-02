
Security is a concern for multi-user wikis.

But rather than worry about it overly much, a viable option is to indicate that some words are 'important' and to simply delay their proposed commitment and wait for approval. Or alternatively, to simply have a buffering transaction between whomever is doing the editing and the final commit to trunk.

I think the model I'm developing will make it easier to do this, though it might require some extra conventions to force use of the buffering transaction (and to let users know that their transaction will require approval). A simple convention might be to mark such words, e.g. by defining `important!foo`. 

