
I'm interested in developing an extensible state model for Wiki services, user models, and administration. 

A constraint on AO is that updates to state resources must be spatially idempotent and causally commutative. 

For Wikilon, I plan to support some imperative updates, albeit transactionally (serializable updates), because this fits the use of web services. Transactional operations do preserve spatial idempotence and causal commutativity, so long as we understand each operation to receive its own transaction-specific read/write capability.

RDP behaviors and state models will be better in the long run. But a relevant question, then, is how RDP state should interact (if at all) with transactional state. It does seem useful that we could, for example, transactionally obtain a snapshot of RDP states, or transactionally assign an RDP state.

In any case, I'll want the state itself to be modeled using Awelon project values and/or AO resources.

I'll also want to model state as 'external' to whatever process views and modifies it.

I'll probably want to continue with exponential decay of history, just as I do for the Wiki dictionary, such that I can undo, redo, debug, etc. a great deal of state.
