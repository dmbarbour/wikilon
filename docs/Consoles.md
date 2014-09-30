
Consoles will be one of the first apps I implement, but are something of a special case because they aren't reactive. Consoles are more functional/imperative in nature.

One of my principles is that anything we can access through a web service should also be accessible within the language. Consoles should thus be modeled within ABC, e.g. using a Process object. We'll need this process to receive some reflective caps, both to access words in the AO dictionary and to translate arbitrary ABC text into an ABC block.

Potentially, we could have different console types by modeling different `wikilon\foo` web-apps. Each web-app might be staged, and able to request a *set* of capabilities (depending on whether the app is more reactive or more imperative in nature). 

Consoles will need to interact with [transactions](TransactionalEditing.md) in some useful way. Potentially, this could be modeled by shifting to an updated dictionary (via a special console request). Conceptually, consoles won't always keep their history and so cannot be re-computed when updates occur on the dictionary. For that role, we'll instead want a class of applications modeled off of iPython notebook and similar.


