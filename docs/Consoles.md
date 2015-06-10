
Consoles will be one of the first apps I implement, but are something of a special case because they aren't reactive. Consoles are more functional/imperative in nature.

One of my principles is that anything we can access through the web service should also be accessible for an application modeled in the language (sometimes indirectly, via the [effects model](EffectsModel.md)). Consoles should ideally be modeled as an [application](ApplicationModel.md) hosted by Wikilon, not implemented as a primitive in Wikilon. It might be useful to support volatile or short-lived abstract virtual machines in Wikilon, at least by policy (enabling old content to be deleted).

See also [CommandLine.md](CommandLine.md)
