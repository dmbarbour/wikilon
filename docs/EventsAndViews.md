
Wikilon Events and Views
========================

So far (June 2015) I've used a rather ad-hoc approach for Wikilon to provide views and events via WAI. However, this probably won't work very well when I start introducing security concerns, logging and history tracking, cache invalidation, multiple pages with common views, etc.. I need a more robust approach.

What I would like to do is divide the responsibilities more clearly:

* one module that supports the stateful model and update events 
* one module that supports flexible, composable snapshot views of Wikilon
 * may need to integrate with events in some way, i.e. for mutable views
* the WAI module that presents views over HTTP
* perhaps a separate AVM module that presents Wikilon over the AVM network

It might take a week or two to perform this refactoring, if I focus on just it.
