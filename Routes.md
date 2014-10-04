
HTTP Routing Structure for Wikilon
==================================

At the moment, I'm far from sure about how I should arrange everything.

Some considerations:

* access a complete list of AO words
* get word definitions via HTTP GET
* update multiple words atomically, or one at a time via PUT?
* access historical views of dictionary
* observe changes in word definitions via websockets
* ABC and AO control streams via HTTP and websockets
* support for 'console' applications; multiple consoles
* privileged console for toplevel config and admin (capability URL?)

I suspect I should predefine all the toplevel routes, then possibly provide some mechanism of adding new toplevel web apps via dictionary naming conventions... e.g. a word like `wikilon/foo` may describe a handler/lens for words via the `/foo` URL path, receiving HTTP requests: the remaining path + query params. I'll want to support a lot of staging, caching, and partial evaluation.

Cookies? I'm tempted to prevent use of cookies from reaching web apps. I might use them separately to track the user for purpose of display preferences and so on.

It might be useful to limit which words can be processed. But that could be handled by `wikilon/foo` if we also provide it some information about the input (e.g. the rest of the URL, including the query), along with reflection on the dictionary... so it can compute a web-app for a given route. (We could track which values it accesses in the dictionary to know when the web-app must be updated.)

Security will be a concern, but can mostly be addressed later. I might need to consider per-word edit authorities via capability URLs (with individual or timeout-based revocations).

# Specific APIs

* ask for public branch capabilities? 
 * publishing of capabilities in general? 
 * fits my older designs for ambient authority in capability systems
* start a new transactional session on a named branch
* create a new root branch in which you're the master
* extend wiki with a remote root

* PUT/GET a single word
* PUT/GET a batch of words
  * GET batch of words matching pattern
  * GET batch of words plus all dependencies
* variations on PUT with constraint on prior version (for atomic updates)
  * might be good as the default limitation to limit accidents



# Streaming ABC or AO via HTTP and Websockets

Websockets are relatively easy for streaming. HTTP is a more interesting case. One possibility is to use PUT on a sequence of URLs representing different paragraphs (sort of a single-assignment variable approach to streaming - e.g. PUT socket/1 (abc code); PUT socket/2 (abc code). Another possibility is to use POST or PATCH. But I like how PUT can provide nice properties: idempotence and commutativity of the messages. Older paragraphs may gradually be lost.

New socket IDs can be based on a large random number, or can be more deterministic in nature using a secure hash or HMAC.

I would like for websocket variations to be persistent most of the time, such that we can continue working with them after we reconnect. Though we can gradually destroy old connections.

# Consoles Model

Streaming ABC or AO in one direction. Reactive observation of a server object in the other direction. Keeping it separate for simplicity, and potentially more flexibility. To protect the spatial commutativity and spatial idempotence, we'll be using a single powerblock in the target.

A 'privileged' console may have a more powerful powerblock, e.g. capable of defining new toplevel services or configuring which ports the webserver is open on. Initially, a capability URL for this will be provided via the command line when Wiki starts.

# Observations Model

In general, we should be streaming updates for performance reasons (with the option of sending a whole value). I like the possibility of treating the observation stream also as an ABC stream. But we could also perform a server-side ABC to JavaScript conversion.

