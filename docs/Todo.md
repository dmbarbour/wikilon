
# TODO

* performance and C runtime
 * build my own arenas & GC models
 * optimized bytecode representations
 * leverage partial evaluations


* persistence and reflection
 * dictionaries as runtime values
 * Wikilon state as runtime values
 * eliminate dependency on VCache

* simplify semantics
 * (DONE) inline semantics for word definitions 
 * (DONE) simplified Wikilon application model 

* background computations
 * for types, errors, cycles, undefined words
 * continuous zero-button testing, live tests
 * quickly find type errors and failed tests
 * background compuations and eventual consistency
 * maintaining lists of words with properties
 * caching, recomputation, wide reuse of cache
  * preferably: over history and between dicts
  * leverage structure sharing and stowage
 * index to find words with a suitable type
 * index for fast fuzzy find, tab completion
 * index for full text search 

* debuggable runtimes
 * replays in debug view: animations, etc.
 * automatic debugging and error isolation

* heavier focus on Claw views and dictionary apps
 * (DONE) model for claw command sequences
 * (DONE) improve claw view for large texts
 * visual claw, widgets in source
 * named variables and closures

* less ad-hoc web application views 
 * application logic into AO dictionaries
 * subscriptions model: long polling, web-sockets
 * shared, updateable views as the default
 * favor semantic spans/divs, forms, and CSS
 * try for generic designs (MFlow, etc.)
 * use ETags correctly (e.g. If-None-Match)

* model metadata per dictionary
 * edit sessions
 * bug trackers
 * stats and logs
 * geneology: fork, merge, branch
 * users and activities, collision prevention
 * recent changes 

* generic web resources
 * support for image resources (PNG, SVG, favicon)
 * applications for edit sessions, bug trackers
 * support to upload/download binaries and texts
 * model REPLs, forums, wikis, spreadsheets, etc.
 * box and wires or VRML systems above visual claw
 * support for video-audio resources (GIF, HTML5 video, etc.)
 * turtle graphics, iPython notebooks, xiki

* extraction of applications
 * Optimized ABC
 * JavaScript + DOM
 * Unikernels or Docker apps

* develop FUSE adapters and command line tools
 * directly above C runtime or via web services
 * support for editing multiple words in one file

* user model
 * names and logins
 * themes, configuration
 * quotas, etc.
 * presence (tracking activity via dictionary)
 * support for 'secured' dictionary apps (games, etc.)

Note that I've decided to model almost everything as dictionary apps, including stuff like edit sessions, bug trackers, and users. The idea, or goal, is to keep all of this information uniformly available for import, export, history, views of the dictionary, etc..


