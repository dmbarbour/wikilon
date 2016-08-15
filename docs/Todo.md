
# TODO

My primary efforts at the moment should be:

* profile Haskell interpreter vs C interpreter.
 * use a few shared, simple benchmarks
 * make sure I have a good start, here

* simplify & accelerate Wikilon's ABC code
 * systematic support for accelerators
 * develop more wide use accelerators
  * fast fixpoint inline (fix.i)
   * maybe as an attribute?
  * faster conditionals
   * `if` and `if_` behaviors.
  * common EQ/NEQ behavior.
  * sum to boolean conversion.
  * some deep stack manipulations
 * develop specialized accelerators:
  * deep structural manipulations
  * list processing accelerators
  * consider support for 'compact' texts

* compact bytecode model
 * simplify fast-copy, etc..
 * contain binaries, blocks, and texts
 * design with eye towards sharing

* persistence and stowage
 * potential for shared, stable object models


* shared bytecode model
 * avoid full copies for large bytecode.
 * for now, maybe separate from stowage.


* profiling and debugging of AO code
 * simplified stack traces
 * other profiling options

* get web service active again!
 * develop useful application models
 * develop some console style apps

* command line utilities
 * favor via web service, not filesystem
 * support queries, claw, computations
 * support import/export to AO files
  * with or without history
 * FUSE adapter (maybe)

* runtime performance
 * compact bytecode
 * shared memory (loopy code, binaries, texts)
 * simplification and optimizations
  * partial evaluation and applications
  * dead code and data elim - trash, etc..

* extraction of data
 * texts, images/video, sound/music
 * binaries
 * cacheable results, please.

## Lower Priority (for now)

* dictonary applications
 * model of edit and debug sessions
 * multi-media interactive fictions 
 * multi-media multi user dungeons 
 * quests

* persistence and reflection
 * dictionaries as runtime values
 * Wikilon state as runtime values
 * eliminate dependency on VCache

* extraction of executables or libraries
 * extract to Rust, C, JavaScript
 * perhaps via reflection of dictionaries

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
 * visual claw, stateful widgets in source
 * named variables and closures (?)
  * I haven't found any variation of this I like
  * maybe to fix a local `x y z` variable environment

* Tunable claw (not critical)
 * bytecode views defined within dictionaries?
 
* Neat features
 * render environments, animate evaluations 
 * color-coded namespaces
 * bolder or larger words for expensive words

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

* user model
 * names and logins
 * themes, configuration
 * quotas, etc.
 * presence (tracking activity via dictionary)
 * support for 'secured' dictionary apps (games, etc.)

Note that I've decided to model almost everything as dictionary apps, including stuff like edit sessions, bug trackers, and users. The idea, or goal, is to keep all of this information uniformly available for import, export, history, views of the dictionary, etc..

The main challenge with this concept is security. Security *within* an AO dictionary isn't really a feasible thing. What we can probably do, however, is control interactions (merges, dataflows, etc.) between dictionaries, and perhaps also constrain merging of value sealers. An external agent would make decisions given a set of dictionaries.
