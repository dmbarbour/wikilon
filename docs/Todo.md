
# TODO

I'm doing a pretty big rewrite for the following purposes:

* simplify the AO semantics
 * return to *inline the definition* semantics for words
 * undefined words are still useful as *holes* in dictionary
 * no explicit staging; limit is partial evaluation
* background computation of types, errors, cycles, undefined words
 * need model for background computations
 * need model for eventual vs. immediate consistency
 * need model for caching and recomputation
* stronger push towards use of optimized bytecode representation
 * push towards much faster computations
 * push towards partial eval as basis for dictionary apps
* develop a less ad-hoc web application view (e.g. MFlow, Snap, Servant, Salvia).
 * maybe build my own variant oriented around ABC and JavaScript?
* heavier focus on Claw-like code views, support Claw sequences
 * claw sequences potentially very useful
 * let's also try for a Form-variant of Claw (Fawn?)
* heavier focus on dictionary applications (sessions, forums, etc.)
* develop a FUSE filesystem adapter to the Wikilon web service

I'll need to re-implement a lot of existing code in the new framework. 

To Redo:

* list, fork, rename, delete dictionaries
* import and export dictionaries
* list, view, update, delete, rename words
* compute stylesheet from dictionary


## OLD STUFF

At this time I'm trying to develop lots of little 'spike' solutions. The UX isn't particularly refined yet. I might need to use reflex-dom or similar to develop the rich UX that I want. Actually, the UX needs an enormous revamp to make it nicer to use.

I should probably work to make editing AO code more comfortable for the common case. In particular, I should be able to quickly build and test a definition using Claw code, perhaps entirely via REPL. And I also need to get the REPL going ASAP.

DONE:

* list of dictionaries
* create a new dictionary
* export a dictionary to a file
* import a dictionary from a file
* edit fragments of a dictionary
* browse words by name
* obtain a list of word names 
* view a word
* delete a word 
 * by clearing its definition
 * simplified deletion via dedicated link, button
* rename a word
* view and edit `[command][]` definitions as claw code
* stylesheets with dark background by default
* most CSS pushed into stylesheets (instead of inline css)
 * CSS provided via dictionary (default /d/master/w/wikilon.css).
* secure hash per word for deep source
* simplistic stateless REPL based on Claw code
 * including time elapsed during evaluation

* (meta) separated model from implementation

NOTES:

I'll need a proper 'incremental evaluation/compilation' model or similar, something that can effectively support progress bars, 202 responses, and so on. I can probably get by without this for now, but I think it would serve me well in the long term to model the incremental steps more precisely and provide better information about what goes wrong or how much progress is made.

I also need to get to work quickly on developing a 'generic' model for web applications and images.


PRIMARY TODO:

* recent changes
* background computations (per branch?)
* incremental caching model
* machine-optimized bytecode and data model

TODO:

* develop a model for indexing words with a given property
 * e.g. all words that are undefined or badly defined or badly typed
 * e.g. all words that have a given type on input or output


* forum-functions...
 * model forum within a dictionary
 * 

* update the branch storage to PVar per branch... 
 * or multiple PVars? e.g. so we don't load history to load head dict.
 * consider extra PVars for branch metadata

* implement a cache in wikilon-store?
 * consider a metadata cache per dictionary
 * cached hashes could be useful, then
 * other cached content could be useful
 * but... cache per dictionary might hinder management
 * 

* develop model for tracking 'words with a given property' 
 * e.g. undefined words, badly typed words, words with a given type
 * per branch? or per dictionary?
 * extensible set of simple, named properties

* consider a switch to Snap widgets or similar
 * built-in routing features
 * automatic AJAX support if feasible
 * Scotty, Yesod, Salvia?, MFlow, Wheb

* implement a simple user/group permissions model.
 * perhaps based initially on unix filesystems, for familiarity
 * check authority when loading a branch PVar
* implement incremental compilation above cache
* implement faster interpreters
* easily delete a word
* user authentication and capabilities needed

* use ETags correctly (If-None-Match)
 * create a monadic model to simplify this, 
 * perhaps based on MFlow, multi-page forms?


* favicons
* image resources in general! SVG, GIF, PNG?
* basic support for static web content
 * static web pages
 * or at least texts and forums

* same monad for authority validation & 

* tune how requests and routing are handled in wikilon-wai
 * take inspirations from mflow and RDP or FRP

* generate favicon from dictionary
* generate front page from dictionary

* develop generic, effective cache model 
 * try to use just a few different cache types
 * e.g. binaries and Wikilon-ABC values only
 * or maybe generic support for Wikilon-ABC 'types'?
  * types-as-values? for abstract interpretation?
  * model all values as types...? functions as relations...
 * binaries might be valuable for caching web output

* PERFORMANCE OF INTERPRETER! !!!
 * I need and want this to be a lot faster
 * even impl. from older awelon project is 3-10x faster
 * it may require cached compilation, partial eval, etc.
 * support for very large values, world manipulations
 * compilation to JavaScript also desirable

* implement the persistent REPL session forums model
* develop web-app model so I can create apps in dict
 * compilation to javascript + DOM
 * capabilities for reflection on dictionary

* sessions model, editing multiple words together

* link to word documentation, discussion pages
 * maybe inline word documentation
* add a simple *delete* option for words 
* move dictionary import, export into new pages

* add a link to the 




* develop generic indexing mechanism for finding words
 * by input or output types
 * by errors/warnings and cause
 * by text values

* link 'related' words?
 * clients + dependencies of word
 * filter for those that contain word or are contained by it
 * run up to a few steps?

* improve evaluator performance!
 * need ~10x efficiency to match old awelon project
 * (probably) mostly need to simplify quotation
 * leverage cached pre-compiled values


* cache word-based HTTP resources
 * each resource tied to a word
 * words of a given type, via common prefix
 * model existing CSS within dictionary as resource
 * model static web-pages within dictionary
 * model icons and images within dictionary
 * model queried web-pages within dictionary
* separate model and implementation
* track recent updates and events

* basic user login model
 * maybe have a dictionary associated with each user? 
 * at least for theme, style, etc.?
 
* configure cookies at least for 'master' dictionary and CSS words
 * or maybe later bind this to a login or user


* generic model for developing basic web content?
 * an API for (reactive?) web page construction
 * ability to cache these ad-hoc web resources

* user models
 * within a dictionary? master dict?
 * ideally model stream-manipulation of values for per-user environment!
  * part of the Awelon project UI concept
  * could be modeled within a dictionary or as an AVM

* widgets for editing words
 * tabbed views, or multiple side-by-side views

* simplify WAI widgets
 * create a Wikilon-WAI monad model
 * automatic generation of AJAX?
 * automatic support for multi-page sessions?

* continuous profiling
 * consider: ThreadScope, EKG

* given WikilonStore, provide a ModelRunner
* model admin, logs, stats, geneology, issue trackers, etc. as dictionaries?
 * this might simplify common presentation and extension models
 * basically, uses dictionaries as additional filesystems
 * via auxilliary dictionary?

* improve Data.VCache.Trie.toKey performance

* improve evaluation performance!
 * old ao/aoi evaluation is 3x-10x faster depending on test
 * profile current interpreter
 * performance variant for ABC
 * partial evaluation, interning, memoization, etc.

* preserve `{%foo}` etc. links in block values, if feasible
 * for presentation to user, instead of massive floods of bytecode
 * may need to perform escape-analysis on blocks?
 * may need to build as feature for performance variant of ABC.

* separate data model from web service
 * generalize caching for resources
 * bind resources to secure hash identities
 * support binary, text, value, type resources

* AJAX/React interfaces in general
 * favor editable texts/views instead of forms
 * RDP basis? and/or AVMs?
 * model generically, above AO/ABC 
 * leverage ABC to JS compilers if feasible

* more colorful and graphical
 * push displays into mixed text and SVG
 * develop a boxes and wires language over claw or other


* gradual, long-running, background computations
 * long running compilation, evaluation, typechecking

* searches based on type or structure
* tab-completion based on type matching

* cache Wikilon-ABC representations, partial eval

* numbers libraries...
 * support rational and decimal numbers
 * beginning support for vectors
  * maybe vectors from lists?
  * maybe sequences from lists?

* clean up rendering of values, bytecode, etc.
 * maybe add some color or syntax highlighting 
 * numbers vs. texts vs. data plumbing vs. other?

* lazy linking so REPL can return claw code blocks
 * partial 'flattening' for toplevel meaning of word
 * probably a moderate performance hit, larger for Wikilon.ABC

* persistent REPL sessions (threads, forums)
 * review [REPL.md](REPL.md) for this idea

* graphical REPL views? e.g. turtle graphics or SVG
 * maybe use words to define value to HTML conversions
 * might need better caching...

* tab completion in REPL
 * need AJAX, probably need a new widgets model

* model CSS as dictionary application or resource
 * need cache to be working more broadly and effectively

* model 'associated words' via a metadata function or word
 * e.g. user-defined computation for suffixes and prefixes

* REFACTOR!
 * further separate the model, web interface, storage
 * switch to MFlow or Yesod-based widgets and routing
  * automate parse validation, auto-refresh, editable textareas, etc.
  * may need to implement my own variant, maybe RDP-based

* dictionary application resources:
 * based on compiling and evaluating a word
 * use secure hash of word to cache instance
 * css
 * static HTML
 * icon files
 * image files
 * binaries, in general
 * compiled applications (model for compilers?)

* spreadsheet-based dictionary application
* iPython notebook inspired dictionary application

* type checker and cache for ABC and Claw code
 * list undefined or ill-typed words in dict
* compilation and cache for ABC and Claw code
 * maybe use secure hashes in cache?
* support for developing web applications

* continuous compilation, testing, type checking

* simple ABC to JavaScript compiler
 * this might simplify issues regarding non-termination
 * e.g. can push non-terminating computation to browser
 * can also enable dynamic display of functions


* fork a dictionary
* view historical versions of a dictionary
* fork historical versions of a dictionary

* push towards intermediate data model
* exponential decay for dictionaries

* convenient editing of multiple words
 * view and edit multiple words together
  * workspace model? word sets?
  * edit words used in REPL session?
  * directly edit words via REPL?
  * remember workspaces... 
   * recent 'edit sets'?
   
 * automatic word sets or workspaces 
  * based on prefix, dependencies, etc. 
 * edit multiple words quickly together
 * claw variant of AODict fragment editor?

* begin development of interactive editing
 * use more javascript and websockets
 * interactive should be default
 * pure HTML fallbacks are okay
 * editable pre-code blocks?
 * continuous feedback and testing

* clean up word lists
 * automatic compression based on common prefixes
 * if too many words (e.g. under `integer`), use fallback page

* create a more generic intermediate graphics model
 * something an AVM or pure function could draw to easily
 * integrate with AO code.
 * concepts similar to HTML and SVG, but indirectly?


* need convenience links near bottom of word pages

* abstract POST-based editor concept
 * perhaps add 'return to' continuations
 * alternatively, create a resource model for intermediate states
 * multi-stage: parse, auth, conflict, accept (and return)

* switch to model-based manipulation of Wikilon resource state
 * easier to update backends
 

* create generic app for diffing resources, e.g. to resolve edit conflicts?
* create a more general model for loading resources


* review all of 'aoi' and 'ao' tool operations and model them

* improve AODict editor:
 * support claw-style namespaces when loading words into editor?

* I can easily find recently changed words for fast review

* interactive editing
* list 'TODO' annotations in code, maybe with timestamps and red/green/etc.
* perhaps inject annotations and dates for 'holes' - undefined words
* browse words by suffix
* full text search

* dictionary history
 * diff dictionaries from different times
 * diff dictionaries across branches and versions
 * obtain events for individual dictionaries
 * observe geneologies involving many dictionaries
 
* support user-defined stylesheets via dictionary

* support for server-side AVMs?

* rename all words with a given prefix, or whole lists of words
* I can easily browse words by type in the dictionary
* I can easily browse words by intermediate structure in dictionary
* I can easily browse a reverse lookup for words in the dictionary
* I can browse words based on dictionary driven categorical metadata
* list 'root' words - words that aren't directly used within the dictionary
 * perhaps minus documentation, etc.
* I can search for words based on name (with optional, default fuzzy find)
* I can search for words based on type (with optional, default fuzzy find)
* I can easily diff words (and information about them)
* I can easily link to clients of a word, to depth N
* I can easily link to dependencies of a word, to depth N
* I can easily link to related words based on name structure
 * minimally, words with common prefixes
 * maybe words with common suffixes or infixes (requires new index)
* I can access the definition for each word via browsing
* Structured Editing of Words based on first stage computations

* I can evaluate useful functions from words
* I can access precompiled s

* linter: validate hash names
* linter: validate types against naming conventions


* ACTIONS
 * I can access the precompiled structure for each word
 * I can access a word as inlined ABC
 * I can access a word as compiled ABC
 

* I can render ABC structured values in HTML or SVG
* I can render word structure (pre-compiled) in HTML or SVG
* I can do simple turtle graphics on an HTML canvas
* I can compile ABC into JavaScript

* I can render words with colors or icons based on prefix or suffix
 * support a simple legend
 * may need to record style information in dictionary?

## REFACTORING


* Switch to events system for actions. 
 * A POST or PUT action may create one or more events.
 * State of Wikilon is only modified via events.
 * e.g. a 'RenameWord' event when renaming a word
 * Security aspects also captured in each event.
* Switch to a views system for GET.
 * can integrate with cache.
 * integrate with security.
 * composable views
 * mutable views?
* Adjust model for dictionary
 * Trie: move prefixes into parent, 



* HISTORY
 * view and navigate old versions of dictionary
 * maybe require branch old version for anything but view?

* GENEOLOGY
 * I can view relationships between 

* HEALTH
 * I can see which words don't compile
 * I can see which tests don't pass
 * I can see which words don't typecheck
 * I can see which assertions are weakly 'probably' valid
* METRICS
 * I efficiently see how many words a dictionary has
* CACHE
* BACKUP
 * I can import/export whole history and geneology
 


## Old Stuff

* rename and merge words
* fork a dictionary


* import and export of a dictionary as a file 
 * this makes it easier to export content then reset Wikilon
 * can begin editing via external dictionary files

* interfaces to edit and view words within a dictionary
 * view the raw definiton ABC code
 * view the structural of the definition
 * view the compiled ABC code
  * view annotated compiled ABC code
 * edit the raw definition ABC code
 * edit the structure of the definition

* interactive editing
 * editor on left, results on right 
 * leverage iframes, forms, SVG
 * avoid JavaScript for now 
  * have entire site work without 
  * plus a variation that uses scripts
 
* automatic zero-button testing, typechecks 
* track recent changes (and change efforts? patches?) for a dictionary
* track type failures, test failures, etc..
* simple compilation of words to JavaScript?
* development of 'dictionary apps': 
 * apps having no dependencies outside of a dictionary
 * notebooks, spreadsheets, wikis, calculators, small games
 * may be able to update dictionary (dict as state, dataflow)
 * may have some client-side state in browser (browser AVMs? React?)

* Users should have logs for items updated 
 * logs contain enough information to track abuse
 * simple exponential decay model of some sort
* Dictionary should have log for recent updates
 * ability to query which words are updated and when
 * ability to 'slice' time on these updates
 * ability to subscribe to log as a web socket
 * includes whodunnit information
* 





* consider separating branches of dictionary into multiple PVars, i.e. such that each branch has one PVar rather than sharing an additional trie at the toplevel. The main advantage here would be that there is no risk of conflict when performing large dictionary-level operations, such as importing a dictionary. (Priority: Low. Minor performance issue.)



* further improve import performance
 * probably need to profile.
 * might need to validate ABC without full parse?
