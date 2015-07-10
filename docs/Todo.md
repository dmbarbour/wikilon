
# TODO

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
* delete a word (by clearing its definition)
* rename a word
* view and edit `[command][]` definitions as claw code
* stylesheets with dark background by default

TODO:

* simplistic REPL based on Claw code
 * skip compilation and cache for now?

* maybe add support for 

* move all css to use 'classes'; multiple classes
* css styles for diffs, etc.
* clean up backrefs and clients lists; 
    e.g. list only ten items

* type checker and cache for ABC and Claw code
* compilation and cache for ABC and Claw code
 * maybe use secure hashes in cache?
* support for graphical REPL, e.g. turtle graphics or SVG
* support for developing web applications

* continuous compilation, testing, type checking
* simple ABC to JavaScript compiler

* consider upgrade to MFlow or Yesod or similar for pages
 * interactive widgets 
 * model browser/page AVM? 
  * Could be very effective and generic
  * probably slow to start
 * develop a more generic widget concept
 * use widgets instead of HTML directly

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


## Alternative Workflows

I'm interested in Paul Chiusano's work with Unison and the edit model presented there, e.g. based around manipulating sets of hashes. It might be worth trying to model this workflow in Wikilon.







