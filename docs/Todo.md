
# TODO

At this time I'm trying to develop lots of little 'spike' solutions. The UX isn't particularly refined yet.

DONE:

* list of dictionaries
* create a new dictionary
* export a dictionary to a file
* import a dictionary from a file
* edit fragments of a dictionary
* browse words by name (not very useful)
* obtain a list of word names 
* view a word




TODO:

* directly edit a word
* delete a word
* rename a word



* simplistic REPLs, both Claw code and ABC
* basic typechecking 
* simple ABC to JavaScript compiler
* I can easily find recently changed words for fast review

* interactive editing
* list 'TODO' annotations in code, maybe with timestamps and red/green/etc.
* browse words by suffix
* full text search
 
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
* Edit words using Claw Code

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

## BUGHUNT

I'm receiving some strange errors, when importing a dictionary twice into Wikilon:

*** Error in `wikilon': double free or corruption (out): 0x00007f59cc002b10 ***
Aborted (core dumped)

OR

*** Error in `wikilon': corrupted double-linked list: 0x00007f8878018c90 ***
Aborted (core dumped)

This behavior happens the second time I load a value. It seems GHC error to me, or perhaps an issue with how GC interacts with VCache.

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

* add words or edit a dictionary
 * mostly done, though could be improved

* evaluation of code expressions
 * REPL and [command line](CommandLine.md) environments
 * get a simple REPL working

* views of words
 * 


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


## 







