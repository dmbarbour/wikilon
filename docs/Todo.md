
# TODO

At this time I'm trying to develop lots of little 'spike' solutions. The UX isn't particularly refined yet.

DONE:

* I can see a list of dictionaries
* I can create a new dictionary
* I can export a dictionary to a file
* I can import a dictionary from a file
* I can edit fragments of a dictionary

TODO:

* I can easily browse words in the dictionary
* I can easily link to clients of a word, to depth N
* I can easily link to dependencies of a word, to depth N
* I can easily link to related words by name structure
 * e.g. prefixes on `foo.*` and `foo:*`
* I can access the definition for each word via browsing

* I can evaluate useful functions from words
* I can render ABC structured values in HTML or SVG
* I can render word structure (pre-compiled) in HTML or SVG
* I can do simple turtle graphics on an HTML canvas
* I can compile ABC into JavaScript

* ACTIONS
 * I can access the precompiled structure for each word
 * I can access a word as inlined ABC
 * I can access a word as compiled ABC
 
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







