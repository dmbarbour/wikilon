Wikilon
=======

A wiki and web-services based development environment for Awelon project

## Overview

Wikilon is a development environment for Awelon project, aiming for a wiki-like live-programming experience. 

Wikilon will serve as an alternative to the existing Text Editor + REPL development environment. Escaping the text editor and **.ao** files allows Wikilon to react more responsively to changes in the AO dictionary, and will more readily support incremental and persistent computation (e.g. of tests, typechecking, staged programming, partial evaluation). Shifting from a console REPL to browser services offers a much richer canvas for graphics and user interactions. Finally, the web server offers an interesting platform for long-running persistent services.

Wikilon should provide a *minimal kernel* of capabilities - e.g. persistent state, bootstrap interpreters, the user model, recovery services. Most logic should be shifted into the dictionary (leveraging reflection as needed). We may need to start with a much larger Wikilon kernel until such a time as the dictionary is suitably mature.

See also: 

* [Awelon Object (AO) language](https://github.com/dmbarbour/awelon/blob/master/AboutAO.md)
* [Awelon Bytecode (ABC)](https://github.com/dmbarbour/awelon/blob/master/AboutABC.md)
* [Awelon Project vision](https://github.com/dmbarbour/awelon/blob/master/AwelonProject.md)

## Intended Features

Some of the following will depend heavily on a maturing dictionary. Others may require code at the Wikilon core. Others may shift from core to dictionary over time. It will certainly take time to accomplish much of the following:

* view definitions for AO dictionary words
* edit definitions for AO dictionary words
* console-like web service to replace AOI
* access histories for AO dictionary words

* refactoring tools, e.g. rename a word, find uses of word
* shorthand views: render with color or icons or small suffix, etc. to compact text
* shorthand writing: fuzzy find, tab completion, type-sensitive search

* compile AO code to ABC (head or history)
* import and export for AO dictionary formats
* automatic testing as word definition changes
* view live use-cases for words while editing
* debugging, visualization, animation of AO code
* record errors; automatic regression test generation
* serve as repository for ABC resources
* integrate distributed access to ABC resources
* development of [embedded literal objects](../doc/ExtensibleLiteralTypes.md)
* define words that add new web applications
* define words that add new web services
* plugins or processes for service performance
* JavaScript compilation of ABC for browser
* cross-compile apps for Android, desktop, etc.
* DVCS-like forking and push/pull of AO dictionary
* support for multiple users, logins, security
* development of RDP reactive behaviors and resources
* publish or use services in open distributed systems
* distributed behaviors, user agents, and mobile code

Code that runs on the server will generally be given limited resources and some means for job control. I would prefer that resource failures be deterministic.

All state associated with Wikilon will be persistent. Also, I'm likely to keep logarithmic histories for everything, including service states, to allow for robust recovery and debugging.

## Setup and Configuration

When Wikilon is in a usable state, I'll provide instructions for setting it up and getting started. 

## Implementation

Still in early planning phases. 

* **Haskell** for implementation
* **acid-state** for persistence
* **Warp** or **WAI** for HTTP services
* **websockets** for web reactivity & live programming
  * won't bother with fallbacks; just require Chrome or Firefox :)
* persistence and logarithmic history for all states
* implement initial console app by hand (with user or session?)
  * idea: sessions akin to iPython notebook or xiki (persistent, live, AO code)
* build in a user-model to start (a root user)
* build in import/export services
* run all ABC code transactionally (for now)

Until it's ready for multiple users, I'll bind by default to localhost (127.0.0.1).

I may need some external support to edit and view definitions via command line, e.g. for recovery purposes. In that case, I can either leverage Data.Acid.Remote for multi-process communication (that seems dubious and counter-productive to my long term goals), or I can try HTTP communication with an active instance and have a special localhost connection... or, alternatively, print a privileged capability-URI on the command line.

I like the idea of supporting multiple 'code bubbles' on a browser. I suppose I could do this via OWS or iframes or similar. But an important part will be getting started with reactivity from early on. So it might be a good idea to focus on websockets support for reactive updates in the browser. In addition to xiki, CodeBubbles might be a good inspiration.

## Design Thoughts

I might choose to provide a more structured editing experience. I'd like to consider including the full ABC for inline ABC in the AO code, perhaps excepting ad-hoc tokens. (At least sealers, unsealers, and annotations will be allowed as tokens. Maybe those can be treated as special cases, too.)

I really want to support embedded literal objects. Those will require some special consideration, I imagine.

It might be useful to begin providing the 'ABC stream' control concept early on, as a generic interface to manipulate the Wikilon server. This could be performed via websockets or even via HTTP POST or PUT (with some sequencing model, e.g. single assignment on an incremental identifier). POST could be used to ask for a new session identifier with a set of authorities based on a capability URI.

ABC and AO streams would make an excellent basis for debugging, recovery, consoles, etc.. 

## Re: Keeping Lots of History

I wonder if acid-state will keep up with my goals for long-running histories. The first thing to do is try it, I guess. :)  If not, I might start pushing old values (the long tail of history) to disk, under the premise that history access should be relatively infrequent.

I'd like to keep a reasonably interesting history for every word, while gradually eliminating the fine-grained changes. My idea is to use a logarithmic history. A relevant question is to use logarithmic history per word vs. for the whole dictionary.

