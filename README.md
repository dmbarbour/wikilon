Wikilon
=======

A wiki and web-services based development environment for Awelon project

## Overview

Wikilon is a development environment for Awelon project, aiming for a wiki-like programmer experience. 

Wikilon will serve as an alternative to the existing Text Editor + REPL development environment. Escaping the text editor and **.ao** files allows Wikilon to react more responsively to changes in the AO dictionary, and support incremental computation (e.g. of tests and typechecking). Shifting from a console REPL to browser services offers a much richer canvas for graphics and user interactions. Finally, the web server offers an interesting platform for long-running persistent services.

Wikilon should provide a *minimal kernel* of capabilities - e.g. persistent state, bootstrap interpreters, the user model, recovery services. Most logic should be shifted into the dictionary (leveraging reflection as needed). We may need to start with a much larger Wikilon kernel until such a time as the dictionary is suitably mature.

## Intended Features

Some of the following will depend heavily on a maturing dictionary. Others may require code at the Wikilon core. Others may shift from core to dictionary over time. It will certainly take time to accomplish much of the following:

* view definitions for AO dictionary words
* edit definitions for AO dictionary words
* console-like web service to replace AOI
* access histories for AO dictionary words
* refactoring tools, e.g. rename a word
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

Code that runs on the server will generally be given limited resources and some means for job control.

All state associated with Wikilon will be persistent. Also, I'm likely to keep logarithmic histories for everything, including service states, to allow for robust recovery and debugging.

## Setup and Configuration

When Wikilon is in a usable state, I'll provide instructions for setting it up and getting started. 

## Implementation

Still in early planning phases. 

* **Haskell** for implementation
* **acid-state** for persistence
* **Warp** or **WAI** for web services
* persistence for all state, with logarithmic history (exponential decay)
* implement initial console app by hand (with user or session?)
  * idea: sessions akin to iPython notebook or xiki (persistent, live, AO code)
* build in a user-model to start (a root user)
* build in import/export services
* run all ABC code transactionally (for now)

