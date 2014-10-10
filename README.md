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
* access histories for AO dictionary words
* transactional editing (edit many words at once)

* console-like web service to replace AOI
* implement command-line console interface to the web services
 * system-integration; access to defs, ABC, cross-compiled code, etc.
 * may also serve as watch-dog app, i.e. restart automatically on crash
* iPython notebook inspired apps - i.e. living consoles

* refactoring tools
 * rename a word (or a common suffix/prefix); renaming may need special transaction
 * find all uses of a word
 * automatic test generation, property testing
 * automatic word-variant generation (e.g. for operating lower on stack)
 
 
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

* SSL/TLS support! (Though, this can be added later.)
 * potentially add an optional redirect (e.g. from 80 to 443)
* Compression support!


Code that runs on the server will generally be given limited resources and some means for job control. I would prefer that resource failures be deterministic.

All state associated with Wikilon will be persistent. Also, I'm likely to keep logarithmic histories for everything, including service states, to allow for robust recovery and debugging.

## Setup and Configuration

As Wikilon approaches a usable state, I'll provide instructions for setting it up and getting started. 

* cabal install
* if ~/.cabal/bin directory is not in PATH, consider adding it
* set WIKILON_HOME environment variable to a directory name

After install and PATH is set, you should be able to run `wikilon -pPort`.

### HTTPS (TLS/SSL) Setup

I recommend you enable TLS. **warp-tls** supports RSA keys. Quick start:

        cd $WIKILON_HOME
        openssl genrsa -out wiki.key 2048
        openssl req -new -key wiki.key -out wiki.csr
        openssl x509 -req -days 365 -in wiki.csr -signkey wiki.key -out wiki.crt

This creates a self-signed certificate, which should result in blaring warnings on your first visit from a security conscious browser. But at least the connection will be encrypted! To get a properly signed and validated certificate, send the `wiki.csr` file and some money to a certificate authority. If you'd rather use plain old HTTP, simply remove the `wiki.key` and `wiki.crt` files.

If TLS is enabled, Wikilon will reject insecure connections. If you plan to use port 443 (the default HTTPS port) for Wikilon, you might wish to run a trival separate process on port 80 that will redirect users to the https URL - e.g. HTTP code 307 or 308.

## Under The Hood

* **Haskell** for implementation
* **acid-state** for persistence
* **warp** for HTTP connectivity
* **websockets** for web reactivity & live programming

* persistence and logarithmic history for all states
* implement initial console app by hand (with user or session?)
  * idea: sessions akin to iPython notebook or xiki (persistent, live, AO code)
* build in a user-model to start (a root user?)
* build in import/export services
* run all ABC code transactionally (for now)

I like the idea of supporting multiple 'code bubbles' within a single browser tab/window, and keeping them up-to-date via websockets. This would make it easier to edit definitions and see the results in multiple other views. I should probably look into similar systems, which I understand to leverage iframes and similar. Both the CodeBubbles IDE and Xiki might be 

I may need some external support to edit and view definitions via command line, e.g. for recovery purposes. In that case, I can either leverage Data.Acid.Remote for multi-process communication (that seems dubious and counter-productive to my long term goals), or I can try HTTP communication with an active instance and have a special localhost connection... or, alternatively, print a privileged capability-URI on the command line for administration.

