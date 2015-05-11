Wikilon
=======

**NOTE:** Wikilon is NOT usable at this time. It's making steady progress, but it's still immature. The content below is based on what I want to be true when I'm closer to finished with Wikilon.

Wikilon is a wiki-inspired software platform and development environment for Awelon project.

An [Awelon Object (AO)](docs/AboutAO.md) dictionary contains a set of words with acyclic definitions. Each word defines a concrete function, leveraging [Awelon Bytecode (ABC)](docs/AboutABC.md). DVCS-based conventions are leveraged for sharing, versioning, and distributing code. The expectation is that communities will curate open-source dictionaries, while projects or developers will tend to fork a dictionary for local use and eventually feed back content or utilities. Supporting hundreds of projects in a community dictionary provides opportunities for cross-project maintenance and refactoring.

AO dictionaries correspond nicely to [wikis](http://en.wikipedia.org/wiki/Wiki): 

* each wiki is a dictionary
* each page is a word
* dependencies between words become links 

Wikilon can host multiple dictionaries, e.g. to support forking and unstable vs. stable branches. Each dictionary is given a name and URI, under `/d/dictName`. 

As a software platform, Wikilon supports a few [application models](docs/ApplicationModel.md). 

First, with sufficient tooling, dictionaries can be used as [spreadsheets](http://en.wikipedia.org/wiki/Spreadsheet) or [notebook-style apps](http://en.wikipedia.org/wiki/IPython). Words correspond to functions. Much like cells in a spreadsheet, pure functions can be rendered for human consumption through typeful views:

* a function of type `ℝ²→Color` could be rendered as an image
* a function of type `∀e.e→(Text*e)` renders as static text
* a function of type `ℝ→ℝ` can be rendered as a graph or table
* a function of type `QueryString→SVG` renders as an HTTP form

Typeful views may be supported with some naming conventions for words. A lot of static content (blogs, images, etc.) can be modeled this way, and a lot of useful features can be oriented towards editing the dictionaries.

Second, interactive content is viable with appropriate types, e.g. a simple interactive fiction (or any command-line app) could be modeled using a function of type `µState.(Text→(Text*State))` with the text input corresponding to the user command string. A different types might be more suitable for ad-hoc web applications. Wikilon should ultimately be able to cross-compile a lot of interactive content: simple games, calculators, [unhosted web apps](https://unhosted.org/), etc..

Third, Wikilon will also serve as a host for *abstract virtual machines* (AVMs). AVMs use a simple value for state, and communicate (or perform effects) via message passing through a simple [network model](docs/NetworkModel.md). AVMs are necessary whenever developers want to extend Wikilon with ad-hoc server-side state, e.g. for a chat server or a multi-user dungeon. Using many small AVMs can limit entanglement compared to image-based programming environments. A long term goal will be to compile AVMs into hypervisor-layer [unikernels](https://queue.acm.org/detail.cfm?id=2566628).

Between these approaches, Wikilon should be tailorable into arbitrary new applications or services. 

Related: 

* [Awelon Object (AO) language](docs/AboutAO.md)
* [Awelon Bytecode (ABC)](docs/AboutABC.md)
* [Awelon Project vision](docs/AwelonProject.md)

## Caveats

Awelon Bytecode (ABC) lacks sufficient work on optimizers or compilers, and is simply not performance competitive at this time. At this time, I won't recommend Wikilon for serious use where performance is a major concern. Eventually, this condition should change. One goal with Wikilon is to bootstrap the compilers and optimizers and other tools, and perhaps bootstrap Wikilon itself.

## Setup and Configuration

To get started with Wikilon:

* Install Haskell. I recommend instructions at [stackage](http://www.stackage.org/install).
* Install LMDB library and headers (for lmdb Haskell package)
 * (Ubuntu) `sudo apt-get install liblmdb-dev`
 * (Source) try from [github.com/LMDB/lmdb](https://github.com/LMDB/lmdb)
* Recommend sandbox installation of Wikilon
 * obtain sources for Wikilon, enter directory
 * cabal sandbox init
 * cabal sandbox add-source wikilon-abc
 * cabal install --only-dependencies
 * cabal install

This has been developed using Stackage lts-2.8, but I'd like to hear if Wikilon doesn't compile with the recent stackage. To use stackage, see [the page](https://www.stackage.org/) and obtain the current cabal.config.

After installation and PATH is set, you should be able to run `wikilon` and browse `http://127.0.0.1:3000`. The port and other parameters may be configured; see `wikilon --help`. 

### HTTPS (TLS/SSL) Setup

I recommend you enable TLS. **warp-tls** supports RSA keys. Quick start:

        cd $WIKILON_HOME
        openssl genrsa -out wiki.key 2048
        openssl req -new -key wiki.key -out wiki.csr
        openssl x509 -req -days 365 -in wiki.csr -signkey wiki.key -out wiki.crt

This creates a key and a self-signed certificate. This should result in blaring warnings on your initial visit from any security conscious browser. But the connection will at least be encrypted. To obtain a properly signed and verified certificate (and to guard against man-in-the-middle attacks), send the `wiki.csr` file and some money to your least untrustworthy certificate authority. If you'd rather use plain old HTTP, remove or rename both the `wiki.key` and `wiki.crt` files.

If TLS is enabled, Wikilon will reject insecure connections. If you plan to use port 443 (the default HTTPS port) for Wikilon, you might wish to run a trival separate process on port 80 that will redirect users to the https URL.

## Under The Hood

* **Haskell** for implementation
* **warp and wai** for HTTP connectivity
* **websockets** for liveness and reactivity
* **VCache and LMDB** for ACID persistence
* **logarithmic history** for version control, debugging, regression testing
* **object capability model** for security and collaboration

The main Wikilon application is implemented as a WAI app in a library. Hence, developers can implement an alternative executable that combines Wikilon with some other features in one process.
