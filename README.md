Wikilon
=======

**NOTE:** Wikilon is NOT usable at this time. It's making steady progress, but it's still immature. The content below is based on what I want to be true when I'm closer to finished with Wikilon.

A wiki-inspired software platform and development environment for Awelon project.

An [Awelon Object (AO)](docs/AboutAO.md) dictionary contains a set of words with acyclic definitions. Each word defines a concrete function, leveraging [Awelon Bytecode (ABC)](docs/AboutABC.md). DVCS-based conventions are leveraged for sharing, versioning, and distributing code. The expectation is that communities will curate open-source dictionaries, while projects or developers will tend to fork a dictionary for local use and eventually feed back content or utilities. Supporting hundreds of projects in a community dictionary provides opportunities for cross-project maintenance and refactoring.

AO dictionaries correspond nicely to [wikis](http://en.wikipedia.org/wiki/Wiki): 

* each wiki is a dictionary
* each page is a word
* dependencies between words become links 

Wikilon can host multiple dictionaries, e.g. to support forking and unstable vs. stable branches. Each dictionary is given a name and URI, under `/d/dictName`. Dictionaries can be exported to a file or imported, providing a simple basis for backup. Internally, Wikilon keeps an extensive history for each dictionary to resist corruption or attack and support future study of how software develops. Wikilon is configured to favor one dictionary as the 'master'. Mostly, this provides control over the front page, favicons, CSS, and other special content based on definitions of specific words such as `wikilon:FrontPage`. Any dictionary may be viewed as master via `/d/dictName/wiki` URIs.

As a software platform, Wikilon supports several [application models](docs/ApplicationModel.md). **Dictionary applications** provide ad-hoc 'views' of a dictionary and might serve as spreadsheets, interactive documentation, cross-compiled programs, blogs, images, music. Also, Wikilon can host or deploy **Abstract Virtual Machines** (AVMs), which effectively extends server-side state and communication patterns. This may be useful when modeling certain kinds of applications (e.g. a chat server or multi-user dungeon).

Ultimately, Wikilon should be tailorable for use as a web server, game server, robotics controller, etc... while providing a very different (and hopefully better!) programmer experience compared to conventional systems. However, at the moment Awelon Bytecode (ABC) lacks performance-competitive compilers and interpreters. Until this changes, I would not recommend Wikilon for serious use.

Related: 

* [Awelon Object (AO) language](docs/AboutAO.md)
* [Awelon Bytecode (ABC)](docs/AboutABC.md)
* [Awelon Project vision](docs/AwelonProject.md)

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
