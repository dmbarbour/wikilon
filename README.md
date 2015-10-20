Wikilon
=======

**NOTE:** Wikilon is NOT usable at this time. It's making steady progress, but it's still immature. The content below is based on what I want to be true when I'm closer to finished with Wikilon.

Wikilon is a wiki-inspired software platform and development environment for Awelon project.

An [Awelon Object (AO)](docs/AboutAO.md) dictionary contains a set of words with acyclic definitions. Each word defines a concrete function, represented in [Awelon Bytecode (ABC)](docs/AboutABC.md). The definitions are stored in their primary bytecode representation, but are typically viewed and edited through higher level interfaces (providing syntax or structured editing). Instead of libraries and packages, AO favors DVCS-like forking and merging as a basis for sharing, distributing, and maintaining code. 

By leveraging structural conventions within and between definitions, dictionaries may directly host ad-hoc application state: forums, wikis, graphs, spreadsheets, bulletin boards, interactive fictions, and so on. This is the primary [application model](docs/ApplicationModel.md) for Wikilon. I might later also explore abstract virtual machines and unikernels.

Related: 

* [Awelon Object (AO) language](docs/AboutAO.md)
* [Awelon Bytecode (ABC)](docs/AboutABC.md)
* [Awelon Project vision](docs/AwelonProject.md)

## Caveats

Awelon Bytecode (ABC) lacks sufficient work on optimizers or compilers, and is simply not yet performance competitive. At this time, I won't recommend Wikilon for serious use where performance is a concern. Eventually, this condition should change. One goal with Wikilon is to bootstrap the compilers and optimizers and other tools, and perhaps bootstrap Wikilon itself.

## Setup and Configuration

To get started with Wikilon:

* Install Haskell. I recommend instructions at [stackage](http://www.stackage.org/install).
* Install LMDB library and headers (for lmdb Haskell package)
 * (Ubuntu) `sudo apt-get install liblmdb-dev`
 * (Source) try from [github.com/LMDB/lmdb](https://github.com/LMDB/lmdb)
* Recommend [sandbox installation](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html) of Wikilon
 * obtain sources for Wikilon, enter directory
 * cabal sandbox init
 * cabal sandbox add-source wikilon-model
 * cabal install --only-dependencies
 * cabal install

This has been developed using Stackage lts-2.12, but I'd like to hear if Wikilon doesn't compile with the recent stackage. To use stackage, see [the page](https://www.stackage.org/) and obtain the current cabal.config. Wikilon itself is not hosted on Hackage or Stackage at this time. 

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
