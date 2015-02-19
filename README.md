Wikilon
=======

A wiki-inspired development environment and software platform for Awelon project.

Every 'page' in the wiki defines a concrete function, with acyclic dependencies on other functions. Functions subsume roles of data, documentation, tests, compilers, applications, and more. Initially, pages shall be written primarily using the Awelon Object (AO), a Forth-like language that Wikilon understands natively. However, Wikilon will support [user-defined syntax](docs/ExtensibleSyntax.md), requiring only that the syntax is well defined by another page.

Wikilon can cross-compile functions, using compilers defined in the wiki. Access to compiled content is simple, just an HTTP GET on a link whose URL indicates the compiler and the function to be compiled, no sophisticated build step required. Given appropriate compilers, one might 'compile' documentation to PDF, or compile specialized monadic functions to a JavaScript web-application, or compile a byte stream transformer into a console application. Compiler developers are free to experiment with application models, effects models (monadic, capability-based), staging, targets (mobile, browser, desktop, unikernel), and so on. 

As a software platform, Wikilon will host web applications and services. The details here aren't fully hammered out, but the vision is that Wikilon should be tailorable for use as a web server, game server, robotics controller, etc.. or at least support deployment of virtual machines when, for security or performance reasons, it would be better to avoid directly hosting the service.

Related: 

* [Awelon Object (AO) language](https://github.com/dmbarbour/awelon/blob/master/AboutAO.md)
* [Awelon Bytecode (ABC)](https://github.com/dmbarbour/awelon/blob/master/AboutABC.md)
* [Awelon Project vision](https://github.com/dmbarbour/awelon/blob/master/AwelonProject.md)

## Setup and Configuration

NOTE: Wikilon is NOT usable at this time. It's still in early development. 

To get started with Wikilon:

* install LMDB library and headers
 * (Ubuntu) `sudo apt-get install liblmdb-dev`
* cabal install --only-dependencies
* cabal install
* if ~/.cabal/bin directory not in PATH, consider adding it
* configure WIKILON_PATH environment variable (or use default)

After installation and PATH is set, you should be able to run `wikilon -pPort` and browse `http://127.0.0.1:Port`.

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
