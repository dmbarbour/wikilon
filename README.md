Wikilon
=======

A wiki-inspired development environment and software platform for Awelon project.

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
* **VCache and LMDB** for persistence
* **logarithmic history** for version control, debugging, regression testing
* **object capability model** for security and collaboration

The main Wikilon application is implemented as a WAI app in a library. Hence, developers can implement an alternative executable that combines Wikilon with some other features in one process.
