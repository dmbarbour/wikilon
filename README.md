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

Code that runs on the server will generally be given limited resources and some means for job control. I would prefer that resource failures be deterministic.

All state associated with Wikilon will be persistent. Also, I'm likely to keep logarithmic histories for everything, including service states, to allow for robust recovery and debugging.

## Setup and Configuration

As Wikilon approaches a usable state, I'll provide instructions for setting it up and getting started. 

* install BerkeleyDB
 * (Ubuntu) sudo apt-get install libdb-dev
* cabal install --only-dependencies
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

This creates a key and a self-signed certificate. This should result in blaring warnings on your initial visit from a security conscious browser. But the connection will be encrypted. To obtain a properly signed and verified certificate (and to guard against man-in-the-middle attacks), send the `wiki.csr` file and some money to a certificate authority. If you'd rather use plain old HTTP, remove the `wiki.key` and `wiki.crt` files.

If TLS is enabled, Wikilon will reject insecure connections. If you plan to use port 443 (the default HTTPS port) for Wikilon, you might wish to run a trival separate process on port 80 that will redirect users to the https URL.

## Under The Hood

* **Haskell** for implementation
* **warp and wai** for HTTP connectivity
* **websockets** for liveness and reactivity
* **Berkeley DB** for persistence and atomicity
* **object capability model** for security and collaboration
* **logarithmic history** for version control and regression testing




