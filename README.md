Wikilon
=======

Wikilon is a wiki-insipired software platform and development environment for Awelon project and the Awelon programming language.

[Awelon programming language](docs/AwelonLang.md) has a simplistic, Forth-like syntax, a simple semantics, and a purely functional evaluation model based on confluent rewriting of concatenative combinators. This simple syntax can be leveraged for projectional editing, and views may be preserved over evaluation.

Awelon computations cannot reference external data. Instead, Awelon codebases may serve as simple databases, documents, filesystems, spreadsheets, depending on how they're used an updated. Awelon explores non-conventional [application models](docs/ApplicationModel.md).

Wikilon presents Awelon language through a web service, with wiki inspirations. 

## Installation

Wikilon is implemented using F# on CLR. The code generation and JIT capabilities of CLR or JVM are convenient for implementing a lightweight compiler for user created code. CodeDOM and support for tail calls have me favoring CLR.

Dependencies:

* [LMDB](http://www.lmdb.tech/doc/) 
 * sudo apt-get install liblmdb-dev
* [.Net core](https://www.microsoft.com/net/core#linuxubuntu) tools
 * instructions on linked website

Assuming the dependencies and a good Internet connection, you may use `dotnet restore` to download required .Net packages. Use `dotnet run -- -help` to view command line options. *Aside:* I'm favoring .Net core over mono largely for the streamlined `dotnet` CLI tooling. This does incur a few unfortunate opportunity costs, such as WebSharper isn't yet available for .Net core. 

## Components

Primary project components:

* `src/Wikilon` - the web application and UI definitions
* `src/Awelon` - Awelon language and runtime model
* `src/Stowage/Data` - non-relational database model
* `src/Stowage` - LMDB-based implementation of database
* `src/Data.ByteString` - bytestring values, builders, parsers

This toplevel directory also has `app.fsproj` referencing `src/Main.fs`, which simply processes command line configuration options and runs a web server.

## Configuration

Configuration of Wikilon is performed online, generally through a browser. Initial configuration requires the `-admin` flag to provide an ephemeral password for a root administrative account. But the admin may assign administrative authorities to other accounts.

