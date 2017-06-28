Wikilon
=======

Wikilon is a wiki-insipired software platform and development environment for Awelon project and the Awelon programming language.

[Awelon programming language](docs/AwelonLang.md) has a simplistic, Forth-like syntax, a simple semantics, and a purely functional evaluation model based on confluent rewriting of concatenative combinators. This simple syntax can be leveraged for projectional editing, and views may be preserved over evaluation.

Awelon computations cannot reference external data. Instead, Awelon codebases may serve as simple databases, documents, filesystems, spreadsheets, depending on how they're used an updated. Awelon explores non-conventional [application models](docs/ApplicationModel.md).

Wikilon presents Awelon language through a web service, with wiki inspirations. 

# Installation

Wikilon is implemented using F# on CLR. The code generation and JIT capabilities of CLR or JVM are convenient for implementing a lightweight compiler for user created code. CodeDOM and support for tail calls have me favoring CLR.

Dependencies:

* Linux (I'm using Ubuntu 16.04)
* [LMDB](http://www.lmdb.tech/doc/) 
 * sudo apt-get install liblmdb-dev
* [.Net core](https://www.microsoft.com/net/core#linuxubuntu) tools
 * instructions on linked website

Assuming the dependencies and a good Internet connection, you may use `dotnet restore` to download required .Net packages and `dotnet run` to initiate the Wikilon web service. *Aside:* I favor .Net core over mono largely for the streamlined `dotnet` CLI tooling. This does incur some unfortunate opportunity costs, such as WebSharper isn't yet available for .Net core. 

