Wikilon
=======

Wikilon is a wiki-insipired software platform and development environment for Awelon project and the Awelon programming language.

[Awelon programming language](docs/AwelonLang.md) has a simplistic, Forth-like syntax, a simple semantics, and a purely functional evaluation model based on confluent rewriting of concatenative combinators. This simple syntax can be leveraged for projectional editing, and views may be preserved over evaluation.

Awelon computations cannot reference external data. Instead, Awelon codebases may serve as simple databases, documents, filesystems, spreadsheets, depending on how they're used an updated. Awelon explores non-conventional [application models](docs/ApplicationModel.md).

Wikilon presents Awelon language through a web service, with wiki inspirations. 

# Dependencies

Wikilon is implemented using F# on CLR. The code generation and JIT capabilities of CLR or JVM are convenient for implementing a lightweight compiler for user created code. CodeDOM, WebSharper, and support for tail calls have me favoring CLR. 

Dependencies:

* Linux (I'm using Ubuntu 16.04)
* [LMDB](http://www.lmdb.tech/doc/) 
 * sudo apt-get install liblmdb-dev
* [mono](http://www.mono-project.com/download/#download-lin-ubuntu) and [fsharp](http://fsharp.org/use/linux/)
 * instructions on websites.
* [paket](https://fsprojects.github.io/Paket/)
 * `mono .paket/paket.bootstrap.exe`
 * `mono .paket/paket.exe restore`
* [msbuild](https://github.com/Microsoft/msbuild)
 * sudo apt-get install msbuild

I'm unhappy with the F# ecosystem for build tools. The widespread use of scaffolding and templates is nauseating. The msbuild files are stupidly verbose. However, when in Rome. 


