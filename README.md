Wikilon
=======

Wikilon is a wiki-insipired software platform and development environment for Awelon project and the Awelon programming language.

[Awelon programming language](docs/AwelonLang.md) has a simplistic, Forth-like syntax, a simple semantics, and a purely functional evaluation model based on confluent rewriting of concatenative combinators. The simple syntax can be leveraged for projectional editing: we can present a more advanced syntax or even a graphical syntax to the programmer. The evaluation model rewrites a program to another representation of the same program, which then may be projected and rendered. Further, rewriting supports rendering incomplete evaluations during debugging, or even leverage incomplete programs for monotonic application models.

Awelon 'programs' may behave a lot more like databases or spreadsheets or filesystems depending on how they are used. Conventional effect-based programs can be expressed, compiled, and 'run' by an agent. But a major goal of Awelon project is to explore [alternative application models](docs/ApplicationModel.md) or explore 'software artifacts' that aren't something we normally consider an application by itself (ray-traced images, software music, mining databases).

Wikilon presents Awelon language through a web service, with wiki inspirations. A web service offers both a level of indirection essential for working with multiple projections and access through a browser can offer an easier transition to graphical projections than would be achieved via filesystem. And a 'wiki' seems a natural fit for the Awelon language dictionary and evaluation model - if each page is a subprogram, we can edit or evaluate each page. Later, to support conventional text editors, is feasible to develop a [filesystem in userspace (FUSE)](https://en.wikipedia.org/wiki/Filesystem_in_Userspace) binding to the web service.

Performance and caching are both essential features for Wikilon to succeed. 

**NOTE:** Performance of AO/ABC is high priority, to enable more code to be shifted into the dictionaries. Wikilon's bootstrap runtime is being re-implemented for performance. Additionally, the new vision favoring dictionary-based applications requires the existing UI to be more or less replaced wholesale.
