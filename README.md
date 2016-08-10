Wikilon
=======

Wikilon is a wiki-inspired software platform and development environment for Awelon project. A dictionary word is a pure function, modeled in the [Awelon Object (AO)](docs/AboutAO.md) language. These words may be presented as 'wiki' pages and [flexibly interpreted as applications](docs/ApplicationModel.md). Effectful interaction with applications is understood as a multi-agent system, with both software agents (bots) and humans updating, observing, and managing the dictionary. 

For humans, AO code is inconvenient to work with directly. However, it is possible to provide high level editable views of AO code, such as the Forth-like [Command Language for Awelon (claw)](docs/CommandLine.md). A *visual claw* variant that supports layout annotations, sliders, checkboxes, canvases, etc. could potentially be very flexible.

**NOTE:** Performance of AO/ABC is high priority, to enable more code to be shifted into the dictionaries. Wikilon's bootstrap runtime is being re-implemented for performance. Additionally, the new vision favoring dictionary-based applications requires the existing UI to be more or less replaced wholesale.
