Wikilon
=======

**NOTE:** Wikilon is NOT usable at this time. It's making steady progress, but it's still immature. The content below is based on what I want to be true when I'm closer to finished with Wikilon.

Wikilon is a wiki-inspired software platform and development environment for Awelon project.

An [Awelon Object (AO)](docs/AboutAO.md) dictionary contains a set of words with acyclic definitions. Each word defines a concrete mathematical function, represented in [Awelon Bytecode (ABC)](docs/AboutABC.md). Definitions are stored in their raw bytecode representation, but we generally interact with them through editable views. For example, we have a Forth-like command language named [Claw](docs/CommandLine.md). And [dictionary applications](docs/ApplicationModel.md) can easily model wikis, spreadsheets, forums, REPLs, and other RESTful systems.

Conventional applications are viable with support from software agents, or 'bots'. In the simple case, a bot could watch an application for a representation of requests: an outbox of messages, a list of subscriptions. The bot could then send messages and report them delivered, or access a publisher for data. Feedback might be provided through an inbox. Real-time Wikilon apps are feasible with bots shoving time-series data into the dictionary and observing application states.

Instead of libraries and packages, AO favors DVCS-like forking and merging as a basis for sharing, distributing, and maintaining code. An AO dictionary is a complete system.

I have a promising [strategy for performance](docs/Performance.md). This should offer performance competitive with Java and C# after maturing, and should not overly complicate. Achieving better than that is feasible, with the right accelerators.

## Setup and Configuration
