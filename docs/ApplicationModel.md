
# Application Models for Awelon Project

Awelon project will focus on RESTful applications, with the relevant resources being [Awelon Object](AboutAO.md) dictionaries and ad-hoc objects represented within them. Applications are modeled in terms of creating, reading, updating, and deleting words or dictionaries. AO preserves link structure upon evaluation. Together with object attributes (to provide metadata for rendering and updating objects), we can offer a simple and effective basis for hypermedia.

Updates to a dictionary can propagate to ad-hoc views. In addition to implicit word-level caching, stowage together with explicit `[computation]{&cache}` can support efficient recomputation of a compositional view upon a change in a persistent structure. For example, we could update a graph in real-time based on changes deep within a database maintained in another word. Further, we can efficiently cache ad-hoc views that aren't statefully preserved by permanent words in a dictionary.

In context of a publish-subscribe protocol (or Comet based view), it is feasible to keep external systems and users up to date on a system in real-time. 

Effects can be supported by the multi-agent layer:

* For one-off effects, we might represent a *request* in a dictionary to perform the external effect, which is later fulfilled by a human or software agent. The agent in question may update information about success/failure/etc.. An observer might see the move from a 'pending' state to a 'success, here's the data' or 'failed, here's why' state. 

* Publish-subscribe options are possible, e.g. continuously updating a dictionary with time-series data or maintaining current or recent information. On the subscribe side, the spreadsheet-style realtime updates would be very useful in combining data from many time-varying resources and we could push that information out into real-world decision making.

However, Awelon project systems will not directly perform side-effects. Though, it is feasible to extract an application that might operate in a more conventional manner with [imperative](NotImperative.md) side-effects.

## Dictionary Objects and Attributes

A dictionary contains a set of defined words. However, it is frequently convenient to imagine these words as representing objects. This can be supported with simple naming conventions. Given a word `foo` we might look into the dictionary and discover attribute words like `foo.doc`, `foo.talk`, `foo.type`, `foo.author`, and so on. We may understand these as being usefully related object `foo`.

Dictionary objects and attributes have an ad-hoc semantics based on the conventions surrounding their use. 

Dictionary objects can be flat, non-hierarchical. That is, if we need `foo.doc` to refer to something with its own attributes, we can a redirect from `foo.doc` to a separate documentation object. This isn't a requirement, but it might prove to be a best practice. AO words remain limited to 30 UTF-8 bytes, so deep hierarchy isn't possible.

*Note:* Attributes can usefully express cyclic relationships between objects. For example `foo.child` could redirect to `bar` and `bar.parent` back to `foo`. Word level cycles aren't permitted by AO, but the ability to express object-level cyclic relationships could very convenient for RESTful views.

## Dictionary Tables and Spreadsheets

A set of dictionary objects readily be viewed as implicitly having a sparse, tabular layout. For example, word `foo.doc` may be interpreted as specifying row `foo`, column `doc`. The actual definition of `foo` might be represented as another implicit column. 

To select a set of objects to view, we could filter dictionary words by attributes, types, or values. We could compute a list of `[{%word}]` objects. Or we could represent a table as another, second-class dictionary object. 

        @myTable.row1 {%foo}
        @myTable.row2 {%bar}
        @myTable.row3 {%baz}
        ...

Regardless of how we specify our set of objects, AO makes it easy to render both the definitions of each word and their context-free evaluations or linker objects. Thus, we can render evaluated cells in a table while enabling editing of each cell's definitions, and propagating updates to the rendered views. We effectively get spreadsheets without any special effort.

## Command Pattern in AO

The command pattern might be represented as:

        @foo.v0 (initial state)
        @foo.v1 {%foo.v0}{%foo.u1}
        @foo.v2 {%foo.v1}{%foo.u2}
        ...
        @foo.v99 {%foo.v98}{%foo.u99}
        @foo {%foo.v99}

Preserving history by default is a good thing. It simplifies features like fork, merge, undo, and history debugging. However, large histories for small objects may need to be compacted and garbage collected over time.

Conveniently, command pattern is readily represented at the dictionary level, e.g. with streaming dictionary updates. We might essentially HTTP PUT a patch on a named dictionary with a structure:

        ~
        @command99 (...)
        @foo.v99 {%foo.v98}{%command99}
        @foo {%foo.v99}

From a human user's perspective, command pattern will frequently be associated with HTTP POST or the pushing of a button. From a software agent's perspective, it might be based on a publish-subscribe model. 

## Managed Dictionaries

We may need to perform garbage collection at the dictionary level, eliminating words and collapsing command histories that are no longer relevant. This could be performed by a software agent, e.g. assume three attributes:

* *opaque* - definition's structure is irrelevant and may be rewritten
* *frozen* - behavior of this word should never change in the future
* *hidden* - assume no external references directly access this word

This would permit a corresponding set of rewrites:

* *opaque* definitions may be simplified, evaluated, compressed
* *frozen* definitions may be *inlined* into *opaque* clients
* *hidden* definitions may be *deleted* if they have no clients

Each declaration gives our software agent a more opportunity to safely rewrite, manage, and optimize the dictionary in specific ways. Compression can be achieved by introducing new hidden, frozen words for common substructures - i.e. dictionary compression.

## Application Security

AO supports multiple named dictionaries, which may be referenced by `{%word@dict}`. Security properties will generally focus on a named dictionary.

I would like to have a generic security model for AO that is useful for developing applications in the face of mutual distrust. The *granularity* for security should be a full 'dictionary'. However, we may have some static type constraints. 

because dictionaries are the unit for communication in AO systems.


Preferably while ensuring dictionaries are used a holistically in the common case (i.

*TODO:* Redesign in context of multi-dictionary evaluations.
* distributed programming: HMAC or PKI
* controlling connectivity between dicts
* eval/read/write access control?
* 




When multiple agents muck about in a stateful dictionary, it becomes valuable to precisely control who interacts with what. Fortunately, the rich computational structure of our dictionary admits some expressive security models. For example, automatic curation is feasible - rejecting updates that break types or tests. We can also adapt object capability security for a dictionary-level object model.

Consider two authorities:

* query *object* via *interface* 
* update *object* via *interface*

In context of a dictionary, words are the smallest unit that we observe and control. Our interface and object name words. Authorities can generally be represented and shared via cryptographic bearer-tokens of the form `update:object/interface/HMAC`. User logins might be modeled as a bag of bearer tokens together with relevant view states (active edits, color schemes, alerts, etc..).

Query and update permissions specify an interface:

        query interface:    Params → Object → Result
        update interface:   Params → Object → Object

These interfaces constrain our agents. We're only permitted to observe the result of our query, which allows for flexible information hiding. Similarly, update interfaces constrain which updates may be expressed for our object. There is a most powerful interface: `apply :: (o → a) → o → a`, which can be used to construct any other interface. 

At the dictionary level, updates could be implemented by command pattern:

        @alice.foo.update (alice's foo update interface)
        @bob.foo.update (bob's foo update interface)

        @foo.v0 (init foo)
        @foo.v1 [{%foo.v0}][Parameters1]{%alice.foo.update}
        @foo.v2 [{%foo.v1}][Parameters2]{%alice.foo.update}
        @foo.v3 [{%foo.v2}][Parameters3]{%bob.foo.update}
        @foo    {%foo.v3}

Queries might be reified in the dictionary:

        @alice.foo.query (alice's foo interface)
        @alice.foo.q0 [{%foo}][Params]{%alice.foo.query}

For the update case, we may be limited to constant parameters. It may require a separate authority for retroactive update.

*Aside:* Agent specific interfaces give us a lot of precision. If modeling a multi-user dungeon, for example, players might each have interfaces specific their avatar. This also enables precise management and revocation of authority.

The above model is insufficient, and could probably be improved. In addition to query and update, we need authorities regarding source-level reads, writes, delete, rename, fork, history, search and discovery, and other such features. In general, I imagine it would be *frustrating* to use a dictionary through a limited interface, and it would certainly undermine some advantages of dictionary applications. 

Nonetheless, it seems dictionaries could be used as an effective foundation for mutually distrustful multi-agent systems with a relatively generic security layer.

## Extraction of Applications

Extracting and compiling an 'application' for external use is certainly feasible, especially for certain kinds of applications (e.g. those with monadic effects models). Ideally, this extraction process should be very simple and cacheable, expressed as a simple HTTP GET.
