
# Wikilon Application Models

Wikilon can support several kinds of applications, leveraging the dictionaries and potentially a little extra state. At this time, I've identified a few very broad classes of application that Wikilon could or should support, allowing developers to mostly make up the differences here.

## Cross-Compiled Applications

One basic mode for Wikilon as a development platform is to *compile* applications for use elsewhere. In this case, we specify the compiler and application - each of them functions - in a URL for HTTP GET. Typically, these links will be easily constructed. We may also need to specify a dictionary and version number. The 'build' process is implicit, highly cacheable, and performed by the Wikilon server.

For these apps, nothing is directly run on Wikilon other than the compiler function, which is pure or close enough (maybe leveraging memoization or ABC's linker model). There are no stateful requirements other than maintenance of the dictionary. There is no entanglement between the dictionary and the application after compilation. The application is downloaded for use elsewhere.

The encoding of functions in a URL can be pretty straightforward, perhaps using base64 if we need more than a single word. Any user-defined syntax is feasible in a base64 encoding. By encoding ad-hoc functions in URLs, it becomes easier to share or bookmark content up to about the size of an AO module or command line, without always being asked to generate a new wiki page.

"Application" here doesn't need to be executable. It might be documentation, music, video, etc.. It's just something that requires an additional compilation pass after we hit the ABC bedrock.

## Dictionary Applications

It seems feasible to leverage a dictionary in a manner similar to iPython Notebook or a spreadsheets. In this case, the user experience becomes an edit-eval-render cycle, using information primarily from the dictionary. 

I recently developed a [new approach to syntax](ExtensibleSyntax.md) that is more or less the dual of the Lisp/Scheme homoiconic code. I use bytecode to build a data structure that sits very near the edge of compilation. This structure then supports structured editing, or at least has much potential to do so, and I can even support lense-based views and updates of the structure. 

I could have a spreadsheet where every 'cell' is simultaneously a structure-editor and a 'lens' on the original structure, thus enabling multiple ad-hoc views of any module. If the dictionary evolves to include a lot of world data and content, the dictionary itself might provide interesting inputs to view through such lenses. In context of CSCW, I could even support multiple concurrent views and edits of this structure, assuming I'm careful about it. 

The simplicity and purely functional nature of ABC should make this entire approach a lot easier than iPython notebooks.

To render functions, operating on `void` might simulate abstract interpretation, and we might also leverage approaches similar to tangible functional programming. We might also use annotations to indicate intermediate rendering points, perhaps under different named 'views' that can be toggled by the viewer.

But there are many hurdles to clear for this approach, such as making it perform well.

## Hosted Web Service Applications

As an IDE and software platform, Wikilon should be able to directly execute at least a useful subset of applications. I'd like to focus on those requiring persistent state and network access - e.g. web services, publish-subscribe systems, orchestration models, internet of things. I have additional desiderata here. 

* **live programming**: edit code to modify behavior at runtime
* **extensibility**: add features by adding code, shallow modification
* **portability**: control *entanglement*; may migrate app or compile to server 
* **maintainability**: easy debugging, resilience, persistence, administration
* **securability**: can control who administrates any given application

### Abstract Virtual Machines for Extensibility, Portability

Extensibility can be achieved by sharing stateful resources, e.g. a common database or queue or tuple-space or demand monitors. Unfortunately, this sharing entangles components. To control entanglement, we can partition resources, such that each partition may be internally entangled but is cleanly separated from other partitions. Communications between partitions are then subject to delays and disruptions. Modeling potential disruption allows a partition to be migrated to another physical machine. Each independent application uses an independent partition. 

Each partition is essentially an abstract virtual machine. Eventually, we might even compile these abstract virtual machines (or subsets thereof) into unikernels. But for now it's sufficient that our abstract machines operate within one Wikilon instance or can migrate between Wikilon servers.

More thoughts:

* [state model for AVM](StateModels.md)
* [network model for AVM](NetworkModel.md)

I've been refining my thoughts and designs over a few weeks, so don't expect full consistency.

### Machine Layer Resources with Stable Identity for Live Programming

*Live programming is of primary importance.* A principle of Awelon project is that all long-running behaviors have human-meaningful, human-modifiable representation, such that modifying the representation causes corresponding change in behavior. This gives humans comprehension and control over processes and systems, helps unify PX and UX, and supports intuitive physical metaphors. Securability is then addressed by controlling access to the representation.

Live programming requires stable identity for stateful resources, such that code and programmed behavior may change independently of state. In context, this suggests our stateful resources are modeled as belonging to our abstract virtual machines - i.e. no `new StatefulObject()` created by code because that clearly lacks stable identity. Each abstract virtual machine must also accept changes to its defined behaviors, perhaps via RDP or RESTful update. 

The dictionary might be available as an external resource for some machines.

### Branching Dictionary or Version Tags for Securable Live Programming

To address joint concerns of security and live programming, it must be possible to control update of the representation of a machine's behavior, which happens to be a function in a dictionary, which in turn may have relatively ad-hoc dependencies on other functions. This might be achieved by using tagged versions - or perhaps a [branching versions](BranchingDictionary.md) - of the dictionary. Then, tags or branches are secured such that only a subset of users may manipulate them. 

More security might be achieved via capability model, value sealing, monadic effects, etc.. but most of this is at other layers. Though, I suppose machines might benefit from a global identity.

### Atomic Updates, Batched Messages

Atomic updates greatly simplify maintenance. The developers needn't think about partial failures, and may focus on consistency within any given step. Explicit support for partial failure might legitimately be modeled as a side-effect, e.g. a `{try}` capability or monadic effect. For updates to remain atomic, of course, we must batch all the outputs during a step and process them only after successful commit.

Since we're batching messages anyway, it seems reasonable that the basic communication primitive between machines should be 'batched messages'. The ability to receive batches of messages associated with atomic operations in other partitions is convenient for reasoning about consistency, i.e. you only see consistent snapshots of other machines. Further, it can vastly improve performance relative to individual messaging.

### Copy on Write or Purely Functional Machines

To further simplify maintenance and debugging, the ability to take cheap snapshots of a full machine - for rollback and recovery, for historical debugging, for exploration and 'what if' development - is also very convenient. So I'd like to focus on state models suitable for copy-on-write updates and structure sharing between snapshots. It turns out, these same state models are probably also suitable for purely functional implementation, which would be very convenient for modeling abstract virtual machines within Wikilon. 

### Flexible Networking

Communication between machines, batched messages, is essentially an abstract primitive. It could be implemented many ways, simultaneously - e.g. an ad-hoc mix of HTTP and web-sockets and AMQP. I think this could be very useful because it allows us to migrate apps and adapt to whichever communication protocol they need based on where they're hosted. Applied carefully, I can ensure that messaging is idempotent and monotonic over pretty much any medium. E.g. HTTP PUT to an incremental address is favorable to HTTP POST. 

I may need ad-hoc sockets or similar for networking with external resources. But I don't want to focus application development on low-level communications. It may be useful to model layers of abstract virtual machines, with lower layers adapting communication protocols and other low-level details.

Disruption must also be easily modeled. I have a few ideas there, e.g. based on logical connections. It might be useful to estimate logical latencies and clock drift, both for imperative code and RDP.

### Unikernels

Long term, I'd like the ability to migrate an application to a unikernel... and back again. I think this is viable if we keep the model simple enough.

