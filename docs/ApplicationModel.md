
# Wikilon Application Models

Wikilon will support several kinds of applications, leveraging the dictionaries and potentially a little extra state. At this time, I've identified a few very broad classes of application that Wikilon could or should support, allowing developers to mostly make up the differences here.

## Cross-Compiled Applications

One basic mode for Wikilon as a development platform is to *compile* applications for use elsewhere. In this case, we specify the compiler and application - each of them functions - in a URL for HTTP GET. Typically, these links will be easily constructed. We may also need to specify a dictionary and version number. The 'build' process is implicit, highly cacheable, and performed by the Wikilon server.

For these apps, nothing is directly run on Wikilon other than the compiler function, which is pure or close enough (maybe leveraging memoization or ABC's linker model). There are no stateful requirements other than maintenance of the dictionary. There is no entanglement between the dictionary and the application after compilation. The application is downloaded for use elsewhere.

The encoding of functions in a URL can be pretty straightforward, perhaps using base64 if we need more than a single word. Any user-defined syntax is feasible in a base64 encoding. By encoding ad-hoc functions in URLs, it becomes easier to share or bookmark content up to about the size of an AO module or command line, without always being asked to generate a new wiki page.

"Application" here doesn't need to be executable. It might be documentation, music, video, etc.. It's just something that requires an additional compilation pass after we hit the ABC bedrock.

## Dictionary Applications

It seems feasible to leverage a dictionary in a manner similar to iPython Notebook or a spreadsheets. In this case, the user experience becomes an edit-eval-render cycle, using information primarily from the dictionary. However, an application that takes no arguments is not easy to compose, abstract, or reuse. In the interest of composition and reuse, we should also provide some means of introducing inputs and output in a composable manner, and also compose the renders. This suggests a model something like:

        input   
          ↓      
        dictApp 1 → view 1 ↘
          ↓                 rendered view
        dictApp 2 → view 2 ↗ 
          ↓
        output

Rather than providing a default input value such as `unit`, I suggest the non-value `void`. Void is effectively a typed hole where the type must be inferred. We might render an ad-hoc mix of statically computable values and typed holes, offering a convenient sliding scale between abstract interpretation and concrete evaluation. Further, any detected type errors could also be rendered in-context if treated as first-class values (e.g. via sealer). In context of Wikilon, it might be useful to support tuning the input transform function separately from the rendering operation, i.e. such that we can easily view a dictApp in several different input contexts.

A *rendering model* must tell us how to decompose an application into multiple renderable components, then recompose the views. Earlier (now deprecated) efforts on the rendering model focused on using some simple structure for a few built-in rendering models - e.g. one for spreadsheets, another for notebook-style apps. But this approach is rigid, fragile, non-extensible. An involves leveraging annotations, e.g. embedding `[renderModel]{&render}%` where the render model tells us how to take a value and render it (e.g. a monad for generating diagrams, hyperlinks and progressive disclosure, etc.). This might be coupled with some scoping annotations operating on blocks, or other specialized rendering annotations, and perhaps some access to abstract resources for view state, navigation state, and user preferences. 

But annotations have at least two major difficulties. First, they're noisy, even after refactoring, distracting from the main thrust of the code. Second, they don't clearly indicate where to render, especially in relationship to the original syntax or a structured editor. To address these concerns may require shifting annotations into the [parser function](ExtensibleSyntax.md), at least for most of them. At the very least, spreadsheet and notebook apps could easily be annotated by their parsers to support rendering.

A lot of details still must be hammered out. 

## Hosted Web Service Applications

As an IDE and software platform, Wikilon should be able to directly execute at least a useful subset of applications. I'd like to focus on those requiring persistent state and network access - e.g. web services, publish-subscribe systems, orchestration models, internet of things. I have some additional desiderata here. 

First, *live programming* should be pervasive, such that application code can easily be edited at runtime and there is a tight relationship between the code and the runtime behavior. The idea that long-running behaviors have a human-meaningful and human-modifiable representation (in code) simplifies concerns like process control, and is important to Awelon project's long term goals of unifying PX and UX. 

Second, *extensibility* should be easy, i.e. we can add new features to an existing application by adding new code, rather than by modifying existing code. An easy approach to extensibility is to support state or resource sharing in some simple ways.

Third, *entanglement* must be easily avoided. It should be possible to move the application to a dedicated server, back up the state, or potentially compile a unikernel just for it, without interfering in unpredictable ways with other applications hosted by the same Wiki. The behavior and state should be separable from the dictionary if it pleases the developer. 

To achieve extensibility without entanglement, one viable option is to model Wikilon as hosting many 'abstract virtual machines' where each machine has its own independent resources. This allows us to extend said machine with new behaviors that do become entangled with other behavior on the same machine, while allowing us to easily disentangle behaviors on different logical machines. Binding stateful resources to the machine layer is also valuable for live programming. We can atomically restart a behavior with updated code, or roll back to an older machine state.

Modeling these abstract virtual machines seems worthwhile. But a simple, implicit machine model to start might be useful - i.e. something with basic imperative state and services (CGI, RPC) and also some support for RDP resources. To simplify debugging, we might keep a logarithmic history of the abstract virtual machine's states. Modeling state resources explicitly may also help with sharding or migration.

Creating an abstract virtual machine seems to be an administrative task. We must consider a lot of administrative concerns, and possibly the question of global uniqueness (for state, security) and possible migration. In some ways, wikilon just then becomes a higher level abstract virtual machine that happens to be hosting a few smaller ones.
