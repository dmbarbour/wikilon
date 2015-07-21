
The 'Read Eval Print Loop' or REPL is a staple of interactive programming languages. It provides quick feedback for users, effective access to computation. I'm very interested in providing a good REPL service in Wikilon. Obviously any REPL will use the [Command Language for Awelon (claw)](CommandLine.md) view because this is what claw was designed for. However, this still leaves a lot of design considerations for presenting a REPL via web service.

## REPL Environment

One option is to provide a fixed default environment, e.g. unit or a `(stack*(hand*env))` structure. The REPL would evaluate from there, manipulating this environment. And we'd render, say, the top object on the stack (if any). This approach is highly predictable, but also somewhat inflexible.

An interesting alternative is to provide an amorphous environment, e.g. apply our REPL to a *void* environment then leverage abstract interpretation to obtain whatever information we need. 

Usefully, if operating on *void*, session could be extended at both ends. A prefix extension provides more information about context. A suffix extension adds more information about action. Sessions can be understood as both functions and constraint models (via assertions). Arguments to them could be manipulated. Users might see multiple running example inputs and/or inferred type information.

## REPL State and History

With a web service, a REPL could be *stateless* by pushing the code into a query string. This could be useful for quick views, bookmarking and sharing computations without saving them. But an interesting possibility is a *stateful* REPL, where I record the session for future use, continuous testing, examples and documentation. And since a REPL is a pure function, I could also have *branching* state, with multiple futures (and maybe multiple histories?)

The stateful variation could be a [dictionary application](ApplicationModel.md). That is, the posts and threads are modeled as words in the dictionary, perhaps together with some simple naming conventions to support efficient rendering. This is useful because it allows reuse of all the normal dictionary manipulation tools (refactoring, renaming, continuous testing, typechecking, etc.).

So, perhaps something like:

        @post:postId words and numbers here become content
        @post:postId.author "david"
        @post:postId.subject "REPLs"
        @post:postId.created "2015-07-16T23:14Z"
        @thread:childId thread:parentId post:postId

Root threads might use a sim threads might use `{&thread:root}` instead of a parent thread word, i.e. to ensure that root threads always have a neutral starting point. Each `post:postId` is a reusable word independent of context, and the `post:` prefix isn't essential. In some cases a post could be an [embedded object](ExtensibleSyntax.md) or another ad-hoc dictionary app (spreadsheet, table, iPython notebook, etc.). However, the thread prefix may be necessary for threads to help identify their usage characteristics.

So, the REPL concept for Wikilon web service includes a functional forum-like aspect (for sessions, state, history).

