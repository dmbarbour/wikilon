
The 'Read Eval Print Loop' or REPL is a staple of interactive programming languages. It provides quick feedback for users, effective access to computation. I'm very interested in providing a good REPL service in Wikilon. Obviously any REPL will use the [Command Language for Awelon (claw)](CommandLine.md) view because this is what claw was designed for. However, this still leaves a lot of design considerations for presenting a REPL via web service.

First, I have an interesting option of providing a *stateless* REPL. This is feasible by simply pushing an entire session (or alternatively a state setup behavior) into a query string. Combined with careful caching, this could even be reasonably efficient. A stateless REPL would be subject to easy bookmarking and sharing.

Second, I have an option of modeling a REPL more like a forum or bulletin board, i.e. with each command as a stateful 'post'. Interestingly, multiple threads could provide a sort of 'branching' REPL. Editing a post could feasibly be modeled as non-destructive, but that could be optional.

For the second option, I would treat the forum as a [dictionary application](ApplicationModel.md). Each post becomes a word, or set of related words (e.g. by naming conventions and dependencies) in the dictionary. So, perhaps something like:

        @thread:childId thread:parentId words and numbers here become content
        @thread:childId.author "david"
        @thread:childId.subject "REPLs"
        @thread:childId.created "2015-07-16T23:14Z"

The root threads might use `{&threadRoot}` instead of a parent thread word, i.e. to ensure that root threads always have a neutral starting point. Rather than combining responsibilities for threading and content, it might be better to separate them a little:

        @post:postId words and numbers here become content
        @post:postId.author "david"
        @post:postId.subject "REPLs"
        @post:postId.created "2015-07-16T23:14Z"
        @thread:childId thread:parentId post:postId

An explicit `post:postId` becomes a reusable word independent of context. However, this might not offer any significant advantages because posts don't make a lot of sense as words and refactoring isn't difficult when we want it. OTOH, a perhaps greater advantage of the second option is that it provides a more rigid structure for threads (a simple pair of words), and might permit more flexible representation of thread content (e.g. presenting [embedded objects](ExtensibleSyntax.md) at each post). Since I gain a little flexibility without significant loss, I should favor the explicit post words.

The forum idea might not be called a REPL. It might be better understood as a branching persistent session, or as a computational/functional forum. (Having a branching persistent session seems pretty neat for a lot of use cases, tests, etc.). Each post could have a bunch of surrounding metadata (e.g. the author and subject) to be rendered. The major advantage of a forum would be our ability to *browse* it, and to *typecheck* it as a sort of continuous test, and (as a dictionary app) to easily *share* it. Meanwhile, we gain all the benefits of REPLs, and a possibility for automatic branching and refactoring and so on.

A stateless REPL and a functional forum might interact in some interesting ways. In particular, we could apply our REPL to extend an arbitrary named thread (a single word for the parent thread), and we could be presented with an option of saving REPL content into our forum. But it might be useful to treat these as (more or less) two separate features.





