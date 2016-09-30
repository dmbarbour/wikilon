
# Why Not Imperative?

At the toplevel, application models are conventionally oriented around imperative side-effects. That is, we might have a toplevel function of type `int main(args)`, but neither the arguments nor the return value are especially indicative of the application's behavior. Instead, the application communicates with an implicit environment - a machine or operating system - and, through it, various physical devices and networks.

Haskell pioneered expression of conventional application models within a purely functional system. The effectful application is represented as data structure that represents either a final result or an intermediate (request, continuation) pair. We could intercept and interpret the request, it is possible to hide the intermediate structure then optimize heavily to achieve bare-metal performance. The latter is what Haskell does with `main :: IO ()`.

This model has the advantage of being 'direct', but also has significant costs: 

* difficult to compose applications, no clear connectivity points
* difficult to observe an application's actions or track dependencies
* difficult to intercept effects for security or environment adaptation 
* difficult to serialize applications entangled with environment
* difficult to reproduce environment or results for debugging
* difficult to control applications, ad-hoc signals and state

In addition, the introduction of multi-threading and pervasive non-determinism further exacerbates the challenges of testing, debugging, and comprehending program behavior. We end up with Heisenbugs, errors that are difficult to reproduce, different orderings of events on different physical machines, etc..

There are ways to mitigate some of these problems, e.g. with use of sandboxes or object capability security. However, it seems a lot like turd polishing. I feel believe we can do better than to follow conventions.

An interesting alternative is [Kahn process networks for effects](KPN_Effects.md). KPNs can be composed in the style of FBP, and 'effects' can be modeled as requests pending on open output channels, with an associated input channel to place the results (i.e. essentially channels augmented with effects). We could intercept effects easily by routing requests elsewhere. We can easily observe where requests are made, and potential dependencies. Etc..

However, even KPNs seem awkward as a basis for 'effects' in context of my favored [application model](ApplicationModel.md). In particular, the idea of deleting a request that is the result of an evaluation seems rather awkward in context of a spreadsheet-like system where the result of any evaluation might change. Instead, single-assignment futures and monotonic tuple spaces are a better fit.
