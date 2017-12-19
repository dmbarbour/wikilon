
# Kahn Process Networks in Awelon

[Awelon language](AwelonLang.md) is purely functional. Although purely functional code permits a lot of simple fork-join parallelism, it presents a challenge for distributed parallel computation, modeling of parallel pipelines and so on. Mostly, the issue regards routing of messages. A purely functional representation of routing will be relatively deterministic and centralized, so it's difficult to leverage arrival-order non-determinism (race conditions) to improve parallelism. 

To resolve this, my current intention is to borrow inspiration from a deterministic concurrency model - especially one such as [Kahn Process Networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks) that does not require external effects to model channels or single-assignment variables. 

KPNs are also very useful as a potential [effects model](KPN_Effects.md) and process or application model, essentially capable of representing monads and comonads with multiple IO ports. KPNs support reactive or real-time systems by adding a simple time model, e.g. the ability to deliver `Tick + a` messages on every channel instead of just `a` messages. Pushback can also be modeled explicitly by adding acknowledgement feedback messages to the downstream processes.

## Single Assignment Placeholders

We can conceptually model single assignment variables using pseudo-words:

        [A] /put_K =>       (single assignment)
        /get_K => [A]       (after assignment)

Awelon doesn't support single assignment variables directly, but an optimizer or interpreter could presumably introduce placeholders to improve parallelism. In particular, `\get_K` may behave as a value word to be delivered through a computation in parallel with the computation that produces the final value `[A]`.

Using this technique, we can shift some computations (specifically those with a known output type) into a separate "parallel region" such that the parallel region will ultimately have an identity behavior. 

        [Expr](par)(t2) =>  [/get_1 /get_2]         (locally)
                            Expr /put_2 /put_1      (parallel region)

We'll create fresh variables as needed, i.e. very `/put_K` will use a distinct `K`. If computation terminates successfully, all of these put and get pseudo-words should be eliminated. However, if we halt due to error or quota, we may need to extract these placeholders. This can be achieved via simple value extraction algorithm:

        T(X,E) - extract variable X from E
           X T(X,E) == E
        T(X,E) when E does not have X => d
        T(X,X) =>
        T(X,[E]) => [T(X,E)]b
        T(X,F G) 
            when only F has X => T(X,F) G
            when only G has X => [F]a T(X,G)
            when both have X => c [T(X,F)] a T(X,G)

Essentially, we can prefix incomplete parallel computations onto our programs then rewrite them back to equivalent inline programs.

## Stable Frame Routing

To model routing of messages, one option is to process available messages in large "frames", touching every channel even if no message is available for that frame. Doing so naively is inefficient but does have potential advantages for batching and especially for routing of *placeholders* because we don't need to know anything about the potential messages before we route them.

An interesting possibility is to leverage knowledge about stable routes such that we only need to route placeholders once for a frame then deliver multiple times by reusing the existing routes. Essentially, routes become `/put_K` and `/get_K` pairs reusable across multiple update frames without explicitly performing the data plumbing each time. 

## Identity Frames and Fixpoints

A KPN won't update further when no messages are added. Essentially, we have a clear fixpoint condition - something easy to check dynamically, and potentially to verify statically. Evaluation of KPNs will always proceed to this fixpoint. Knowing this condition, we can optimize by avoiding routing to a subnetwork when there are no messages. More importantly, we can support "race conditions" within the KPN so long as we clearly determine which subnets might update.

However, we might need to pay attention to representation of pending messages to ensure this fixpoint behavior. For example, we might need to know we're appending lists.

If we route stable frames in a fixpoint loop, with a clear halting condition, we'll have the essence of KPNs.

## Acceleration

Ideally, acceleration of KPNs in Awelon shouldn't rely too much on a specialized KPN data structure. We could do that, but I'd prefer something more generalized based on lightweight static analysis involving stable routes and identity frames.


