
# Kahn Process Networks in Awelon

[Awelon language](AwelonLang.md) is purely functional. Although purely functional code permits a lot of simple fork-join parallelism, it presents a challenge for the sort distributed parallel computation that requires a lot of message routing. Routing of messages becomes a form of centralized computation without a lot of fine-grained parallelism. To resolve this, my current intention is to borrow inspiration from a deterministic concurrency model such as [Kahn Process Networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks). KPNs are also very useful as a potential [effects model](KPN_Effects.md).

My current intention is to work with KPNs via accelerated functions. But it is tempting to accept KPNs as the foundation for the Awelon project programming language, assuming appropriate rewrite rules can be discovered. What would this look like?

## Data Extraction Primitive? Reject.

One feature of KPNs is that we can extract outputs monotonically. We might represent this in Awelon by introducing an operator `e`:

        [B[A]]e == [B][A]       (extract)

The extraction operation does enable us to represent non-terminating computations within `B` that will consume any number of inputs to produce any number of outputs - a lot like a single input channel coupled to a single output channel. This is incomplete for KPNs. We need more than one channel. I've contemplated this operator before, but it seems wiser to simply require programmers to model explict result-continuation pairs, explicit input-output frames.

## Explicit, Effectful Channels? Reject.

The Oz programming language has an interesting feature: variables are declared and assigned separately. The variables are single-assignment, but this is essentially a single-assignment channel. From this, we can define multiple-input channels by creating a variable for each input. For example:

        [[A0] [[A1] [[A2] [{A_REM}] cons] cons] cons]

Here `{A_REM}` would be a hole to be filled concurrently with `null` or `[A3] [{A_REM'}] cons`. In the latter case, we can add more to the channel. Hence, we could model full channels using single-assignment channels. 

Potentially, we could augment data extraction to instead create holes:

        [A]e => [A {:Hole}] [{.Hole}]
        [A]{:Hole} B [{.Hole}] => B [A]

Here `{:Hole}` would be the assignment side of a hole, while `{.Hole}` would be the observer side. This acts as a wormhole of sorts to move data instantly. With this, we could represent first-class holes using `[]e`. Unfortunately, there are many difficulties with first-class holes. It's the same problem of "new" things in general - we need a global context to name the holes, and we cannot consider computations to be referentially transparent because `[]e` will create a distinct hole at each place it is used.

If we take this route, the Awelon language would naturally be effectful. Although this is simpler than many impure functional programming language effects models, it is a semantic complication I'd rather avoid. However, I mostly describe it as a basis for discussing *Implicit Channels*.

## Implicit Channels

If we reject modeling channels or holes explicitly, perhaps we can still have them implicitly, leveraging annotations. For example, given code of form `[P](t2)` we know it must evaluate to a pair, i.e. `P =>* [A][B]`. A parallel interpreter could feasibly produce placeholders for `[A][B]` before `P` even finishes evaluation. We might indicate this by combining `(t2)` and `(par)`.

        [P](par)(t2) => [[P{:B}{:A}](proc)d[{.A}][{.B}]]

Here `(proc)` suggests we must evaluate to a unit/identity behavior with assignment effects. A potential concern is that `P` might not complete evaluation - we might halt on a quota or error. Fortunately, we can recover by writing `P` back into our program and using a simple extraction algorithm to erase holes:

        T(X,E) - extract variable X from E
           X T(X,E) == E
        T(X,E) when E does not have X => d
        T(X,X) =>
        T(X,[E]) => [T(X,E)]b
        T(X,F G) 
            when only F has X => T(X,F) G
            when only G has X => [F]a T(X,G)
            when both have X => c [T(X,F)] a T(X,G)

Implicit placeholders hence have the nice properties of not requiring new features and being fully eraseable. The ability to work with placeholder results allows more parallelism opportunities compared to `(par)` alone. 

Unfortunately, it is not obvious how to leverage this technique to represent KPN-style message routing. We cannot route message conditionally! However, we could try *KPN Message Batching*. 

## KPN Message Batching

I know at least two ways to represent a message between KPN processes.

* variant that selects a port and carries one message
* record of optional messages, one label is activated

The variant approach is more efficient. However, the latter representation is a lot more friendly for implicit channels and placeholders because it enables "blind" routing of messages without even knowing which output port was used. We'll only need to filter `None` messages eventually when we try to observe the next message on a port.

Further, the record of optional messages is trivially upgraded to a record of message lists, such that we can route entire batches of messages. This batching could mitigate many inefficiencies from blindly flooding every route.

Evaluation of KPNs in this case could be frame-driven. In each frame:

* every process receives a batch of messages on every port
* we immediately produce placeholders for process outputs
* in parallel, every process evaluates as far as it can
* in parallel, we route placeholders to produce more inputs

We must also limit how a process awaits messages to remove frame aliasing. But we would get a nice structural enforcement that routes are linear. 

## Static Route Acceleration

Routing placeholders on each evaluation frame is rather wasteful. But with KPNs, each evaluation frame has the same basic structure and shape. It seems feasible that we could perform some sort of logical fixpoint where we route sample placeholders only once and then pretend each future frame looks a lot like the previous (albeit, with each hole identifier including an implicit frame number).

This would be interesting, because it would generalize KPN acceleration to any static, frame-driven routes within a fixpoint evaluation. This is a promising approach, although the details still escape me. Importantly, static routing is exactly where we benefit for distributed computations.

