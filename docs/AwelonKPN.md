
# Awelon with Kahn Process Networks

[Awelon language](AwelonLang.md) is purely functional. Unfortunately, this presents significant challenges for scalable parallism and effects models. I've been considering use of [Kahn Process Networks (KPNs)](KPN_Effects.md) for this role, via acceleration. But it might be worthwhile to extend Awelon so KPNs are essentially the normal value. This is especially tempting in context of adding labeled data extensions to Awelon.

## Data Extraction?

One feature of KPNs that makes them valuable is the monotonic structure. We can incrementally add inputs and extract outputs. Ignoring concurrency concerns, this might be understood in terms of adding a primitive feature to Awelon to extract data:

        [B[A]]e == [B][A]       (extract)

Essentially, this would give us specialized support for partial computations. In a primitive step we can either bind one more input to our function or extract one more output. This would correspond to an external view of a KPN - we can add one input or extract one output without directly observing the internal process. *Aside:* I have contemplated an extract primitive `e` for Awelon in the past. But so far I've always rejected it. It seems simpler and wiser to explicitly use a pair to represent partial outputs and continuations.

The other feature of KPNs that makes them valuable is concurrency. Inputs and outputs occur on multiple channels. There is a first-in-first-out ordering within a channel, but we cannot observe the order of messages between two channels. Further, a process network consists of multiple component processes. When we supply an input to the KPN, we can easily determine which component process will observe the input.

## Labeled Channels?

Recently, I've introduced provisional labeled data extensions for Awelon. It occurs to me that labels have a lot in common with channels. We can model records as trivial processes. However, there are also some important differences to ensure order-independence of inputs and concurrent computation. If I want to upgrade labeled data to labeled channels, how should this work? Specifically, how should it work while preserving local, confluent rewriting semantics?

Our Awelon functions additionally produce output on multiple channels. Channel outputs no longer interfere with computation, and outputs to distinct channels are order independent.

        [B] [A] :a == [A] :a [B]            channels hide communication
        [A] :a [B] :b == [B] :b [A] :a      distinct channels commute

We also need some means to observe data on a channel. This seems to be the more challenging feature. One significant issue is that observations on distinct channels mustn't interfere. Inputs must propagate across irrelevant observers. Hence, we need to scope our observers, such that we may commute across them. Hence, observers should be blocks of some sort. This could be represented as follows:

        [A] :a .b == .b [A] :a
        [B] :b [OnB] .b == [[B]OnB]

This would use labels to bind inputs. Unfortunately, this doesn't make it easy to work with multiple inputs and outputs. What we really need is the ability to receive multiple inputs and produce multiple outputs within a scope. We could try a distinct scoping membrane:

        {a b - c | CODE}        receives on a,b; outputs on c
        [A]:a {a | CODE} == {a | [A] :a CODE}
        [C]:c {a | CODE} == {a | CODE} [C]:c
        :a .a == 
        {a - b | CODE [C]} == {a - b | CODE} [C]
        {a - b | CODE [B]:b} == {a - b | CODE} [B]:b

This would allow us to specify for a subprogram which channels it observes or writes to, and we could thus commute subprograms that don't have conflicting labels. Each scope would allow for multiple inputs and outputs. Essentially, this would give us syntactically enforced effect types for a channel-based effects model. I don't like it. I feel this adds too much syntactic overhead.

## Single Assignment Futures?

Another approach to deterministic concurrency involves single-assignment variables. Each variable is declared and assigned only once. This assignment is non-local, separated from the declaration. When a variable is assigned twice, the program is clearly in error. This is capable of modeling channels, i.e. a channel involves producing an infinite list of single-assignment variables, copying this list, then having one computation assign while the other reads. The difficulty with single assignment futures are those unassigned variables that escape local scope and might see use in multiple contexts. I'd prefer to avoid this technique, and similar techniques that construct 'new' stateful objects and rely on non-local interaction.

However, I wouldn't mind using a similar idea without true single-assignment semantics. If we know from our type system that a value should be produced in the future, it seems feasible to introduce a placeholder in the meantime. This could permit a certain level of dataflow pipelining for carefully structured programs.

## Other Ideas?

I'll keep an open mind. If I can find a nice way to integrate KPN behavior with simple local rewrite semantics, I'll give it serious consideration.

I should also pay attention to whether labeled data is directly usable for a KPN accelerator.
