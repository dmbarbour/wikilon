
# Stream Language Rewrite Processing (SLRP)

See the [SLRP-Article.md](SLRP-Article.md).

The essential idea for SLRP is that we have fixed-memory processors rewrite an input stream in multiple passes. This has a many benefits for pipelining, partitioning, and parallelism. In the article, I outline an initial SLRP machine code based on a concatenative combinatory logic.

Unfortunately, data plumbing (dip, dup, swap, loop, etc.) is too expensive in our initial example language. I can hack a solution with virtual memory. However, if this were adequately solved in pure SLRP, SLRP could become a very nice, practical foundation for computing.

## Data Plumbing

The initial SLRP machine code has several binary operators. However, these operators only "see" one operand within our stream. To overcome this limit, we're using expensive divide-and-conquer tactics to ferry `xyz` buffers of data. This can be mitigated by large buffers, such that `xyz` may involve tens of kilobytes. However, even if we mitigate this, it still requires our processor's full attention, and occurs too frequently for common operations.

To solve this problem, we need processors that support multiple input streams. This way, an operator could observe two streams and either merge them element-wise (zip, merge-sort, etc.) or at least reorder large volumes efficiently (e.g. all of X followed by all of Y, no ferrying small buffers). 

However, divided streams introduce semantic and mechanical challenges. 

Where does that second stream come from? How does it interact with abstraction?

## Brainstorming

No solid solutions yet, just some ideas.

## Multiple Channel SLRP

With a multi-channel SLRP, processors have a fixed number K of input and output streams. This fixed number would be determined by our machine-code and hardware, and is program-independent. The processor must rewrite these several streams collectively.

To support pipeline processing with bounded buffers, these channels must be synchronous. If the channels were asynchronous, then bounded buffers will fill at variable rates. This would result in pushback, and downstream processors will become "stuck" without input on every channel. It is certainly feasible to represent 'whitespace' NOPs, though. It is feasible to model channels that are synchronous at different 'rates' however. For example, two bytes of channel B for every byte in channel A.

In mechanical terms, a multi-channel SLRP is equivalent to a single-stream SLRP with a word-size proportional to the number of bytes transferred in a synchronous time-step. Thus, a "multi-channel" behavior must be part of our semantics, all about how we interpret and leverage these extra bytes.

*Note:* For convenient programming, I would favor a semantics where all valid programs can be represented as one stream, assuming the other streams are initially just whitespace. We should also be able to extract the interleaved stream back into a single stream.

## Bi-directional SLRP 

The idea with bi-directional SLRP is to have input-output channels flowing in opposite directions.

A difficulty with this is that, unless we're willing to accept a non-deterministic model, we need to clearly understand where each "rendezvous" between channels should occur. (And for non-determinism, I'd rather just use effects for this role.)
























Awkwardly, to support pipeline processing, these channels must be 'synchronous' at the byte level. Otherwise, bounded buffers will at variable rates and downstream processors may become stuck, unable to observe at least one of the K in

It is feasible to have some channels with larger "word sizes" than others, e.g. so channel B has 3 bytes for every 1 byte in channel A. However, without a loss of generality, we could simply divide B into 3 channels that are byte-for-byte synchronous with A. 



 That is, every output from one channel must be accompanied by outputs on other channels. 

To support pipeline processing, these channels must (rather awkwardly) be synchronous at the byte-level. The mechanical reason for this is that we'll have bounded buffers between processors. 

These channels would need to be *synchronous*. The reason for this: if we 'pushback' on a stream



Can this work?

Ignoring pipelining, we can guarantee our processor has inputs on every port. Thus, we could develop a set of rewrite rules that always make progress. Unfortunately, this no longer works when we pipeline our processors together. Relevantly, 





## Concrete Channels?


, and we rewrite them collectively into several unbounded output streams. Processors may pushback on their unbounded input sources, and are not guaranteed to provide output on any given output channel in a timely fashion.

A consequence of 'pushback' means we cannot usually expand a si



Mechanically, this seems feasible. 

Semantically, this is a challenge. That is, assuming we intend to maintain nice features such as pipeline processing, easy partitioning, confluence up to effects, virtual memory, etc..

What is our mechanical foundation upon which we may construct a semantics?

* Processors are physically wired to K input streams.
* Processors are wired to K' output streams. K = K'?
* Operations and operands are encoded in the streams.
* Stream states: terminated, waiting, data available.
* We can only see the "header" for larger operands.

If we assume multiple rewrites are required to complete a computation, and that pipeline processing might be useful, then our number of outputs channels must match the number of input channels, and routing must be simple and linear. 

If we want confluent computation, then we must not observe the "waiting" condition for any input stream. If we want local rewrites, then we also shouldn't be able to observe the "terminated" condition. Instead, we should have a simple approach to determine how much data a given operation is waiting on. It is feasible to achieve limited parallelism if rewrites on multiple inputs are not conflicting. 

Our implementation must look at a small buffer of data available on each stream and simply *know* what to do next.




