Streaming Language Rewrite Processing (SLRP) is a low-level computing model that is essentially characterized by limited-memory processors rewriting a huge input stream. Processing under this condition requires multiple passes, but is extremely amenable to pipelining. SLRP processors may have side channels to external devices, accessed as part of a few 'effectful' rewrite rules.

This article describes SLRP, its strengths and weaknesses, and methods to mitigate those weaknesses.

TLDR: SLRP is an promising alternative to the von Neumann architecture. SLRP offers advantages for security, concurrency, and scalability. However, SLRP performs poorly for data plumbing and loops because there is no pass-by-reference for large values within a stream. This weakness can be mitigated by effects: we could interact with external storage devices and hybridize this useful aspect of the von Neumann architecture.

<em>SLRP Machine Code</em>

The SLRP architecture does not specify a language. However, a usable SLRP language is valuable as a proof of concept and a concrete foundation for discussion. This particular SLRP machine code is based on a concatenative combinatory logic. An initial subset of the rewrites I'll support:

<code><pre>a[A][B] == [B]A     (apply)
b[A][B] == [A[B]]   (bind)
c[A]    == [A][A]   (copy)
d[A]    ==          (drop)</pre></code>

These four combinators form the semantic foundation of my Awelon language, albeit presented here to reflect stream order: the operator `a` will be observed in the stream before the operand `[A]`.

The challenge for an SLRP implementation is that these blocks may be larger than our processor's memory. Thus, given `a[A...` we generally cannot see the end of the first operand. 

To solve this challenge, I introduce auxiliary rewrites, make one convenient assumption that block and use a divide and conquer strategy.

<code><pre>w[A][B] == [B][A]   (swap)
q[A]    == [[A]]    (quote)
o[A][B] == [AB]     (compose)*
i[A]    == A        (inline)

a[XY == a[X]a[Y
a[[X]Y == w[X]a[Y
a[ops][B] == [B]ops

w[XY == a[o]w[X]w[Y
w[[X]Y == a[oq]w[X]w[Y
w[ops][B] == [B][ops]

c[XY == oa[a[o]w]c[X]c[Y
c[[X]Y == oqa[a[oq]w]c[X]c[Y
c[ops] == [ops][ops]</pre></code>

Above, I'm assuming `ops` is any subprogram that's small enough to fit into our processor's limited input buffer. A larger buffer will have direct and positive consequences on SLRP data-plumbing performance. 

We can implement the bind operator as `b = oa[q]`. Whether we keep it as a distinct operator may probably depend on empirical performance analysis, and the sort of optimizations a processor might perform upon recognizing an `oa[q]` sequence.

Further, I'm assuming that block-depth is bounded (e.g. by 2^32). Thus, a trivial block-depth counter will allow our processor to detect whether it's reached the end of a block. Under this assumption, the drop, quote, and inline operators are mechanically trivial in SLRP.

This leaves only composition operator `o[A][B] == [AB]`. Unlike the other operators, there's no benefit from a divide and conquer strategy. Our `[A]` block is already where we want it to be! However, in general, we still cannot see the end of our first operand in `o[A...`.

To implement composition, I introduce the concept idea of open-ended blocks: `o[A] == [A\`. Here, `\` is our open block terminator, with the behavior that `\[` cancels, such that `o[A][B] == [A\[B] == [AB]`. As an optimization, we may also delete `o[]` or `[\`. 
 
In the presence of open-ended blocks, we must extend our rewrite rules:

<code><pre>-- Easy!
d[A\ == d
q[A\ == [[A\\q
i[A\ == Ai
o[A\ == [A\o
a[ops][B\ == [B\ a[ops]
w[ops][B\ == [B\ w[ops]

-- Problematic!
a[[X\Y == a[o]w[X]a[Y
w[[X\Y == a[ok]w[X]w[Y
c[[X\Y == oka[a[ok]w]c[X]c[Y

-- Auxiliary (just for symmetry)
k[A] == [[A\]    (k = b[o])</pre></code>

I described three cases as problematic. In SLRP, a rule such as `w[[X]Y == a[oq]w[X]w[Y` can be mechanically implemented by first recognizing `w[[` then immediately printing prefix `a[oq]w[` then all of `X`, detecting the end via counting delimiters. Importantly, that prefix was decided at `w[[`. However, to handle open-ended blocks, the three problematic cases need a different prefix. Fortunately, this is easily resolved: we define a prefix like `iw[` that defers the actual prefix decision. The modified divide-and-conquer rules for open blocks:

<code><pre>a[[X]Y == iw[X]  [w]           a[Y
a[[X\Y == iw[X]  [a[o]w]       a[Y
w[[X]Y == iw[X]  [a[oq]w]      w[Y
w[[X\Y == iw[X]  [a[ok]w]      w[Y
c[[X]Y == iw[X]  [oqa[a[oq]w]  c[X]c[Y
c[[X\Y == iw[X]  [oka[a[ok]w]  c[X]c[Y</pre></code>

Performance of data-plumbing aside, the above set of combinators gives us Turing complete, confluent computation and a concatenative syntax. We could easily construct large computations using component-based programming style with deep inlining. Also, for those more familiar with lambda calculus, we can very easily <a href="https://awelonblue.wordpress.com/2016/11/15/lambdas-in-tacit-code/">compile lambda expressions</a> to use these tacit `a b c d` operators.

An SLRP machine code benefits from several more extensions.

A delay operator: `~[A][B == [A][B`. Delay represents an intention to wait for two operands, allowing the second operand to be open-ended. When `[A]` is small, our processor can simply look ahead in its buffer. If `[A` is large, we could instead rewrite `~[A` to `dwa[~[]][A`. A delay operator allows programmers to control evaluation strategy, represent call-by-need computations and codata via `[ctor~[Arg]]`, or embed comments via `d~[comment]`. 

A strict fixpoint combinator: `z[F][X] == F[z[F]][X]`. We could implement this directly as `z = ic~[iwba[c]b[ic~a[~]]]~`. However, a dedicated operator will reduce overheads and simplify recognition of loops for tight-loop optimizations within a processor. If a processor cannot see the end of `[F` we might rewrite via a generalized rule `z[F == ia[b[z]]c~[F`. 

I'm sure we can find more operators that improve expressiveness or performance. However, two items deserve special attention: data and effects!

<em>SLRP Data</em>

For high-level data structures, a Mogensen-Scott encoding isn't bad. However, as a low-level language, SLRP must support a reasonably efficient embedding, processing, and extraction for binary and numeric data.

Fortunately, it is not difficult to extend SLRP with binary data embeddings. A header that indicates size can distinguish binaries from normal SLRP code. For example, header `F` might indicate a four-byte IEEE floating point representation. In this case, `F]]]]` represents the number 996937981862346752 instead of four block terminals. To simplify data plumbing, we'll box every binary value within a block, like `[F....]`.

Besides floats, we can easily adopt other common number types. Further, we might support a few simple combinatorial types such as `V` for vectors and `P` for pairs, such that `V32PBF` would indicate a vector of 32 byte-float pairs, followed by 160 bytes of binary data.

After we're satisfied with our binary data embedding, we must develop operators to manipulate it. We can provide common mathy operations such as `+` and `-` and `*`, and make these polymorphic in our types such that `+[F....]` might require the second operation to also be a float.

Intriguingly, it's feasible to squeeze some APL or J style collections-oriented programming from SLRP processors. Adding a float to a vector of floats might add it to every entry. We could develop specialized operators for arithmetic scans and folds over a vector. 

<em>Effectful SLRP</em>

An SLRP processor may have side channels for interaction with the external environment - disk, display, sensor, network, etc.. An SLRP stream may act as a 'driver' for these channels, with some rewrites invoking interaction with the environment. We can add a request-response operator to our SLRP language:

<code><pre>![ChannelID][Request] => [Response]</pre></code>

Operator `!` tells our processor to push a request down the specified channel, then replaces the request by the response (if any). The channel identifier must be a simple binary type. The request and response will frequently be binaries, but that isn't necessarily the case.

We can easily leverage effects for performance reasons, e.g. moving vectors of floats to a GPGPU for high performance computing.

Requests in SLRP streams are opportunistic and unordered. Further, copy and drop operations might replicate or remove a request. We won't solve this at the SLRP layer. Application programmers should use a monadic programming pattern or some other pattern to control effects when it matters. 

Centralizing effects to a single operator `!` hugely simplifies security. We can trivially enforce capability-security for compiled apps, forbidding `!` in the app logic, then instead supply a set of effectful methods as an argument to the application.

<em>Virtual Memory for SLRP</em>

The conventional notion of 'virtual memory' mapping an address space does not apply to SLRP streams (which are not addressed). However, we can adapt the idea: we can offload a volume of our stream through a side-channel, leaving a placeholder. When the code or data is needed again, we can load the data back into our stream.

There are many benefits for virtual memory in SLRP: We can reduce data-plumbing overheads, passing large objects by a reference to virtual memory. We can support a cheaper, higher-latency memory for code and data that we aren't using frequently. We can virtualize 'active' regions of a stream to either delay computation or move it to a remote processor.

Virtual memory must be implemented effectfully, and could use the `!` operator and a request channel. But, properly implemented, the only observable impact to the SLRP computation should be performance, and there are some optimizations we might want. So dedicated operators would make sense.

<em>SLRP Hardware and Virtualization</em>

An SLRP processor has a fixed amount of memory. However, larger memory can significantly improve performance. There are two reasons for this: First, the base case for many data plumbing rewrites such as `a[ops][B] == [B]ops` depends on input buffer size. Second, we can maintain a large output buffer and use it to backtrack. This allows our processor to focus attention and effort on tight loops and computations within each full-stream pass.

Where SLRP shines is multi-processor systems. We can pipeline an SLRP stream through dozens of processors, thus doing parallel work per step. We can use virtual memory to partition and schedule an SLRP stream across remote processor groups.

Intriguingly, we can support heterogeneous multi-processor systems where different processors support different request channels. Then we could heuristically schedule and migrate code on the processors that can handle pending requests, thus supporting remote-procedure-calls and mobile code within SLRP's linear-stream semantics.

SLRP systems can easily integrate 'virtual' processors, implemented in software. Obviously this is an economic necessity to get SLRP off the ground. However, even assuming SLRP has its day and grows into a popular hardware architecture, virtualized processors would be convenient for integrating effects that we're unready or unwilling to implement in hardware - like GUI layers.

<em>Aside:</em> SLRP processors are free to optimize rewrites, such as fusing `oa[ops]` or `ow[ops]` into a single rewrite, simply erasing `a[]` and `ww`. Such optimizations are welcome, especially as the technology matures. But I feel we shouldn't rely too much on these little tricks.

