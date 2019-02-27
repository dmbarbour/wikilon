Streaming Language Rewrite Processing (SLRP) is a low-level computing model that is essentially characterized by limited-memory processors rewriting a much larger input stream. Computation under this condition requires multiple passes, but is highly amenable to pipelining and partitioning, and thus parallelism. SLRP processors may have side channels to external devices that we drive through 'effectful' rewrite rules.

This article describes SLRP, its strengths and weaknesses, and methods to mitigate those weaknesses.

TLDR: SLRP is a very promising alternative to the von Neumann architecture. SLRP offers advantages for security, concurrency, distribution, and scalability. However, SLRP performs poorly for data plumbing and loops because there is no pass-by-reference within the stream. This weakness can be mitigated by effects, shunting a region of the stream to a remote device and keeping a small placeholder - a reference.

<em>SLRP Background</em>

The essential requirement for SLRP is a rewrite language that can be mechanically implemented under the constraint of a fixed-memory processor.

Fixed memory is obviously a non-issue for working with fixed-width values, such as natural numbers in range 0..999. A fixed-memory processor can rewrite `*(6)(7)` to `(42)` if it wants, no problem at all if number of digits is limited. In practice we might favor 32-bit or 64-bit binary encodings of numbers, but it's the same basic idea.

The challenge is dealing variable width values, including first-class functions.

For variable-width values, I assume we can detect the 'end' of a value either by counting delimiters or by having a prefix that indicates size then counting bytes. Depth or size is still bounded in these cases, but the bound may be absurdly large, like <code>2^64</code>.

Mechanically, here's what we can do: we can erase a variable-width value, we can write a variable-width value to output, we can write a prefix before writing a variable-width value to output (to be handled by a later pass), we can ferry a fixed-width buffer of code or data across a variable-width value, and we keep have a finite state machine that reminds us what to do when we're done skipping a variable-width value. That's it.

Below, I implement a Turing complete concatenative combinatory logic via stream rewriting under SLRP constraints. The article assumes at least a distant "I foggily remember reading about it years ago" familiarity with those subjects and either Church encodings or Mogensen-Scott encodings. If you don't know about these things, or feel you need a refresher, try Wikipedia. 
 
<em>SLRP Machine Code</em>

A usable SLRP machine code is valuable as a proof of concept and a concrete foundation for discussion. The machine language I develop below is based on a concatenative combinatory logic. I'll start with a semantic foundation of my Awelon language:

<code><pre>a[A][B] == [B]A     (apply)
b[A][B] == [A[B]]   (bind)
c[A]    == [A][A]   (copy)
d[A]    ==          (drop)</pre></code>

The SLRP processor will observe `<code>a[A][B]</code>` from left to right. The essential challenge for SLRP is that our processor has limited memory, yet our operands are unbounded. Hence, given a huge operand, our processor will only see the front end like `<code>a[A...</code>`, and must achieve useful progress within that constraint. Nonetheless, we can implement this in bounded memory if we leverage several auxiliary rewrites.

<code><pre>-- language extensions
w[A][B] == [B][A]   (swap)
q[A]    == [[A]]    (quote)
o[A][B] == [AB]     (compose)*
i[A]    == A        (inline)
\[      ==          (join)

-- mechanically trivial rewrites
 \[  == 
o[A] == [A\
o[A\ == [A\o
q[A] == [[A]]
q[A\ == [[A\\q
d[A] ==
d[A\ == d
i[A] == A
i[A\ == Ai
b    == oa[q]

-- `xyz` fits processor memory
a[xyz][B] == [B]xyz
a[xyz][B\ == [B\a[xyz]
w[xyz][B] == [B][xyz]
w[xyz][B\ == [B\w[xyz]
c[xyz]    == [xyz][xyz]

-- divide-and-conquer tactics
a[XY    == a[X]a[Y
a[[X]Y  == iw[X][a[q]w]a[Y
a[[X\Y  == iw[X][a[o]w]a[Y

w[XY    == a[o]w[X]w[Y
w[[X]Y  == iw[X][a[oq]w]w[Y
w[[X\Y  == iw[X][a[ok]w]w[Y
    where k = o[o]q

c[XY    == oa[a[o]w]c[X]c[Y
c[[X]Y  == iw[X][oqa[a[oq]w]c]c[Y
c[[X\Y  == iw[X][oka[a[ok]w]c]c[Y
    where k = o[o]q
</code></pre>

To overcome the limitation that our SLRP processor generally cannot see the end of an operator's first operand, I introduce open-ended blocks, terminated by `\`. Formally, we have `<code>[A\ == o[A]</code>`, so `\` is a valid block terminator where it counts.

Note that several rewrites have two cases depending in whether a block ends in `\` vs `]`. Note also that, in each such case, that particular block has the same output prefix. Thus, we're only deciding on some conditional behavior after we reach the block terminator.

Swap, copy, and apply are complicated in the general case, requiring divide-and-conquer tactics. However, in practice we will mostly use the base-case where `<code>xyz</code>` fits our processor's input buffer. A larger memory will improve data plumbing performance.

Performance of data-plumbing aside, the above set of combinators gives us Turing complete, confluent computation. The machine code is concatenative, which is very convenient for modularizing and constructing programs - e.g. concatenation of machine-code files corresponds to a composition of functions. For those who favor a lambda calculus, we can very easily <a href="https://awelonblue.wordpress.com/2016/11/15/lambdas-in-tacit-code/">compile lambda expressions</a> to use tacit `<code>a b c d</code>` operators.

I immediately recommend two more operators for convenience:

A delay operator: `<code>~[A][B == [A][B</code>`. Delay represents an intention to wait for two operands, allowing the second operand to be open-ended. A delay operator allows programmers to control partial evaluations and represent call-by-need computations and codata (such as infinite, procedurally generated trees) via `<code>[ctor~[Arg]]</code>`. It can also be used to embed comments via `<code>d~[comment]</code>`. If our first operand is too large, we can rewrite `<code>~[A == dwa[~[]][A</code>`. 

A strict fixpoint combinator: `<code>z[F][X] == F[z[F]][X]</code>`. We could implement this by hand: `<code>z = ic~[iwba[c]b[ic~a[~]]]~</code>`. However, a dedicated loop combinator can reduce overheads and simplify recognition of loops by a processor that might want to perform internal optimizations. If the function operand is too large, we can rewrite `<code>z[F == ia[o[z]q]c~[F</code>`.

I'm sure we can find more operators that improve expressiveness or performance. However, two items deserve special attention: data and effects!

<em>SLRP Data</em>

For high-level data structures (trees, stream values, etc.), a Mogensen-Scott encoding isn't bad. However, it isn't suitable for lower-level data IO. So we'll extend our SLRP language with binary data types.

To embed binaries in SLRP, it's sufficient to use a sized header such as `F` before a four-byte floating point. This way, our processor knows that `<code>F]]]]</code>` represents the number <code>996937981862346752</code> instead of four block terminators. Besides floats, we could support double-precision floats and integers of common sizes (1,2,4,8 bytes, signed and unsigned). Further, it's feasible to support vectors and simple pairs of fixed-width elements such that `V32PBF` would represent the header for an sequence of 32 byte-float pairs.

Encoding type information with our binary is convenient for safety, for rendering and debugging our streams, and for polymorphic manipulations - e.g. such that `+F....` is implicitly a floating-point add. It is feasible to achieve some collections-oriented programming features with dedicated manipulations of vectors, or adding a float to a vector implicitly adding to every element.

Our drop, quote, swap, and copy operations must be extended to handle binary data. (We cannot apply or inline a binary.) Swap and copy may need divide-and-conquer tactics for large binaries, which might be achieved using vector and pair manipulations (split, append, etc.). 

<em>Effectful SLRP</em>

SLRP processors may have side channels for interaction with the external environment - disk, display, sensor, network, etc.. The SLRP stream may act as a 'driver' for these channels, with a subset of rewrites involving interaction with the environment. I propose a request operator `!` for this role:

<code><pre>![ChannelID][Request] => [Response]</pre></code>

Operator `!` tells our processor to push a request down the specified channel, then replaces the request by the response, if any. The channel identifier should be a binary type. The request and response will often be binaries, but that may depend on the channel.

Requests in SLRP streams are opportunistic and unordered. Further, copy and drop operations might replicate or remove a request before it executes. Insofar as this is a problem, SLRP just leaves it to application programmers, who could use patterns such as monadic programming to control use of effects.

<em>Virtual Memory for SLRP</em>

The conventional notion of 'virtual memory' mapping an address space does not apply to SLRP streams (which are not addressed). However, we can adapt the idea: we offload a volume of our stream through a side channel, leaving behind a placeholder. When the code or data is needed again, we can load the data back into our stream.

There are several benefits for virtual memory in SLRP: The placeholder may be much smaller than the data it replaces, allowing for more efficient data plumbing. We can offload to a cheap, high-latency memory to simulate larger memory. We could evaluate the offloaded region on remote processors, thus using virtual memory to partition a parallel computation.

Virtual memory will be implemented effectfully. But, properly implemented, the only observable impact to the SLRP computation should be performance. We might wish to specialize handling of virtual memory to further improve performance. I'm inclined to develop dedicated operators for this purpose.

<em>SLRP Hardware and Virtualization</em>

SLRP processors have a fixed amount of memory. I imagine this quantity to be at least tens of kilobytes. A larger processor memory will allow more data plumbing to be handled by base case rewrite `<code>a[xyz][B] == [B]xyz</code>`, which is much more efficient than divide-and-conquer techniques. Further, processors may buffer output and backtrack, computing multiple passes within a 'sliding window' over the larger input stream. Doing so can greatly improve efficiency for tight loops, allows the processor to focus effort where progress is visible.

However, where SLRP shines is multi-processor systems. We can pipeline our SLRP stream through dozens of processors, thus achieving a great deal of parallel work per full-memory pass. Further, we can use virtual memory to partition and schedule fragments of SLRP streams across distinct processor groups, further improving parallelism and concurrency.

In a heterogeneous multi-processor system, where different processors support different request channels, we could heuristically arrange for code to move to the processor that can handle a given request. This would simulate remote procedure calls or mobile code, while preserving SLRP's simple stream rewriting semantics.

SLRP systems may integrate 'virtual' processors, implemented in software. This is an economic necessity to get SLRP started. However, even assuming SLRP grows into a popular hardware architecture, virtualized processors might prove convenient for integrating 'effects' channels that we're unready or unwilling to implement in hardware - such as higher level GUI layers, or sandboxes.

<em>Aside:</em> SLRP processors are free to optimize rewrites, such as fusing `oa[ops]` or `ow[ops]` into a single rewrite, simply erasing `a[]` and `ww`. Such optimizations are welcome, especially as the technology matures. But I feel we shouldn't rely too much on these little tricks.

