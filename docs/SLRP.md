
# Stream Language Rewrite Processing (SLRP)

NOTE: This document is a bit old. See [SLRP-Article.md](SLRP-Article.md).


SLRP is an idea for a stream-rewriting processor and computing architecture.

The essential idea is that we have bounded-memory processors operating on an unbounded stream of mixed code and data. The processor cannot jump backwards in the stream, though it may remember a bounded volume of things. 

At the moment, it's more of a thought experiment. 

Design Constraints:

* A stream is an ubounded sequence mixing code, control, and data.
* Processors have bounded memory and monotonically rewrite a stream.
* The stream represents a concatenative, combinatory logic program.
* Multi-pass. Computation is complete when output matches input.
* Computation confined and confluent on word-aligned boundaries. 

This design specifically avoids use of jumps, pointers, and memory addressing. There are no 'side effects' of computation. SLRP is purely functional to simplify parallelism and caching. However, we may develop a free-monadic effects model to represent interactions with external systems. 

This design assumes a purely functional language, which simplifies many things but does require explicit handling for external effects. I assume effects will be achieved via a free-monadic effects model, handled by an external (conventional) process or a special processor. More on this later!

Structural Features:

* Pipelining. Arrange processors end-to-end for big steps.
* Parallelism. Assign many processors among many regions. 
* Prioritization. Focus processor effort on relevant regions.
* Buffering. A processor with a big memory can optimize easily.
* Codata. Mix code and data, and delay some computations.
* Control. Processor can output incomplete results at quota.
* Incremental. Memoize result of subprograms. Recompute on changes.
* Holes. Specify an stop operator for partial programs.
* Debuggable. Render stream at any time. Repeatable computations.
* Implementable. A very simple program can model a processor.

Special Challenges:

* Unbounded data may be larger than our bounded memory processor.
* Data plumbing is more expensive than logical pointer movement.
* Data must be efficiently embedded, processed, and extracted.
* Data types: difficult to embed many data types within stream.
* Loops. Must fixpoint loops computations. Many logical copies.
* Projections. No human-meaningful symbols in output. Decompile.

I'm uncertain how to improve data-plumbing in general. But it is feasible for a processor to have a large internal memories and use pointers internally for data shuffling (and binding) of small blocks.

SLRP performance might feasibly be augmented if we can develop suitable extensions to interact with GPGPUs or construct KPNs. I'll definitely include an extensions operator.

## Minimal SLRP

Let's represent Awelon's basis for Turing-powerful computations.

        a[A][B] == [B] A        (apply)
        b[A][B] == [A [B]]      (bind)
        c[A]    == [A][A]       (copy)
        d[A]    ==              (drop)

In SLRP, these might not be the best choice for primitive operators because we're not assuming lightweight manipulation of blocks via references. However, we can at least take these as a starting point. And if we end somewhere else, we should still be able to implement these operators.

One difficulty is that we cannot assume our SLRP processor can see both sides of the first block, much less see a second block. We only see the equivalent of `a[` or `b[` or `c[` or `d[`. Thus, we must usefully rewrite given a pinhole view like this or at most a fixed number of bytes in advance. 

My ideas to handle these: 

First, we can represent special state-machine operations that process the codes they observe from left to right. These state machines must represent their state and track block depth. Operators `0-9A-F` can be introduced for compact data. Our state machine might then be represented via `\[C][3F]` to indicate state `C` and block depth `3F`. (Alternatively, we could save a couple bytes with `\C[3F]`.)

Second, we can represent a special 'floating' phase for operations in the language. The high bit for every word could be used to represent whether the instruction is floating. Floating instructions move downstream across normal operators except for those state-machines (because those might produce more floating instructions) and a special 'dock' instruction.

Third, we can represent dock instructions which receive a block of floating instructions as normal instructions, erasing the outermost block delimeters. We'll add them back as needed. `|[3F]`.

Anyhow, it's feasible for these state machines to perform the deletion, prepare a copy, add an extra `]` at the end of a block, etc. Below, `'o` represents the floating version of `o`, and `o` or `p` stands for a normal operator excepting `\|[]`. 
        

        'op             =>      p'o
        a[              =>      '[  \A[0]               *
        \A[0]][         =>      '][ \E[0]               *
        \A[N][          =>      '[  \A[(N+1)]
        \A[(N+1)]]      =>      ']  \A[N]
        \A[N]o          =>      'o  \A[N]               *
        \E[0]]          =>      ] |[0]                  *
        \E[N][          =>      [ \E[(N+1)]
        \E[(N+1)]]      =>      ] \E[N]
        \E[N]o          =>      o \E[N]

        ']|[0]          =>      |[1]                    
        '[|[1]          =>      
        ']|[(N+1)]      =>      |[N]]
        'o|[N]          =>      |[N]o

This would basically implement the `a` operator, modulo debugging. The interesting points are starred. The `\A` state scans across the first block, shifting every operation to floating phase. Upon finishing the first block, we switch to state `\E` which basically skips the second block then sets up a dock for the floating data produced by `\A`.

Copy should be easier (no need for a second state) while bind also requires two states (but no use of floating ops). And delete's the easiest of all. Actually building all these rewrite rules into our SLRP processor is a little awkward, but not too difficult. And a processor with a big buffer could easily perform bigger steps. It might be feasible to simplify walkers by choosing a different base to start with, to minimize the number of states.

Is this correct?

We cannot copy an block that's still being processed by a walker because the copy itself will use a walker. Also, say a walker that produces floating operations is half-way through a block before we begin evaluation and produce another walker *behind* the first walker yet *ahead* of some floating data. Assuming this new walker is correct, it is inductively safe because only floating data produced by the new walker will be handled by this dock.

It should be correct. A more rigorous proof would be nice.

## Cleaner SLRP?

Let's assume SLRP streams are always block-balanced. If we don't have the full stream, it's sufficient to wait for some upstream input. 

Some features I'd like to see:

* No counters for basic operations within the SLRP stream itself.
* No walkers, sliders, or placeholders. These are simply too awkward. 
* Computations can continue *upon* partial results. Stream fusions!
* Option to wait for a block to fully compute. (Maybe via effects?)

Desired operations:

        PRIMARY
        a[A][B] == [B]A
        b[A][B] == [A[B]]
        c[A]    == [A][A]
        d[A]    == 

        AUXILIARY? (ADD MORE AS NEEDED!)
        o[A][B] == [AB]
        w[A][B] == [B][A]
        i[A]    == A
        q[A]    == [[A]]


Mechanically, an SLRP processor cannot contain an entire block. However, we can try some divide-and-conquer approaches, such as rewriting `a[[X]Y] => w[X]a[Y]`. In the end, we'll have some bounded buffer like `a[ops][B]` that fits into our processor's tiny memory, which we can rewrite directly to `[B]ops`, outputting `[B]` unmodified as we pass it. This shifts a lot of work to later passes, but that's okay: we'll have a small army of processors.

How far can we get with just divide-and-conquer?

        The 'apply' operation:
        a[[X]Y == w[X]a[Y
        a[XY   == a[X]a[Y
        a[ops][B] == [B]ops

        The 'swap' operation:
        w[[X]Y == a[oq]w[X]w[Y
        w[XY == a[o]w[X]w[Y
        w[ops][B] == [B][ops]

        The 'copy' operation:
        c[XY == oa[a[o]w]c[X]c[Y
        c[[X]Y == oqa[a[oq]w]c[X]c[Y
        c[ops] == [ops][ops]

        Trivial (no factoring needed): quote, inline, drop
        Bind: b = oa[q]

        The 'compose' operation: ???
        o[[X]Y == ???
        o[XY == ???
        o[ops][B] == [opsB]

The 'compose' operation `o` is where I hit the limits of divide-and-conquer. Essentially, the problem is that at `o` we already have the optimal position for our first operand, so there's no need to move anything. And if both operands are available, we only need to erase the `][` pair in the middle. But we have no way at `o` to see whether the second operand is available yet.

An idea with some potential is to support "open ended" blocks, using a special block terminator.
        
        \[ == 
        [X\[Y] == [XY]
        o[X] == [X\

Then, we must implement our rewrite for every case where we might not observe an open-ended block until we reach it:

        The Easy Cases:

        a[buffered][B\ == [B\ a[ops]
        w[buffered][B\ == [B\ w[ops]
        q[X\ == [[X\\q
        o[X\ == [X\o
        i[A\ == Ai
        d[X\ == d

        The Tricky Cases:

        a[[X\Y == 
        w[[X\Y ==
        c[[X\Y ==

The challenge with these 'tricky' cases is that, due to the buffering limits of our processors, we must print out a common prefix for `a[[X` or `w[[X` or `c[[X` before we see the terminator. Fortunately, we could solve this by choosing a prefix like `iw` to defer our "what to do with `[X]`?" logic as a d operand. This works as follows:

        a[[X]Y ==   iw[X]   [w]             a[Y
        a[[X\Y ==   iw[X]   [a[o]w]         a[Y
        w[[X]Y ==   iw[X]   [a[oq]w]        w[Y
        w[[X\Y ==   iw[X]   [a[ok]w]        w[Y
        c[[X]Y ==   iw[X]   [oqa[a[oq]w]c]  c[Y
        c[[X\Y ==   iw[X]   [oka[a[ok]w]c]  c[Y
            where k = o[o]q     to help show symmetry

Okay! This seems extremely promising as a basis for SLRP. However, the performance of our split cases kind of sucks: too many swaps. Could we do better?



## Threaded SLRP

The biggest weakness of SLRP as defined is that our processor is unable to operate efficiently on several data items at once. A potential solution for this is to develop a *threaded* SLRP, where we model multiple streams threaded together. E.g. we could have four streams via 32-bit words where each word represents one byte in each of four different streams.



## Working with Data

## Registration of Code







* Data manipulations. Let's access binaries in user-code, too.
* Symbolic Code Refs. Support or preinstall of named functions.



