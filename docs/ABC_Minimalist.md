
# Minimalist ABC

*Note:* This proposal has been accepted and has been partially merged into the main [ABC docs](AboutABC.md). Some content remains but will eventually be moved out of this document.

## Runtime

### API

The API will be oriented around building a 'program' left to right in terms of injecting data (texts, numbers) and operations, evaluating it, then extracting data. We also need APIs for active debugging and persistence.

*Note:* I may provide an option/mode to perform lightweight optimization and simplification during program entry. But I will want a version that exactly preserves a program as entered.

*Aside:* I won't introduce support for binaries yet - at least not before accelerators exist to simplify rendering within a program. 

### API for linking and dictionaries

*Note:* This API will need some review upon introducing an AO-level linking model, e.g. with `{%word@source}`. Mostly, we may need to bind multiple dictionaries, one for each `source`. See also [Linking.md](Linking.md). I think the ideas below will hold, mostly.

With program rewriting, dynamic linking is easy. Even when linking fails, computation may continue around the unknown token. Dynamic linking is advantageous for debugging because we can render a symbolic context after failure. Dynamic linking is convenient for separate compilation because we may load pre-compiled versions of code, and reuse them across multiple computations. So, I believe this is a worthy pursuit.

To support separate compilation and other caching, I may want a two-level linking format:

        {%word} → ResourceID
        ResourceID → Program

This two-level structure has a lot of advantages. First, I can easily guarantee that my program dependencies are acyclic and well-formed. Second, I can cache computations against a stowage ID, for example:

        ResourceID → Type
        ResourceID → LLVM Bitcode
        Type → ResourceIDs (reverse lookup)

Third, I can support flexible structure sharing of program data and cached computations across many versions of the dictionary. This is valuable for supporting DVCS-style development of Wikilon - rapid versioning, forking, merging, etc..

A `ResourceID` is a small representation for a program. It could be achieved by secure hash or by stowage. I'm leaning towards the latter option because it's more compact, simplifies local GC, is easily secured by HMAC, etc.. I could use normal heuristics here, i.e. where smaller resources (under so many bytes) are represented directly instead of indirecting through a lookup table. It shouldn't change the caching behavior, so long as the maximum size for a `ResourceID` is bounded.

Use of *stowage* specifically for resources should simplify development of *reflective applications*, e.g. agents that view a dictionary, i.e. because the `ResourceID → Program` lookup can be implicit... as may be access to much cached computations, assuming a suitable caching model.

To simplify debugging, programs could have tokens embedded that indicate their origin. This could easily be driven by attributes or similar, or just preserve certain debug tokens across many partial evaluations (like gates).

*Aside:* Bidirectional lookup tables between secure hash and resource IDs may be developed later, if necessary.

### Bit Representations

A pointer has spare bits that are convenient for tagging and embedding of values. For my original ABC, these bits were oriented around pairs, sums, and small integers. With minimalist ABC, any focus for tight representation would instead orient around natural numbers, opcodes, tokens, and block embeddings. In both cases, the general escape for 'tagged' objects, allowing me to handle arbitrary special cases (like stowage or texts) is essential.

Candidate representation:

        x01     small natural numbers
        011     other small constants 
        111     common operation codes

        000     block (list of ops)
        010     (unused, deep object)
        100     tagged objects
        110     tagged actions

        (Fast Bit Tests)
        Shallow Copy:   (1 == (x & 1))
        Action Type:    (6 == (6 & x))
        Tagged Item:    (4 == (5 & x))

        (Tagged Items)
        tag uses first byte via (uint8_t*) reference.
        same tag set is used for both
            any tagged object may become a tagged action
            equivalent to 'inlining' said object
            no extra allocation necessary
        special tags for 'small' binaries/texts.
            (smaller, reverse ordered)
        actions include compact bytecode, etc..

        (Small Constants)
        singleton blocks, one for every opcode!
        plus the empty block (null)
        (that's all we need if we include identity opcode)

        (Common Opcodes)
        A very few ABC codes
        Recognized annotations
        An 'undefined' token def 
        A great many accelerators

This seems an efficient and lightweight representation.

### Parallel Runtime

I intend to support **pipeline parallelism**, such that we may flexibly express computations that stream outputs from one task as inputs to another. Parallelism can be indicated by marking a block with `{&par}`, indicating that evaluation should proceed in parallel. Pipelining happens when multiple parallel computations are composed or inlined together. Consider:

        [A]{&par}i  [B]{&par}i

A runtime can dynamically recognize this case during evaluation, then directly attach output from `[A]` to input of `[B]` such that communication is no longer indirected through the parent. Establishing connections dynamically is expressive, enabling pipelines that shrink or grow over time (e.g. because `[]{&par}i` can be removed from a pipeline, while another computation may split into multiple paralle processes).

A runtime may use parallelism even if not indicated. It might be useful, for example, to divide `A [B]{&par}i C` into three parallel computations. However, it is feasible to model some sort of processor affinity groups so sequential bits remain sequential.

See [Parallism.md](Parallelism.md) for more on context-local implementation.

### Program Evaluation

The normal program representation a list of `(op, block)` cons cells, terminating with a singleton block (`[]` is singleton identity). During evaluation, we can use a Huet zipper as a basis to 'open' this list and work within it. This effectively gives us a `(stack, prog)` pair - i.e. the reversed program to our left can be understood as a stack, and the program is operating upon it. The main difference is that our 'stack' might contain actions, not just values. 

It is possible to divide a program into multiple subprograms, each with its own zipper, i.e. to effectively support processing of multiple expressions at once and moving data between them without unnecessary 'scanning' of the program. The ability to evaluate 'wide' in addition to 'deep' can resist getting stuck on any one problem.

Use of `{&par}` essentially enables programmers to control chunking of evaluation, specify the 'interesting' parts of the computation that deserve special focus. Every `{&par}` node would additionally be hooked into a big work pool and pipelines that carry values from one process to another. The "work pool" might be understood as simply a way to attach *very deeply* into within the program, in a thread-safe manner.

*Todo:* I know the above evaluator will work well, but it would be nice to support something closer to a Forth-like stack. At least for cases where I know it can mostly complete. Alternatively, it might be feasible to use a contiguous stack in place of our list-based zipper stacks (maybe a tagged object?).

