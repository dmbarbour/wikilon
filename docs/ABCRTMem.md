
# Memory in Wikilon Runtime

This is an issue I've revisited many times, so I've decided to make a list of conclusions and reasons:

## Memory Layout

Each context will have one contiguous block of memory, page-aligned.

        [(context)..(memory).....]

Our memory region is divided into a context header and the main memory region. The goal here is to simplify both offset and absolute addressing within context memory. 

I've decided to exclude a built-in semispace. So compacting collection is no longer the default. It will instead be supported indirectly, with responsibility shifted to the runtime API client. Moving a computation into another context will effectively defragment that computation and enable resize. Maintaining a small pool of contexts for defragmentation is a lot more space-efficient than having an empty semi-space for *every* context, especially if defrag proves a rare event.

## Word Sizes

In favor of 64-bit:

* roughly 4% performance boost from direct addressing
* direct scaling to massive, multi-gigabyte contexts

In favor of 32-bit:

* requires half as much memory for common structures
* Wikilon won't be allocating multi-gigabyte contexts

I believe that 32-bit is a better fit for Wikilon's common use case: multiple contexts each with smaller 'working' memories, each servicing separate web client or background computation goals. Scalability to very large computations will be addressed instead by value stowage.

However, I'll aim to ensure this is a simple compiler switch.

## Bit Representations

I assume allocations in context will be aligned to at least 8 bytes. This gives 3 flag bits per reference to provide a lightweight indicator of data type. 

This is the primary candidate at this time:

        x01     small natural numbers
        011     other small constants 
        111     common action codes

        000     cons cell (2 words)
        010     (unused, reserved)
        100     tagged objects
        110     tagged actions

        (Fast Bit Tests)
        Shallow Copy:   (1 == (x & 1))
        Action Type:    (6 == (6 & x))
        Tagged Item:    (4 == (5 & x))

The current candidate oriented towards [minimalist ABC](ABC_Minimalist.md). In this case, a cons cell is the common representation for a block or stack. An 'object' represents a value that can be bound, moved, or applied. An 'action' cannot be moved, and requires inputs, but is still first-class in context of rewriting as a basis for evaluation. For tagged objects and actions, the first byte (taking a `uint8_t*` pointer) offers more information about type.

The `010` tag is currently unused. It *might* be used as 'null' value to enable fail-safe traversals. OTOH, that's only necessary if I can fail during copy. If I use two passes on copy - the first to compute size - then I should not need to handle allocation failure upon deep-copy. In that case, it might see use for common tagged objects.

## In-Place Traversals

As an additional constraint on data structure, I will require that copy, drop, and traversal of large structured objects may be performed non-destructively and with constant extra space. Doing so will simplify a lot of problems: efficient copy and drop, stowage, serialization, etc..

The Morris algorithm could be adapted. However, tree structures have a common `null` value on the RHS to serve in place of the stack, and a relatively simple structure so we don't struggle to navigate *within* nodes (e.g. if a node has 8 children, that might be a bit troublesome).

To achieve this, all list structures shall end in `null`. We might use a smart constructor such that if we would construct a list terminating in a tagged object or small constant, we'll instead allocate it. Further, there will be restrictions on structure of tagged objects. (Though, it is possible for a tagged object to provide its own in-place traversal iterator.)

## Fail-Safe Allocation

When memory allocation fails, our context should be left in a valid state, such that we may easily continue computation or serialize it. This greatly simplifies multi-threaded computations. To achieve this:

* intermediate, continuable evaluation state is represented directly
* update only from one intermediate, continuable state to another

## Owned and Shared Objects and Fragments

By default, objects in memory are uniquely 'owned' and permit destructive operations. This is convenient for parallelism, even when most objects are simple representations of immutable values. Shared objects, however, are also essential for performance (especially in loopy code) and must not be neglected.

Ideally, sharing is easily predicted, understood, debugged, and recovered between serializations. So I'd like to utilize a simple rule: *shared objects use tokens for serialization and linking*. Recovery of token-based sharing is performed easily at parse-time, and it is easy for developers to look at bytecode containing tokens and understand precisely where structure sharing is possible.

This is, however, a severe restriction for shared objects at the runtime level. Unless I use tokens to name objects local to a context's scope, I won't be sharing at that layer. Most likely, sharing shall involve either our AO dictionary (which might be associated with a context) or value stowage, neither of which are context-specific.

An interesting possibility with 'shared objects' is the ability to share just fragments of an object. This seems feasible, e.g. if our resource ID for stowage tokens may include not just an object but also a fragment type and byte-range.

## Multi-Threaded Context

A Wikilon runtime context will accept a single API 'main' thread. It performs no locking for main-thread access. Additional 'worker' threads will be possible. Anyhow, my goal is multiple threading with minimal synchronization by the main thread. No reference counts. Minimal use of mutexes or waits. A read-write lock for compacting GC is a likely special, rare case for synchronization.

I have not yet decided whether or not to externalize the worker thread pool, i.e. providing a special API method for workers. Having a pool of worker-threads at the environment level could be convenient for performance, and CPU distribution might be managed via effort quotas.

## Memory Management

The explicit copy/drop of ABC simplifies memory management. Effectively, I have manual memory management as the baseline, especially with the above limitations on shared objects.

I could use multiple collectors: 

* rare compacting collection (uses semi-space)
* common free-list management (via explicit 'drop')

When a value is dropped, I'll generally have some opportunity to push it into a stack of recently dropped values. I can defer the bulk of memory recovery until more memory is needed, at which point I may batch process the entire stack. This *should* be simplified by in-place traversals, i.e. I can easily destructure objects without an auxilliary stack.

Compacting collection could be performed when fragmentation is high. This might be recognized heuristically by dropping 'small' fragments from my free-lists, or based on counting bytes allocated vs. left unused. 

I might shift compaction to the API level instead, i.e. model compaction by moving our program from one context to another. If I make this an explicit evaluation status, I could make it a more formal and well documented concept of my API.

