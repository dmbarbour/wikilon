
# Awelon's Application Model

In Awelon, user-interfaces are modeled as editable projections over an environment. Back-end processing is modeled by sharing this environment with bots. Bots are modeled as transactions that repeat indefinitely, which gives provides an excellent nice basis for concurrency control, process coordination, and live coding. Effects are achieved by special 'system' bots that can process a task queue, interact with the network, etc..

All of this gives some convenient system properties. For example, we can allow users to operatate through multiple projections before committing, and we could reactively update our projections in scope of our transaction that has not committed. We can still support conventional GUIs with buttons that perform macro-edits. We also get some nice spreadsheet-like properties and live coding when manipulating our dictionaries.

This document is brainstorming how to build some useful things.

## Multi-Line REPLs and Command Shells?

A REPL experience usually has each line of code followed by rendering a result, with some connectivity to prior lines. Let's assume line breaks are command separators implicitly ending in `return`.

        X [Y return [Z] cseq] cseq
        == X; Y return; Z
        == X; Y
           Z

This allows us to freely mix pure and impure lines of code, and we can effectively treat semicolons as 'execute' statements internally. The remaining issue is how to work with deal with *incremental* programs. The simplest option is that we rebuild our view from scratch - this works well enough for a purely functional computation, though it might hinder caching. A more sophisticated option, perhaps, is to introduce holes for incremental processing of our program up to some undefined continuation.

## Deterministic Concurrent Programming?

I'm very interested in observably deterministic concurrency.

The most direct approach is perhaps single-assignment variables. However, I'm not fond of variables because it's too difficult to manage them (explicit deletion doesn't fit single-assignment, garbage-collection doesn't fit Awelon, and there's some weirdness with results containing variables). Let's exclude that for now.

The other option I see is *Kahn Process Networks*. We could model this using monadic computations with two effects: read from input port, write to output port. The ports, in this case, could have simple labels (strings) for names. 

        




Kahn Process Networks are essentially the most general

The easiest way to support deterministic concurrency is via stream processing:

* each 'process' has a set of input and output streams
* a process can bind outputs from one process as inputs to another 
* 

To support concurrency, my simplest option is to model monadic threads or coroutines. This can be trivially supported as an effectful API above monadic command sequences. We could create, read, update, and delete variables. Deterministic concurrency (where result is independent of schedule), however, is rather difficult because it requires all "effects" are commutative.

Nonetheless, if we do have a suitable API, it would be feasible  be feasible t





 It seems difficult to enforce this without linear types for variables or channels.



 and would depend heavily on our communications model.



Observable determinism (independent of scheduler) is feasible if we carefully design our APIs, e.g. with linear variables or channels. But

 but it might not be worthwhile. 

arranging them around linear-writer channels or single-assignment variables.




 with explicit copy and reference counting.

With monads, observable determinism is feasible and would depend on our effects API. This isn't a bad thing, since it allows us to develop concurrency models at the API layer (rather than a syntactic layer). 


This works, but gives us a highly non-deterministic model, insofar as the behavior of the system depends on an external scheduler. I'd very much like to have easy access to concurrency with determinism, instead.

Deterministic concurrency with shared variables is feasible if we restrict how variables are used.

 Perhaps the simplest option is linear write-once read-once variables for promise pipelining, or a variant that allows for explicit duplication.

        valloc : m (wv a * m a)
        vbind : (a -> m b) -> rv a -> rv b
        vcopy : rv a -> m (v a * v a)
        vdrop : v a -> m ()
        vwrite : a -> 
        vdrop : v a -> m ()
        vcopy : v a -> (v a * v a)
        vwrite : v a -> a -> m ()
        read : v a -> m a
          



, e.g. single-assignment and single-observer variables. 

Kahn Process Networks are closer to the ideal here, but it's unclear how to represent KPNs to guarantee the channel structure for determinism. However, let's say our processes are similar: each process accepts a list of input channels and deterministically produces a list of output channels.




We could certainly do this, but then our behavior depends on our explicit scheduler, which does not easily allow for a high level of parallelism. What I want are concurrent, parallel, computations that do not depend on scheduling, and that support a reasonable level of internal buffering.




## Binding Live Data

Bots can scour the network and copy data into a dictionary or filesystem.  this wouldn't effectively leverage Awelon's binary resources. Alternatively, bots could subscribe to dictionary packages, and maintain them, which would let us more easily take advantage of structure sharing and lazy downloads.



## Spreadsheets and Worksheets

We could project a volume of our dictionary `foo-*` as a spreadsheet, with `foo-a2` and `foo-b3` representing cells at the appropriate locations. We can locally evaluate each cell. This would allow us to render a sheet of cells. Internally, we might use `$a2` to name a local cell within the spreadsheet. Potential weakness here: copy-paste rewriting of local cells is awkward.

A worksheet is like a spreadsheet except we use ad-hoc 'variable' names instead of cells. Each variable becomes a definition `foo-var1`, `foo-var2`, etc.. We could use a shorthand internally, perhaps just `var1`.

That said, with *Named Local Variables*, we don't really need worksheets as multi-definition objects. We can just do them within one definition that requires no arguments. This might prove more convenient.



