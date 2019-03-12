# Generic Programming in Awelon

Generic programming is about writing programs in terms of requirements, the abstraction and deferral of non-essential decisions such as data types, sorting algorithms, dependencies, configuration, and extensions. Ideally, this should be achieved without significant overheads for syntax or performance.

Awelon directly supports parametric polymorphism, which is a useful but limited form of generic programming. Advanced forms of generic programming will require explicit modeling and suitable projections. Unfortunately, popular approaches like multi-methods or type-classes rely on a global registry or type system, and are awkward in Awelon.

I have some vague ideas involving a monadic constraint system. 

Constraints are already close to generic programming: a constraint will express or refine a requirement. The difficulty is gathering requirements, efficient search, and constructing a result.

The monadic API can be convenient for abstracting and composing our constraints. Unfortunately, it is somewhat awkward to 'defer' choice based on downstream requirements in the monadic API. This means we either must front-load our requirements (which is awkward) or find an alternative to true deferal (such as iterative search).





somewhat awkward because we cannot conveniently modify any decisions after we observe them - doing so would result in some form of paradox. 

 due to committed choice: we must be careful about where we *observe* 

constraints, but is a little bit awkward: downstream requirements cannot easily affect prior decisions. 



I'm hoping that monads could help with this. If constraints are expressed within a monad

With monads, a major challenge is that we cannot easily have 'downstream' requirements influence 'upstream' decisions insofar as we observe dependencies between the two. 

A first thought is that I should pursue something 'weaker' than monads, such as applicatives, so we can examine the entire  before making decisions. However

Applicative or Selective style might be a better fit, allowing us to examine the entire computation before making any decisions. 






Ideas: 

* single-assignment variables combined with a search model
* answer-set programming - variables are not independent
* attaching tactical solutions to a variable (or set thereof)
* option to 'fail'

How would we support extensions, too? I suppose it could be encoded by building a channel using constraint variables and prioritized selection. Intriguingly, this constraint system could enable a high level of parallel computation insofar as different parts are reading or writing.

        type GX e a -- monadic, with error type 'e'
        try : GX e a -> GX e' (Either e a)
        fail : e -> GX e a
        return : a -> GX e a

This design is sufficient to give us the ability to try


        





The monad provides an API to read and write an ad-hoc registry while computing a value, and also some APIs for failure and fallback, e.g. an exception-safe `try` that rolls back writes upon error. This would be sufficient to model lightweight forms of search, for example. This monad can perform ad-hoc computations, and would return the final result and registry.



We could perform computations 

 Further, the monad may support failure and fallback behavior, like a try-catch, allowing for program search. Further, the monad may support a "scoring" mechanism


This would allow us to construct collections and options

 while computing a value, and perhaps support ad-hoc alternatives and partial failures. 






A viable idea for generic programming is to develop a monadic API for a lightweight constraint programming model. We would simultaneously declare constraint variables and add options to them, while also computing a value. 

 our options, while computing a value. This would 

with inspiration from declar constraint systems. We could declare constraint variables, the


 The monad would allow us to construct a registry and a program at the same time.

could declare various rules: how to build a set of resources given another set. 

- how to build a resource given some other resources, and which new rules become available and requirements (whi







In many cases, such as multi-methods, we must model a registry of instances. To fit Awelon's overall design vision, this registry must be *local* to a program. 

 (for caching, control, etc.) this should not be a global registry - reject a global `mm-registry` for multi-methods. Indeed, it's best if the registry is local to each program definition. 

 of the generic programming system. If we intend to simulate typeclasses or multi-methods, we must model a registry of instances.

 classes and methods.

It is convenient to represent registration of resources monadically, mixed together with specification of the program. Intriguingly, we could generalize monadic mechanisms to support dependency-injection or configuration. With support for alternatives and program search, it would also be convenient for rapid prototyping.

This is a direction I'd very much like to pursue for Awelon, long term. We may need to build upon projections to make it usable.

