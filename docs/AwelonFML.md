
# Awelon with First-Class Modules

Awelon is currently a very simplistic language with four primitive combinators. This gives the language a very dynamic feel. Many programs can be statically typed, but it requires a lot of non-local knowledge. I'd like to consider a variation of Awelon with some nicer static reasoning properties:

* unify values and modules, per [Rossberg's 1ML](https://people.mpi-sws.org/~rossberg/1ml/1ml.pdf)
* dependent types so we can express rich theories
* advanced effects model, such as [Frank's Do Be Do Be Do](https://arxiv.org/pdf/1611.09259.pdf)

If we start with a richer language, we can go a lot further with Awelon. For example, it becomes feasible to use Awelon as a systems programming language (without relying too much on accelerators).

Awelon's current language has some nice properties: point-free, concatenative, deterministic, simple syntax, local rewriting. Those latter three properties - deterministic, simple syntax, local rewriting - are the essential properties for Awelon's goals of unifying PX and UX. The simple syntax enables us to project code into a graphical interface, determinism enables sharing code change in meaning, and local rewriting allows projection of the results. Point-free and concatenative are convenient for streaming a program, but are not essential.

So the goal with this Awelon variant is to preserve those essential properties, while also enabling types and effects.

## Modules?

The basic module interface is described by:

* a collection of associated types
* a collection of operations on them

Types and operations are identified symbolically. Description of the types and interfaces might be a result of integrating other interfaces - a type also has a module interface. We can instantiate an interface, or partially instantiate one.

## Module Rewriting?

In the original Awelon, all values are functions. Functions are convenient because the only way to observe a function is to apply it, and the only way to construct a function is to bind an argument. Thus, we only need combinators `a` and `b` to model observations and constructions. Besides these, we had extra combinators for data plumbing.

With modules, what kinds of observations and constructors do we require?







For constructors, we might:

* parameterize a module
* abstract a module
* 
 wish to instantiate a module with a given type.






 so we have primitive combinators to observe and construct functions, in addition to combinators for basic data plumbing. 

If all values are modules, then we'll instead want combinators for observing and constructing modules.

 and vice versa, then what does it mean to rewrite them?




There are other approaches, such as use of pattern-match rules. In that case, we might follow 'order' of patterns to achieve determinism. However, pattern matching is a little awkward in general: we need to know a lot about representations or views of the arguments. I think with modules, I'd prefer to follow Awelon's original example: simple confluent combinators.

With functions, we observe a function by applying it, or we may construct a function by binding an argument. These correspond to Awelon's `a` and `b` primitive combinators, respectively. Besides these, we can copy or drop a function.

With modules, observations and constructors are much richer, we'll need more combinators.




* to use one module to parameterize another
* to use one module to instantiate another
* to compose modules, unifying some shared types by name
* to abstract a module, forgetting some details
* 



## First Class Modules

Rossberg's 1ML proposes to 


## Effects Model



