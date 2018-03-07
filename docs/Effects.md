# Awelon Effects Model

Awelon language is purely functional at the moment, and requires explicit modeling of effects, e.g. monadically or with [KPNs](KPN_Effects.md) or alternative [application models](ApplicationModel.md).

A modern movement in FP is towards algebraic effects and user-defined handlers. The Frank language paper [Do Be Do Be Do](https://arxiv.org/pdf/1611.09259.pdf) seems especially relevant, with the support for *multihandlers* that could feasibly handle multiple effects concurrently. I'm still studying this possibility.

Naturally, if an effects model can easily be modeled functionally and accelerated, I should simply do. This passes the buck to the runtime environment rather than complicating Awelon semantics. Imperative style effects, for example, can be modeled monadically. And when I examined [KPNs in Awelon](AwelonKPN.md) I ultimately decided that acceleration is the right path there, too.

## Requirements

If I have a built-in effects model, it must be:

* simple - ideally just one or two more primitive combinators
* serializable - we can render, save, and restore the effects
* composable - restrict, rewrite, filter, and integrate effects
* concurrent - difficult to model with pure functions

I've observed that any effect model that uses `new` as in `newIORef` or `new Object()` is problematic for serialization. But we can safely use resource discovery idioms, e.g. closer to working with a filesystem.

## Maybe Reactive Demand Programming? No.

I developed a Reactive Demand Programming (RDP) model circa 2011, essentially an attempt to model "declarative" effects close in nature to functional programming. It is promising, a reasonably close fit for my requirements.

* A computation emits a *set of demands* upon its environment. 
* Each demand receives reactive feedback - a time-varying value. 
* Demands are idempotent. Duplicates have no extra effect, same feedback.
* Demands are commutative. Order of demands is never observed.
* Demands are reactive. Demands may depend on prior feedback.
* State is external. The program only wires up computations.

If we insist on RDP-based effect primitives, it initially seems feasible:
        
        [Demand]e => [Feedback]
        [Program][Handler]f => [(rewrites `e` to `Handler` in Program)]
            with Handler : ∀S.S [Demand] → S [Feedback]

Essentially, `e` emits an effect - a demand value - while `f` can process demands and adapt an RDP subprogram to an alternative effects context. 

Unfortunately, there is no means to integrate effects or simulate a partial context. This seems sadly incomplete. I don't believe we can generally support integration or simulation without knowing a lot more about commutativity of functions, such that we may safely observe sets. RDP will be difficult to accelerate for the same reason. 

We could acclerate the more limited form that does not integrate concurrent effects, based on `e` and `f` above. We simply model "demand generators" that yield demands and accept feedback on the continuation. The placeholders techniques described for [KPNs in Awelon](AwelonKPN.md) seem applicable for optimizing stable routes and supporting efficient reactive feedback.


