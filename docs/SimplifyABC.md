
# Simplifying Awelon Bytecode

I want enough simplification rules to cover frequent use cases for partial evaluations, especially operating within a `(stack*(hand*ext))` environment or other multi-stack environments. This document is more brainstorming than specification, since I'm still figuring out how to simplify ABC.

Some obvious simplifications for data plumbing:

        ww → 
        zz → 
        lr → 
        rl → 
        vc → 
        cv → 
        zwz → wzw
        (similar for VRWLCZ)

These actually help a lot for common data plumbing. I've seen considerable reductions. But I'll need something more to properly handle stack manipulations. Let's consider the following, with `(a)` representing a subprogram that constructs a value...

        (a)(b)w → (b)(a)
        (a)z → w(a)
        (a)(b)+ → (a+b)

Well, the question then is how well we can take code for a multi-stack environment and optimize data interactions that are further apart. Some use cases:

Swapping and rotating stack content:

        (a)l(b)l rwrwzwlwl
            should simplify to
        (b)l(a)l

        (a)l(b)l(c)l rwrwrwzwlzwlwl
            should simplify to
        (b)l(c)l(a)l
        
Adding two numbers from our stack: 

        (a)l (b)l rwrzw+l
            should simplify to 
        (a+b)l

Simplify stack and hand writers:

        (c)l(a)zlw(d)l(b)zlw
            should simplify to
        w(a)l(b)lw(c)l(d)l

Swapping the stack and hand:

        w(a)l(b)lw(c)l(d)l  w
            should simplify to
        w(c)l(d)lw(a)l(b)l

Moving content between stack and hand:

        (b)zlw(a)l wrzl
            should simplify to
        (a)l(b)l

Assuming I'm able to recognize and simplify these cases, I should be able to perform a lot more partial evaluation (without translating through an intermediate language, like lambda calculus). Granted, I always have the *option* of translating through lambda calculus, but rewriting the ABC directly is simpler to implement and use, and likely higher performance.

A possible technique is to also capture 'value manipulators'. Instead of just `(a)`, maybe I can recognize subprograms of type `(a→b)` or operations of even higher arity. In some cases, I gain some obvious translations like: `(a→b)z ⇒ z(a→b)`. 

Alternatively, perhaps our simplifier can explicitly recognize stack captures? and perhaps the swap pattern captures? This might work pretty well, assuming I find a sufficient set of rewrites to cover the common patterns.


