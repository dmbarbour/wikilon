
# Simplifying Awelon Bytecode

I want enough simplification rules to cover frequent use cases for partial evaluations, especially operating within a `(stack*(hand*ext))` environment or other multi-stack environments. This document is more brainstorming than specification, since I'm still figuring out how to simplify ABC.

Some obvious simplifications:

        ww → 
        zz → 
        lr → 
        rl → 
        vc → 
        cv → 
        zwz → wzw
        (similar for VRWLCZ)

        w+ → + (commutative)
        w* → * (commutative)
        #0 → #
        

These actually help a lot for common data plumbing. I've seen considerable improvements. But I'll need something more to properly handle stack manipulations. Let's consider the following, with `(a)` representing a subprogram that constructs a value (e.g. number, text, block, pairs thereof).

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

Moving content between stack and hand, e.g.:

        (b)zlw (a)l wrzl
            should simplify to
        (a)l(b)l

Let's consider delayed dataplumbing, i.e. where we shift plumbing until after a value is introduced.

        l(a)    =   (a)wzlw
        r(a)    =   (a)wrzw
        w(a)    =   (a)z
        z(a)    =   (a)lzr
        v(a)    =   (a)vr
        c(a)    =   (a)lc

Taking this to derive one useful rule:

        (a)l(b)w    =   (a)(b)wzlww
                    =   (a)(b)wzl
                    =   (b)(a)zl
                    =   (b)w(a)l

        (b)w(a)l    =   (b)(a)zl
                    =   (b)(a)wwzl
                    =   (a)(b)wzl
                    =   (a)(b)wzlww
                    =   (a)l(b)w

Nice. Though the second option is a lot more difficult to derive because it involves two `ww` injections. Okay, what about our stack manipulations?

        (a)l(b)l rwrwzwlwl
            = (a)l(b) wrwzwlwl
            = (a)(b) wzlw wrwzwlwl
            = (a)(b) wz wzwlwl
            = (a)(b) w zwz wlwl
            = (a)(b) w wzw wlwl
            reducing ww            
                = (a)(b) zlwl
                = (a)w(b)lwl ???
            reducing (a)(b)w
                = (b)(a) wzlwl
                = (b)l(a)l

So, this one's a bit tricky. There are several reduction options, not all of them useful. Moving content `(b)wzlw → l(b)` is certainly optional. More generally, I should probably recognize `(a)(b)zlw` as `(b)(a)wzlw` as `(b)l(a)`. Also, `(a)w(b)lw` might frequently benefit from pushing the inner `w` to the right.








        (a)l(b)w    
            = (b)w(a)l    
            = (b)(a)zl  
            = (a)(b)wzl

        (a)l(b)                         (s*e) → ((a*s)*e) → (b * ((a*s)*e)
            = (a)(b)(???)
        
        l(b)                if I have an easy answer here, it might help a lot
            = (b)(???)

        l(b) = (b)


Also, while we have `w` and `l` as special cases for rewriting stacks, what about `r`?

        (a,b)r
            = (b)(a)lr
            = (b)(a)



But is there an easy way to compute this property?

        (a)l(b)zlw  =   (a)lw(b)lw  = ???


Assuming I'm able to recognize and simplify these cases, I should be able to perform a lot more partial evaluation without translating through an intermediate language, like lambda calculus. Granted, I have the *option* of translating through lambda calculus, but rewriting the ABC directly should be simpler to use and validate, and likely higher performance.

A possible technique is to also capture 'value manipulators'. Instead of just `(a)`, maybe I can recognize subprograms of type `(a→b)` or operations of even higher arity. In some cases, I gain some obvious translations like: `(a→b)z ⇒ z(a→b)`. 

Alternatively, perhaps our simplifier can explicitly recognize stack captures? and perhaps the swap pattern captures? This might work pretty well, assuming I find a sufficient set of rewrites to cover the common patterns (and define common patterns for easy simplifications).


