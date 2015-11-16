
# Simplifying Awelon Bytecode

Simplifying ABC involves two ongoing challenges: 

1. to discover useful, valid simplification rules
2. to determine a good strategy for applying rules

Simplification rules are behavior-preserving rewrites for our bytecode or a simple editable view thereof. A strategy is important to ensure simplification terminates and accomplishes something useful. Ideally, the simplifier and its strategy should be simple and predictable enough for developers to internalize. The performance of the simplifiers will be relevant, too, because they'll run very frequently.

Minimally, the simplifier should handle basic data plumbing and maths for a `(stack*(hand*ext))` environment. Other ad-hoc environments would be nice, but are less essential at this time.

## Simplification Rules

An aggregation of potentially useful rules.

        BASIC DATA PLUMBING SIMPLIFICATION RULES

        ww = 
        zz = 
        lr = 
        rl = 
        vc = 
        cv = 
        zwz = wzw 
        lzrw = wlzr (lzr doesn't impact first two items)
            (Haven't found a use case for this one.)
            (Similar for llzrrw, llzrrz.)

        (similar for VRWLCZ)

        DELAYED DATA PLUMBING SIMPLIFICATION RULES

            here (a) is a subprogram that generates a value, such as `#42`

        l(a)    =   (a)wzlw
        r(a)    =   (a)wrzw
        w(a)    =   (a)z
        z(a)    =   (a)lzr
        v(a)    =   (a)vr
        c(a)    =   (a)lc

        FLOATING VALUES 

        (a)l(b)w = (b)w(a)l     (via delayed plumbing)
            (a)l(b)w = (a)(b)wzlww = (b)(a)zl = (b)w(a)l
            (b)w(a)l = (b)(a)zl = (b)(a)wwzlww = (a)(b)wzlww = (a)l(b)w

        
        SOME MATH SIMPLIFICATION RULES

        w+ → + (commutativity)
        w* → * (commutativity)

## Simplification Strategy

So far I haven't found any simple, reliable strategy. Most simplification efforts seem to be ad-hoc.

## Simplification Problems

Adding two numbers on the stack.

        (a)l (b)l rwrzw+l
            = (a)l(b) wrzw+l
            = (a)(b)wzlw wrzw+l
            = (b)(a)+l
            = (a+b)l

Stack swap.

        (a)l(b)l rwrwzwlwl
            = (a)(b)wzlwl rwrwzwlwl
            = (b)(a)zlwl  rwrwzwlwl
            = (b)(a)zwzwlwl
            = (b)(a)wzlwl
            = (b)l(a)l

Adding a number on the stack to a number in the hand.

        put = wrzl
        take = rzlw

        (a)zlw (b)l wrzl rwrzw+l

Even simple stack-hand manipulations seem out of reach with the rules I know. However, I *know* that `(a)zlw(b)l = (b)l(a)zlw`. I just don't want to use this as a specific rule, it seems much too specific. Further, this would be enough to solve the problem. Where is my set of simple simplification rules?

        (a)zlw(b)l
            = (a)zl(b)zl
            = (a)z(b)wzlwzl
            = (a)(b)lzrwzlwzl
            ...

        Let's look at this: `lzrwzlwzl`.
        Simplifications here appreciated. 
        
                (b*(a*(s*(h*e))))
        l       ((b*a)*(s*(h*e)))
        z       ((b*a)*(h*(s*e)))
        r       (b*(a*(h*(s*e))))
        w       (a*(b*(h*(s*e))))
        z       (a*(h*(b*(s*e))))
        l       ((a*h)*(b*(s*e)))
        w       (b*((a*h)*(s*e)))
        z       (b*(s*((a*h)*e)))
        l       ((b*s)*((a*h)*e))

        w       ((a*h)*((b*s)*e))
        w       ((b*s)*((a*h)*e))

        Equivalent, internally simple strings:
            lzrwzlwzl
            wlzrzlwzl       (lzrw = wlzr)
            zlwzlw 
            zwlwzlwrwl
            wzlzrzlwzwl

        Common Substrings?
            1: l, z, w
            2: wz, zl, lw
            3: wzl, zlw, lwz
            4(*): wzlw, zlwz, lwzl 
            5(*): wzlwz
            (*) with injections

        Common subprograms for fragments?

            wzlwz   :: (b*(a*(h*(s*e)))) → (b*(s*((a*h)*e)))
                lzrz lwzlwr
                lzrzwlwzlwrw
                zlzrzlwzwlwr

            wzlw :: (b*(a*(h*(s*e)))) → (b*((a*h)*(s*e)))
            wzlw :: ((a*h)*(b*(s*e))) → ((a*h)*((b*s)*e))
            wzlw :: (ah*(b*(s*e))) → (ah*((b*s)*e))
                lzrzlwzlwrz
                lzrwzlzrzlzw
                zlzrwlzrzwlw
                rwlwzlwrwl
                zrwlwzwlwrwl
                rzlzrwzlwzl
                zrzlzrwzwlwzl
                rwzlzrwzlwzwl
                wzwlwrzwrwlwzl

            zlwz :: (a*(b*(h*(s*e)))) → (b*(s*((a*h)*e)))
                lzrwz lwzlwr
                lzrwzwlwzlwrw

            lwzl :: (a*(h*(b*(s*e)))) → ((b*s)*((a*h)*e))
                wlwzlwrwlw
                wlzrwlwzwl
                zlzrwzlwzlw
            
            wzl :: (b*(a*(h*(s*e)))) → ((a*h)*(b*(s*e)))
            wzl :: ((a*h)*(b*(s*e))) → ((b*s)*((a*h)*e))
            wzl :: (ah*(b*(s*e))) → ((b*s)*(ah*e))
                lzrwlzrzl
                lzrwzlzrzlz
                lzrzlwzlwrzw
                zlzrwlzrzwl
                rwlwzlwrwlw
                rzlzrwzlwzlw

            zlw :: (a*(b*(h*(s*e)))) → (b*((a*h)*(s*e)))
            zlw :: (b*((a*h)*(s*e))) → ((a*h)*((b*s)*e))
            zlw :: (b*(ah*(s*e))) → (ah*((b*s)*e))
                lzrwzlwzlwrz
                lzrzlzrzlzw
                wrwlwzlwrwl
                wrzlzrwzlwzl

            lwz :: (a*(h*(b*(s*e)))) → (b*(s*((a*h)*e)))
                wlwzlwrwlwr
                zlzrwzlwzlwr

I'm really not seeing any **simple** simplification rules: nothing that would be trivial for human programmers to internalize, nothing that would prove generic to alternative multi-stack environments. This suggests our stack-hand environment needs an alternative to string rewriting. An alternative might be to construct some ad-hoc collection of 'unary' and 'constant' operation on an initial environment, together with accumulating data plumbing for the future? Arrow optimizations, of some sort?

        (first (a→b))z = z(first (a→b))
        (second (a→b))z = z(third (a→b))
        ((a→b) *** (c→d))l = l(first ((a→b) *** (c→d)))

We could understand data-plumbing as routing for operations on an environment. The details seem rather challenging, but this path could be pretty powerful if pursued because all `(a)l` and such would qualify unary operations of some kind.

## Useless Simplifications?

        lzrz :: (a*(b*(c*(d*e)))) → (a*(d*(b*(c*e)))
            zlzrzlzr
            
        zlzr :: (a*(d*(b*(c*e))) → (a*(b*(c*(d*e))))
            lzrzlzrz

