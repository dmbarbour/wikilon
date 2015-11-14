
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
        zwz = wzw (or vice versa)
        lzrw = wlzr (since `lzr` doesn't impact first two items)
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

So far I haven't found a reliable strategy for most problems. Equality saturation would probably work for most things, though is rather expensive with so many equivalent permutations, and would probably be too sophisticated to easily internalize.


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
            = (a)(b)wz wzwlwl
            = (a)(b)ww zwwlwl
            = (b)(a)wzlwl
            = (b)l(a)l

Adding a number on the stack to a number in the hand.

        put = wrzl
        take = rzlw

        (a)zlw (b)l wrzl rwrzw+l

The simplest stack-hand manipulations seem out of easy reach with the rules I have. However, I *know* that `(a)zlw(b)l = (b)l(a)zlw`, and that would be enough to solve this problem. Can I find a simpler set of rules that produce his effect?


