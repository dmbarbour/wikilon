
# Simplifying Awelon Bytecode

Simplifying ABC involves two ongoing challenges: 

1. to discover useful, valid simplification rules
2. to determine a good strategy for applying rules

Simplification rules are behavior-preserving rewrites for our bytecode or a simple editable view thereof. A strategy is important to ensure simplification terminates and accomplishes something useful. Ideally, the simplifier itself should be simple and predictable enough for developers to internalize. The performance of the simplifiers will be relevant, too, because they'll run very frequently.



 that is both efficient and reasonably complete, where by 'complete' I mean that we can write normal code with an expectation that it is fully partially evaluated, but it's okay that we might also stymie our simplifier if we try.

Minimally, the simplifier should handle basic data plumbing and maths for a `(stack*(hand*ext))` environment. Other ad-hoc environments would be nice, but are less essential at this time.

Some obvious simplifications:

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

        SOME MATH SIMPLIFICATION RULES

        w+ → + (commutativity)
        w* → * (commutativity)
        #0 → #

There are probably many more. It might be useful to construct an exhaustive list of length-up-to-K ABC data plumbing strings that are equivalent and are not reduced by smaller simplification rules. I could probably develop a logic program to do this.

I'll need something more to properly handle stack manipulations. Let's consider the following, with `(a)` representing a subprogram that constructs a value (e.g. number, text, block, pairs thereof).

        (a)z → w(a)
        (a)(b)+ → (add a b)
        (a)(b)w → (b)(a)
        (a)(b)l → (b,a)
        (b,a)r → (a)(b)

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

Let's consider delayed data plumbing, i.e. where we shift plumbing until after a value is introduced.

        DELAYED DATA PLUMBING SIMPLIFICATION RULES

        l(a)    =   (a)wzlw
        r(a)    =   (a)wrzw
        w(a)    =   (a)z
        z(a)    =   (a)lzr
        v(a)    =   (a)vr
        c(a)    =   (a)lc

Taking this to derive another useful rule:

        (a)l(b)w    =   (a)(b)wzlww
                    =   (a)(b)wzl
                    =   (b)(a)zl
                    =   (b)w(a)l

        (b)w(a)l    =   (b)(a)zl
                    =   (b)(a)wwzl
                    =   (a)(b)wzl
                    =   (a)(b)wzlww
                    =   (a)l(b)w

Nice. I could probably do a lot of data plumbing with just this rule, e.g. representing a two-stack system consisting of one stack of type `w` and one stack of type `l`.

        (stack swap using latent data plumbing)

        (a)l(b)l rwrwzwlwl
            = (a)l(b) wrwzwlwl
            = (a)(b) wzlw wrwzwlwl
            = (b)(a) zlw wrwzwlwl
            = (b)(a) z wzwlwl       (elim `lwwr`)
            = (b)(a) wzwwlwl        (zwz → wzw)
            = (b)(a) wzlwl
            = (b)l(a)l

        (stack swap directly using floating (a)l(b)w=(b)w(a)l)

        (a)l(b)l rwrwzwlwl
            = (a)l(b) wrwzwlwl
            = (a)l(b)w rwzwlwl
            = (b)w(a)l rwzwlwl
            = (b)w(a)w   zwlwl
            = (b)(a)zw   zwlwl
            = (b)(a)wz   wwlwl
            = (b)(a)wz     lwl
            = (b)(a)wzlw l
            = (b)l(a)l

Oh, well it didn't really save any work. But maybe it will help with larger samples? Efficient simplification is going to be very important, because it's largely going to be substituting for evaluation in Wikilon (i.e. simplifying code performs most evaluation for dictionary objects).


Let's try a simple strategy of shifting all data plumbing to the right, performing `w` flips where feasible, then shifting it back left. The main disadvantage here is that we're rapidly increasing the amount of data plumbing. For a single stack swap:
        
        (a)l(b)l rwrwzwlwl
            = (a)(b)wzlwlrwrwzwlwl
            = (b)(a)zlwlrwrwzwlwl
            = (b)(a)zwzwlwl
            = (b)(a)wzwwlwl
            = (a)(b)zwwlwl
            = (a)(b)zlwl   (special case for `ww` injection)
            = (a)(b)wwzlwl
            = (b)(a)wzlwl
            = (b)l(a)l

For a full stack rotation:

        (a)l(b)l(c)l rwrwrwzwlzwlwl
            should simplify to (b)l(c)l(a)l
            = (a)(b)wzlwl(c)l rwrwrwzwlzwlwl
            = (b)(a)zl(c)zwzlwl rwrwrwzwlzwlwl
            = (b)(a)zl(c)wz rwzwlzwlwl
            = (b)(a)(c)lzrwzlz rwzwlzwlwl
            = (b)(c,a)  zrwzlz rwzwlzwlwl
            = (b)w(c,a)  rwzlz rwzwlzwlwl
            = (b)w(a)(c)  wzlz rwzwlzwlwl
            = (b)w(c)(a)   zlz rwzwlzwlwl
            = (b)w(c)w(a)   lzr wzwlzwlwl
            = (b)w(c)wz(a) wzw lzwlwl
            = (b)w(c)wz(a) zwz lzwlwl
            = (b)w(c)wzw(a) wz lzwlwl
            = (b)w(c)wzw(a) wz lwzwzlwl
            = (b)w(c)wzw(a) wzlw zwzlwl
            = (b)w(c)wzwl(a)     zwzlwl
            = (b)(c)zwzwl(a)     zwzlwl
            = (b)(c)wzl(a)  zwzlwl
            = (b)(c)wzlw(a)  wzlwl
            = (b)l(c)(a) wzlw l
            = (b)l(c)l(a)l

Yikes. I'm not sure how I'd go about efficiently automating this reduction. Compare the 'floating `w` and `l` stacks?

        (a)l(b)l(c)l rwrwrwzwlzwlwl
            should simplify to (b)l(c)l(a)l
            = (a)l(b)l(c)w rwrwzwlzwlwl
            = (c)w(a)l(b)l rwrwzwlzwlwl
            = (c)w(a)l(b)w   rwzwlzwlwl
            = (c)w(a)l(b)w   rwzwlzwlwl
            = (c)w(b)w(a)     wzwlzwlwl
            = (c)w(b)w(a)     zwzlzwlwl
            = (c)w(b)w(a)     zwz lzwlwl
            = (c)w(b)w(a)     zwz lzwlwl
            = (c)w(b)ww(a)     wz lzwlwl
            = (c)w(a)(b)        z lzwlwl
            = (c)w(a)w(b)l         zwlwl 
            = (b)l(c)w(a)w         zwlwl
            = (b)l(c)w(a)z         wzlwl
            = (b)l(c)ww(a)         wzlwl
            = (b)l(c)(a)wzlwl
            = (b)l(c)l(a)l

This seems a bit simpler, but there are still some tricky steps to recognize. And, I'm not especially interested in depending on a simplification strategy that works only for a two-stack environment.

Let's try another sample: math

        (a)l (b)l rwrzw+l
            should simplify to (a+b)l
            = (a)l (b)l rwrzw+l
            = (a)l (b)   wrzw+l
            = (a)(b)+l
            = (a+b)l

Stack and hand operations? 

        put = wrzl
        take = rzlw

        (a)l(b)zlw
            should simplify to (b)zlw(a)l as needed
            = (a)(b)wzlwzlw
            = (b)(a)zlwzlw
            ...
            = (a)lw(b)lw
            ...
            = (a)l(b)w wzlw (ww injection)
            = (b)w(a)l wzlw
            = (b)(a)zlwzlw
            ...

With the simplification rules I know so far, even the simplest stack-hand manipulations seem out of easy reach. I believe I need to discover more simplification rules. Something especially for `(a)l(b)zlw`. I know the `(a)l` and `(b)zlw` operations can float past each other freely, but this doesn't appear obvious to my simplifier.

I don't want a bunch of specialized 'value crossing' rules in my simplifier, except as an obvious optimization of what could be achieved with other rules already in the simplier.


        

 Perhaps `(a)lw → w(a)l` would be valid? 

        (a)lw :: (s*(h*e)) → ((a*s)*(h*e)) → (h*((a*s)*e))
        w(a)l :: (s*(h*e)) → (h*(s*e)) → ((a*h)*(s*e))      NOPE!

Well, maybe something like this then. Something based on the idea that we move the implicit stacks around as part of maintaining multiple stacks, so we can also work in the reverse direction.