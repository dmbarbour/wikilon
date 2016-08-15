
See AboutABC for full explanations and design. This file just records each code, and a pseudocode representation of its type.

## Operators and Literals

        [] :: (special - block literal)
        "~ :: (special - text literal)
        {} :: (special - token or capability; see below)
        (|) :: (reserved)
        SP,LF :: x → x (whitespace as identity)

        l :: (a * (b * c)) → ((a * b) * c)
        r :: ((a * b) * c) → (a * (b * c))
        w :: (a * (b * c)) → (b * (a * c))
        z :: (a * (b * (c * d))) → (a * (c * (b * d)))
        v :: a → (a * 1)
        c :: (a * 1) → a
        % :: (Droppable x) ⇒ (x * e) → e
        ^ :: (Copyable x) ⇒ (x * e) → (x * (x * e))

        $ :: [x→x'] * (x * e) → (x' * e)
        m :: [x→y] * ([y→z] * e) → [x→z] * e
        ' :: (Quotable x) ⇒ (x * e) → ([s→(x*s)] * e)
        k :: ([x→y] * e) → ([x→y]k * e) -- keep, relevant, no drop
        f :: ([x→y] * e) → ([x→y]f * e) -- affine, no copy

        # :: e → (N(0) * e)
        0 :: N(x) * e → N(10x+0) * e
        1 :: N(x) * e → N(10x+1) * e
        2 :: N(x) * e → N(10x+2) * e
        3 :: N(x) * e → N(10x+3) * e
        4 :: N(x) * e → N(10x+4) * e
        5 :: N(x) * e → N(10x+5) * e
        6 :: N(x) * e → N(10x+6) * e
        7 :: N(x) * e → N(10x+7) * e
        8 :: N(x) * e → N(10x+8) * e
        9 :: N(x) * e → N(10x+9) * e

        + :: N(a) * (N(b) * e) → N(a+b) * e
        * :: N(a) * (N(b) * e) → N(a*b) * e
        - :: N(a) * e → N(0-a) * e
        Q :: N(non-zero b) * (N(a) * e) → N(r) * (N(q) * e)
            where qb+r = a, q integral, r between 0 and b (excluding b)
        G :: N(x) * (N(y) * e) → ((N(y)*N(x))+(N(x)*N(y)) * e -- y > x
            #4 #2 G -- observes 4 > 2. Returns (N(2)*N(4)) on right.

        L :: (a + (b + c)) * e → ((a + b) + c) * e
        R :: ((a + b) + c) * e → (a + (b + c)) * e
        W :: (a + (b + c)) * e → (b + (a + c)) * e
        Z :: (a + (b + (c + d))) * e → (a + (c + (b + d))) * e
        V :: a * e → (a + 0) * e
        C :: (a + 0) * e → a * e

        ? :: (Droppable b) :: b@[x→x'] * ((x+y)*e) → (x'+y)*e
        D :: a * ((b+c) * e) → ((a*b) + (a*c)) * e -- distrib
        F :: ((a*b) + (c*d)) * e → (a+c) * ((b+d) * e) -- partial factor
        M :: (a + a') * e → a * e -- merge; a and a' compatible
        K :: (a + b ) * e → b * e -- assert; must be in b


Legend for types: `*` is a product or pair, `+` is a sum or Either type, `[x→y]` is a block type that can map from type `x` to type `y`, `N(x)` indicates a number with value x (numbers should be tracked in types as much as possible). 

        "embedded text
         may have multiple lines
        ~

Embedded text is a compact representation for a list of type `μL.((codepoint*L)+1)` where codepoints are natural numbers in range 0 to 1114111. There are a few blacklisted codepoints for embedded texts: control characters - C0, C1, DEL (excepting LF) - surrogates (U+D800..U+DFFF), and the replacement character (U+FFFD). Other non-printing characters (e.g. zero-width space) are not recommended. LF is escaped by following it with SP, while the text is terminated by the two character sequence LF ~.

## Tokens and Capabilities

Tokens in ABC are extensions to the bytecode. Tokens may be context-specific to a codebase, a runtime, or even a network session. Tokens are expressed by wrapping a short text between curly braces, e.g. `{foo}`. (Token texts must be valid embedded texts, 1 to 63 bytes utf8, and forbid curly braces and LF.) Environment specific tokens should include an HMAC or other authentication to resist forgery, to simplify reasoning about security.

Tokens must have *pure* semantics. Tokens mustn't constrain us against laziness, parallelism, and caching. However, within this limitation, tokens may support privileged or security-sensitive effects, such as reflection, copying affine values, or backtracking upon error. Together with affine blocks (to partition and sequence access to 'worlds'), tokens could feasibly be leveraged for ad-hoc side effects (cf. Clean and Mercury programming languages). 

However, Awelon project favors externalizing and explicitly handling effects, e.g. via use of [command sequences](CommandLine.md), [dictionary apps](ApplicationModel.md), [virtual machine models](NetworkModel.md). Awelon project uses tokens mostly for modularity and linking, performance annotations, metadata attributes, and type safety. 

## ABC CHANGE LOG

March 2014: 
* eliminated operators `PSBN`, which would observe type information

April 2014: 
* made `>` monomorphic, so it operates only on a pair of numbers
* modify list type from `µL.(1+(elem*L))` to `µL.((elem*L)+1)`
 * this impacts meaning of embedded texts

August 2014
* swap order of arguments to operator `o`, to better match common use

June 2015:
* eliminate `/` operator, leave ABC number type as integers, library-layer rationals
 * original definition: `/ :: N(non-zero a) * e → N(1/a) * e`
 * note: not fully committed to this change, may restore `/` later if necessary

January 2016:
* replace opcode `>` with `G` to reduce conflict with XML and HTML

April 2016:
* change `o` for compose to `m`, to eliminate use of vowels.

## ABCD

ABCD extends ABC with a standard dictionary of functions, i.e. such that a single character expands into a subprogram that is well defined in ABC. If well chosen, ABCD operators compress streaming bytecode, improve interpreted performance, and simplify optimization. For best results, the standardization process for ABCD must be both empirical and carefully reasoned.

I have yet to define ABCD extensions. Candidates for extension should first be explored within implementations of ABC.
