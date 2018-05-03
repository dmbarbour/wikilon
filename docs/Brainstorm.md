
# Ideas of Dubious Utility

Just a scratch pad for ideas I'm contemplating but not committed to in context of Awelon.

## Automatic Definitions

Awelon currently provides automatic definitions for:

* natural numbers `[1-9][0-9]*`
* embedded texts

I find it tempting to introduce more general support for automatic definitions, e.g. such that we could derive `cddadr`. I also like the idea of automatic definition for lists/tables based on common word structure. At the moment, we must explicitly manage any lists. But we could feasibly support `foo-*` words that automatically compose a list of words such as `foo-1` and `foo-42`.

Effectively, these functions would either require some form of macro definitions or some built-in schema. Macro definitions would be more flexible, but also rather more difficult to index for reactive computations (e.g. to automatically add `foo-36` to the appropriate `foo-*` list). I'd prefer to avoid the resulting complexities in my implementations and tooling. 

For the moment, we can shove this issue to external software agents or interactive editing tools, which could automatically define words as-needed based on rules represented within the dictionary. Similarly, we could automatically import or maintain definitions from trusted external sources.

## Exchange and Apply

Right now I have:

        [B][A]a == B[A]         (apply)

But I could split the exchange aspect as follows:

        [B][A]a == B[A]         (apply')
        [B][A]e == [A][B]       (exchange)

This would result in greater separation of responsibilities. It simplifies the inline function to just `[] a d`. But the benefits ultimately seem limited. OTOH, the current 'apply' has a nice symmetry with bind, regarding the second argument:

        [B][A]a == A[B]         apply to all but second element
        [B][A]b == [[B]A]       apply to just the second element




