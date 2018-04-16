
# Ideas of Dubious Utility

Just a scratch pad for ideas I'm contemplating but not committed to in context of Awelon.

## Automatic Definitions

Awelon currently provides automatic definitions for:

* natural numbers `[1-9][0-9]*`
* embedded texts

I find it tempting to introduce more general support for automatic definitions, e.g. such that we could derive `cddadr` from `car` and `cdr` - perhaps a user would define `c{[ad]+}r`. 

I also like the idea of automatic definition for lists/tables based on common word structure. At the moment, we must explicitly manage any lists, though a software agent could help with this. But we could feasibly support `foo-*` words that automatically compose a list of words such as `foo-1` and `foo-42`.

These ideas unfortunately complicate implementation, semantics, dependency analysis (for cycle detection, incremental computing), and local reasoning. For the general case, we would essentially require reflective access to dictionaries as values. For now, my intention is to set this idea aside. I can return to it at a later date, extending Awelon dictionaries with symbols for user-defined, templated macro definitions. 

## Exchange and Apply

Right now I have:

        [B][A]a == B[A]         (apply)

But I could split the exchange aspect as follows:

        [B][A]a == B[A]         (apply')
        [B][A]e == [A][B]       (exchange)

This would result in greater separation of responsibilities. It simplifies the inline function to just `[] a d`. But the benefits ultimately seem limited. OTOH, the current 'apply' has a nice symmetry with bind, regarding the second argument:

        [B][A]a == A[B]         apply to all but second element
        [B][A]b == [[B]A]       apply to just the second element




