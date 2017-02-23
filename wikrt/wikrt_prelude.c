
#include "wikrt.h"

#define DEF(X,Y) "@" X " " Y "\n"
#define STR(X) "\"" X "\""

// multi-line - TEXT(L(...) B L(...))
//  L for line, B for blank
#define TEXT(X) "\"\n" X "~"
#define L(X) " " X "\n"
#define B "\n"

char const prelude[] =
// document primitives
DEF("prelude", TEXT(
    L("Awelon is a purely functional concatenative combinator based")
    L("programming language evaluated by local, confluent rewriting.")
    B
    L("This is an Awelon dictionary that attempts to document Awelon")
    L("well enough for a human to reimplement it if necessary, and")
    L("serve as a seed for a larger dictionary.")
    ))
DEF("prim:", STR("Awelon language primitives"))
DEF("prim:a",STR("[B][A]a == A[B]   (apply)"))
DEF("prim:b",STR("[B][A]b == [[B]A] (bind)"))
DEF("prim:c",STR("   [A]c == [A][A] (copy)"))
DEF("prim:d",STR("   [A]d ==        (drop)"))
DEF("prim:blocks", TEXT(
    L("Blocks")
    B
    L("    [code goes here]")
    B
    L("Blocks are the values of Awelon, and represent first class")
    L("functions. Semantically, all values in Awelon blocks, even")
    L("natural numbers and texts.")
    B
    L("An Awelon runtime or compiler will recognize and optimize")
    L("a useful subset of values, effectively elevating them to")
    L("to primitive performance features. This is acceleration.")
    B
    L("An Awelon development environment can effectively extend")
    L("Awelon syntax via editable views, for example `4/10` may")
    L("expand to `[4 10 ratio]` in source. Even local variables")
    L("and lambdas be supported this way.")
    ))
DEF("prim:numbers", TEXT(
    L("Natural Numbers")
    B
    L("    42 = [41 S]")
    B
    L("Natural numbers in Awelon are encoded in base 10. Predefined")
    L("number words use regex `[1-9][0-9]*`. Definitions of `0` and")
    L("`S` are left to the user, but accelerated definitions should")
    L("be favored for performance.")
    B
    L("Awelon does not directly support other number types, but use")
    L("of editable views can provide syntax for integers, rationals,")
    L("decimals, and even vectors or matrices.")
    ))
DEF("prim:texts", TEXT(
    L("Embedded Texts")
    B
    L("    \"hello\" = [104 \"ello\" :]")
    L("    \"\"      = ~")
    B
    L("    \"")
    L("     multi-line texts starts with `\" LF` (34 10)")
    L("     each non-empty line is indented one space (32)")
    L("     terminate multi-line text with `LF ~` (10 126)")
    L("    ~")
    B
    L("Texts must be encoded in valid UTF-8 excluding C0 (0-31) and")
    L("DEL (127), excepting LF (10). Inline texts cannot contain LF")
    L("or \" (10 or 34). Indentation after LF is the only escape.")
    B
    L("Semantically each text encodes a list of natural numbers, one")
    L("element per Unicode codepoint. Definitions of `:` (cons) and") 
    L("`~` (nil) are left to the user, but accelerated definitions are")
    L("recommended for performance.")
    ))
DEF("prim:words", TEXT(
    L("Awelon Words")
    B
    L("    [B][A]w == [A][B]   w = (a2) [] b a     (swap)")
    L("    [A]i == A           i = [] w a d        (inline)")
    L("    [X][F]z == [X][[F]z]F                   (fixpoint)")
    L("       z = [[(a3) c i] b (=z) [c] a b w i] (a3) c i")
    B
    L("Words are defined in context of a dictionary, the Awelon")
    L("codebase, with a few exceptions like natural numbers.")
    B
    L("The semantics for Awelon words are trivial: lazily inline")
    L("word's definition. By lazily, I mean we only inline if it")
    L("results in further rewrites. We preserve human meaningful")
    L("structure where feasible, and maximize structure sharing.")
    L("Use of arity annotations like (a2) and (a3) are valuable")
    L("to help control lazy linking.")
    B
    L("Syntactically, words consist of a sequence of characters")
    L("excluding a short blacklist:")
    B
    L("    @#()<>{}\/,;|&='\", SP, C0, and DEL (0-32 and 127)")
    B
    L("See `prim:hierarchy` for namespaces.")
    ))
DEF("prim:dictionary", TEXT(
    L("Awelon Dictionary")
    B
    L("    secureHashOfPatchA")
    L("    secureHashOfPatchB")
    L("    @word1 definition1")
    L("    @word2 definition2")
    L("    ...")
    B
    L("An Awelon codebase is called a dictionary. A dictionary defines")
    L("a set of words, and is self-contained. Awelon specifies a patch")
    L("based representation for constructing large dictionaries, using")
    L("secure hashes to reference other patches and support incremental")
    L("import and export, indexing, and structure sharing.")
    B
    L("The initial patches are logically inlined. The last definition")
    L("of a word is the one we use. To logically drop a word, we use")
    L("`@foo foo` to define a trivial cycle. Normally, cycles are an")
    L("error. Awelon code uses explicit fixpoints for cyclic behavior.")
    B
    L("See `prim:hierarchy` for hierarchical structure.")
    ))
DEF("prim:hierarchy", TEXT(
    L("Hierarchical Dictionaries, Namespaces, and Localization")
    B
    L("    ... in body of dictionary ...")
    L("    @@dict1 secureHashOfDict1")
    L("    @@dict2 secureHashOfDict2")
    L("    @word1  [42 foo]@dict1 bar@dict2")
    L("    ...") 
    B
    L("Hierarchical dictionaries are represented by defining symbol")
    L("`@dict` with a secure hash referencing a dictionary patch.")
    L("Like any word, this symbol may be freely redefined by later")
    L("patches.")
    B
    L("Words in a hierarchical dictionary can be referenced using")
    L("a hierarchical namespace qualifier, like `bar@dict`. A full")
    L("block may be qualified: `[42 foo]@d` means `[42@d foo@d]`.")
    L("But these qualifiers are second-class, and no whitespace is")
    L("allowed between the word or block and the `@d` qualifier.")
    B
    L("There is no means for a dictionary to reference its parent.")
    L("Thus, each dictionary must be self-contained and will often")
    L("replicate a lot of definitions. Fortunately, secure hashes")
    L("support structure sharing among hierarchical dictionaries.")
    B
    L("Localization is an optimization: when a namespace qualifier")
    L("does not affect meaning, we may drop the qualifer. If `42@d`")
    L("behaves as `42`, we should favor `42`. Localization is good")
    L("for both performance and aesthetics.")
    ))
DEF("prim:annotations", TEXT(
    L("Annotations in Awelon")
    B
    L("    (nat)    assert value is natural number")
    L("    (nc)     assert value is not copied later")
    L("    (trace)  record value to a debug log")
    L("    (par)    request parallel evaluation of value")
    L("    (a3)     arity three, wait for three args")
    L("    (jit)    request runtime to compile code")
    L("    (=z)     replace [definition of z] by [z]")
    L("    (stow)   [large value] => [$secureHash]")
    L("    ...")
    B
    L("Annotations are represented by parenthetical symbols.")
    B
    L("Semantically, annotations have identity behavior. But they may")
    L("impact performance, support debugging, cause incorrect programs")
    L("to fail fast, affect aesthetics. In context of editable views,")
    L("annotations also may provide rendering hints.")
    B
    L("Supported annotations are documented under prefix `anno:`.")
    ))
DEF("prim:acceleration", TEXT(
    L("Acceleration of Awelon")
    B
    L("Example. A typical model of natural numbers is:")
    B
    L("    type Nat = Zero | Succ Nat")
    B
    L("This model has nice formal properties, but its direct use in")
    L("computation is inefficient. Conventional languages might add")
    L("a primitive number type for performance. Awelon aims instead")
    L("to recognize a few such models and accelerate them, using a")
    L("specialized representation and a few optimized operations.")
    B
    L("Acceleration of numbers, records, and arrays would cover many")
    L("conventional languages. It is feasible to further accelerate")
    L("linear algebra via GPGPUs or process networks via the cloud.")
    L("A few carefully chosen models cover many performance cases.")
    B
    L("Accelerated models effectively become performance primitives,")
    L("even if not semantic ones. Awelon relies on this technique.")
    B
    L("Supported accelerators are documented under prefix `accel:`.")
    ))
DEF("prim:secureHash", TEXT(

    ))
// Todo: describe secure hash resources

// document annotations
DEF("anno:", STR("describing annotations"))
DEF("anno:arity", TEXT(
    L("Arity Annotations (a2)..(a9)")
    B
    L("       [B][A](a2) == [B][A]")
    L("    [C][B][A](a3) == [C][B][A]")
    B
    L("Arity annotationss prevent access to values at their left until")
    L("the requisite number have been provided. The annotation is then")
    L("erased and computation continues.")
    B
    L("Arity annotations support lazy evaluations, inline comments of")
    L("form `[comment](a2)d`, and help control lazy linking of words.")
    L("They are among the most versatile and useful of annotations.")
    ))
DEF("anno:substructure", TEXT(
    L("Substructural Type Annotations (nc) and (nd)")
    B
    L("Substructural type systems make assertions about whether a value")
    L("may be copied or dropped. They are useful for modeling resources")
    L("and protocols where values represent obligations.")
    B
    L("Annotation (nc) marks a value no-copy, (nd) no-drop. This offers")
    L("a simple basis for affine (nc), relevant (nd), and linear (both)")
    L("data types, readily verified statically or dynamically.")
    ))
// value sealing
// =z name matching
// trace
// error
// trash
// par
// eval
// memo
// stow
// nat, text, binary
// tuples
// bool, opt, sum, cond.
// gates


        
// document and implement accelerators



// extra metadata
DEF("prelude.runtime", STR("Wikilon"))
DEF("prelude.version", STR("2017 Feb 22"))
;



char const* wikrt_prelude()
{
    return prelude;
}

