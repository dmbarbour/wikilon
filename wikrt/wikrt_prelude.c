
#include "wikrt.h"

#define DEF(X,Y) "@" X " " Y "\n"
#define STR(X) "\"" X "\""

char const prelude[] =
// hint for a human reader
DEF("language", STR("Awelon"))

// document primitives
DEF("prim:", STR("describing Awelon primitives"))
DEF("prim:a",STR("[B][A]a == A[B]   (apply)"))
DEF("prim:b",STR("[B][A]b == [[B]A] (bind)"))
DEF("prim:c",STR("   [A]c == [A][A] (copy)"))
DEF("prim:d",STR("   [A]d ==        (drop)"))
DEF("prim:nat", "\"\n"
    " Natural Numbers\n"
    "\n"
    "     42 == [41 S]\n"
    "\n"
    " Awelon defines all natural number words except 0.\n"
    " Definitions of 0 and S are left to the dictionary.\n"
    " Use accelerated definitions for performance.\n"
    "~")
DEF("prim:text", "\"\n"
    " Embedded Texts\n"
    "\n"
    "     \"hello\" == [104 \"ello\" :]\n"
    "     \"\" == ~\n"
    "\n"
    "     \"\n"
    "      multi-line texts starts with `\" LF` (34 10)\n"
    "      each line is indented by one space (32)\n"
    "      excepting if line is empty `LF LF` (10 10)\n"
    "      terminate the text with `LF ~` (10 126)\n" 
    "     ~\n"
    "\n"
    " Awelon supports embedded texts, both multi-line and\n"
    " inline for convenience. Definitions of : (cons) and\n"
    " ~ (nil) are left to the dictionary. Use accelerated\n"
    " definitions for performance.\n"
    "~")
// Todo: describe secure hash resources
// Todo: describe dictionary structure
        
// document and implement accelerators

// document annotations
DEF("anno:", STR("describing supported annotations"))
DEF("anno:nc", STR("(nc) marks value non-copyable (affine type)"))
DEF("anno:nd", STR("(nd) marks value non-droppable (relevant type)"))
DEF("anno:arity", "\"\n"
    " Arity annotations (a2)..(a9)\n"
    " These resist evaluation up to specified arity.\n"
    " For example:\n"
    "\n"
    "        [B][A](a2) == [B][A]\n"
    "     [C][B][A](a3) == [C][B][A]\n"
    "\n"
    " Arity annotations control evaluation and linking.\n"
    "~")




// extra metadata
DEF("prelude.runtime", STR("Wikilon"))
DEF("prelude.version", STR("2017 Feb 22"))
;



char const* wikrt_prelude()
{
    return prelude;
}

