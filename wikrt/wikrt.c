
#include "wikrt_private.h"

_Static_assert((sizeof(wikrt_val) == 4) || (sizeof(wikrt_val) == 8), 
    "expecting 32-bit or 64-bit words");
_Static_assert((sizeof(wikrt_val) >= sizeof(size_t)),
    "expecting to store sizes in a single word");
_Static_assert((sizeof(uint8_t) == sizeof(char)), 
    "expecting uint8_t* aligns with char*");


