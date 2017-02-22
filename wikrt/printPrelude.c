// Print the Wikilon prelude to console, for perusal.

#include <stdio.h>
#include <string.h>
#include "wikrt.h"

int main(int argc, char** argv)
{
    char const* const s = wikrt_prelude();
    size_t const len = strlen(s);
    size_t const written = fwrite(s, 1, len, stdout);
    int const flush_error = fflush(stdout);
    bool const ok = (written == len) && (0 == flush_error);
    return (ok ? 0 : -1);
}

