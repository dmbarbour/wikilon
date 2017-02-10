
#include "wikrt_private.h"
#include <time.h>
#include <stdlib.h>
#include <stdio.h>

_Static_assert((sizeof(wikrt_v) == 4) || (sizeof(wikrt_v) == 8), 
    "expecting 32-bit or 64-bit words");
_Static_assert((sizeof(wikrt_v) >= sizeof(size_t)),
    "expecting to store sizes in a single word");
_Static_assert((sizeof(uint8_t) == sizeof(char)), 
    "expecting uint8_t* aligns with char*");
_Static_assert(sizeof(wikrt_ws) == (4*sizeof(wikrt_v)),
    "flexible array members don't work the way I think they should");

uint64_t wikrt_thread_time()
{
    struct timespec tm;
    int const st = clock_gettime(CLOCK_THREAD_CPUTIME_ID, &tm);
    if(!st) {
        fprintf(stderr, "%s cannot get time\n", __FUNCTION__);
        abort();
    }
    uint64_t const usec_sec = ((uint64_t)tm.tv_sec) * (1000 * 1000);
    uint64_t const usec_nsec = ((uint64_t)tm.tv_nsec) / 1000;
    return (usec_sec + usec_nsec);
}
