
/** wikrt_hash is a trivial utility to access the
 * BLAKE2b hash as 60 bytes of UTF-8. 
 */
#include <stdint.h>
#include <blake2.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "b64.h"

int main(int _argc, char const* const* _argv) 
{
    blake2b_state s;
    blake2b_init(&s, 45);

    size_t const buffer_size = (1<<15);
    uint8_t buffer[buffer_size];
    size_t bytes_read;
    do {
        bytes_read = fread(buffer, 1, buffer_size, stdin);
        blake2b_update(&s, buffer, bytes_read);
    } while(0 != bytes_read);

    uint8_t h[45];
    blake2b_final(&s, h, 45);

    uint8_t hstr[60];
    b64_encode(h, 45, hstr);
    fwrite(hstr, 60, 1, stdout);
    fflush(stdout);

    return 0;
}

