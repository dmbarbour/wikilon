
#include "b64.h"

char const b64[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

static inline void b64_encode_block(uint8_t const* in, uint8_t* out)
{
    out[0] = b64[ (in[0] >> 2) ];
    out[1] = b64[ ((in[0] & 0x03) << 4) | ((in[1] & 0xf0) >> 4) ];
    out[2] = b64[ ((in[1] & 0x0f) << 2) | ((in[2] & 0xc0) >> 6) ];
    out[3] = b64[ (in[2] & 0x3f) ];
}

void b64_encode(uint8_t const* input, uint64_t input_size, uint8_t* output)
{
    while(input_size >= 3) {
        b64_encode_block(input, output);
        input += 3;
        output += 4;
        input_size -= 3;
    }
    if(0 != input_size) {
        // input_size is 1 or 2. 
        uint8_t input_final[3];
        input_final[0] = input[0];
        input_final[1] = (2 == input_size) ? input[1] : 0;
        input_final[2] = 0;
        b64_encode_block(input_final, output);
    }
}

