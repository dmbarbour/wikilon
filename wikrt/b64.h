/* base64url conversions */
#pragma once
#ifndef B64_H
#include <stdint.h>

/** Encode a binary to base64 representation.
 * 
 * I assume the output buffer is sufficiently sized, having four output
 * bytes for every three bytes of input (round up). The input is padded
 * logically with zeroes. No NUL terminal is added to the output.
 */
void b64_encode(uint8_t const* input, uint64_t input_size, uint8_t* output);

#define B64_H
#endif

