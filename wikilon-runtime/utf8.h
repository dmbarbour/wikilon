// lightweight utilities for UTF-8 encoding or decoding.
#pragma once
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/** Read a single utf-8 codepoint.
 *
 * In case of error, cp is set to the replacement character and we
 * return zero bytes read. This function does restrict against
 * surrogate codepoints (U+D800..U+DFFF) but any other validly
 * encoded codepoint may be returned, even those that do not 
 * encode valid characters.
 */
size_t utf8_readcp(char const* s, size_t strlen, uint32_t* cp);

/** Read a single character, update state for convenient looping. */
static inline bool utf8_step(char const** s, size_t* strlen, uint32_t* cp) 
{
    size_t const k = utf8_readcp((*s), (*strlen), cp);
    (*s)      += k;
    (*strlen) -= k;
    return (0 != k);
}

/** Validate and obtain string length. 
 *
 * Will return number of consecutive valid utf8 codepoints. 
 * Returns true iff the entire strlen was consumed.
 */
bool utf8_valid_strlen(char const* s, size_t strlen, size_t* utf8len);

// todo: support reverse-ordered read from end

/** Is a character a control char? (C0, DEL, C1) */
static inline bool isControlChar(uint32_t c) {
    return (c <= 0x1F) || ((0x7F <= c) && (c <= 0x9F));
}

/** Is a codepoint the replacement character? 0xFFFD */
static inline bool isReplacementChar(uint32_t c) {
    return (0xFFFD == c);
}

