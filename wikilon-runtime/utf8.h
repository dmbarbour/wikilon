// lightweight utilities for UTF-8 encoding or decoding.
#pragma once
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/** Read a single utf-8 codepoint.
 *
 * In case of error, cp is set to the replacement character and we
 * return zero bytes read. This function doesn't discriminate on 
 * the codepoint read (even surrogates may be read), but does require
 * a codepoint be encoded in the smallest possible size.
 */
size_t utf8_readcp(char const* s, size_t strlen, uint32_t* cp);

// todo: support reverse-ordered read from end

/** Is a character a control char? (C0, DEL, C1) */
static inline bool isControlChar(uint32_t c) {
    return (c <= 0x1F) || ((0x7F <= c) && (c <= 0x9F));
}

/** Is a codepoint an invalid surrogate? (0xD800..0xDFFF) */
inline bool isSurrogateCodepoint(uint32_t c) { 
    return (0xD800 <= c) && (c <= 0xDFFF);
}

/** Is a codepoint the replacement character? 0xFFFD */
inline bool isReplacementChar(uint32_t c) {
    return (0xFFFD == c);
}

