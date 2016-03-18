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
size_t utf8_readcp(uint8_t const* s, size_t byteLen, uint32_t* cp);

/** Read a single character, update state for convenient looping. */
static inline bool utf8_step(uint8_t const** s, size_t* byteLen, uint32_t* cp) 
{
    size_t const k = utf8_readcp((*s), (*byteLen), cp);
    (*s)       += k;
    (*byteLen) -= k;
    return (0 != k);
}

/** Return size of current codepoint (no validation). */
static inline size_t utf8_cpsize(uint8_t const* s) 
{
    uint8_t const c = *s;
    return  (c < 0x80) ? 1 :
            (c < 0xE0) ? 2 :
            (c < 0xF0) ? 3 : 4;
}

/** Read a utf-8 character without validation. */
static inline size_t utf8_readcp_unsafe(uint8_t const* s, uint32_t* cp) 
{
    uint32_t const c0 = (uint32_t)(*s);
    if(c0 < 0x80) {
        (*cp) = c0;
        return 1;
    } else if(c0 < 0xE0) {
        (*cp) = ((uint32_t)(0x1F &  c0 ) <<  6) 
              | ((uint32_t)(0x3F & s[1])      );
        return 2;
    } else if(c0 < 0xF0) {
        (*cp) = ((uint32_t)(0x0F &  c0 ) << 12)
              | ((uint32_t)(0x3F & s[1]) <<  6)
              | ((uint32_t)(0x3F & s[2])      );
        return 3;
    } else {
        (*cp) = ((uint32_t)(0x07 &  c0 ) << 18)
              | ((uint32_t)(0x3F & s[1]) << 12)
              | ((uint32_t)(0x3F & s[2]) <<  6)
              | ((uint32_t)(0x3F & s[3])      );
        return 4;
    }
}

size_t utf8_readcp_unsafe(uint8_t const* s, uint32_t* cp);

/* read a codepoint, knowing one is there. */
static inline uint32_t utf8_step_unsafe(uint8_t const** s)
{
    uint32_t cp;
    size_t const k = utf8_readcp_unsafe((*s), &cp);
    (*s) += k;
    return cp;
}

/** Read a single utf-8 codepoint from the end of the text.
 *
 * This is intended for use with string reversals. A character is
 * read from the rear-end of the text. Otherwise this behaves as
 * utf8_readcp. (It's probably a bit less efficient.)
 */
size_t utf8_readcp_r(uint8_t const* s, size_t strlen, uint32_t* cp);

/** Read a single character in reverse & update state for convenient looping. */
static inline bool utf8_step_r(uint8_t const* s, size_t* strlen, uint32_t* cp) 
{
    size_t const k = utf8_readcp_r(s, (*strlen), cp);
    (*strlen) -= k;
    return (0 != k);
}

/** Read a utf-8 character from the end of the text without validation. */
static inline size_t utf8_readcp_unsafe_r(uint8_t const* s, size_t byteLen, uint32_t* cp)
{
    s += byteLen; 
    while(0x80 == ((*s) & 0xC0)) { --s; }
    return utf8_readcp_unsafe(s, cp); 
}

static inline bool utf8_step_unsafe_r(uint8_t const* s, size_t* byteLen, uint32_t* cp)
{
    if(0 == (*byteLen)) { return false; }
    (*byteLen) -= utf8_readcp_unsafe_r(s, (*byteLen), cp);
    return true;
}

/** Validate and obtain utf8 string length. 
 *
 * Will return number of consecutive valid utf8 codepoints. 
 * Returns true iff the entire strlen is valid.
 */
bool utf8_valid_strlen(uint8_t const* s, size_t strlen, size_t* utf8len);

// todo: support reverse-ordered read from end

/** Is a character a control char? (C0, DEL, C1) */
static inline bool isControlChar(uint32_t c) {
    return (c <= 0x1F) || ((0x7F <= c) && (c <= 0x9F));
}

/** Is a codepoint the replacement character? 0xFFFD */
static inline bool isReplacementChar(uint32_t c) {
    return (0xFFFD == c);
}

static inline size_t utf8_writecp_size(uint32_t cp) {
    return  (cp <= 0x7F)    ? 1 :
            (cp <= 0x7FF)   ? 2 :
            (cp <= 0xFFFF)  ? 3 : 4;
}

/** Write a codepoint to a buffer, assuming buffer has sufficient size.
 *  (conservatively, at least four bytes).
 */
static inline size_t utf8_writecp_unsafe(uint8_t* buff, uint32_t cp) 
{
    if(cp <= 0x7F) { 
        (*buff) = (uint8_t) cp; 
        return 1;
    } else if(cp <= 0x7FF) {
        //110xxxxx 10xxxxxx 
        buff[0] = 0xC0 | (0x1F & (cp >>  6));
        buff[1] = 0x80 | (0x3F & (cp      ));
        return 2;
    } else if(cp <= 0xFFFF) {
        //1110xxxx 10xxxxxx 10xxxxxx
        buff[0] = 0xE0 | (0x0F & (cp >> 12));
        buff[1] = 0x80 | (0x3F & (cp >>  6));
        buff[2] = 0x80 | (0x3F & (cp      ));
        return 3;
    } else {
        //11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
        buff[0] = 0xF0 | (0x03 & (cp >> 18));
        buff[1] = 0x80 | (0x3F & (cp >> 12));
        buff[2] = 0x80 | (0x3F & (cp >>  6));
        buff[3] = 0x80 | (0x3F & (cp      ));
        return 4;
    }
}


