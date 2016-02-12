#include "utf8.h"

static inline bool cc(uint32_t c) { return (0x80 == ((c) & 0xC0)); }
static inline bool surrogate(uint32_t c) { return ((0xD800 <= c) && (c <= 0xDFFF)); }

/** Is a codepoint an invalid surrogate? (0xD800..0xDFFF) */
inline bool isSurrogateCodepoint(uint32_t c) { 
    return (0xD800 <= c) && (c <= 0xDFFF);
}


size_t utf8_readcp(char const* const s, size_t const strlen, uint32_t* const r)
{
    if(0 == strlen) { goto e; }

    unsigned char const* p = (unsigned char const*) s;
    uint32_t const c0 = (*p);

    if(c0 < 0x80) {
        // U+0 .. U+7F 
        (*r) = c0;
        return 1;
    } else if(c0 < 0xE0) {
        // U+80 .. U+7FF 
        if(strlen < 2) goto e;
        uint32_t const c1 = p[1];
        uint32_t const cp = ((0x1F & c0) << 6 )
                          | ((0x3F & c1)      );
        if(!cc(c1) || (cp < 0x80)) { goto e; }
        (*r) = cp;
        return 2;
    } else if(c0 < 0xF0) {
        // U+800 .. U+FFFF
        if(strlen < 3) goto e;
        uint32_t const c1 = p[1];
        uint32_t const c2 = p[2];
        uint32_t const cp = ((0x0F & c0) << 12)
                          | ((0x3F & c1) << 6 )
                          | ((0x3F & c2)      );
        if(!cc(c1) || !cc(c2) || (cp < 0x800) || surrogate(cp)) { goto e; }
        (*r) = cp;
        return 3;
    } else {
        // U+10000 .. U+10FFFF
        if(strlen < 4) goto e;
        uint32_t const c1 = p[1];
        uint32_t const c2 = p[2];
        uint32_t const c3 = p[3];
        uint32_t const cp = ((0x07 & c0) << 18)
                          | ((0x3F & c1) << 12)
                          | ((0x3F & c2) << 6 )
                          | ((0x3F & c3)      );
        if(!cc(c1) || !cc(c2) || !cc(c3) || (cp < 0x10000) || (cp > 0x10FFFF)) { goto e; }
        (*r) = cp;
        return 4;
    }
    
e: // if we have any validation errors...
    (*r) = 0xFFFD;
    return 0;
}

bool utf8_valid_strlen(char const* s, size_t strlen, size_t* utf8len)
{
    uint32_t cp;
    size_t ct = 0;
    while((strlen != 0) && utf8_step(&s, &strlen, &cp)) { ++ct; }
    (*utf8len) = ct;
    return (0 == strlen);
}



