#include "utf8.h"

#define UTF8_CONT(c) (((c) & 0xC0) == 0x80)

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
        uint32_t const cp = ((0x1F & c0) << 6)
                          | ((0x3F & c1)     );
        if(!UTF8_CONT(c1) || (cp < 0x80)) { goto e; }
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
        if(!UTF8_CONT(c1) || !UTF8_CONT(c2) || (cp < 0x800)) { goto e; }
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
        if(!UTF8_CONT(c1) || !UTF8_CONT(c2) || !UTF8_CONT(c3) ||
           (cp < 0x10000) || (cp > 0x10FFFF)) 
        { 
            goto e; 
        }
        (*r) = cp;
        return 4;
    }
    
e: // if we have any errors at all...
    (*r) = 0xFFFD;
    return 0;
}

