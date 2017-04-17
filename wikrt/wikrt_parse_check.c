
#include <assert.h>
#include "utf8.h"
#include "wikrt.h"

// Awelon forbids some ASCII range characters from words.
// Blacklist: @#[]()<>{}\/,;|&='", SP, C0 (0-31), and DEL.
//
// We can also blacklist bytes not permitted in UTF-8 more
// generally: 192, 193, 245-255. But properly, a separate
// scan is required to check for a valid UTF-8 encoding.
//
// BLOCK                                                    BITFIELD
//  0-31:   forbid everything                               0
//  32-63:  forbid 32 34 35 38 39 40 41 44 47 59 60 61 62   ~(0x780093cd)
//  64-95:  forbid 64 91 92 93                              ~(0x38000001)
//  96-127: forbid 123 124 125 127                          ~(0xb8000000)
//  128-159: allow everything                               ~0
//  160-191: allow everything                               ~0
//  192-223: forbid 192 193                                 ~3
//  224-255: forbid 245-255                                 ~(0xffe00000)
//
// I'm encoding this as a bitfield.
static uint32_t const valid_word_char_bitfield[8] = 
    {  0, ~(0x780093cd), ~(0x38000001), ~(0xb8000000)
    , ~0, ~0,            ~3,            ~(0xffe00000) };
static inline bool is_valid_word_byte(uint8_t u) 
{
    return (0 != (valid_word_char_bitfield[ (u >> 5) ] & (1 << (u & 0x1F))));
}
static inline uint8_t const* scan_valid_word(uint8_t const* iter, uint8_t const* const end)
{
    while((iter < end) && is_valid_word_byte(*iter)) { ++iter; }
    return iter;
}

/* number of valid word characters (assuming valid UTF-8) */
size_t wikrt_word_len(uint8_t const* const src, size_t maxlen)
{
    // just looking at valid word bytes, not valid utf-8
    return scan_valid_word(src, src+maxlen) - src; 
}

static uint8_t const* scan_ns_qualifier(uint8_t const* iter, uint8_t const* const end)
{
    // word@ns, or [block]@ns, or even a "text"@ns
    // potentially hierarchical, e.g. `foo@ns1@ns2`
    do {
        bool const qualified = 
            ((end - iter) >= 2) &&
            ('@' == iter[0]) &&
            is_valid_word_byte(iter[1]);
        if(!qualified) { return iter; }
        iter = scan_valid_word(iter+2, end);
    } while(1);
}
static inline bool basic_text_byte(uint8_t c) { return (c > 31); }
static uint8_t const* scan_inline_text(uint8_t const* iter, uint8_t const* const end)
{
    _Static_assert((34 == '"'), "assuming ASCII encodings");
    while((end != iter) && basic_text_byte(*iter) && ('"' != *iter)) { ++iter; }
    return iter;
}
static uint8_t const* scan_multi_line_text(uint8_t const* iter, uint8_t const* const end)
{
    _Static_assert((10 == '\n') && (32 == ' '), "assuming ASCII encodings");
    while(end != iter) {
        if('\n' == *iter) { ++iter; } 
        else if(' ' == *iter) { 
            // skip to end of line
            do { ++iter; } while((end != iter) && basic_text_byte(*iter));
        } else { return iter; }
    } 
    return end;
}

bool wikrt_parse_check(uint8_t const* const start, size_t const input_size, wikrt_parse_data* data)
{
    _Static_assert((91 == '[') && (93 == ']') && (40 == '(') && (41 == ')') &&
                   (10 == '\n') && (32 == ' ') && (34 == '"')
        , "assuming ASCII encodings");
        
    wikrt_parse_data r = { 0 };
    size_t const utf8_size = utf8_strlen(start, input_size);
    assert(input_size >= utf8_size);
    uint8_t const* const end = start + utf8_size;
    uint8_t const* scan = start;
    do {
        if(end == scan) { goto parse_halt; }
        uint8_t const c = *(scan++);
        if(is_valid_word_byte(c)) { // word
            scan = scan_valid_word(scan, end);
            scan = scan_ns_qualifier(scan, end);
        } else if((' ' == c) || ('\n' == c)) { // whitespace
            r.parsed = scan - start;
            if(0 == r.balance) { r.accepted = r.parsed; }
        } else if('[' == c) { // block start
            ++(r.balance);
        } else if((']' == c) && (r.balance > 0)) { // block end
            --(r.balance);
            scan = scan_ns_qualifier(scan, end);
        } else if('(' == c) { // annotations
            if((end == scan) || !is_valid_word_byte(*scan)) { goto parse_halt; }
            scan = scan_valid_word(1+scan, end);
            if((end == scan) || (')' != *scan)) { goto parse_halt; }
            scan = scan_ns_qualifier(1+scan, end);
        } else if('"' == c) { // embedded texts
            if(end == scan) { goto parse_halt; }
            else if('\n' == (*scan)) {
                scan = scan_multi_line_text(1+scan, end);
            } else {
                scan = scan_inline_text(scan, end);
            }
            if((end == scan) || ('"' != *scan)) { goto parse_halt; }
            scan = scan_ns_qualifier(1+scan, end);
        } else { // illegal input
            goto parse_halt; 
        }
    } while(1);

parse_halt:
    // accept data after final block
    r.scanned = scan - start;
    if(NULL != data) { (*data) = r; } 
    return (input_size == r.accepted);
}




