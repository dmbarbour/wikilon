// This file is just for `wikrt_text_to_block` and any variants.
// It involves a simplistic hand-written parser.

#include <string.h>
#include <assert.h>
#include <stdio.h>
#include "wikrt.h"

typedef enum 
{ WIKRT_PARSE_OP  = 0
, WIKRT_PARSE_TXT = 1
, WIKRT_PARSE_TXT_LF = 2
, WIKRT_PARSE_TOK = 3
} wikrt_parse_type;

#define WIKRT_PARSE_BUFFSZ (WIKRT_CELLSIZE * 1024)
typedef struct {
    wikrt_parse_type type;        // special parser states
    wikrt_size       depth;       // stack size, hierarchical depth of `[`

    // For tokens and texts, use intermediate buffer.
    wikrt_size buffsz;
    wikrt_size charct;
    uint8_t    buff[WIKRT_PARSE_BUFFSZ];
} wikrt_parse_state;

// How much to read in one step.
#define WIKRT_PARSE_READSZ (30 * 1000)

/* In addition to wikrt_parser_state, the context will hold a lot of data.
 * In particular, it will hold:
 *
 *   (1) The object actively being constructed
 *   (2) A stack of continuations to return to
 *   (3) The text that we're reading
 *
 * These are ordered in expected frequency of access. So order on our
 * stack will be (object * (stack * (text * e))). Our 'object' is either
 * an inverse list of operators, or an inverse list of text chunks.
 */

static bool wikrt_intro_parse(wikrt_cx* cx, wikrt_parse_state* p) 
{
    size_t const szAlloc = 2 * WIKRT_CELLSIZE;
    if(!wikrt_mem_reserve(cx, szAlloc)) { return false; }
    wikrt_intro_r(cx, WIKRT_UNIT);      // introduce our stack
    wikrt_intro_r(cx, WIKRT_UNIT_INR);  // toplevel (reverse) list of ops
    p->type = WIKRT_PARSE_OP;
    p->depth = 0;
    return true;
}

// Ops are initially constructed in a reverse-ordered list.
// I need to reverse this list ordering after all is done.
//   (reversed ops * e) → (ops * e)
static void wikrt_reverse_opslist(wikrt_cx* cx) 
{
    // Goal is to reverse the operations list.
    // The current implementation is non-allocating.
    wikrt_val hd = wikrt_pval(cx, cx->val)[0];
    wikrt_val tl = WIKRT_UNIT_INR;
    while(WIKRT_UNIT_INR != hd) 
    {
        // assuming cons list (cell per value), cf. wikrt_cons
        assert(wikrt_pl(hd)); 
        wikrt_val* const phd = wikrt_pval(cx, hd);
        wikrt_val const next_hd = phd[1];
        phd[1] = tl;
        tl = hd;
        hd = next_hd;
    }
    wikrt_pval(cx, cx->val)[0] = tl;
}

static bool wikrt_flush_parse_text(wikrt_cx* cx, wikrt_parse_state* p)
{
    if(0 == p->buffsz) { return true; } // nothing to flush

    // sanity check
    assert( (p->charct <= p->buffsz) && 
            (p->buffsz <= 0xFFFF) &&
            (p->buffsz <= (UTF8_MAX_CP_SIZE * p->charct)) );

    wikrt_sizeb const szBuff  = wikrt_cellbuff(p->buffsz);
    wikrt_sizeb const szAlloc = szBuff + (2 * WIKRT_CELLSIZE);
    if(!wikrt_mem_reserve(cx,szAlloc)) { return false; }

    // context should (texts * e).
    wikrt_val* const texts = wikrt_pval(cx, cx->val);

    // copy text from parse buffer into context
    wikrt_addr const addr_buff = wikrt_alloc_r(cx, szBuff);
    memcpy(wikrt_paddr(cx, addr_buff), p->buff, p->buffsz);

    // (OTAG_TEXT, next, (size-char, size-bytes), buffer)
    wikrt_addr const addr_hdr = wikrt_alloc_r(cx, (2 * WIKRT_CELLSIZE));
    wikrt_val* const phdr = wikrt_paddr(cx, addr_hdr);
    phdr[0] = WIKRT_OTAG_TEXT;
    phdr[1] = (*texts);
    phdr[2] = (p->charct << 16) | (p->buffsz);
    phdr[3] = addr_buff;
    (*texts) = wikrt_tag_addr(WIKRT_O, addr_hdr);

    // clear buffer and continue
    p->buffsz = 0;
    p->charct = 0;

    return true;
}

static bool wikrt_write_parse_text_char(wikrt_cx* cx, wikrt_parse_state* p, uint32_t cp) 
{
    _Static_assert((WIKRT_PARSE_BUFFSZ >= UTF8_MAX_CP_SIZE), "parse buffer too small to safely process text");
    _Static_assert((WIKRT_PARSE_BUFFSZ <= 0xFFFF), "parse buffer too large to trivially flush to text chunk");
    p->charct += 1;
    p->buffsz += utf8_writecp_unsafe((p->buff + p->buffsz), cp);
    if(p->buffsz >= (WIKRT_PARSE_BUFFSZ - UTF8_MAX_CP_SIZE)) {
        bool const okFlush = wikrt_flush_parse_text(cx, p);
        if(!okFlush) { return false; }
    }
    return true;
}

// After we build a block or text, we'll need to push it into the
// `ops` list on our current stack.
//
// (val * ((ops * stack) * e)) → (opval:ops) * (stack * e)
//
// For our simplistic parser, our `val` should be a text or block.
// E.g. because we aren't simplifying numbers.
//
// Assumes reserve of at least WIKRT_CELLSIZE.
static void wikrt_fini_parse_opval_r(wikrt_cx* cx)
{
    // Quote the value by wrapping it with WIKRT_OPVAL.
    wikrt_val* const v = wikrt_pval(cx, cx->val);
    wikrt_alloc_cellval_r(cx, v, WIKRT_O, WIKRT_OTAG_OPVAL, (*v)); // text → opval

    // Now we add opval to list. Corresponds to ABC code `wrzwlV`
    wikrt_err st = WIKRT_OK;  // (opval * ((ops * stack) * e))
    st |= wikrt_wswap(cx);    // ((ops * stack) * (opval * e))  
    st |= wikrt_assocr(cx);   // (ops * (stack * (opval * e)))  
    st |= wikrt_zswap(cx);    // (ops * (opval * (stack * e)))  
    st |= wikrt_wswap(cx);    // (opval * (ops * (stack * e)))  
    st |= wikrt_assocl(cx);   // ((opval * ops) * (stack * e))  
    st |= wikrt_wrap_sum(cx, WIKRT_INL); // ((opval:ops) * (stack * e))
    assert(WIKRT_OK == st);
}

static bool wikrt_fini_parse_text(wikrt_cx* cx, wikrt_parse_state* p) 
{
    assert(WIKRT_PARSE_TXT_LF == p->type);

    // Allocate the final chunk of text (if necessary and possible).
    if(!wikrt_flush_parse_text(cx, p)) { return false; }

    // Our text chunks are reverse ordered right now, e.g.
    //
    // we have  "World!" → ", " → "Hello"
    // we want  "Hello" → ", " → "World!"
    //
    // Of course, our chunks are much larger than that, based on
    // WIKRT_PARSE_BUFFSZ. Fortunately, repairing the order is a
    // trivial operation, and doesn't require allocation.
    wikrt_val hd = wikrt_pval(cx, cx->val)[0];
    wikrt_val txt = WIKRT_UNIT_INR;
    while(WIKRT_UNIT_INR != hd) {
        wikrt_val const t = hd;
        wikrt_val* const pt = wikrt_pval(cx, t);
        assert(wikrt_o(t) && wikrt_otag_text(*pt)); // assume text chunks!
        hd = pt[1]; // next chunk
        pt[1] = txt;
        txt = t;
    }
    wikrt_pval(cx, cx->val)[0] = txt;

    if(!wikrt_mem_reserve(cx, WIKRT_CELLSIZE)) { return false; }
    wikrt_fini_parse_opval_r(cx);

    return true;
}

static wikrt_err wikrt_step_parse_char(wikrt_cx* cx, wikrt_parse_state* p, uint32_t cp)
{ switch(p->type) {
    case WIKRT_PARSE_TOK: {
        _Static_assert(('}' == 125), "assuming '}' is 125"); 
        if('}' == cp) {

            // ensure valid token size
            if(0 == p->buffsz) { return WIKRT_TYPE_ERROR; }
            assert(p->buffsz < WIKRT_TOK_BUFFSZ);

            wikrt_sizeb const optoksz = wikrt_cellbuff(p->buffsz + sizeof(wikrt_val));
            wikrt_sizeb const szAlloc = WIKRT_CELLSIZE + optoksz;
            if(!wikrt_mem_reserve(cx, szAlloc)) { return WIKRT_CXFULL; }

            wikrt_addr const a = wikrt_alloc_r(cx, optoksz);
            wikrt_val* const pa = wikrt_paddr(cx, a);
            pa[0] = (p->buffsz << 8) | WIKRT_OTAG_OPTOK;
            memcpy(1 + pa, p->buff, p->buffsz);

            wikrt_intro_r(cx, wikrt_tag_addr(WIKRT_O, a));  // (optok * (ops * e))
            wikrt_assocl(cx);                               // ((optok * ops) * e)
            wikrt_wrap_sum(cx, WIKRT_INL);                  // ((optok : ops) * e)

            // done with token, parse next op
            p->type = WIKRT_PARSE_OP;

        } else { 
            _Static_assert((WIKRT_PARSE_BUFFSZ >= (UTF8_MAX_CP_SIZE + WIKRT_TOK_BUFFSZ)), 
                "assuming buffer sufficient for slightly oversized tokens");

            // ensure character is valid within a token
            if(!wikrt_token_char(cp)) { return WIKRT_TYPE_ERROR; }            

            // add character to buffer
            p->buffsz += utf8_writecp_unsafe((p->buff + p->buffsz), cp);
            if(p->buffsz >= WIKRT_TOK_BUFFSZ) { return WIKRT_TYPE_ERROR; }
        }
    } return WIKRT_OK;
    case WIKRT_PARSE_TXT_LF: {
        _Static_assert((10 == '\n'), "assuming '\\n' is 10, LF");
        _Static_assert((32 == ' '), "assuming ' ' is 32");
        _Static_assert((126 == '~'), "assuming '~' is 126");

        // Prior LF must be followed by SP or ~
        if(' ' == cp) { // SP escapes prior LF
            if(!wikrt_write_parse_text_char(cx, p, '\n')) { return WIKRT_CXFULL; }
            p->type = WIKRT_PARSE_TXT;
        } else if('~' == cp) { // ~ terminates text 
            if(!wikrt_fini_parse_text(cx, p)) { return WIKRT_CXFULL; }
            p->type = WIKRT_PARSE_OP;
        } else { return WIKRT_TYPE_ERROR; }
    } return WIKRT_OK;
    case WIKRT_PARSE_TXT: {
        _Static_assert((10 == '\n'), "assuming '\\n' is 10, LF");

        // Within an embedded ABC text.
        if('\n' == cp) { p->type = WIKRT_PARSE_TXT_LF; }
        else if(!wikrt_text_char(cp)) { return WIKRT_TYPE_ERROR; }
        else if(!wikrt_write_parse_text_char(cx, p, cp)) { return WIKRT_CXFULL; }

    } return WIKRT_OK;
    case WIKRT_PARSE_OP: {
        // forbid localization other than the ASCII subset of Unicode...
        _Static_assert(('[' == 91), "assuming '[' is 91");
        _Static_assert((']' == 93), "assuming ']' is 93");
        _Static_assert(('{' == 123), "assuming '{' is 123");
        _Static_assert(('"' == 34), "assuming '\"' is 34");

        size_t const worst_case_alloc = (2 * WIKRT_CELLSIZE); // for `opval block` wrapper
        if(!wikrt_mem_reserve(cx, worst_case_alloc)) { return WIKRT_CXFULL; }
        
        if('[' == cp) {

            wikrt_intro_r(cx, WIKRT_UNIT_INR);
            wikrt_assocl(cx);
            p->depth += 1;

        } else if(']' == cp) {

            if(p->depth < 1) { return WIKRT_TYPE_ERROR; } // underflow ']'
            p->depth -= 1;

            wikrt_reverse_opslist(cx);

            // wrap `ops` in (block ops) so we have a full value
            wikrt_val* const v = wikrt_pval(cx, cx->val);
            wikrt_alloc_cellval_r(cx, v, WIKRT_O, WIKRT_OTAG_BLOCK, (*v)); 
            wikrt_fini_parse_opval_r(cx);


        } else if('{' == cp) {

            p->type = WIKRT_PARSE_TOK;
            p->buffsz = 0;
            p->charct = 0;

        } else if('"' == cp) {

            wikrt_intro_r(cx, WIKRT_UNIT_INR);  // (text * ((ops*stack)*e))
            wikrt_assocl(cx);                   // ((text * (ops*stack))*e)
            p->type = WIKRT_PARSE_TXT;
            p->buffsz = 0;
            p->charct = 0;

        } else {

            bool const recognizedOpCode = (NULL != wikrt_abcd_expansion(cp));
            if(!recognizedOpCode) { return WIKRT_TYPE_ERROR; }
            _Static_assert((WIKRT_SMALLINT_MAX >= 0x10FFFF), "assuming chars are smallnums");
            wikrt_intro_r(cx, wikrt_i2v((wikrt_int)cp));

            // (op * (ops * e)) → ((op : ops) * e)
            wikrt_assocl(cx);
            wikrt_wrap_sum(cx, WIKRT_INL);
        }
    } return WIKRT_OK;
    default: {
        fprintf(stderr, "%s: invalid parser state (%d)\n", __FUNCTION__, p->type);
        abort();
    } return WIKRT_IMPL;
}}

/* Process a given buffer of text. 
 *
 * For the moment, my goal is to get this working correctly and keep it simple.
 * However, this will hurt performance in some cases - such as fast processing
 * of large texts. This assumes our text is valid utf8.
 */
static inline wikrt_err wikrt_step_parse(wikrt_cx* cx, wikrt_parse_state* p, uint8_t const* s, size_t buffsz) 
{
    uint8_t const* const s_end = s + buffsz;
    while(s_end != s) { 
        // parse character at a time for simplicity
        uint32_t const cp = utf8_step_unsafe(&s);
        wikrt_err const st = wikrt_step_parse_char(cx, p, cp);
        if(WIKRT_OK != st) { return st; }
    }
    return WIKRT_OK;
}

// (reversed ops * (unit * (emptyText * e))) → (block * e)
static wikrt_err wikrt_fini_parse(wikrt_cx* cx, wikrt_parse_state* p) 
{
    bool const okParseState = (0 == p->depth) && (WIKRT_PARSE_OP == p->type);

    wikrt_reverse_opslist(cx);
    wikrt_assocl(cx); 
    wikrt_wswap(cx); // (text * ((ops * unit) * e))

    // Drop the empty text.
    bool const atEndOfText = (WIKRT_UNIT_INR == wikrt_pval(cx, cx->val)[0]);
    wikrt_drop(cx, NULL);

    // Reuse the (ops * unit) cell as a (block ops) object.
    wikrt_val* const v = wikrt_pval(cx, cx->val);
    wikrt_addr const a = wikrt_vaddr(*v);
    wikrt_val* const pa = wikrt_paddr(cx, a);
    bool const atEndOfStack = (WIKRT_UNIT == pa[1]);
    pa[1] = pa[0]; //
    pa[0] = WIKRT_OTAG_BLOCK;
    (*v) = wikrt_tag_addr(WIKRT_O, a);

    bool const everythingIsOk = okParseState && atEndOfText && atEndOfStack;
    if(!everythingIsOk) { 
        wikrt_drop(cx, NULL); // drop our block
        return WIKRT_TYPE_ERROR; 
    }
    return WIKRT_OK;
}

/* Construct a block value from a text. 
 *
 * The current implementation will construct a basic linked list. Long
 * term, I'll want to compact operations tightly for performance. And I
 * may need to move tight inner loops into a special non-copying space. 
 *
 * I'm going to try to ensure a simple invariant on failure: that the only
 * change is the loss of our (alleged) text argument.
 */
wikrt_err wikrt_text_to_block(wikrt_cx* cx)
{
    _Static_assert(sizeof(char) == sizeof(uint8_t), "assuming safe cast between char* and uint8_t*");

    if(!wikrt_p(cx->val)) { 
        return WIKRT_TYPE_ERROR; 
    }

    wikrt_parse_state p;
    if(!wikrt_intro_parse(cx, &p)) { 
        wikrt_drop(cx,NULL); 
        return WIKRT_CXFULL; 
    }

    // read and process the text!
    size_t const max_read = WIKRT_PARSE_READSZ;
    char buff[max_read];
    size_t bytes_read;

    do { // (object * (stack * (text * e)))

        // Read a chunk of text into our read buffer.
        wikrt_assocl(cx); wikrt_wswap(cx); // swizzle text to top
        bytes_read = max_read;
        wikrt_read_text(cx, buff, &bytes_read, NULL);
        wikrt_wswap(cx); wikrt_assocr(cx); // swizzle text to bottom

        if(0 == bytes_read) { 
            // If we didn't read anything, we're finished.
            return wikrt_fini_parse(cx, &p); 
        } else {
            // Otherwise, process the read buffer.
            wikrt_err const status = wikrt_step_parse(cx, &p, (uint8_t const*) buff, bytes_read);
            if(WIKRT_OK != status) {
                // drop our object, stack, text values then fail.
                wikrt_drop(cx, NULL); wikrt_drop(cx, NULL); wikrt_drop(cx, NULL);
                return status;
            }
        }
    } while(true);
}
