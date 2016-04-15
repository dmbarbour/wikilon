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

#define WIKRT_PARSE_STATE_INIT (wikrt_parse_state) { 0 }
#define WIKRT_PARSE_BUFFSZ (4 * 1024)
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

// Non-allocating cons operation.
// (op * ((ops * stack) * e)) → ((op:ops * stack) * e)
static void wikrt_cons_op(wikrt_cx* cx)
{
    #if 0

    // LONG FORM `wrzwlVl`
    wikrt_err st = WIKRT_OK;    // (a* ((l*s) * e))
    st |= wikrt_wswap(cx);  // ((l*s) * (a*e))
    st |= wikrt_assocr(cx); // (l*(s*(a*e)))
    st |= wikrt_zswap(cx);  // (l*(a*(s*e)))
    st |= wikrt_wswap(cx);  // (a*(l*(s*e)))
    st |= wikrt_assocl(cx); // ((a*l)*(s*e))
    st |= wikrt_wrap_sum(cx, WIKRT_INL); // ((a:l)*(s*e))
    st |= wikrt_assocl(cx); // ((a:l * s) * e)
    assert(WIKRT_OK == st);
 
    #else 
    
    // FAST FORM
    wikrt_addr const a = wikrt_vaddr(cx->val);
    wikrt_val* const pa = wikrt_paddr(cx, a);
    cx->val = pa[1]; // context is now ((l*s)*e)
    wikrt_val* const pl = wikrt_pval(cx, *(wikrt_pval(cx, cx->val)));
    // pa[0] remains stable
    pa[1] = (*pl);
    (*pl) = wikrt_tag_addr(WIKRT_PL, a);

    #endif
}


/* In addition to wikrt_parser_state, I'll use a simple state within the
 * context consisting of (ops * stack), where our `stack` is either unit
 * or has the same structure. Here `ops` is a list of operations for our
 * block of code. Our stack will have a size matching `depth` in parser
 * state.
 *
 * When parsing text, this instead becomes (texts * stack) with our text
 * consisting of a list of text chunks.
 *
 * For the moment, I'm just adding this to cx->val. I could use another
 * register, but I'd rather try to keep things closer to how I might
 * write a parser within AO code.
 */
static bool wikrt_intro_parse(wikrt_cx* cx) 
{
    size_t const szAlloc = 2 * WIKRT_CELLSIZE;
    if(!wikrt_mem_reserve(cx, szAlloc)) { return false; }
    wikrt_intro_r(cx, WIKRT_UNIT);      // end of stack
    wikrt_intro_r(cx, WIKRT_UNIT_INR);  // toplevel (reverse) list of ops
    wikrt_assocl(cx); // (opslist * 1) as initial stack value
    return true;
}

// Ops are initially constructed in a reverse-ordered list.
// I need to reverse this list ordering after all is done.
//   (reversed ops * e) → (ops * e)
static void wikrt_parser_reverse_ops(wikrt_cx* cx) 
{
    // assuming ((opslist * stack) * e) environment. 
    // Goal is to reverse the operations list.
    // The current implementation is non-allocating.
    wikrt_val hd = wikrt_pval(cx, cx->val)[0];
    wikrt_val tl = WIKRT_UNIT_INR;
    while(WIKRT_UNIT_INR != hd) 
    {
        assert(wikrt_pl(hd)); // assume plain old 'cons' list (no compact arrays)
        wikrt_val* const phd = wikrt_pval(cx, hd);
        wikrt_val const next_hd = phd[1];
        phd[1] = tl;
        tl = hd;
        hd = next_hd;
    }
    wikrt_pval(cx, cx->val)[0] = tl;
}

static wikrt_err wikrt_parse_stack_to_block(wikrt_cx* cx) 
{
    // assuming ((opslist * unit) * e) environment used for parsing.
    // output is (block * e). 
    //
    // Reusing (opslist, unit) cell for (WIKRT_OTAG_BLOCK, opslist).
    // Consequently, no allocation is required. This operation will
    // always succeed under the listed assumptions.

    wikrt_assocr(cx);
    wikrt_parser_reverse_ops(cx); // reverse ops list!
    wikrt_assocl(cx);

    wikrt_val* const v = wikrt_pval(cx, cx->val);
    wikrt_addr const a = wikrt_vaddr(*v);
    wikrt_val* const pa = wikrt_paddr(cx, a);
    assert(WIKRT_UNIT == pa[1]);
    pa[1] = pa[0];
    pa[0] = WIKRT_OTAG_BLOCK;
    (*v) = wikrt_tag_addr(WIKRT_O, a);
    return WIKRT_OK;
}

static bool wikrt_flush_parse_text(wikrt_cx* cx, wikrt_parse_state* p)
{
    if(0 == p->buffsz) { return true; } // nothing to flush

    wikrt_sizeb const szBuff = wikrt_cellbuff(p->buffsz);
    wikrt_sizeb const szAlloc = szBuff + (2 * WIKRT_CELLSIZE);
    if(!wikrt_mem_reserve(cx,szAlloc)) { return false; }

    // context should ((text * (ops*stack)) * e).
    // the text has a reversed chunk ordering.
    wikrt_val* const texts = wikrt_pval(cx, *(wikrt_pval(cx, cx->val)));

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

// (broken texts * e) → (text * e). 
static void wikrt_parser_reverse_texts(wikrt_cx* cx) 
{
    wikrt_val hd = wikrt_pval(cx, cx->val)[0];
    wikrt_val txt = WIKRT_UNIT_INR;
    while(WIKRT_UNIT_INR != hd) {
        assert(wikrt_o(hd));
        wikrt_val* const phd = wikrt_pval(cx, hd);
        assert(wikrt_otag_text(*phd));
        wikrt_val const hd_next = phd[1];
        phd[1] = txt;
        txt = hd;
        hd = hd_next;
    }
    wikrt_pval(cx, cx->val)[0] = txt;
}

static bool wikrt_fini_parse_text(wikrt_cx* cx, wikrt_parse_state* p) 
{
    assert(WIKRT_PARSE_TXT_LF == p->type);
    if(!wikrt_flush_parse_text(cx, p)) { return false; }
    if(!wikrt_mem_reserve(cx, WIKRT_CELLSIZE)) { return false; }

    // Context has shape: ((texts * (ops * stack)) * e)
    wikrt_assocr(cx); // (texts * ((ops * stack) * e))
    wikrt_parser_reverse_texts(cx); // repair ordering of texts.
    wikrt_val* const v = wikrt_pval(cx, cx->val);
    wikrt_alloc_cellval_r(cx, v, WIKRT_O, WIKRT_OTAG_OPVAL, (*v)); // text → (opval text)
    wikrt_cons_op(cx); // (((opval text):ops  * stack) * e)
    return true;
}

static wikrt_err wikrt_step_parse_char(wikrt_cx* cx, wikrt_parse_state* p, uint32_t cp)
{ switch(p->type) {
    case WIKRT_PARSE_TOK: {
        if('}' == cp) {
            if(0 == p->buffsz) { return WIKRT_TYPE_ERROR; }
            assert(p->buffsz < WIKRT_TOK_BUFFSZ);

            wikrt_sizeb const optoksz = wikrt_cellbuff(p->buffsz + sizeof(wikrt_val));
            wikrt_sizeb const szAlloc = WIKRT_CELLSIZE + optoksz;
            if(!wikrt_mem_reserve(cx, szAlloc)) { return WIKRT_CXFULL; }

            wikrt_addr const a = wikrt_alloc_r(cx, optoksz);
            wikrt_val* const pa = wikrt_paddr(cx, a);
            pa[0] = (p->buffsz << 8) | WIKRT_OTAG_OPTOK;
            memcpy(1 + pa, p->buff, p->buffsz);
            wikrt_intro_r(cx, a);
            wikrt_cons_op(cx);

            // done with token, parse next op
            p->type = WIKRT_PARSE_OP;
        } else { 
            // add character, if valid for a token, to the buffer
            if(!wikrt_token_char(cp)) { return WIKRT_TYPE_ERROR; }            
            _Static_assert((WIKRT_PARSE_BUFFSZ >= (4 + WIKRT_TOK_BUFFSZ)), "assuming safe overflow for tokens");
            p->buffsz += utf8_writecp_unsafe((p->buff + p->buffsz), cp);
            if(p->buffsz >= WIKRT_TOK_BUFFSZ) { return WIKRT_TYPE_ERROR; }
        }
    } return WIKRT_OK;
    case WIKRT_PARSE_TXT_LF: {
        // Prior LF must be followed by SP or ~
        if(32 == cp) { // SP escapes prior LF
            if(!wikrt_write_parse_text_char(cx, p, 10)) { return WIKRT_CXFULL; }
            p->type = WIKRT_PARSE_TXT;
        } else if(126 == cp) { // ~ terminates text 
            if(!wikrt_fini_parse_text(cx, p)) { return WIKRT_CXFULL; }
            p->type = WIKRT_PARSE_OP;
        } else { return WIKRT_TYPE_ERROR; }
    } return WIKRT_OK;
    case WIKRT_PARSE_TXT: {

        // Within an embedded ABC text.
        if(10 == cp) { p->type = WIKRT_PARSE_TXT_LF; }
        else if(!wikrt_text_char(cp)) { return WIKRT_TYPE_ERROR; }
        else if(!wikrt_write_parse_text_char(cx, p, cp)) { return WIKRT_CXFULL; }

    } return WIKRT_OK;
    case WIKRT_PARSE_OP: {
        size_t const worst_case_alloc = (2 * WIKRT_CELLSIZE);
        if(!wikrt_mem_reserve(cx, worst_case_alloc)) { return WIKRT_CXFULL; }
        
        if('[' == cp) {

            wikrt_intro_r(cx, WIKRT_UNIT_INR);
            wikrt_assocl(cx);
            p->depth += 1;

        } else if(']' == cp) {

            if(p->depth < 1) { return WIKRT_TYPE_ERROR; }
            p->depth -= 1;

            wikrt_assocr(cx);
            wikrt_parser_reverse_ops(cx);

            // Wrap ops → (opval (block ops))
            wikrt_val* const v = wikrt_pval(cx, cx->val);
            wikrt_alloc_cellval_r(cx, v, WIKRT_O, WIKRT_OTAG_BLOCK, (*v));
            wikrt_alloc_cellval_r(cx, v, WIKRT_O, WIKRT_OTAG_OPVAL, (*v));

            // add block to ops list.
            wikrt_cons_op(cx);

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
            wikrt_cons_op(cx);
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
 * of large texts. 
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
    wikrt_err status = WIKRT_IMPL;
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    if(!wikrt_intro_parse(cx)) { wikrt_drop(cx,NULL); return WIKRT_CXFULL; }
    wikrt_parse_state parse_state = WIKRT_PARSE_STATE_INIT;

    // read and process the text!
    size_t const max_read = WIKRT_PARSE_READSZ;
    char buff[max_read];
    size_t bytes_read;
    do { 
        wikrt_wswap(cx); // (text * (parse stack * e))
        bytes_read = max_read;
        status = wikrt_read_text(cx, buff, &bytes_read, NULL);
        wikrt_wswap(cx);
        bool const okRead = ((WIKRT_OK == status) || (WIKRT_BUFFSZ == status));
        if(!okRead) { goto parse_fail; }
        status = wikrt_step_parse(cx, &parse_state, (uint8_t const*)buff, bytes_read);
        if(WIKRT_OK != status) { goto parse_fail; }
     } while(0 != bytes_read);

    assert(WIKRT_OK == status);
    wikrt_wswap(cx); // (text * (parse stack * e))
    bool const final_state_ok =
        (0 == parse_state.depth) &&
        (WIKRT_PARSE_OP == parse_state.type) &&
        (WIKRT_UNIT_INR == wikrt_pval(cx,cx->val)[0]);
    if(!final_state_ok) { status = WIKRT_TYPE_ERROR; goto parse_fail; }
    wikrt_drop(cx, NULL); // drop end of text.
    return wikrt_parse_stack_to_block(cx); // finalize the parse stack.

parse_fail:
    assert(WIKRT_OK != status);
    wikrt_assocl(cx);     // bundle block and text into a pair
    wikrt_drop(cx, NULL); // drop the bundle
    return status;

}

/* Convert a block into a text.
 *
 * I'm contemplating a streaming variation of this process, i.e. via dedicated
 * WIKRT_OTAG.  
 */
wikrt_err wikrt_block_to_text(wikrt_cx* cx)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    if(!wikrt_mem_reserve(cx, WIKRT_CELLSIZE)) { wikrt_drop(cx, NULL); return WIKRT_CXFULL; }
    return WIKRT_IMPL;
}

