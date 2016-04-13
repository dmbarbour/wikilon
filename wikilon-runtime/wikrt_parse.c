#include <string.h>
#include <assert.h>
#include <stdio.h>
#include "wikrt.h"

typedef enum 
{ WIKRT_PARSE_OP  = 0
, WIKRT_PARSE_TXT = 1
, WIKRT_PARSE_TOK = 2
} wikrt_parse_type;

typedef struct {
    wikrt_parse_type type;        // special parser states
    wikrt_size       depth;       // stack size, hierarchical depth of `[`
    char token[WIKRT_TOK_BUFFSZ]; // if within a token!
    wikrt_size       toksz;
} wikrt_parse_state;
#define WIKRT_PARSE_STATE_INIT (wikrt_parse_state) { 0 }

/* For performance, I'll try buffer reasonably large chunks of text. */
#define WIKRT_PARSE_CHUNKSZ (40 * 1000)

/* In addition to wikrt_parser_state, I'll use a simple state within the
 * context consisting of (ops * stack), where our `stack` is either unit
 * or has the same structure. Here `ops` is a list of operations for our
 * block of code. Our stack will have a size matching `depth` in parser
 * state.
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

static void wikrt_reverse_parse_ops(wikrt_cx* cx) 
{
    // assuming ((opslist * stack) * e) environment. 
    // Goal is to reverse the operations list.
    // This is non-allocating at this time.
    wikrt_assocr(cx); // shift `opslist` to top.
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
    wikrt_assocl(cx); // recover (opslist*stack) structure.
}

static wikrt_err wikrt_parse_stack_to_block(wikrt_cx* cx) 
{
    // assuming ((opslist * unit) * e) environment used for parsing.
    // output is (block * e). 
    //
    // Reusing (opslist, unit) cell for (WIKRT_OTAG_BLOCK, opslist).
    // Consequently, no allocation is required. This operation will
    // always succeed under the listed assumptions.

    wikrt_reverse_parse_ops(cx); // reverse the opslist!
    wikrt_val* const v = wikrt_pval(cx, cx->val);
    wikrt_val* const pv = wikrt_pval(cx, (*v));
    pv[1] = pv[0]; 
    pv[0] = WIKRT_OTAG_BLOCK;
    (*v) = wikrt_tag_addr(WIKRT_O, wikrt_vaddr(*v));
    return WIKRT_OK;
}

/* Process a given buffer of text. 
 *
 * It is possible for our buffer to halt in the middle of a text or token,
 * but we should at least have valid utf8 text (no stopping in the middle
 * of a codepoint). 
 */
wikrt_err wikrt_step_parse(wikrt_cx* cx, wikrt_parse_state* parse_state, char const* buff, size_t buffsz) 
{
    return WIKRT_IMPL;
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
    wikrt_err status = WIKRT_OK;
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    if(!wikrt_intro_parse(cx)) { wikrt_drop(cx,NULL); return WIKRT_CXFULL; }
    wikrt_parse_state parse_state = WIKRT_PARSE_STATE_INIT;

    // read and process the text!
    do { 
        size_t const max_read = WIKRT_PARSE_CHUNKSZ;
        char buff[max_read];
        size_t bytes_read = max_read;
        wikrt_wswap(cx); // (text * (parse stack * e))
        status = wikrt_read_text(cx, buff, &bytes_read, NULL);
        wikrt_wswap(cx);
        bool const okRead = ((WIKRT_OK == status) || (WIKRT_BUFFSZ == status));
        if(!okRead) { goto parse_fail; }
        if(0 == bytes_read) { goto reader_done; }
        status = wikrt_step_parse(cx, &parse_state, buff, bytes_read);
        if(WIKRT_OK != status) { goto parse_fail; }
     } while(true);

reader_done:
    assert(WIKRT_OK == status);
    wikrt_wswap(cx); // (text * (parse stack * e))
    bool const final_state_ok =
        (0 == parse_state.depth) &&
        (WIKRT_PARSE_OP == parse_state.type) &&
        (WIKRT_UNIT_INR == wikrt_pval(cx,cx->val)[0]);
    if(!final_state_ok) { status = WIKRT_TYPE_ERROR; goto parse_fail; }
    wikrt_drop(cx, NULL); // drop end of text.
    return wikrt_parse_stack_to_block(cx); // finalize the parse stack.

parse_fail: // bundle and drop block and text
    assert(WIKRT_OK != status);
    wikrt_assocl(cx); 
    wikrt_drop(cx, NULL);
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

