// This file is just for `wikrt_block_to_text` and any variants.
// The primary way to ser


#include <string.h>
#include <assert.h>
#include <stdio.h>
#include "wikrt.h"

// For the moment, this conversion to text is eager. The context
// will contain three values:
//
//   (1) the current list of operators we're processing
//   (2) a stack corresponding to `] continuation` for quoted blocks 
//   (3) a stack of 'text chunks' generated for output (in reverse order)
//
// This ordering also corresponds to the expected frequency of access.
// So our context can provide this as a stack of three items, i.e.
// (ops * (blocks * (texts * e))).
//
// Our `ops` can be used as a processing stack for quoted values, e.g.
// to process a quoted pair, first spread it into `fst snd w l`. The
// handling for quoted values will certainly be the most sophisticated
// part of our writer.
//
// A particular concern is values with WIKRT_OPVAL_LAZYKF, which may be
// inherited by one or more parent blocks. For this, I'll need extra data
// upon entering a block to track lazy attributes.
//
// In addition, I have a buffer that I'll be filling for the next text
// chunk. This buffer is limited by the maximum text chunk size (0xFFFF)
// but may be smaller (e.g. 16k). It should be large enough that I rarely
// need multiple output buffers except in unusual circumstances.
//

// A reasonable chunk size for our texts.
#define WIKRT_WRITE_BUFFSZ (16 * 1000)
typedef struct wikrt_writer_state 
{
    // bool       lazykf;
    wikrt_size depth;
    wikrt_size buffsz;
    wikrt_size charct;
    uint8_t    buff[WIKRT_WRITE_BUFFSZ];
} wikrt_writer_state;

// (block * e) → (opslist * e)
static bool wikrt_writer_unwrap_block(wikrt_cx* cx) 
{
    if(wikrt_p(cx->val)) {
        wikrt_val* const v = wikrt_pval(cx, cx->val);
        if(wikrt_o(*v)) {
            wikrt_val* const pv = wikrt_pval(cx, (*v));
            if(wikrt_otag_block(*pv)) {
                (*v) = pv[1];
                return WIKRT_OK;
            }
        }
    }
    return WIKRT_TYPE_ERROR;
}

static wikrt_err wikrt_writer_init(wikrt_cx* cx, wikrt_writer_state* w) 
{
    // (block * e) → (ops * e)
    wikrt_err const stUnwrap = wikrt_writer_unwrap_block(cx);
    if(WIKRT_OK != stUnwrap) { wikrt_drop(cx, NULL); return stUnwrap; }

    // space for `texts` and `stack`
    wikrt_size const szAlloc = 2 * WIKRT_CELLSIZE;
    if(!wikrt_mem_reserve(cx, szAlloc)) { wikrt_drop(cx, NULL); return WIKRT_CXFULL; }

    wikrt_intro_r(cx, WIKRT_UNIT_INR);  // initial texts
    wikrt_intro_r(cx, WIKRT_UNIT);      // initial stack
    wikrt_zswap(cx); wikrt_wswap(cx);   // swizzle ops to top

    w->depth  = 0; // empty stack
    w->buffsz = 0; // empty buffer
    w->charct = 0;

    return WIKRT_OK;
}

static bool wikrt_writer_flush(wikrt_cx* cx, wikrt_writer_state* w)
{
    _Static_assert((WIKRT_WRITE_BUFFSZ <= 0xFFFF), "write buffer too large to trivially flush");
    if(0 == w->buffsz) { return true; }

    // sanity check
    assert((w->buffsz <= WIKRT_WRITE_BUFFSZ)
        && (w->charct <= w->buffsz)
        && (w->buffsz <= (UTF8_MAX_CP_SIZE * w->charct)));
    
    wikrt_sizeb const szBuff  = wikrt_cellbuff(w->buffsz);
    wikrt_sizeb const szHdr   = 2 * WIKRT_CELLSIZE;
    wikrt_sizeb const szAlloc = szHdr + szBuff;
    if(!wikrt_mem_reserve(cx, szAlloc)) { return false; }

    wikrt_addr const addr_buff = wikrt_alloc_r(cx, szBuff);
    memcpy(wikrt_paddr(cx, addr_buff), w->buff, w->buffsz);

    // hdr is (OTAG_TEXT, next, (size-chars, size-bytes), buffer).
    wikrt_addr const addr_hdr = wikrt_alloc_r(cx, szHdr);
    wikrt_val* const phdr = wikrt_paddr(cx, addr_hdr);
    phdr[0] = WIKRT_OTAG_TEXT;
    phdr[2] = (w->charct << 16) | w->buffsz;
    phdr[3] = addr_buff;

    // (ops * (stack * (texts * e)))
    wikrt_assocl(cx); wikrt_wswap(cx); // swizzle texts to top

    // output written texts so far (in reverse chunk order)
    wikrt_val* const v = wikrt_pval(cx, cx->val);
    phdr[1] = (*v);
    (*v) = wikrt_tag_addr(WIKRT_O, addr_hdr);

    wikrt_wswap(cx); wikrt_assocr(cx); // return texts to bottom

    // clear the buffer
    w->buffsz = 0;
    w->charct = 0;

    return true;
}

static bool wikrt_writer_putchar(wikrt_cx* cx, wikrt_writer_state* w, uint32_t cp)
{
    _Static_assert((WIKRT_WRITE_BUFFSZ >= UTF8_MAX_CP_SIZE), "write buffer too small for safely processing text");
    w->charct += 1;
    w->buffsz += utf8_writecp_unsafe((w->buff + w->buffsz), cp);
    if(w->buffsz >= (WIKRT_WRITE_BUFFSZ - UTF8_MAX_CP_SIZE)) {
        return wikrt_writer_flush(cx, w);
    } else { return true; }
}


/* convert a block on the stack to text. */
wikrt_err wikrt_block_to_text(wikrt_cx* cx)
{
    wikrt_writer_state w;
    wikrt_err const stInit = wikrt_writer_init(cx, &w);
    if(WIKRT_OK != stInit) {
        wikrt_drop(cx, NULL); 
        return stInit; 
    }
    

#if 0
    // At this point we have (ops * (stack * (texts * e))).
    // Our output texts and stack should be empty. `ops` is filled.
    do {
        // Obtain our operation.
        wikrt_val const ops = wikrt_pval(cx, cx->val)[0];
        
    } while(true);
#else
    // so GCC doesn't complain about unused functions
    wikrt_writer_putchar(cx, &w, 'x');
#endif

    wikrt_drop(cx, NULL); wikrt_drop(cx, NULL); wikrt_drop(cx, NULL);
    return WIKRT_IMPL;
}

