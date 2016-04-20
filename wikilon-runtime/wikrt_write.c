// This file is just for `wikrt_block_to_text` and any variants.

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
// to process a quoted pair, first spread it into `fst snd w l`. 
//
// In addition, I have a buffer that I'll be filling for the next text
// chunk. This buffer is limited by the maximum text chunk size (0xFFFF)
// but may be smaller (e.g. 16k). It should be large enough that I rarely
// need multiple output buffers except in unusual circumstances.
//

// A reasonable chunk size for our texts.
#define WIKRT_WRITE_BUFFSZ (16 * 1000)
#define WIKRT_WRITER_INIT { 0 }
typedef struct wikrt_writer_state 
{
    wikrt_size buffsz;
    wikrt_size charct;
    uint8_t    buff[WIKRT_WRITE_BUFFSZ];
} wikrt_writer_state;

/* convert a block on the stack to text. */
wikrt_err wikrt_block_to_text(wikrt_cx* cx)
{
    wikrt_err status = WIKRT_IMPL;
    goto fail;


fail:
    assert(WIKRT_OK != status);
    wikrt_drop(cx, NULL);
    return status;
}

