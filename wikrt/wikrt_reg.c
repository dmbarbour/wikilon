#include <assert.h>
#include <string.h>
#include "wikrt_private.h"

static wikrt_n rtb_idx(wikrt_rtb const*, wikrt_r);
static void rtb_clear(wikrt_rtb*, wikrt_r);

static inline wikrt_r* rtb_ids(wikrt_rtb const* rtb) { 
    // WIKRT_OTYPE_BINARY, with light offset to ensure alignment
    _Static_assert((0 == (WIKRT_CELLSIZE % sizeof(wikrt_r))),
        "assuming safe alignment of register ID into cell");
    return (wikrt_r*)(WIKRT_CELLSIZE + wikrt_v2a(rtb->ids)); 
}
static inline wikrt_v* rtb_data(wikrt_rtb const* rtb) {
    // offset one word for the WIKRT_OTYPE_ARRAY header
    return (1 + wikrt_v2p(rtb->data));
}

// getting a register value always succeeds
wikrt_v wikrt_get_reg_val(wikrt_rtb const* rtb, wikrt_r const r) 
{
    if(0 == rtb->size) { return 0; } 
    wikrt_n const idx = rtb_idx(rtb, r);
    return rtb_data(rtb)[idx];
}

// assumes sufficent space has been preallocated
void wikrt_set_reg_val(wikrt_rtb* rtb, wikrt_r r, wikrt_v v)
{
    if(0 == v) { rtb_clear(rtb, r); return; } 
    wikrt_n const idx = rtb_idx(rtb, r);
    wikrt_v* const pdata = idx + rtb_data(rtb);
    if(0 == *pdata) {
        rtb_ids(rtb)[idx] = r;
        ++(rtb->fill);
    }
    (*pdata) = v;
}

static wikrt_n rtb_idx(wikrt_rtb const* rtb, wikrt_r r)
{
    // assumes non-empty table
    assert(0 != rtb->size);
    // simple linear collision hash option.
    wikrt_n idx = (r * 77977) % (rtb->size);
    do {
        bool const match = (r == rtb_ids(rtb)[idx])     // match ID
                        || (0 == rtb_data(rtb)[idx]);   // stop on empty slot
        if(match) { return idx; }
        idx = (idx + 1) % (rtb->size);
    } while(1);
}

// clearing a register always succeeds.
//
// Potential linear collision entries are relocated to preserve the
// hashtable structure without need for 'marked deleted' slots.
static void rtb_clear(wikrt_rtb* rtb, wikrt_r r)
{
    if(0 == rtb->size) { return; }
    wikrt_n idx = rtb_idx(rtb, r);
    if(0 == rtb_data(rtb)[idx]) { return; }

    assert(rtb->fill > 0);
    --(rtb->fill);
    rtb_data(rtb)[idx] = 0;

    // relocate all potential linear collision indices
    // (i.e. everything up to the first empty slot)
    do {
        idx = (idx + 1) % rtb->size;
        if(0 == rtb_data(rtb)[idx]) { return; }
        wikrt_n const dst = rtb_idx(rtb, rtb_ids(rtb)[idx]);
        if(dst != idx) {
            rtb_ids(rtb)[dst]  = rtb_ids(rtb)[idx];
            rtb_data(rtb)[dst] = rtb_data(rtb)[idx];
            rtb_data(rtb)[idx] = 0;
        }
    } while(1);
}

bool wikrt_grow_registers(wikrt_cx* cx, wikrt_rtb* rtb, wikrt_n new_size)
{
    assert(new_size > rtb->fill);
    wikrt_z const size_ids = WIKRT_CELLSIZE + wikrt_cellbuff(new_size * sizeof(wikrt_r));
    wikrt_z const size_data = wikrt_cellbuff((1 + new_size) * sizeof(wikrt_v));
    wikrt_z const size_total = size_ids + size_data;

    if(!wikrt_thread_mem_available(&(cx->memory), size_total)) {
        // out of context memory (might need to GC first).
        return false; 
    }

    // allocate and clear registers
    wikrt_a const a_ids = wikrt_thread_alloc(&(cx->memory), size_ids);
    wikrt_a const a_data = wikrt_thread_alloc(&(cx->memory), size_data);
    memset((void*)a_ids, 0, size_ids);
    memset((void*)a_data, 0, size_data);

    // set object headers
    *wikrt_a2p(a_ids) = ((size_ids - sizeof(wikrt_v)) << WIKRT_O_DATA_OFF) | WIKRT_OTYPE_BINARY;
    *wikrt_a2p(a_data) = (new_size << WIKRT_O_DATA_OFF) | WIKRT_OTYPE_ARRAY;

    // move old data into new table
    wikrt_rtb new_rtb = { 0 };
    new_rtb.size = new_size;
    new_rtb.ids  = WIKRT_VOBJ | a_ids;
    new_rtb.data = WIKRT_VOBJ | a_data;
    for(wikrt_n ix = 0; ix < rtb->size; ++ix) {
        if(0 != rtb_data(rtb)[ix]) {
            wikrt_set_reg_val(&new_rtb, rtb_ids(rtb)[ix], rtb_data(rtb)[ix]);
        }
    }
    assert(new_rtb.fill == rtb->fill);
    (*rtb) = new_rtb;
    return true;
}

bool wikrt_rtb_prealloc(wikrt_cx* cx, wikrt_rtb* rtb, wikrt_n amt)
{
    // conservatively, we'll permit up to a 2/3 fill on the hashtable.
    wikrt_n const new_fill = rtb->fill + amt;
    bool const space_ok = (rtb->size * 2) >= (new_fill * 3);
    if(space_ok) { return true; }
    // when we do grow the table, aim for just under a 50% fill ratio.
    wikrt_n const new_size = (new_fill * 2) + 1;
    return wikrt_grow_registers(cx, rtb, new_size);
}
    
