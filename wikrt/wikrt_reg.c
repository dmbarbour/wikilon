#include <assert.h>
#include <string.h>
#include "wikrt_private.h"

static inline wikrt_v* rtb_data(wikrt_rtb const* rtb) {
    return 1 + wikrt_v2p(rtb->data);
}
static inline wikrt_r* rtb_ids(wikrt_rtb const* rtb) {
    return (wikrt_r*)(WIKRT_CELLSIZE + wikrt_v2a(rtb->ids));
}

static wikrt_z rtb_index(wikrt_rtb const* rtb, wikrt_r const r) 
{
    wikrt_r const* const ids = rtb_ids(rtb);
    wikrt_v const* const data = rtb_data(rtb);
    wikrt_z ix = (r * 4567) % rtb->size; // small prime to spread registers
    do {
        bool const match = (r == ids[ix]) || (0 == data[ix]);
        if(match) { return ix; }
        ix = (ix + 1) % rtb->size; // linear collision
    } while(1);
}

static void rtb_clear(wikrt_rtb* const rtb, wikrt_r const r)
{
    if(0 == rtb->size) { return; }

    wikrt_v* const data = rtb_data(rtb);
    wikrt_r* const ids  = rtb_ids(rtb);
    wikrt_z ix = rtb_index(rtb, r);

    // clear the target register
    if(0 == data[ix]) { return; }
    assert(rtb->fill > 0);
    data[ix] = 0;
    --(rtb->fill);

    // shift linear collision registers into emptied slots
    do {
        ix = (ix + 1) % (rtb->size);
        if(0 == data[ix]) { return; }
        wikrt_z const new_ix = rtb_index(rtb, ids[ix]);
        if(new_ix != ix) {
            ids[new_ix] = ids[ix];
            data[new_ix] = data[ix];
            data[ix] = 0;
        }
    } while(1);
}

static void rtb_put(wikrt_rtb* rtb, wikrt_r r, wikrt_v v) 
{
    assert((rtb->size > rtb->fill) && (0 != v));
    wikrt_z const ix = rtb_index(rtb, r);
    wikrt_v* const data = rtb_data(rtb);
    if(0 == data[ix]) {
        rtb_ids(rtb)[ix] = r;
        ++(rtb->fill);
    }
    data[ix] = v;
}


static bool wikrt_rtb_resize(wikrt_cx* cx, wikrt_z new_size)
{
    assert(cx->rtb.fill < new_size); // reject data loss

    wikrt_z const data_bytes = wikrt_array_size(new_size);
    wikrt_z const id_bytes = wikrt_cellbuff(WIKRT_CELLSIZE + (sizeof(wikrt_r) * new_size));
    if(!wikrt_api_mem_prealloc(cx, (data_bytes + id_bytes))) { return false; }

    wikrt_a const data_addr = wikrt_api_alloc(cx, data_bytes);
    wikrt_a const ids_addr  = wikrt_api_alloc(cx, id_bytes);
    memset((void*)data_addr, 0, data_bytes);
    memset((void*)ids_addr, 0, id_bytes);
    *((wikrt_o*)ids_addr)  = wikrt_new_obj_hdr(WIKRT_OTYPE_BINARY, id_bytes - sizeof(wikrt_o));
    *((wikrt_o*)data_addr) = wikrt_new_obj_hdr(WIKRT_OTYPE_ARRAY, new_size);

    wikrt_rtb new_rtb = { 0 };
    new_rtb.ids  = ids_addr | WIKRT_VOBJ;
    new_rtb.data = data_addr | WIKRT_VOBJ;
    new_rtb.size = new_size;
    new_rtb.fill = 0;

    // copy old register data into new table
    wikrt_v* const old_data = rtb_data(&(cx->rtb));
    wikrt_r* const old_ids  = rtb_ids(&(cx->rtb));
    for(wikrt_z ix = 0; ix < cx->rtb.size; ++ix) 
    {
        if(0 != old_data[ix]) {
            rtb_put(&new_rtb, old_ids[ix], old_data[ix]);
        }
    }
    assert(cx->rtb.fill == new_rtb.fill);
    cx->rtb = new_rtb;
    return true;
}

bool wikrt_rtb_prealloc(wikrt_cx* cx, wikrt_z amt)
{
    // permit maximum 2/3 fill 
    wikrt_z const new_fill = amt + cx->rtb.fill;
    bool const overfilled = (3 * new_fill) > (2 * cx->rtb.size);
    if(!overfilled) { return true; }

    // resize to under 50% fill
    return wikrt_rtb_resize(cx, 3 + (2 * new_fill));
}

void wikrt_reg_set(wikrt_cx* cx, wikrt_r r, wikrt_v v) 
{
    if(0 == v) { rtb_clear(&(cx->rtb), r); }
    else { rtb_put(&(cx->rtb), r, v); }
}

wikrt_v wikrt_reg_get(wikrt_cx const* cx, wikrt_r r)
{
    wikrt_z const ix = rtb_index(&(cx->rtb), r);
    return rtb_data(&(cx->rtb))[ix];
}

void wikrt_reg_addend(wikrt_cx* cx, wikrt_r r, wikrt_v v) 
{
    if(0 == v) { return; } // NOP

    wikrt_v const v0 = wikrt_reg_get(cx, r);
    if(0 == v0) { wikrt_reg_set(cx, r, v); }
    else { 
        _Static_assert(WIKRT_REG_ADDEND_PREALLOC == WIKRT_CELLSIZE,
            "difference in prealloc and requirement");
        assert(wikrt_mem_avail(&(cx->memory), WIKRT_CELLSIZE));
        wikrt_reg_set(cx, r, wikrt_alloc_comp(&(cx->memory), v0, v));
    }
}

