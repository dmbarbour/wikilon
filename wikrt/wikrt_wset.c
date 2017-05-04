
#include <string.h>
#include <assert.h>
#include "wikrt_private.h"

typedef struct wikrt_wsd {
    wikrt_a     page;
    wikrt_n     bits;
} wikrt_wsd;

_Static_assert(sizeof(wikrt_wsd) >= sizeof(wikrt_o)
    , "using first wikrt_wsd as wikrt_o, but too small");

static inline wikrt_wsd* ws_data(wikrt_ws const* ws) 
{
    return 1 + (wikrt_wsd*)wikrt_v2a(ws->data);
}

static inline wikrt_z ws_page_index(wikrt_ws const* ws, wikrt_a const page) 
{
    wikrt_wsd const* const d = ws_data(ws);
    wikrt_z ix = (page * 4567) % ws->size;
    do {
        bool const match = (page == d[ix].page) || (0 == d[ix].bits);
        if(match) { return ix; }
        ix = (ix + 1) % ws->size;
    } while(1);
}

static bool wikrt_ws_resize(wikrt_thread* t, wikrt_z new_size)
{
    wikrt_ws* const ws = &(t->ws);
    assert(new_size > ws->fill);

    // determine if we have enough space. We'll not perform any GC here.
    wikrt_z const alloc_size = wikrt_cellbuff((1 + new_size) * sizeof(wikrt_wsd));
    bool const size_ok = (alloc_size < WIKRT_O_DATA_MAX) 
                       && wikrt_thread_mem_available(t, alloc_size);
    if(!size_ok) { return false; }

    // allocate a binary for the new write set
    wikrt_a const new_data = wikrt_thread_alloc(t, alloc_size);
    memset((void*)new_data, 0, alloc_size);
    *((wikrt_o*)new_data) = wikrt_new_obj_hdr(WIKRT_OTYPE_BINARY, alloc_size - sizeof(wikrt_o));

    // move data from the old write set to the new
    wikrt_ws new_ws = (wikrt_ws){0};
    new_ws.size = new_size;
    new_ws.data = WIKRT_VOBJ | new_data;
    for(wikrt_z ix = 0; ix < ws->size; ++ix) {
        wikrt_wsd const d = ws_data(ws)[ix];
        if(0 != d.bits) {
            wikrt_z const new_ix = ws_page_index(&new_ws, d.page);
            ws_data(&new_ws)[new_ix] = d;
            ++(new_ws.fill);
        }
    }

    assert(ws->fill == new_ws.fill);
    (*ws) = new_ws;
    return true;
}

bool wikrt_ws_prealloc(wikrt_thread* t, wikrt_z amt)
{
    wikrt_z const new_fill = amt + t->ws.fill;
    bool const overfilled = (3 * new_fill) > (2 * t->ws.size);
    if(!overfilled) { return true; }
    return wikrt_ws_resize(t, 6 + (2 * new_fill));
}

void wikrt_ws_clr(wikrt_thread* t)
{
    // clear all write-set entries
    if(0 == t->ws.fill) { return; }
    memset(ws_data(&(t->ws)), 0, (sizeof(wikrt_wsd) * t->ws.size));
    t->ws.fill = 0;
}


