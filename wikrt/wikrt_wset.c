
/* Write Set
 *
 * The write set is a hashtable from a memory 'page' to a small bitfield
 * that records a set of written addresses, essentially a table of dirty
 * pages in memory. This is more efficient than encoding one address per
 * table element if we have more than two writes per dirty page.
 *
 * The cost of this optimization is that it complicates iteration logic,
 * since the iterator must scan both
 * 
 */

#include <string.h>
#include <assert.h>
#include "wikrt_private.h"

#if (UINTPTR_MAX == UINT64_MAX)
#define WIKRT_WS_ALIGN_BITS 3
#define WIKRT_WS_FIELD_BITS 6
#elif (UINTPTR_MAX == UINT32_MAX)
#define WIKRT_WS_ALIGN_BITS 2
#define WIKRT_WS_FIELD_BITS 5
#else
#error "Expecting 32-bit or 64-bit system."
#endif

#define WIKRT_WS_PAGE_BITS (WIKRT_WS_ALIGN_BITS + WIKRT_WS_FIELD_BITS)
#define WIKRT_WS_MASK_LOWER(NBITS) ((((uintptr_t)1) << NBITS) - 1)
#define WIKRT_WS_ALIGN_MASK WIKRT_WS_MASK_LOWER(WIKRT_WS_ALIGN_BITS)
#define WIKRT_WS_FIELD_MASK WIKRT_WS_MASK_LOWER(WIKRT_WS_FIELD_BITS)
#define WIKRT_WS_PAGE_MASK WIKRT_WS_MASK_LOWER(WIKRT_WS_PAGE_BITS)

typedef struct wikrt_wsd {
    wikrt_a page;
    wikrt_n bits;
} wikrt_wsd;
_Static_assert(sizeof(wikrt_wsd) >= sizeof(wikrt_v)
    , "using first wikrt_wsd as wikrt_o, but too small");

static inline wikrt_z ws_bits_offset(wikrt_a a) { 
    return ((a >> WIKRT_WS_ALIGN_BITS) & WIKRT_WS_FIELD_MASK); }
static inline wikrt_a ws_addr_to_page(wikrt_a a) {
    return (a >> WIKRT_WS_PAGE_BITS);
}

static inline wikrt_wsd ws_field_to_wsd(wikrt_v* f) {
    wikrt_a const a = (wikrt_a)f;
    assert(0 == (a & WIKRT_WS_ALIGN_MASK)); // enforce alignment
    return (wikrt_wsd){ .page = ws_addr_to_page(a)
                      , .bits = (((wikrt_n)1) << ws_bits_offset(a))
                      };
}

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


void wikrt_ws_clr(wikrt_ws* ws)
{
    // clear all write-set entries
    if(0 == ws->fill) { return; }
    memset(ws_data(ws), 0, (sizeof(wikrt_wsd) * ws->size));
    ws->fill = 0;
}

void wikrt_ws_add(wikrt_ws* ws, wikrt_v* field) 
{
    // adds generally only need to set one bit
    wikrt_wsd const d = ws_field_to_wsd(field);
    wikrt_z const ix = ws_page_index(ws, d.page);
    wikrt_wsd* const p = ws_data(ws) + ix;
    if(0 == p->bits) { ++(ws->fill); (*p) = d; }
    else { p->bits = (p->bits | d.bits); }
}

void wikrt_ws_rem(wikrt_ws* ws, wikrt_v* field)
{
    wikrt_wsd const d = ws_field_to_wsd(field);
    wikrt_z ix = ws_page_index(ws, d.page);
    wikrt_wsd* const data = ws_data(ws);
    if(0 == data[ix].bits) { return; } // field not listed

    // clear the bit associated with the field
    data[ix].bits = (data[ix].bits & ~(d.bits));
    if(0 != data[ix].bits) { return; } // other fields remain

    // field cleared
    assert(ws->fill > 0);
    --(ws->fill);

    // relocate prior collisions in hashtable
    do {
        ix = (ix + 1) % ws->size;
        if(0 == data[ix].bits) { return; } 
        wikrt_z const new_ix = ws_page_index(ws,data[ix].page);
        if(new_ix != ix) { // repair collison
            data[new_ix]  = data[ix];
            data[ix].bits = 0;
        }
    } while(1);
}

wikrt_v* wikrt_ws_iter(wikrt_ws const* ws, wikrt_v* last)
{
    // Iterate through the write set. Pages are iterated via index in
    // the hashtable, but within each page we'll scan from the lowest
    // address to the highest. An assumption is that the write set is
    // not modified during iteration.
    wikrt_a const a = (wikrt_a)last;
    wikrt_z at_page = (0 == a) ? 0 : ws_page_index(ws, ws_addr_to_page(a));
    wikrt_z at_bit  = (0 == a) ? 0 : 1 + ws_bits_offset(a);

    wikrt_wsd const* const data = ws_data(ws);
    while(at_page < ws->size) {
        wikrt_wsd const* const p = data + at_page;
        if(0 != (p->bits >> at_bit)) { 
            while(0 == (p->bits & ((wikrt_n)1 << at_bit))) { ++at_bit; }
            return (wikrt_v*)((p->page << WIKRT_WS_PAGE_BITS) 
                            + (at_bit * sizeof(wikrt_v*)));
        }
        at_page += 1;
        at_bit   = 0;
    }
    return 0;
}


static bool wikrt_thread_ws_resize(wikrt_thread* t, wikrt_z new_size)
{
    wikrt_ws* const ws = &(t->ws);
    assert(new_size > ws->fill);

    // determine if we have enough space. We'll not perform any GC here.
    wikrt_z const alloc_size = wikrt_cellbuff((1 + new_size) * sizeof(wikrt_wsd));
    bool const alloc_ok = (alloc_size < WIKRT_HDR_DATA_MAX) 
                       && wikrt_thread_mem_prealloc(t, alloc_size);
    if(!alloc_ok) { return false; }

    // allocate a binary for the new write set
    wikrt_a const new_data = wikrt_thread_alloc(t, alloc_size);
    memset((void*)new_data, 0, alloc_size);
    *((wikrt_v*)new_data) = wikrt_binary_hdr(alloc_size - sizeof(wikrt_v));

    // move data from the old write set to the new space
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

bool wikrt_thread_write_prealloc(wikrt_thread* t, wikrt_z amt)
{
    wikrt_z const new_fill = amt + t->ws.fill;
    bool const overfilled = (3 * new_fill) > (2 * t->ws.size);
    if(!overfilled) { return true; }
    return wikrt_thread_ws_resize(t, 6 + (2 * new_fill));
}


