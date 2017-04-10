#include <assert.h>
#include <string.h>
#include "wikrt_private.h"

void wikrt_reg_set(wikrt_cx* cx, wikrt_r const r, wikrt_v const v)
{
    // assumptions: 
    //   wikrt_reg_get moves target node to root if non-zero
    //   cx->memory has enough space for a node (WIKRT_REG_WRITE_PREALLOC)
    //   context is locked, operation is within mutex
    wikrt_v const v0 = wikrt_reg_get(cx,r);
    if(v0 == v) { /* no change */ return; }  
    else if(0 == v0) {
        // need new register table node
        assert(wikrt_thread_mem_available(&(cx->memory), WIKRT_REG_WRITE_PREALLOC));
        wikrt_a const a = wikrt_thread_alloc(&(cx->memory), WIKRT_REG_WRITE_PREALLOC);
        wikrt_rtb_node* const node = (wikrt_rtb_node*)a;
        node->otype = WIKRT_OTYPE_RTB_NODE;
        node->regid = r;
        node->data  = v;
        node->next  = cx->rtb;
        cx->rtb     = WIKRT_VOBJ | a;
    } else if(0 == v) {
        // clear old register value
        wikrt_rtb_node* const node = (wikrt_rtb_node*)wikrt_v2a(cx->rtb);
        assert(r == node->regid);
        cx->rtb = node->next;
    } else {
        // update existing register value
        wikrt_rtb_node* const node = (wikrt_rtb_node*)wikrt_v2a(cx->rtb);
        assert(r == node->regid);
        node->data = v;
    }
}

// obtaining a register will also move the node to the head of the
// register table, such that multiple operations on a small subset
// of registers should be relatively efficient.
wikrt_v wikrt_reg_get(wikrt_cx* cx, wikrt_r const r)
{
    // assumes lock on context
    wikrt_v* pn = &(cx->rtb);
    while(0 != *pn) {
        wikrt_v const n = *pn;
        wikrt_rtb_node* const node = (wikrt_rtb_node*)wikrt_v2a(n);
        if(r == node->regid) {
            // shift node to head
            (*pn) = node->next;
            node->next = cx->rtb;
            cx->rtb = n;

            // return non-zero register data 
            assert(0 != node->data);
            return node->data;
        } 
        pn = &(node->next);
    }
    return 0;
}

void wikrt_reg_write(wikrt_cx* cx, wikrt_r const r, wikrt_v const vw)
{
    // write a (v0, vw) logical composition cell. This addends the written
    // value vw to the current value v0. 
    assert(wikrt_thread_mem_available(&(cx->memory), WIKRT_REG_WRITE_PREALLOC));
    wikrt_v const v0 = wikrt_reg_get(cx, r);
    if(0 == v0) { 
        wikrt_reg_set(cx, r, vw); 
    } else {
        wikrt_a const a = wikrt_thread_alloc(&(cx->memory), WIKRT_CELLSIZE);
        wikrt_v* const p = (wikrt_v*)a;
        p[0] = v0;
        p[1] = vw;
        wikrt_reg_set(cx, r, (WIKRT_COMP | a));
    }
}


