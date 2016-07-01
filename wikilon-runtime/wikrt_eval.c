
#include <assert.h>
#include "wikrt.h"

/*
 * A pending value is just a tagged (block * value) pair.
 *
 * This gives a very simplistic structure. It's inefficient to use during
 * evaluation. But if wikrt_step_eval is large enough, that's a non-issue.
 * I can reconstruct a block at the end of each step if I must in O(N) time
 * for the stack depth.
 *
 * This doesn't help much with parallel evaluations. When I develop parallel
 * evaluations, I'll need to be careful with how they are represented and 
 * how they interact with copying of data. Some sort of interaction with the
 * GC seems reasonable, i.e. checking to see of the value is complete and
 * reserving enough space for a prior representation of the value.
 */



/* Construct an evaluation. ((a→b)*(a*e)) → ((pending b) * e).
 *
 * At the moment, this constructs a (pending (block * value)) structure.
 * This means our wikrt_step_eval will always need to return a proper
 * list of operators when starting.  
 */
void wikrt_apply(wikrt_cx* cx) 
{
    bool const okType = wikrt_p(cx->val) && wikrt_blockval(cx, *wikrt_pval(cx, cx->val));
    if(!okType) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    wikrt_assocl(cx);
    wikrt_wrap_otag(cx, WIKRT_OTAG_PEND);
}

static inline bool wikrt_not_in_eval(wikrt_cx* cx) 
{
    return (WIKRT_REG_PC_INIT == cx->pc)
        && (WIKRT_REG_CC_INIT == cx->cc);
}

// assuming (a*b) and c, update it to be b and (a*c).
// i.e. shift the (a*_) cell to the right.
static inline void wikrt_move_cell_right(wikrt_cx* cx, wikrt_val* a, wikrt_val* b)
{
    assert(wikrt_p(*a));
    wikrt_pval_swap(1 + wikrt_pval(cx,(*a)), b); // (a*c) and b
    wikrt_pval_swap(a, b);                       // b and (a*c)
}



/* Step through an evaluation.  ((pending a) * e) → (a * e).
 *
 * Perform a large but finite amount of evaluation. The amount
 * might be something clients can tune later at the cx or env 
 * level.
 * 
 * During evaluation, cx->cc will contain both the stack of
 * continuations and the value we need to inject.

 and cx->pc.

 and  of the data currently in the context
 * will be hidden away in the register `cx->cc`. The register
 * `cx->pc` will be used for just the head  
 *
 */ 
bool wikrt_step_eval(wikrt_cx* cx)
{
    assert(wikrt_not_in_eval(cx)); // forbid recursive step_eval
    wikrt_open_pending(cx); // (pending * e) → ((ops stack * value) * e)
    if(wikrt_has_error(cx)) { return false; }
    

    wikrt_assocr(cx); // (ops stack * (


    wikrt_set_error(cx, WIKRT_IMPL);
    return false;
}


wikrt_otag wikrt_open_pending(wikrt_cx* cx)
{
    if(wikrt_p(cx->val)) {
        wikrt_val* const pv = wikrt_pval(cx, cx->val);
        if(wikrt_o(*pv)) {
            wikrt_val* const pobj = wikrt_pval(cx, *pv);
            wikrt_otag const otag = *pobj;
            if(wikrt_otag_pend(otag)) {
                _Static_assert(!WIKRT_NEED_FREE_ACTION, "free the 'pend' tag");
                (*pv) = pobj[1];
                return otag;
            }
        }
    }
    wikrt_set_error(cx, WIKRT_ETYPE); 
    return 0;
}

// (block * e) → (ops * e), returning otag
wikrt_otag wikrt_open_block_ops(wikrt_cx* cx) 
{
    if(wikrt_p(cx->val)) {
        wikrt_val* const pv = wikrt_pval(cx, cx->val);
        if(wikrt_o(*pv)) {
            wikrt_val* const pobj = wikrt_pval(cx, *pv);
            wikrt_otag const otag = *pobj;
            if(wikrt_otag_block(otag)) {
                _Static_assert(!WIKRT_NEED_FREE_ACTION, "free the 'block' tag");
                (*pv) = pobj[1];
                return otag;
            }
        }
    }
    wikrt_set_error(cx, WIKRT_ETYPE);
    return 0;
}
