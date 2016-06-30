
#include <assert.h>
#include "wikrt.h"

/*
 * Internal structure of a 'pending' value?
 *
 * Current proposal is:
 *
 *   (list of [op] * value)
 *
 * This representation is perhaps simplistic. This doesn't work for parallel
 * background evaluation. At least not by itself. It may be I need to consider
 * more explicit support for 'a value with holes' to handle those effectively.
 *
 * Anyhow, this gives me a simple 'stack' for partially completed function
 * calls, with O(1) effort to enter a new function call. It still takes a
 * fair bit of allocation to enter the function call.
 */



/* Construct an evaluation. ((a→b)*(a*e)) → ((pending b) * e).
 *
 * At the moment, this constructs a (pending (ops stack * value)) structure.
 * This means our wikrt_step_eval will always need to return a proper
 * list of operators when starting.  
 */
void wikrt_apply(wikrt_cx* cx) 
{
    wikrt_open_block_ops(cx);
    wikrt_wrap_list_singleton(cx);
    wikrt_assocl(cx);
    wikrt_wrap_otag(cx, WIKRT_OTAG_PEND);
}

/* Step through an evaluation.  ((pending a) * e) → (a * e).
 *
 * Perform a large but finite amount of evaluation. The amount
 * might be something clients can tune later at the cx or env 
 * level.
 *
 * Return 'true' if more evaluation could complete the computation.
 */ 
bool wikrt_step_eval(wikrt_cx* cx)
{
    bool const okType = wikrt_p(cx->val) && wikrt_otag_pend(*wikrt_pval(cx, cx->val));
    if(!okType) { wikrt_set_error(cx, WIKRT_ETYPE); return false; }
    wikrt_set_error(cx, WIKRT_IMPL);
    return false;
}





// (block * e) → (ops * e), returning otag
wikrt_otag wikrt_open_block_ops(wikrt_cx* cx) 
{
    // `block` is (OTAG_BLOCK ops) pair.
    bool const okType = wikrt_p(cx->val) && wikrt_blockval(cx, *wikrt_pval(cx, cx->val));
    if(!okType) { wikrt_set_error(cx, WIKRT_ETYPE); return 0; }

    _Static_assert(!WIKRT_NEED_FREE_ACTION, "free the 'block' tag");
    wikrt_val* const v = wikrt_pval(cx, cx->val);
    wikrt_val* const pblock = wikrt_pval(cx, (*v));
    wikrt_otag const otag = pblock[0];
    assert(wikrt_otag_block(otag));
    (*v) = pblock[1]; // dropping the OTAG
    return otag;
}
