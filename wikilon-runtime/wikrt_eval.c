
#include <assert.h>
#include "wikrt.h"

/*
 * Internal structure of a 'pending' value?
 *
 * A simplistic representation is a tagged (ops * value) pair. I apply
 * the list of ops to the value. This would probably be a good start at
 * least, though it probably isn't optimal for representing parallel
 * evaluation or stacked computations.
 *
 */



/* Construct an evaluation. ((a→b)*(a*e)) → ((pending b) * e).
 *
 * At the moment, this constructs a (pending (ops * value)) structure.
 * This means our wikrt_step_eval will always need to return a proper
 * list of operators when starting.  
 */
void wikrt_apply(wikrt_cx* cx) 
{
    wikrt_open_block_ops(cx);
    wikrt_assocl(cx);
    wikrt_wrap_otag(cx, WIKRT_OTAG_PEND);
}

/* Step through an evaluation.  ((pending a) * e) → (a * e).
 * Return 'true' if more evaluation could complete the computation.
 *
 * This 
 */ 
bool wikrt_step_eval(wikrt_cx* cx)
{
    wikrt_set_error(cx, WIKRT_IMPL);
    return false;
}





// (block * e) → (ops * e), returning otag
wikrt_otag wikrt_open_block_ops(wikrt_cx* cx) 
{
    // `block` is (OTAG_BLOCK ops) pair.
    _Static_assert(!WIKRT_NEED_FREE_ACTION, "free the 'block' tag");
    wikrt_val* const v = wikrt_pval(cx, cx->val);
    wikrt_val* const pblock = wikrt_pval(cx, (*v));
    wikrt_otag const otag = (*pblock);
    assert(WIKRT_OTAG_BLOCK == LOBYTE(otag));
    (*v) = pblock[1]; // dropping the OTAG
    return otag;
}
