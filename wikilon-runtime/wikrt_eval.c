
#include <assert.h>
#include "wikrt.h"

/*
 * Internal structure of a 'pending' value?
 *
 * A simplistic representation is a tagged (ops * value) pair. I apply
 * the list of ops to the value. This would probably be a good start at
 * least.
 *
 */


/* Construct an evaluation. ((a→b)*(a*e)) → ((pending b) * e). */
void wikrt_apply(wikrt_cx* cx) 
{
    wikrt_val* const pv = wikrt_pval(cx, cx->val);
    bool const ok_type = wikrt_p(cx->val) && wikrt_blockval(cx, pv[0]) && wikrt_p(pv[1]);
    if(!ok_type) { wikrt_set_error(cx, WIKRT_ETYPE); return; }

    wikrt_val* const pBlock = wikrt_pval(cx, pv[0]);
    assert(wikrt_otag_block(pBlock[0]));
    (*pv) = pBlock[1]; // drop the `block` tag, so we have `ops`.

    wikrt_assocl(cx); // (ops * value) pair
    wikrt_wrap_otag(cx, WIKRT_OTAG_PEND); // a pending value
}

/* Step through an evaluation.  ((pending a) * e) → (a * e).
 *
 * Return 'true' if more evaluation could complete the computation.
 */ 
bool wikrt_step_eval(wikrt_cx* cx)
{
    wikrt_set_error(cx, WIKRT_IMPL);
    return false;
}


