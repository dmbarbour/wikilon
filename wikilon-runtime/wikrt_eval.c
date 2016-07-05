
#include <assert.h>
#include "wikrt.h"

/*
 * A pending value is just a tagged (block * value) pair.
 *
 * This gives a very simplistic structure between evaluations. During an
 * evaluation, however, I'll need to build a stack via `$` and `?` ops.
 * If an evaluation step should return before evaluation completes, I
 * rebuild a (block * value) pair from the stack.
 *
 * The 'stack' in question could simply be a list of `ops` lists.
 */

/* Construct an evaluation. ((a→b)*(a*e)) → ((pending b) * e).
 *
 * At the moment, this constructs a (pending (block * value)) structure.
 * The function wikrt_step_eval will need to preserve this structure if
 * it returns `true`.
 */
void wikrt_apply(wikrt_cx* cx) 
{
    bool const okType = wikrt_p(cx->val) && wikrt_blockval(cx, *wikrt_pval(cx, cx->val));
    if(!okType) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    wikrt_assocl(cx);
    wikrt_wrap_otag(cx, WIKRT_OTAG_PEND);
}

void wikrt_open_pending(wikrt_cx* cx)
{
    if(wikrt_p(cx->val)) {
        wikrt_val* const pv = wikrt_pval(cx, cx->val);
        if(wikrt_o(*pv)) {
            wikrt_val* const pobj = wikrt_pval(cx, *pv);
            wikrt_otag const otag = *pobj;
            if(wikrt_otag_pend(otag)) {
                _Static_assert(!WIKRT_NEED_FREE_ACTION, "free the 'pend' tag");
                (*pv) = pobj[1];
                return;
            }
        }
    }
    wikrt_set_error(cx, WIKRT_ETYPE); 
}

void wikrt_require_fresh_eval(wikrt_cx* cx)
{
    bool const is_fresh_eval = (WIKRT_REG_CC_INIT == cx->cc) && (WIKRT_REG_PC_INIT == cx->pc);
    if(!is_fresh_eval) { wikrt_set_error(cx, WIKRT_IMPL); }
}

// assuming (a*b) and c, update it to be b and (a*c).
// i.e. shift the (a*_) cell to the right.
static inline void wikrt_move_cell_right(wikrt_cx* cx, wikrt_val* a, wikrt_val* b)
{
    assert(wikrt_p(*a));
    wikrt_pval_swap(1 + wikrt_pval(cx,(*a)), b); // (a*c) and b
    wikrt_pval_swap(a, b);                       // b and (a*c)
}

void wikrt_run_eval_step(wikrt_cx* cx) 
{
}

/* Step through an evaluation.  
 *
 *    ((pending a) * e) → ((pending a) * e) on `true`
 *    ((pending a) * e) → (a * e) on `false` without errors
 *
 * The pending tag wraps a (block * value) pair. I'll keep a
 * stack of incomplete operations lists during evaluation.
 *
 * During evaluation, the `e` value is hidden and I need a
 * stack for performance reasons. Additionally, I want very
 * fast access to the operations list. So I'll use the two
 * eval registers as follows:
 *
 *    cx->pc will contain my operations list (program counter)
 *    cx->cc will contain a (stack, e) pair.
 * A couple registers are used for evaluation. First, the
 * `pc` register is used for the current list of operations.
 * Second, the `cc` register is used for the stack. (These
 * stand for `program counter` and `current continuation`.)
 */ 
bool wikrt_step_eval(wikrt_cx* cx)
{
    // preliminary
    wikrt_require_fresh_eval(cx);
    wikrt_open_pending(cx); // ((block * value) * e)
    if(wikrt_has_error(cx)) { return false; }

    // tuck `e` and an empty continuation stack into `cx->cc`. 
    assert(WIKRT_REG_CC_INIT == cx->cc);
    cx->cc = WIKRT_UNIT_INR;
    wikrt_pval_swap(wikrt_pval(cx, cx->val), &(cx->cc));
    wikrt_pval_swap(&(cx->val), &(cx->cc));

    // initialize `cx->pc` with the block's operations list. 
    // Remove as much indirection as feasible.
    assert(WIKRT_REG_PC_INIT == cx->pc);
    wikrt_open_block_ops(cx);
    wikrt_pval_swap(wikrt_pval(cx, cx->val), &(cx->pc));
    _Static_assert((WIKRT_REG_PC_INIT == WIKRT_UNIT), "assuming elim_unit for pc");
    wikrt_elim_unit(cx);

    // At this point: cx->cc and cx->pc are initialized.
    wikrt_run_eval_step(cx); // run main evaluation loop

    bool const finished = 
        (WIKRT_UNIT_INR == cx->pc) &&
        (WIKRT_UNIT_INR == wikrt_pval(cx, cx->cc)[0]);

    if(finished) {
        // recover the hidden `e` value from cx->cc
        wikrt_pval_swap(&(cx->val), wikrt_pval(cx, cx->cc));
        wikrt_pval_swap(&(cx->val), &(cx->cc));

        // restore the registers.
        cx->pc = WIKRT_REG_PC_INIT;
        cx->cc = WIKRT_REG_CC_INIT;

        return false;
    } else {
        // TODO: rebuild the `block` for the next evaluation step. Restore
        // the pending value structure and registers.
        wikrt_set_error(cx, WIKRT_IMPL);
        return !wikrt_has_error(cx);
    }
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
