
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "wikrt.h"


/* At the moment, I'm just using GC as a heuristic to decide how
 * much evaluation to perform. I'm currently checking whenever I
 * reach the end of a function block. 
 *
 * If an error is infinite recursion, evaluation will stop because
 * a `copy` fails and the stack depth will reach its limit.
 */
#define WIKRT_EVAL_COMPACTION_STEPS 4

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

static inline void wikrt_require_fresh_eval(wikrt_cx* cx)
{
    bool const is_fresh_eval = (WIKRT_REG_CC_INIT == cx->cc) && (WIKRT_REG_PC_INIT == cx->pc);
    if(!is_fresh_eval) { wikrt_set_error(cx, WIKRT_IMPL); }
}

static inline void wikrt_run_eval_token(wikrt_cx* cx, char const* token)
{
    if('&' == *token) {
        // TODO: handle annotations
    } else if('.' == *token) {
        char seal[WIKRT_TOK_BUFFSZ];
        wikrt_unwrap_seal(cx, seal);
        bool const match_seal = (':' == *seal) && (0 == strcmp(seal+1,token+1));
        if(!match_seal) { wikrt_set_error(cx, WIKRT_ETYPE); }
    } else if(':' == *token) {
        // big sealer tokens should be rare.
        wikrt_wrap_seal(cx, token);
    } else {
        wikrt_set_error(cx, WIKRT_IMPL);
    }
    
}

static void wikrt_run_eval_object(wikrt_cx* cx) 
{
    // handle extended operators: tokens and opvals.
    // The operator should be on the cx->val stack.
    assert(wikrt_p(cx->val) && wikrt_o(*wikrt_pval(cx, cx->val)));
    wikrt_val* const pv = wikrt_pval(cx, cx->val);
    wikrt_val* const pobj = wikrt_pobj(cx, *pv);

    if(wikrt_otag_opval(*pobj)) {
        _Static_assert(!WIKRT_NEED_FREE_ACTION, "todo: free WIKRT_OTAG_OPVAL cell");
        pv[0] = pobj[1]; 
    } else if(wikrt_otag_seal_sm(*pobj)) {
        if(!wikrt_p(pv[1])) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
        wikrt_pval_swap(pobj+1, wikrt_pval(cx, pv[1]));
        wikrt_wswap(cx);
        wikrt_elim_unit(cx);
    } else if(wikrt_otag_seal(*pobj)) {
        char tokbuff[WIKRT_TOK_BUFFSZ];
        wikrt_unwrap_seal(cx, tokbuff);
        wikrt_elim_unit(cx);
        wikrt_run_eval_token(cx, tokbuff);
    } else {
        wikrt_set_error(cx, WIKRT_IMPL);
        fprintf(stderr, "%s: unhandled operation (%d)\n", __FUNCTION__, (int) LOBYTE(*pobj));
        abort();
    }
}

static void wikrt_run_eval_operator(wikrt_cx* cx, wikrt_op op)
{
}
 
void wikrt_run_eval_step(wikrt_cx* cx) 
{
    // To resist infinite loops from tying up the client, I'll just use GC
    // compaction steps to estimate total work performed and limit it to
    // some maximum relative to start. 
    uint64_t const tick_stop = cx->compaction_count + WIKRT_EVAL_COMPACTION_STEPS;

    // Loop: repeatedly: obtain an operation then execute it.
    // Eventually I'll need a compact, high performance variant. 
    do { // Obtain an operation from cx->pc.
        if(WIKRT_PL == wikrt_vtag(cx->pc)) {
            wikrt_addr const addr = wikrt_vaddr(cx->pc);
            wikrt_val* const node = wikrt_paddr(cx,addr);
            wikrt_val const  op   = node[0];
            if(wikrt_smallint(op)) {
                _Static_assert(!WIKRT_NEED_FREE_ACTION, "review and repair: free program list cells");
                cx->pc  = node[1];
                wikrt_run_eval_operator(cx, (wikrt_op) wikrt_v2i(op));
            } else {
                cx->pc  = node[1];
                node[1] = cx->val;
                cx->val = wikrt_tag_addr(WIKRT_P, addr);
                wikrt_run_eval_object(cx);
            }
        } else if(WIKRT_UNIT_INR == cx->pc) {
            bool const abort_eval_step = (cx->compaction_count >= tick_stop) || wikrt_has_error(cx);
            if(abort_eval_step) { return; }

            wikrt_val* const pcc = wikrt_pval(cx, cx->cc);
            if(WIKRT_PL == wikrt_vtag(*pcc)) {  
                _Static_assert(!WIKRT_NEED_FREE_ACTION, "todo: free stack cons cell");
                wikrt_val* const pstack = wikrt_pval(cx, (*pcc));
                cx->pc = pstack[0]; // pop the call stack
                (*pcc) = pstack[1]; 
                continue;
            } else if(WIKRT_UNIT_INR == (*pcc)) {
                return; // execution complete! 
            } else {
                // This shouldn't be possible.
                fprintf(stderr, "%s: unhandled evaluation stack type\n", __FUNCTION__);
                abort();
            }
        } else {
            fprintf(stderr, "%s: unhandled (compact?) operations list type\n", __FUNCTION__);
            abort();
        }
    } while(true);
    
    #undef eval_timeout
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
 *
 * The stack is simply a list of ops-lists.
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
