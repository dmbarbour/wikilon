/* NOTES:
 *
 * Representation: Apply will produce a tagged (block * value) pair,
 * same as lazy values. Use of `wikrt_step_eval` will apply the block
 * within a finite effort quota, then return. 
 *
 * Effort Quota: I've decided to leave this decision to the client,
 * via wikrt_set_step_effort. The options are limited, but should
 * cover most use cases.
 *
 * Tail Call Optimization: Long-running loops rely on tail calls 
 * to control memory usage for loopy code. I'll be sure to recognize
 * `[...$c]` patterns in the parser. This enables tail calls without
 * requiring a full optimization pass.
 * 
 * 
 * 
 *
 * The 'tail call optimization' or TCO involves recognizing a `$c`
 * operator sequence at the end of a function call. It allows a
 * loopy computation to continue without increasing the call stack.
 * Some accelerators include the TCO, if performed at the end of
 * a block. So I have a choice:
 *
 *  * recognize a TCO accelerator in the parser
 *  * recognize TCO `$c]` sequence in the evaluator
 *
 * I've decided to move this responsibility into the parser. 
 */



#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "wikrt.h"

/* reasonable 'effort' clocks
 *  The best available options are Linux specific.
 *  I might need to develop portability options, later.
 */
#if defined(CLOCK_BOOTTIME)
#define WIKRT_RT_CLOCK  CLOCK_BOOTTIME
#else 
#define WIKRT_RT_CLOCK  CLOCK_MONOTONIC
#endif

#define WIKRT_CPU_CLOCK CLOCK_THREAD_CPUTIME_ID


static uint64_t wikrt_time_effort(wikrt_cx* cx, clockid_t clk_id) 
{
    struct timespec tm;
    if(0 == clock_gettime(clk_id, &tm)) {
        // time to millisecs
        return (((uint64_t)tm.tv_sec)  *    1000)
             + (((uint64_t)tm.tv_nsec) / 1000000);
    } else {
        fprintf(stderr, "%s: failed to read clock for quota\n", __FUNCTION__);
        wikrt_set_error(cx, WIKRT_IMPL);
        return 0;
    }
}

static uint64_t wikrt_alloc_effort(wikrt_cx* cx)
{
    wikrt_size const alloc_bytes = cx->bytes_collected + wikrt_mem_in_use(cx);
    return (uint64_t)(alloc_bytes >> 20); // bytes → megabytes
}

uint64_t wikrt_effort_snapshot(wikrt_cx* cx)
{ switch(cx->effort_model) {
    case WIKRT_EFFORT_BLOCKS:       return cx->blocks_evaluated;
    case WIKRT_EFFORT_GC_CYCLES:    return cx->compaction_count;
    case WIKRT_EFFORT_MEGABYTES:    return wikrt_alloc_effort(cx);
    case WIKRT_EFFORT_MILLISECS:    return wikrt_time_effort(cx, WIKRT_RT_CLOCK);
    case WIKRT_EFFORT_CPU_TIME:     return wikrt_time_effort(cx, WIKRT_CPU_CLOCK);
    default: {
        wikrt_set_error(cx, WIKRT_INVAL);
        fprintf(stderr, "%s: unhandled wikrt_effort_model (%d)\n", 
            __FUNCTION__, (int)(cx->effort_model));
        return 0;
    } 
}}

// User control of 'wikrt_step_eval' effort.
void wikrt_set_step_effort(wikrt_cx* cx, wikrt_effort_model m, uint32_t e)
{
    cx->effort_model = m;
    cx->effort_value = e;
    wikrt_effort_snapshot(cx); // validate model
}

static inline void wikrt_eval_push_op(wikrt_cx* cx, wikrt_op op) 
{
    if(!wikrt_mem_reserve(cx, WIKRT_CELLSIZE)) { return; }
    cx->pc = wikrt_alloc_cellval_r(cx, WIKRT_PL, wikrt_i2v(op), cx->pc);
}

// (v*e) → e, with `v` added to head of `pc` as an opval.
static inline void wikrt_eval_push_opval(wikrt_cx* cx)
{
    // quote `v` into an opval
    wikrt_wrap_otag(cx, WIKRT_OTAG_OPVAL);

    // shift opval over to cx->pc
    if(!wikrt_p(cx->val)) { return; }
    wikrt_addr const a = wikrt_vaddr(cx->val);
    wikrt_val* const pa = wikrt_paddr(cx, a);
    cx->val = pa[1];
    pa[1] = cx->pc;
    cx->pc = wikrt_tag_addr(WIKRT_PL, a);
}


static void _wikrt_nop(wikrt_cx* cx) {  /* NOP */  }


static void _wikrt_sum_intro0(wikrt_cx* cx) 
{ 
    wikrt_wrap_sum(cx, WIKRT_INL); 
}
static void _wikrt_sum_elim0(wikrt_cx* cx) 
{
    wikrt_sum_tag lr;
    wikrt_unwrap_sum(cx, &lr);
    if(WIKRT_INL != lr) { wikrt_set_error(cx, WIKRT_ETYPE); }
}
static void _wikrt_sum_merge(wikrt_cx* cx) 
{
    wikrt_sum_tag lr;
    wikrt_unwrap_sum(cx, &lr);
    // do nothing with lr result
}
static void _wikrt_sum_assert(wikrt_cx* cx)
{
    wikrt_sum_tag lr;
    wikrt_unwrap_sum(cx, &lr);
    if(WIKRT_INR != lr) { wikrt_set_error(cx, WIKRT_ETYPE); }
}
static void _wikrt_accel_intro_void_left(wikrt_cx* cx)
{
    wikrt_wrap_sum(cx, WIKRT_INR);
}


static void wikrt_dK(wikrt_cx* cx, int32_t k) 
{
    // I could probably do faster integer building.
    // But it shouldn't be especially relevant with simplification.
    wikrt_intro_i32(cx, 10);
    wikrt_int_mul(cx);
    wikrt_intro_i32(cx, k);
    wikrt_int_add(cx);
}
static void _wikrt_intro_num(wikrt_cx* cx) { wikrt_intro_i32(cx, 0); }
static void _wikrt_d0(wikrt_cx* cx) { wikrt_dK(cx, 0); }
static void _wikrt_d1(wikrt_cx* cx) { wikrt_dK(cx, 1); }
static void _wikrt_d2(wikrt_cx* cx) { wikrt_dK(cx, 2); }
static void _wikrt_d3(wikrt_cx* cx) { wikrt_dK(cx, 3); }
static void _wikrt_d4(wikrt_cx* cx) { wikrt_dK(cx, 4); }
static void _wikrt_d5(wikrt_cx* cx) { wikrt_dK(cx, 5); }
static void _wikrt_d6(wikrt_cx* cx) { wikrt_dK(cx, 6); }
static void _wikrt_d7(wikrt_cx* cx) { wikrt_dK(cx, 7); }
static void _wikrt_d8(wikrt_cx* cx) { wikrt_dK(cx, 8); }
static void _wikrt_d9(wikrt_cx* cx) { wikrt_dK(cx, 9); }
static void _wikrt_int_cmp_gt(wikrt_cx* cx) 
{
//  G :: N(x) * (N(y) * e) → ((N(y)*N(x))+(N(x)*N(y)) * e -- y > x
//       #4 #2 G -- observes 4 > 2. Returns (N(2)*N(4)) on right.
    wikrt_ord gt;
    wikrt_int_cmp(cx, &gt);

    if(WIKRT_GT == gt) {
        wikrt_assocl(cx);
        wikrt_wrap_sum(cx, WIKRT_INR);
    } else {
        wikrt_wswap(cx);
        wikrt_assocl(cx);
        wikrt_wrap_sum(cx, WIKRT_INL);
    }
}

static inline bool wikrt_block_is_flagged_lazy(wikrt_otag otag) { 
    return (0 != (WIKRT_BLOCK_LAZY & otag));
}

static void _wikrt_eval_step_inline(wikrt_cx* cx) 
{
    // ([a→b]*a) → b. Equivalent to ABC code `vr$c`.
    wikrt_otag const tag = wikrt_open_block_ops(cx);
    if(wikrt_has_error(cx)) { return; }
    else if(0 != (WIKRT_BLOCK_LAZY & tag)) {
        // Lazy blocks produce pending values. 
        wikrt_wrap_otag(cx, WIKRT_OTAG_BLOCK); // drop other flags
        if(!wikrt_mem_reserve(cx, WIKRT_CELLSIZE)) { return; }
        cx->val = wikrt_alloc_cellval_r(cx, WIKRT_O, WIKRT_OTAG_PEND, cx->val);
    }
    else {
        wikrt_addr const addr = wikrt_vaddr(cx->val);
        wikrt_val* const node = wikrt_paddr(cx, addr);
        wikrt_val* const pctr = &(cx->pc);
        wikrt_val* const cstk = wikrt_pval(cx, cx->cc);
        cx->val = node[1];
        node[1] = (*cstk);
        (*cstk) = wikrt_tag_addr(WIKRT_PL, addr);
        if(WIKRT_UNIT_INR != (*pctr)) { 
            // this was not a tail call.
            wikrt_pval_swap(node, pctr); 
        }
    }
}
static void _wikrt_eval_step_tailcall(wikrt_cx* cx) 
{
    // ([a→b]*(a*unit))→b. 
    // Translate to an ([a→b]*a) inline operation.
    wikrt_assocl(cx);
    wikrt_elim_unit_r(cx);
    _wikrt_eval_step_inline(cx);
}
static void _wikrt_eval_step_apply(wikrt_cx* cx) 
{
    // ([a→b]*(a*e)) → (b*e) 
    // For simplicity, I'll just route this through the `inline` code,
    // even though it will never be in tail call position.
    wikrt_assocl(cx); wikrt_accel_swap(cx);  // (e * ([a→b]*a))
    wikrt_eval_push_op(cx, ACCEL_PROD_SWAP); 
    wikrt_eval_push_opval(cx); // quote `e`
    _wikrt_eval_step_inline(cx);
}
static void _wikrt_eval_step_condap(wikrt_cx* cx) 
{
    wikrt_wswap(cx); // (block * (sum * e)) → (sum * (block * e))
    wikrt_sum_tag lr;
    wikrt_unwrap_sum(cx, &lr);
    if(WIKRT_INR == lr) {
        wikrt_wrap_sum(cx, lr); // preserve sum type
        wikrt_wswap(cx);  
        wikrt_drop(cx);  // drop block, fails if relevant.
    } else {
        wikrt_eval_push_op(cx, OP_SUM_INTRO0); // return argument to left after apply.
        wikrt_wswap(cx);
        _wikrt_eval_step_apply(cx); // normal application of a block.
    }
}

static void wikrt_eval_join(wikrt_cx* cx)
{
    // The {&join} annotation serves a role similar to `seq` in Haskell.
    // It tells our runtime to wait upon a pending computation.
    
    // At the moment, pending computations are all modeled as (block*value)
    // pairs (hidden behind the `pending` tag). This might change in the 
    // future, e.g. for efficient asynch futures. 
    wikrt_open_pending(cx);
    wikrt_assocr(cx);
    _wikrt_eval_step_apply(cx);
}


typedef void (*wikrt_op_evalfn)(wikrt_cx*);
static const wikrt_op_evalfn wikrt_op_evalfn_table[OP_COUNT] = 
{ [OP_SP] = _wikrt_nop
, [OP_LF] = _wikrt_nop
, [OP_PROD_ASSOCL] = wikrt_assocl
, [OP_PROD_ASSOCR] = wikrt_assocr
, [OP_PROD_W_SWAP] = wikrt_wswap
, [OP_PROD_Z_SWAP] = wikrt_zswap
, [OP_PROD_INTRO1] = wikrt_intro_unit_r
, [OP_PROD_ELIM1] = wikrt_elim_unit_r
, [OP_SUM_ASSOCL] = wikrt_sum_assocl
, [OP_SUM_ASSOCR] = wikrt_sum_assocr
, [OP_SUM_W_SWAP] = wikrt_sum_wswap
, [OP_SUM_Z_SWAP] = wikrt_sum_zswap
, [OP_SUM_INTRO0] = _wikrt_sum_intro0
, [OP_SUM_ELIM0] = _wikrt_sum_elim0
, [OP_COPY] = wikrt_copy
, [OP_DROP] = wikrt_drop
, [OP_APPLY] = _wikrt_eval_step_apply
, [OP_COMPOSE] = wikrt_compose
, [OP_QUOTE] = wikrt_quote
, [OP_REL] = wikrt_block_rel
, [OP_AFF] = wikrt_block_aff
, [OP_NUM] = _wikrt_intro_num
, [OP_D0] = _wikrt_d0
, [OP_D1] = _wikrt_d1
, [OP_D2] = _wikrt_d2
, [OP_D3] = _wikrt_d3
, [OP_D4] = _wikrt_d4
, [OP_D5] = _wikrt_d5
, [OP_D6] = _wikrt_d6
, [OP_D7] = _wikrt_d7
, [OP_D8] = _wikrt_d8
, [OP_D9] = _wikrt_d9
, [OP_ADD] = wikrt_int_add
, [OP_MUL] = wikrt_int_mul
, [OP_NEG] = wikrt_int_neg
, [OP_DIV] = wikrt_int_div
, [OP_GT] = _wikrt_int_cmp_gt
, [OP_CONDAP] = _wikrt_eval_step_condap
, [OP_DISTRIB] = wikrt_sum_distrib
, [OP_FACTOR] = wikrt_sum_factor
, [OP_MERGE] = _wikrt_sum_merge
, [OP_ASSERT] = _wikrt_sum_assert

, [ACCEL_TAILCALL] = _wikrt_eval_step_tailcall
, [ACCEL_INLINE] = _wikrt_eval_step_inline
, [ACCEL_PROD_SWAP] = wikrt_accel_swap
, [ACCEL_INTRO_UNIT_LEFT] = wikrt_intro_unit
, [ACCEL_SUM_SWAP]  = wikrt_accel_sum_swap
, [ACCEL_INTRO_VOID_LEFT] = _wikrt_accel_intro_void_left
, [ACCEL_wrzw] = wikrt_accel_wrzw
, [ACCEL_wzlw] = wikrt_accel_wzlw
, [ACCEL_ANNO_TRACE] = wikrt_trace_write
, [ACCEL_ANNO_TRASH] = wikrt_trash
, [ACCEL_ANNO_LOAD] = wikrt_load
, [ACCEL_ANNO_STOW] = wikrt_stow
, [ACCEL_ANNO_LAZY] = wikrt_block_lazy
, [ACCEL_ANNO_FORK] = wikrt_block_fork
, [ACCEL_ANNO_JOIN] = wikrt_eval_join
, [ACCEL_ANNO_TEXT] = wikrt_anno_text
, [ACCEL_ANNO_BINARY] = wikrt_anno_binary
}; 

_Static_assert((WIKRT_ACCEL_COUNT == 17), 
    "evaluator is missing accelerators");

/* Construct an evaluation. ([a→b]*(a*e)) → ((future b) * e).
 *
 * At the moment, this constructs a (pending (block * value)) structure.
 * The function wikrt_step_eval will need to preserve this structure if
 * it returns `true`. Any flags on the block are dropped by apply.
 */
void wikrt_apply(wikrt_cx* cx) 
{
    wikrt_open_block_ops(cx); // validate type
    wikrt_wrap_otag(cx, WIKRT_OTAG_BLOCK); 
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
        // The parser will recognize annotations I want to handle
        // and replace them by the appropriate accelerators. For
        // unrecognized annotations, just ignore them.
    } else if('.' == *token) {
        char seal[WIKRT_TOK_BUFFSZ];
        wikrt_unwrap_seal(cx, seal);
        bool const match_seal = (':' == *seal) && (0 == strcmp(seal+1,token+1));
        if(!match_seal) { wikrt_set_error(cx, WIKRT_ETYPE); }
    } else if(':' == *token) {
        // big sealer tokens should be rare.
        wikrt_wrap_seal(cx, token);
    } else {
        // unrecognized tokens are errors.
        wikrt_set_error(cx, WIKRT_IMPL);
    }
    
}

static void wikrt_run_eval_object(wikrt_cx* cx) 
{
    // handle extended operators: tokens and opvals.
    // The operator should be on the cx->val stack.
    wikrt_val* const pv = wikrt_pval(cx, cx->val);
    assert(wikrt_p(cx->val) && wikrt_o(*pv));
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
    assert((OP_INVAL < op) && (op < OP_COUNT));
    wikrt_op_evalfn_table[op](cx);
}

static void wikrt_eval_loop(wikrt_cx* cx) 
{
    uint64_t const effort_start = wikrt_effort_snapshot(cx);
    do { 

        // Run basic ops in a tight loop.
        while(wikrt_pl(cx->pc)) { 
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
        }

        // TODO: Develop and handle 'compact' operations sequences.

        if(wikrt_has_error(cx)) { return; } // Halt on error.
        else if(WIKRT_UNIT_INR == cx->pc) {
            ++(cx->blocks_evaluated);
            wikrt_val* const pcc = wikrt_pval(cx, cx->cc);
            if(WIKRT_UNIT_INR == (*pcc)) { return; } // Halt on completion.

            // Halt on step quota if necessary.
            uint64_t const effort_current = wikrt_effort_snapshot(cx);
            uint64_t const step_effort = (effort_current - effort_start);
            bool const quota_reached = (step_effort >= cx->effort_value); 
            if(quota_reached) { return; } // Halt on quota.

            // If we haven't halted, pop the stack and continue.
            if(wikrt_pl(*pcc)) {
                _Static_assert(!WIKRT_NEED_FREE_ACTION, "todo: free stack cons cell");
                wikrt_val* const node = wikrt_pval(cx, (*pcc));
                cx->pc = node[0];
                (*pcc) = node[1];  
            } else {
                // This shouldn't be possible.
                fprintf(stderr, "%s: unhandled continuation stack type\n", __FUNCTION__);
                abort();
            }
        } else {
            fprintf(stderr, "%s: unhandled program list type (%d)\n", __FUNCTION__, (int) cx->pc);
            abort();
        }
    } while(true);
}

static void wikrt_eval_init(wikrt_cx* cx)
{
    assert((WIKRT_REG_CC_INIT == cx->cc) 
        && (WIKRT_REG_PC_INIT == cx->pc));
    cx->pc = WIKRT_UNIT_INR;
    cx->cc = WIKRT_UNIT_INR;

    wikrt_open_pending(cx);
    wikrt_pval_swap(wikrt_pval(cx, cx->val), &(cx->cc)); 
    wikrt_pval_swap(&(cx->val), &(cx->cc)); 
    wikrt_run_eval_operator(cx, ACCEL_INLINE); // begin this block
}

static void wikrt_eval_fini(wikrt_cx* cx)
{
    if(wikrt_has_error(cx)) { return; } // abandon effort

    // cc has (stack * e). Tuck `e` back into cx->val.
    wikrt_pval_swap(&(cx->val), wikrt_pval(cx, cx->cc));
    wikrt_pval_swap(&(cx->val), &(cx->cc));
    // cx->val is (v * e), cx->cc is just `stack`

    bool const complete = (WIKRT_UNIT_INR == cx->pc) 
                       && (WIKRT_UNIT_INR == cx->cc);
    if(complete) {
        cx->pc = WIKRT_REG_PC_INIT;
        cx->cc = WIKRT_REG_CC_INIT;
        wikrt_wrap_sum(cx, WIKRT_INR); // complete result in right
        return;
    }

    if(!wikrt_mem_reserve(cx, (2 * WIKRT_CELLSIZE))) { return; }

    // move cx->cc (continuation stack) onto cx->val.  (stack * (v * e))
    wikrt_intro_r(cx, WIKRT_REG_CC_INIT);  
    wikrt_pval_swap(&(cx->cc), wikrt_pval(cx, cx->val));
 
    // move cx->pc (current program list) onto cx->val. (ops * (stack * (v * e)))
    wikrt_intro_r(cx, WIKRT_REG_PC_INIT);  
    wikrt_pval_swap(&(cx->pc), wikrt_pval(cx, cx->val));

    // use cx->pc ops as an initial continuation. 
    // This is probably an empty block, but that's fine.
    wikrt_wrap_otag(cx, WIKRT_OTAG_BLOCK);

    wikrt_wswap(cx);
    wikrt_sum_tag lr;
    wikrt_unwrap_sum(cx, &lr);
    while((WIKRT_INL == lr) && !wikrt_has_error(cx)) {
        // we have ((ops*stack') * (cc * (val * e))) in cx->val
        wikrt_assocr(cx); 
        wikrt_wrap_otag(cx, WIKRT_OTAG_BLOCK); // ops → block
        wikrt_zswap(cx); 
        wikrt_wswap(cx);    // cc to top to compose cc before block
        wikrt_compose(cx);  // compose block and ops
        wikrt_wswap(cx);    // stack' back to top
        wikrt_unwrap_sum(cx, &lr); // continue loop
    }
    wikrt_elim_unit(cx); // remove empty stack.

    // we now have (cc * (val * e)). The `cc` captures the original 
    // call stack. It isn't guaranteed to exactly preserve structure,
    // e.g. compose may heuristically inline vs. quote. But it should
    // preserve behavior.
    wikrt_apply(cx); // back to the (future * e)
    wikrt_wrap_sum(cx, WIKRT_INL); // incomplete result in left
}


/* Step fully or partially through evaluation of a future value.
 * At the moment, this is just lazy futures. But parallel futures
 * are also viable.
 *
 *    ((future a) * e) → (((future a) + a) * e)
 *
 * During evaluation, the `e` value is tucked into cx->cc so it
 * can be reliably recovered whether or evaluation completes. I
 * also use a call stack (again in cx->cc). 
 */ 
void wikrt_step_eval(wikrt_cx* cx)
{
    // preliminary
    wikrt_require_fresh_eval(cx);
    if(wikrt_has_error(cx)) { return; }
    wikrt_eval_init(cx);
    wikrt_eval_loop(cx);
    wikrt_eval_fini(cx);
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
