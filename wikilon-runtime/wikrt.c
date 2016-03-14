
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <sys/mman.h>
#include <errno.h>

#include "wikrt.h"

void wikrt_acquire_shared_memory(wikrt_cx* cx, wikrt_sizeb sz); 
static void wikrt_cx_init(wikrt_cxm*, wikrt_cx*);

char const* wikrt_abcd_operators() {
    // currently just pure ABC...
    return u8"lrwzvcLRWZVC%^ \n$o'kf#1234567890+*-QG?DFMK";
}

char const* wikrt_abcd_expansion(uint32_t opcode) { switch(opcode) {
    case ABC_PROD_ASSOCL: return "l";
    case ABC_PROD_ASSOCR: return "r";
    case ABC_PROD_W_SWAP: return "w";
    case ABC_PROD_Z_SWAP: return "z";
    case ABC_PROD_INTRO1: return "v";
    case ABC_PROD_ELIM1:  return "c";
    case ABC_SUM_ASSOCL:  return "L";
    case ABC_SUM_ASSOCR:  return "R";
    case ABC_SUM_W_SWAP:  return "W";
    case ABC_SUM_Z_SWAP:  return "Z";
    case ABC_SUM_INTRO0:  return "V";
    case ABC_SUM_ELIM0:   return "C";
    case ABC_COPY:        return "^";
    case ABC_DROP:        return "%";
    case ABC_SP:          return " ";
    case ABC_LF:          return "\n";
    case ABC_APPLY:       return "$";
    case ABC_COMPOSE:     return "o";
    case ABC_QUOTE:       return "'";
    case ABC_REL:         return "k";
    case ABC_AFF:         return "f";
    case ABC_NUM:         return "#";
    case ABC_D1:          return "1";
    case ABC_D2:          return "2";
    case ABC_D3:          return "3";
    case ABC_D4:          return "4";
    case ABC_D5:          return "5";
    case ABC_D6:          return "6";
    case ABC_D7:          return "7";
    case ABC_D8:          return "8";
    case ABC_D9:          return "9";
    case ABC_D0:          return "0";
    case ABC_ADD:         return "+";
    case ABC_MUL:         return "*";
    case ABC_NEG:         return "-";
    case ABC_DIV:         return "Q";
    case ABC_GT:          return "G";
    case ABC_CONDAP:      return "?";
    case ABC_DISTRIB:     return "D";
    case ABC_FACTOR:      return "F";
    case ABC_MERGE:       return "M";
    case ABC_ASSERT:      return "K";
    default: return NULL;
}}

char const* wikrt_strerr(wikrt_err e) { switch(e) {
    case WIKRT_OK:              return "no error";
    case WIKRT_INVAL:           return "invalid parameters, programmer error";
    case WIKRT_IMPL:            return "reached limit of runtime implementation";
    case WIKRT_DBERR:           return "filesystem or database layer error";
    case WIKRT_NOMEM:           return "out of memory (malloc or mmap failure)";
    case WIKRT_CXFULL:          return "context full, size quota reached";
    case WIKRT_BUFFSZ:          return "target buffer too small";
    case WIKRT_CONFLICT:        return "transaction conflict";
    case WIKRT_QUOTA_STOP:      return "evaluation effort quota reached";
    case WIKRT_TYPE_ERROR:      return "type mismatch";
    default:                    return "unrecognized error code";
}}

// assumes normal form utf-8 argument, NUL-terminated
bool wikrt_valid_token(char const* cstr) {
    _Static_assert((sizeof(char) == sizeof(uint8_t)), "invalid cast from char* to utf8_t*");
    uint8_t const* s = (uint8_t const*) cstr;
    size_t len = strlen(cstr);
    uint32_t cp;

    bool const validLen = ((0 < len) && (len < 64));
    if(!validLen) { return false; }

    while(len != 0) {
        if(!utf8_step(&s,&len,&cp) || !wikrt_token_char(cp)) {
            return false;
        }
    }
    return true;
}


wikrt_err wikrt_env_create(wikrt_env** ppEnv, char const* dirPath, uint32_t dbMaxMB) {
    _Static_assert(WIKRT_CELLSIZE == WIKRT_CELLBUFF(WIKRT_CELLSIZE), "cell size must be a power of two");

    (*ppEnv) = NULL;

    wikrt_env* const e = calloc(1, sizeof(wikrt_env));
    if(NULL == e) return WIKRT_NOMEM;

    e->mutex = (pthread_mutex_t) PTHREAD_MUTEX_INITIALIZER;

    if(!dirPath || (0 == dbMaxMB)) { 
        e->db = NULL;
    } else if(!wikrt_db_init(&(e->db), dirPath, dbMaxMB)) {
        free(e);
        return WIKRT_DBERR;
    }

    // thread pools? task lists? etc?

    (*ppEnv) = e;
    return WIKRT_OK;
}

void wikrt_env_destroy(wikrt_env* e) {
    assert(NULL == e->cxmlist);
    if(NULL != e->db) {
        wikrt_db_destroy(e->db);
    }
    pthread_mutex_destroy(&(e->mutex));
    free(e);
}

// trivial implementation via LMDB
void wikrt_env_sync(wikrt_env* e) {
    if(NULL != e->db) { 
        wikrt_db_flush(e->db); 
    }
}

static void wikrt_cx_init(wikrt_cxm* cxm, wikrt_cx* cx) {
    cx->cxm = cxm;
    cx->memory = cxm->memory;
    cx->val = WIKRT_UNIT;
    cx->txn = WIKRT_VOID;

    wikrt_cxm_lock(cxm); {
        cx->next = cxm->cxlist;
        if(NULL != cx->next) { cx->next->prev = cx; }
        cxm->cxlist = cx;
    } wikrt_cxm_unlock(cxm);
}

wikrt_err wikrt_cx_create(wikrt_env* e, wikrt_cx** ppCX, uint32_t sizeMB) 
{
    (*ppCX) = NULL;

    bool const bSizeValid = (WIKRT_CX_SIZE_MIN <= sizeMB) 
                         && (sizeMB <= WIKRT_CX_SIZE_MAX);
    if(!bSizeValid) return WIKRT_IMPL;
    wikrt_sizeb const sizeBytes = (wikrt_sizeb) ((1024 * 1024) * sizeMB);

    wikrt_cxm* const cxm = calloc(1,sizeof(wikrt_cxm));
    wikrt_cx* const cx = calloc(1,sizeof(wikrt_cx));
    if((NULL == cxm) || (NULL == cx)) { goto callocErr; }

    static int const prot = PROT_READ | PROT_WRITE | PROT_EXEC;
    static int const flags = MAP_ANONYMOUS | MAP_PRIVATE;
    void* const memory = mmap(NULL, sizeBytes, prot, flags, -1, 0); 
    if(NULL == memory) { goto mmapErr; }

    cxm->cxlist = cx;
    cxm->env = e;
    cxm->mutex = (pthread_mutex_t) PTHREAD_MUTEX_INITIALIZER;
    cxm->memory = memory;
    cxm->size = sizeBytes;

    // I'll block cell 0 from allocation (it's used for 'unit'.)
    // I'll also delay first page of allocations until the end. 
    wikrt_fl_free(memory, &(cxm->fl), (WIKRT_PAGESIZE - WIKRT_CELLSIZE), WIKRT_CELLSIZE); // first page minus first cell
    wikrt_fl_free(memory, &(cxm->fl), (sizeBytes - WIKRT_PAGESIZE), WIKRT_PAGESIZE); // all pages after the first

    // initialize thread-local context
    wikrt_cx_init(cxm, cx);

    // insert into environment's list of contexts
    wikrt_env_lock(e); {
        cxm->next = e->cxmlist;
        if(NULL != cxm->next) { cxm->next->prev = cxm; }
        e->cxmlist = cxm;
    } wikrt_env_unlock(e);

    (*ppCX) = cx;
    return WIKRT_OK;

mmapErr:
callocErr:
    free(cxm);
    free(cx);
    return WIKRT_NOMEM;
}

wikrt_err wikrt_cx_fork(wikrt_cx* cx, wikrt_cx** pfork)
{
    wikrt_cxm* const cxm = cx->cxm;
    (*pfork) = calloc(1, sizeof(wikrt_cx));
    if(NULL == (*pfork)) { return WIKRT_NOMEM; }
    wikrt_cx_init(cxm, (*pfork));
    return WIKRT_OK;
}

void wikrt_cx_destroy(wikrt_cx* cx) 
{
    // drop bound values to recover memory
    wikrt_drop_v(cx, cx->val, NULL);    cx->val = WIKRT_VOID;
    wikrt_txn_abort(cx);

    // remove from cxm
    wikrt_cxm* const cxm = cx->cxm;
    wikrt_cxm_lock(cxm); {
        wikrt_fl_merge(cx->memory, &(cx->fl), &(cxm->fl));
        if(NULL != cx->next) { cx->next->prev = cx->prev; }
        if(NULL != cx->prev) { cx->prev->next = cx->next; }
        else { assert(cx == cxm->cxlist); cxm->cxlist = cx->next; }
    } wikrt_cxm_unlock(cxm);
    
    free(cx);

    if(NULL == cxm->cxlist) {
        // last context for this memory destroyed.
        // remove context memory from environment
        wikrt_env* const e = cxm->env;
        wikrt_env_lock(e); {
            if(NULL != cxm->next) { cxm->next->prev = cxm->prev; }
            if(NULL != cxm->prev) { cxm->prev->next = cxm->next; }
            else { assert(cxm == e->cxmlist); e->cxmlist = cxm->next; }
        } wikrt_env_unlock(e);

        // release memory back to operating system
        errno = 0;
        int const unmapStatus = munmap(cxm->memory, cxm->size);
        bool const unmapSucceeded = (0 == unmapStatus);
        if(!unmapSucceeded) {
            fprintf(stderr,"Failure to unmap memory (%s) when destroying context.\n", strerror(errno));
            abort();
        }
        pthread_mutex_destroy(&(cxm->mutex));
        free(cxm);
    }
}

wikrt_env* wikrt_cx_env(wikrt_cx* cx) {
    return cx->cxm->env;
}

wikrt_err wikrt_move(wikrt_cx* const lcx, wikrt_cx* const rcx) 
{
    wikrt_val const v = lcx->val;
    if(lcx == rcx) { return WIKRT_INVAL; }
    if(!wikrt_p(v)) { return WIKRT_TYPE_ERROR; }

    if(lcx->memory == rcx->memory) {
        // local move between forks, non-allocating.
        wikrt_val* const pv = wikrt_pval(lcx, wikrt_vaddr(v));
        lcx->val = pv[1]; 
        pv[1] = rcx->val; 
        rcx->val = v;
        return WIKRT_OK;
    } else {
        // fail-safe move via copy on rhs then drop on lhs.
        wikrt_err const st = wikrt_copy_move(lcx, NULL, rcx);
        if(WIKRT_OK == st) { wikrt_drop(lcx, NULL); } 
        return st;
    }
}

wikrt_err wikrt_copy(wikrt_cx* cx, wikrt_ss* ss)
{
    return wikrt_copy_m(cx, ss, cx);
}

wikrt_err wikrt_copy_move(wikrt_cx* const lcx, wikrt_ss* ss, wikrt_cx* const rcx)
{
    if(lcx == rcx) { return WIKRT_INVAL; }
    return wikrt_copy_m(lcx, ss, rcx);
}

wikrt_err wikrt_copy_m(wikrt_cx* lcx, wikrt_ss* ss, wikrt_cx* rcx)
{
    // variant of moving copy that procedurally allows for lcx == rcx.
    // in lcx == rcx case, the copy will be stacked with the original.
    wikrt_val const v = lcx->val;
    if(!wikrt_p(v)) { return WIKRT_TYPE_ERROR; }
    wikrt_val* const pv = wikrt_pval(lcx, wikrt_vaddr(v));

    // if lcx has (a * e), extract (a * 1) for copying (leave 'e' in lcx)    
    lcx->val = pv[1];
    pv[1] = WIKRT_UNIT;

    // copy (a * 1) into the right-hand side context
    wikrt_val copy = WIKRT_VOID;
    wikrt_err const status = wikrt_copy_v(lcx, v, ss, rcx, &copy);

    // rejoin the (a * 1) to the lcx
    pv[1] = lcx->val;
    lcx->val = v;

    if(WIKRT_OK == status) {
        // join the (a * 1) to the rcx
        wikrt_pval(rcx, wikrt_vaddr(copy))[1] = rcx->val;
        rcx->val = copy;
    }
    return status;
}

/* Our 'copy_stack' consists of a list of addresses whose lcx-side values we must
 * still copy. The stack is on the 'rcx' side to ensure copy only fails if the rcx
 * is full.
 */
static void wikrt_copy_step_next(wikrt_cx* rcx, wikrt_val* copy_stack, wikrt_val** dst) 
{
    wikrt_addr const addr = wikrt_vaddr(*copy_stack);
    wikrt_tag const tag = wikrt_vtag(*copy_stack);
    wikrt_val* const node = wikrt_pval(rcx, addr);
    if(0 == addr) { (*dst) = NULL; }
    else if(WIKRT_PL == tag) {
        // node is (addr, next)
        (*dst) = wikrt_pval(rcx, node[0]);
        (*copy_stack) = node[1];
        wikrt_free(rcx, WIKRT_CELLSIZE, addr);
    } else if(WIKRT_O == tag) {
        // node is (addr, step, count, next)
        (*dst) = wikrt_pval(rcx, node[0]);
        if(1 == node[2]) {
            (*copy_stack) = node[3];
            wikrt_free(rcx, (2 * WIKRT_CELLSIZE), addr);
        } else {
            node[0] += node[1];
            node[2] -= 1;
        }
    } else {
        // should never happen
        fprintf(stderr, "wikrt: invalid copy stack\n");
        abort();
    }
}

static inline bool wikrt_copy_add_task(wikrt_cx* rcx, wikrt_val* copy_stack, wikrt_addr addr) {
    return wikrt_alloc_cellval(rcx, copy_stack, WIKRT_PL, addr, (*copy_stack));
}

static inline bool wikrt_copy_add_arraytask(wikrt_cx* rcx, wikrt_val* copy_stack, 
    wikrt_addr addr, wikrt_size step, wikrt_size count) 
{
    if(1 == count) { return wikrt_copy_add_task(rcx, copy_stack, addr); }
    else { return wikrt_alloc_dcellval(rcx, copy_stack, addr, step, count, (*copy_stack)); }
}

/* compute number of cells in 'spine' of list or stack for slab copies. 
 */
static inline wikrt_size wikrt_spine_length(wikrt_cx* cx, wikrt_val v) 
{
    wikrt_size ct = 0;
    while(!wikrt_copy_shallow(v) && (WIKRT_O != wikrt_vtag(v))) 
    {
        v = wikrt_pval(cx, wikrt_vaddr(v))[1];
        ct += 1;
    }
    return ct;
}

wikrt_err wikrt_copy_v(wikrt_cx* lcx, wikrt_val lval, wikrt_ss* ss, wikrt_cx* rcx, wikrt_val* const rval)
{
    if(NULL != ss) { (*ss) = 0; }
    wikrt_val* dst = rval; // 'dst' is pointer to rcx copy containing lcx value.
    (*dst) = lval; // 'dst' starts by pointing to the lval
    wikrt_val copy_stack = WIKRT_UNIT_INR; // stack in rcx of future 'dst' targets
    do {
        wikrt_val const v0 = (*dst);

        // shallow copies don't need extra work.
        if(wikrt_copy_shallow(v0)) {
            wikrt_copy_step_next(rcx, &copy_stack, &dst);
            continue;
        }

        // dst points to a value reference in lcx->memory
        wikrt_tag const tag = wikrt_vtag(v0);
        wikrt_addr const addr = wikrt_vaddr(v0);
        wikrt_val const* const pv = wikrt_pval(lcx, addr);
        if(WIKRT_O != tag) {

            // tag is WIKRT_P, WIKRT_PL, or WIKRT_PR. For these cases, I want to
            // perform a 'slab' allocation. This risks fragmentation, but it also
            // reduces copy overheads and improves memory locality.
            wikrt_size const spineCt = 1 + wikrt_spine_length(lcx, pv[1]);
            wikrt_size const spineSize = WIKRT_CELLSIZE * spineCt;
            wikrt_addr spine;

            // allocation of spine and a task to load the spine
            if(!wikrt_alloc(rcx, spineSize, &spine)) { goto cxfull; }
            if(!wikrt_copy_add_arraytask(rcx, &copy_stack, spine, WIKRT_CELLSIZE, spineCt)) 
            {
                wikrt_free(rcx, spineSize, spine);
                goto cxfull;
            }

            // set our value to the head of our spine.
            (*dst) = wikrt_tag_addr(tag, spine);

            // fill internal structure of spine 
            wikrt_val const* orig = pv;
            for(wikrt_size ii = (spineCt - 1); ii > 0; --ii) 
            {
                wikrt_val* const pspine = wikrt_pval(rcx, spine);
                spine += WIKRT_CELLSIZE;
                pspine[0] = orig[0]; // handled by array task
                pspine[1] = wikrt_tag_addr(wikrt_vtag(orig[1]), spine); // final value
                orig = wikrt_pval(lcx, wikrt_vaddr(orig[1]));
            } // intra-spine filled (all but last element)

            // final element in spine.
            wikrt_val* const pspine = wikrt_pval(rcx, spine);
            pspine[0] = orig[0]; // handled by array task
            pspine[1] = orig[1]; // handled next
            dst = (1 + pspine);

        } else { switch(LOBYTE(*pv)) {

            case WIKRT_OTAG_SEAL_SM: // same as DEEPSUM
            case WIKRT_OTAG_DEEPSUM: {
                // (header, value) pairs, referenced via WIKRT_O tag.
                if(!wikrt_alloc_cellval(rcx, dst, WIKRT_O, pv[0], pv[1])) { goto cxfull; }
                dst = 1 + wikrt_pval(rcx, wikrt_vaddr(*dst)); // copy contained value
            } break;

            case WIKRT_OTAG_BLOCK: {
                // a naive (hdr, opcodes list) block representation. Need to track the
                // substructural attributes. The opcodes list is a normal list for copy
                // purposes, albeit with a few special value types (e.g. OPVAL, OPTOK).
                wikrt_capture_block_ss(*pv, ss);
                if(!wikrt_alloc_cellval(rcx, dst, WIKRT_O, pv[0], pv[1])) { goto cxfull; }
                dst = 1 + wikrt_pval(rcx, wikrt_vaddr(*dst));
            } break;

            case WIKRT_OTAG_OPTOK: {
                // A token within a block. Header includes token size.
                wikrt_size const toklen = ((*pv) >> 8) & 0x3F; // 1..63 is valid 
                wikrt_size const szAlloc = sizeof(wikrt_val) + toklen;
                wikrt_addr copy;
                if(!wikrt_alloc(rcx, szAlloc, &copy)) { goto cxfull; }
                memcpy(wikrt_pval(rcx, copy), pv, szAlloc);
                (*dst) = wikrt_tag_addr(WIKRT_O, copy);
                wikrt_copy_step_next(rcx, &copy_stack, &dst);
            } break;

            case WIKRT_OTAG_OPVAL: {
                // A value quoted within our block. This mostly needs some
                // special attention for analyzing substructural attributes.
                // e.g. for `[[foo]k]` we might partial-evaluate substructure,
                // but this mustn't hinder copying of the outer block.
                if(!wikrt_alloc_cellval(rcx, dst, WIKRT_O, pv[0], pv[1])) { goto cxfull; }
                dst = 1 + wikrt_pval(rcx, wikrt_vaddr(*dst));
                if((NULL != ss) && wikrt_opval_hides_ss(*pv)) {
                    // ignoring substructural attributes of opval body
                    if(!wikrt_copy_v(lcx, (*dst), NULL, rcx, dst)) { goto cxfull; }
                    wikrt_copy_step_next(rcx, &copy_stack, &dst);
                }
            } break;

            case WIKRT_OTAG_BIGINT: {
                // (size&sign, array of 32-bit digits in 0..999999999)
                wikrt_size const nDigits = (*pv) >> 9;
                wikrt_size const szAlloc = sizeof(wikrt_val) + (nDigits * sizeof(uint32_t));
                wikrt_addr copy;
                if(!wikrt_alloc(rcx, szAlloc, &copy)) { return WIKRT_CXFULL; }
                memcpy(wikrt_pval(rcx, copy), pv, szAlloc); // copy the binary data
                (*dst) = wikrt_tag_addr(WIKRT_O, copy); 
                wikrt_copy_step_next(rcx, &copy_stack, &dst);
            } break;

            case WIKRT_OTAG_SEAL: {
                // (len, value, token). token is adjacent to cell
                wikrt_size const len = ((*pv) >> 8) & 0x3F; // 1..63 is valid
                wikrt_size const szAlloc = WIKRT_CELLSIZE + len;
                wikrt_addr copy;
                if(!wikrt_alloc(rcx, szAlloc, &copy)) { return WIKRT_CXFULL; }
                memcpy(wikrt_pval(rcx, copy), pv, szAlloc);
                (*dst) = wikrt_tag_addr(WIKRT_O, copy);
                dst = 1 + wikrt_pval(rcx, copy); // copy the sealed value
            } break;

            default: {
                // unhandled types include stowage, arrays.
                fprintf(stderr, u8"wikrt: copy unrecognized value: %u→(%u,%u...)\n", (*dst), pv[0], pv[1]);
                return WIKRT_IMPL;
            }
        }}
    } while(NULL != dst);
    assert(WIKRT_UNIT_INR == copy_stack);
    return WIKRT_OK;

cxfull: 
    // copy fails due to only WIKRT_CXFULL.
    // clear the copy stack and destroy partial copy before returning.
    while(NULL != dst) {
        (*dst) = WIKRT_VOID;
        wikrt_copy_step_next(rcx, &copy_stack, &dst);
    }
    wikrt_drop_v(rcx, (*rval), NULL);
    (*rval) = WIKRT_VOID;
    return WIKRT_CXFULL;
}

wikrt_err wikrt_drop(wikrt_cx* cx, wikrt_ss* ss) 
{
    wikrt_val const v = cx->val;
    if(!wikrt_p(v)) { return WIKRT_TYPE_ERROR; }
    wikrt_val* const pv = wikrt_pval(cx, wikrt_vaddr(v));
    cx->val = pv[1];
    pv[1] = WIKRT_UNIT;
    wikrt_drop_v(cx, v, ss);
    return WIKRT_OK;
}

static void wikrt_drop_step_next(wikrt_cx* cx, wikrt_val* drop_stack, wikrt_val* tgt) 
{
    // basic list is (val, next); I will need a specialization for arrays.
    wikrt_tag const tag = wikrt_vtag(*drop_stack);
    wikrt_addr const addr = wikrt_vaddr(*drop_stack);
    wikrt_val* const node = wikrt_pval(cx, addr);
    if(0 == addr) { (*tgt) = WIKRT_VOID; return; }
    else if(WIKRT_PL == tag) {
        (*tgt) = node[0];
        (*drop_stack) = node[1];
        wikrt_free(cx, WIKRT_CELLSIZE, addr);
    } else {
        fprintf(stderr, "wikrt: invalid drop stack\n");
        abort();
    }
}

void wikrt_drop_v(wikrt_cx* cx, wikrt_val v, wikrt_ss* ss) 
{
    if(NULL != ss) { (*ss) = 0; }
    wikrt_val drop_stack = WIKRT_UNIT_INR; // list of values to drop
    do {
        if(wikrt_copy_shallow(v)) {
            if(WIKRT_UNIT_INR == drop_stack) { return; }
            wikrt_drop_step_next(cx, &drop_stack, &v);
            continue;
        }

        // value references cx->memory
        wikrt_tag const tag = wikrt_vtag(v);
        wikrt_addr const addr = wikrt_vaddr(v);
        wikrt_val* const pv = wikrt_pval(cx, addr);
    
        if(WIKRT_O != tag) {
            // tag is WIKRT_P, WIKRT_PL, or WIKRT_PR; addr points to cell
            v = pv[1]; // continue to delete spine of stack or list.
            pv[1] = drop_stack;
            drop_stack = wikrt_tag_addr(WIKRT_PL, addr);
        } else { switch(LOBYTE(*pv)) {

            case WIKRT_OTAG_SEAL_SM: // same as DEEPSUM
            case WIKRT_OTAG_DEEPSUM: {
                // (header, value) pair. 
                v = pv[1]; // free contained value
                wikrt_free(cx, WIKRT_CELLSIZE, addr);
            } break;

            case WIKRT_OTAG_BLOCK: {
                // track substructure of blocks
                wikrt_capture_block_ss(*pv, ss); // track substructure
                v = pv[1];
                wikrt_free(cx, WIKRT_CELLSIZE, addr);
            } break;

            case WIKRT_OTAG_OPTOK: {
                wikrt_size const toklen = ((*pv) >> 8) & 0x3F;
                wikrt_size const szAlloc = sizeof(wikrt_val) + toklen;
                wikrt_free(cx, szAlloc, addr);
                wikrt_drop_step_next(cx, &drop_stack, &v);
            } break;

            case WIKRT_OTAG_OPVAL: {
                // track substructure only if our operator is LAZYKF
                bool const hide_ss = (NULL != ss) && wikrt_opval_hides_ss(*pv);
                v = pv[1];
                wikrt_free(cx, WIKRT_CELLSIZE, addr);
                if(hide_ss) {
                    wikrt_drop_v(cx, v, NULL);
                    wikrt_drop_step_next(cx, &drop_stack, &v);
                }
            } break;

            case WIKRT_OTAG_BIGINT: {
                wikrt_size const nDigits = (*pv) >> 9;
                wikrt_size const szAlloc = sizeof(wikrt_val) + (nDigits * sizeof(uint32_t));
                wikrt_free(cx, szAlloc, addr);
                wikrt_drop_step_next(cx, &drop_stack, &v);
            } break;

            case WIKRT_OTAG_SEAL: {
                v = pv[1];
                wikrt_size const len = ((*pv) >> 8) & 0x3F; // 1..63 is valid
                wikrt_size const szAlloc = WIKRT_CELLSIZE + len;
                wikrt_free(cx, szAlloc, addr);
            } break;

            default: {
                // TODO: arrays, texts, binaries, stowage, etc.
                fprintf(stderr, u8"wikrt: drop unrecognized value: %u→(%u,%u...)\n", v, pv[0], pv[1]);
            }
        }}
    } while(true);
}

wikrt_err wikrt_intro_unit(wikrt_cx* cx)
{
    bool const ok = wikrt_alloc_cellval(cx, &(cx->val), WIKRT_P, WIKRT_UNIT, cx->val);
    return (ok ? WIKRT_OK : WIKRT_CXFULL);
}

wikrt_err wikrt_intro_unit_r(wikrt_cx* cx)
{
    bool const ok = wikrt_alloc_cellval(cx, &(cx->val), WIKRT_P, cx->val, WIKRT_UNIT);
    return (ok ? WIKRT_OK : WIKRT_CXFULL);
}

wikrt_err wikrt_elim_unit(wikrt_cx* cx)
{
    wikrt_addr const a = wikrt_vaddr(cx->val);
    bool const okType = wikrt_p(cx->val) && (WIKRT_UNIT == wikrt_pval(cx,a)[0]);
    if(!okType) { return WIKRT_TYPE_ERROR; }
    cx->val = wikrt_pval(cx, a)[1];
    wikrt_free(cx, WIKRT_CELLSIZE, a);
    return WIKRT_OK;
}

wikrt_err wikrt_elim_unit_r(wikrt_cx* cx)
{
    wikrt_addr const a = wikrt_vaddr(cx->val);
    bool const okType = wikrt_p(cx->val) && (WIKRT_UNIT == wikrt_pval(cx,a)[1]);
    if(!okType) { return WIKRT_TYPE_ERROR; }
    cx->val = wikrt_pval(cx, a)[0];
    wikrt_free(cx, WIKRT_CELLSIZE, a);
    return WIKRT_OK;
}

wikrt_err wikrt_wrap_sum(wikrt_cx* cx, bool inRight)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_wrap_sum_v(cx, inRight, wikrt_pval(cx, wikrt_vaddr(cx->val)));
}

wikrt_err wikrt_unwrap_sum(wikrt_cx* cx, bool* inRight)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_unwrap_sum_v(cx, inRight, wikrt_pval(cx, wikrt_vaddr(cx->val)));
}

wikrt_err wikrt_wrap_seal(wikrt_cx* cx, char const* s)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_wrap_seal_v(cx, s, wikrt_pval(cx, wikrt_vaddr(cx->val)));
}

wikrt_err wikrt_unwrap_seal(wikrt_cx* cx, char* buff)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_unwrap_seal_v(cx, buff, wikrt_pval(cx, wikrt_vaddr(cx->val)));
}

/** (a*(b*c))→(b*(a*c)). ABC op `w` */
wikrt_err wikrt_wswap(wikrt_cx* cx)
{
    return wikrt_wswap_v(cx, cx->val);
}
wikrt_err wikrt_wswap_v(wikrt_cx* const cx, wikrt_val const abc) 
{
    if(wikrt_p(abc)) {
        wikrt_val* const pabc = wikrt_pval(cx, wikrt_vaddr(abc));
        wikrt_val const bc = pabc[1];
        if(wikrt_p(bc)) {
            wikrt_val* const pbc = wikrt_pval(cx, wikrt_vaddr(bc));
            wikrt_val const b = pbc[0];
            pbc[0] = pabc[0];
            pabc[0] = b;
            return WIKRT_OK;
        }
    }
    return WIKRT_TYPE_ERROR;
}

/** (a*(b*(c*d)))→(a*(c*(b*d))). ABC op `z` */
wikrt_err wikrt_zswap(wikrt_cx* cx)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_wswap_v(cx, wikrt_pval(cx, wikrt_vaddr(cx->val))[1]);
}

/** (a*(b*c))→((a*b)*c). ABC op `l`. */
wikrt_err wikrt_assocl(wikrt_cx* cx) 
{
    wikrt_val const abc = cx->val;
    if(wikrt_p(abc)) {
        wikrt_val* const pa_bc = wikrt_pval(cx, wikrt_vaddr(abc));
        wikrt_val const bc = pa_bc[1];
        if(wikrt_p(bc)) {
            wikrt_val* const pbc = wikrt_pval(cx, wikrt_vaddr(bc));
            wikrt_val const a = pa_bc[0];
            pa_bc[0] = bc; // old a → bc
            pa_bc[1] = pbc[1]; // old bc → c
            pbc[1] = pbc[0]; // old c → b
            pbc[0] = a; // old b → a
            return WIKRT_OK;
        }
    }
    return WIKRT_TYPE_ERROR;
}

/** ((a*b)*c)→(a*(b*c)). ABC op `r`. */
wikrt_err wikrt_assocr(wikrt_cx* cx)
{
    wikrt_val const abc = cx->val;
    if(wikrt_p(abc)) {
        wikrt_val* const pab_c = wikrt_pval(cx, wikrt_vaddr(abc));
        wikrt_val const ab = pab_c[0];
        if(wikrt_p(ab)) {
            wikrt_val* const pab = wikrt_pval(cx, wikrt_vaddr(ab));
            wikrt_val const c = pab_c[1];
            pab_c[1] = ab;
            pab_c[0] = pab[0];
            pab[0] = pab[1];
            pab[1] = c;
            return WIKRT_OK;
        }
    }
    return WIKRT_IMPL;
}

/** (a*b)→(b*a). ABC ops `vrwlc`. */
wikrt_err wikrt_swap(wikrt_cx* cx)
{
    wikrt_val const v = cx->val;
    if(wikrt_p(v)) {
        wikrt_val* const p = wikrt_pval(cx, wikrt_vaddr(v));
        wikrt_val const fst = p[0];
        p[0] = p[1];
        p[1] = fst;
        return WIKRT_OK;
    }
    return WIKRT_TYPE_ERROR;
}

/** ((a+(b+c))*e)→((b+(a+c))*e). ABC op `W`. */
wikrt_err wikrt_sum_wswap(wikrt_cx* cx)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_sum_wswap_v(cx, wikrt_pval(cx, wikrt_vaddr(cx->val)));
}

/** (a+(b+c))→(b+(a+c)).
 *
 * This only allocates if we're in the 'a' position. Conveniently,
 * if we fail to allocate to shift 'a' to 'b', we're still in the
 * 'a' position.
 *
 * That said, that's a shallow analysis. If wikrt_unwrap_sum can
 * allocate, this may fail unless wikrt_unwrap_sum also preserves
 * an extra cell whenever it allocates. 
 */
wikrt_err wikrt_sum_wswap_v(wikrt_cx* cx, wikrt_val* v)
{
    bool inBC;
    wikrt_err st = wikrt_unwrap_sum_v(cx, &inBC, v);
    if(WIKRT_OK != st) { return st; }
    else if(!inBC) { // we have 'a'.
        wikrt_wrap_sum_v(cx, false, v);
        return wikrt_wrap_sum_v(cx, true, v);
    } else { // we have (b + c).
        bool inC;
        st = wikrt_unwrap_sum_v(cx, &inC, v);
        if(WIKRT_OK != st) {
            wikrt_wrap_sum_v(cx, inBC, v); // ? → (_ + ?)
            return st;
        } else if(!inC) { // we have 'b'
            wikrt_wrap_sum_v(cx, false, v);
            return WIKRT_OK;
        } else { // we have 'c'.
            wikrt_wrap_sum_v(cx, true, v);
            wikrt_wrap_sum_v(cx, true, v);
            return WIKRT_OK;
        }
    }
}

/** ((a+(b+(c+d)))*e)→((a+(c+(b+d)))*e). ABC op `Z`. */
wikrt_err wikrt_sum_zswap(wikrt_cx* cx)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_sum_zswap_v(cx, wikrt_pval(cx, wikrt_vaddr(cx->val)));
}

wikrt_err wikrt_sum_zswap_v(wikrt_cx* cx, wikrt_val* v)
{
    bool inBCD;
    wikrt_err st = wikrt_unwrap_sum_v(cx, &inBCD, v);
    if(WIKRT_OK != st) { return st; }
    else if(inBCD) { st = wikrt_sum_wswap_v(cx, v); }
    wikrt_wrap_sum_v(cx, inBCD, v);
    return st;
}

/** ((a+(b+c))*e)→(((a+b)+c)*e). ABC op `L`. */
wikrt_err wikrt_sum_assocl(wikrt_cx* cx)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_sum_assocl_v(cx, wikrt_pval(cx, wikrt_vaddr(cx->val)));
}

/** (a+(b+c)) → ((a+b)+c). Allocates only from 'a' position. */
wikrt_err wikrt_sum_assocl_v(wikrt_cx* cx, wikrt_val* v) 
{
    bool inBC;
    wikrt_err st = wikrt_unwrap_sum_v(cx, &inBC, v);
    if(WIKRT_OK != st) { return st; }
    else if(!inBC) { // we have 'a'.
        wikrt_wrap_sum_v(cx, false, v); // recover 'a' in left.
        return wikrt_wrap_sum_v(cx, false, v); // now 'a' in left-of-left.
    } else {
        bool inC;
        st = wikrt_unwrap_sum_v(cx, &inC, v);
        wikrt_wrap_sum_v(cx, true, v); // (b+c) on failure or b or c, in right of something.
        if((WIKRT_OK == st) && !inC) { wikrt_wrap_sum_v(cx, false, v); }
        return st;
    }
}

/** (((a+b)+c)*e)→((a+(b+c))*e). ABC op `R`. */
wikrt_err wikrt_sum_assocr(wikrt_cx* cx)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_sum_assocr_v(cx, wikrt_pval(cx, wikrt_vaddr(cx->val)));
}

/** ((a+b)+c)→(a+(b+c)). Allocates from 'c' position. */
wikrt_err wikrt_sum_assocr_v(wikrt_cx* cx, wikrt_val* v)
{
    bool inC;
    wikrt_err st = wikrt_unwrap_sum_v(cx, &inC, v);
    if(WIKRT_OK != st) { return st; }
    else if(inC) { // we have 'c'.
        wikrt_wrap_sum_v(cx, true, v);
        return wikrt_wrap_sum_v(cx, true, v);
    } else {
        bool inB;
        st = wikrt_unwrap_sum_v(cx, &inB, v);
        wikrt_wrap_sum_v(cx, false, v); // (a+b) on failure, or a or b, in left of something.
        if((WIKRT_OK == st) && inB) { wikrt_wrap_sum_v(cx, true, v); }
        return st;
    }
}

/** ((a+b)*e)→((b+a)*e). ABC ops `VRWLC`. */
wikrt_err wikrt_sum_swap(wikrt_cx* cx)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_sum_swap_v(cx, wikrt_pval(cx, wikrt_vaddr(cx->val)));
}

/** (a+b)→(b+a). */
wikrt_err wikrt_sum_swap_v(wikrt_cx* cx, wikrt_val* v)
{
    bool inRight;
    wikrt_err st = wikrt_unwrap_sum_v(cx, &inRight, v);
    if(WIKRT_OK != st) { return st; }
    return wikrt_wrap_sum_v(cx, !inRight, v);
}

/** (a*((b+c)*e))→(((a*b)+(a*c))*e). ABC op `D`. */
wikrt_err wikrt_sum_distrib(wikrt_cx* cx)
{
    return WIKRT_IMPL;
}


/** (((a*b)+(c*d))*e)→((a+c)*((b+d)*e)). ABC op `F`. */
wikrt_err wikrt_sum_factor(wikrt_cx* cx)
{
    return WIKRT_IMPL;
}



/* Currently allocating as a normal list. This means we allocate one
 * full cell (WIKRT_CELLSIZE) per character, usually an 8x increase.
 * Yikes! But I plan to later tune this to a dedicated structure.
 */
wikrt_err _wikrt_alloc_text(wikrt_cx* cx, wikrt_val* txt, char const* cstr, size_t len) 
{ 
    _Static_assert((sizeof(char) == sizeof(uint8_t)), "invalid cast from char* to utf8_t*");
    uint8_t const* s = (uint8_t const*) cstr;

    (*txt) = WIKRT_VOID;
    wikrt_val* tl = txt;
    uint32_t cp;
    while((len != 0) && utf8_step(&s, &len, &cp)) {
        if(!wikrt_text_char(cp)) { return WIKRT_INVAL; }
        if(!wikrt_alloc_cellval(cx, tl, WIKRT_PL, wikrt_i2v((int32_t)cp), WIKRT_VOID)) { return WIKRT_CXFULL; }
        tl = 1 + wikrt_pval(cx, wikrt_vaddr(*tl));
    }

    (*tl) = WIKRT_UNIT_INR;
    return WIKRT_OK;
}



/* For the moment, we'll allocate a binary as a plain old list.
 * This results in an WIKRT_CELLSIZE (8x) expansion at this time. I
 * I will support more compact representations later.
 */
wikrt_err _wikrt_alloc_binary(wikrt_cx* cx, wikrt_val* v, uint8_t const* buff, size_t nBytes) 
{
    (*v) = WIKRT_VOID;
    wikrt_val* tl = v;
    uint8_t const* const buffEnd = buff + nBytes;
    while(buff != buffEnd) {
        uint8_t const e = *(buff++);
        if(!wikrt_alloc_cellval(cx, tl, WIKRT_PL, wikrt_i2v((int32_t)e), WIKRT_VOID)) { return WIKRT_CXFULL; }
        tl = 1 + wikrt_pval(cx, wikrt_vaddr(*tl));
    }
    (*tl) = WIKRT_UNIT_INR;
    return WIKRT_OK;
}

wikrt_err wikrt_alloc_medint(wikrt_cx* cx, wikrt_val* v, bool positive, uint32_t d0, uint32_t d1, uint32_t d2)
{
    if(0 == d2) {
        wikrt_size const nDigits = 2;
        wikrt_size const allocSz = sizeof(wikrt_val) + (nDigits * sizeof(uint32_t));
        wikrt_addr addr;
        if(!wikrt_alloc(cx, allocSz, &addr)) { return WIKRT_CXFULL; }
        (*v) = wikrt_tag_addr(WIKRT_O, addr);
        wikrt_val* const p = wikrt_pval(cx, addr);
        p[0] = wikrt_mkotag_bigint(positive, nDigits);
        uint32_t* const d = (uint32_t*)(p + 1);
        d[0] = d0;
        d[1] = d1;
        return WIKRT_OK;
    } else {
        wikrt_size const nDigits = 3;
        wikrt_size const allocSz = sizeof(wikrt_val) + (nDigits * sizeof(uint32_t));
        wikrt_addr addr;
        if(!wikrt_alloc(cx, allocSz, &addr)) { return WIKRT_CXFULL; }
        (*v) = wikrt_tag_addr(WIKRT_O, addr);
        wikrt_val* const p = wikrt_pval(cx, addr);
        p[0] = wikrt_mkotag_bigint(positive, nDigits);
        uint32_t* const d = (uint32_t*)(p + 1);
        d[0] = d0;
        d[1] = d1;
        d[2] = d2;
        return WIKRT_OK;
    }
}

wikrt_err wikrt_peek_medint(wikrt_cx* cx, wikrt_val v, bool* positive, uint32_t* d0, uint32_t* d1, uint32_t* d2)
{
    if(wikrt_i(v)) {
        int32_t n = wikrt_v2i(v);
        (*positive) = (n >= 0);
        n = (*positive) ? n : -n;
        (*d0) = n % WIKRT_BIGINT_DIGIT;
        (*d1) = n / WIKRT_BIGINT_DIGIT;
        (*d2) = 0;
        return WIKRT_OK;
    }

    wikrt_tag const tag = wikrt_vtag(v);
    wikrt_addr const addr = wikrt_vaddr(v);
    wikrt_val const* const pv = wikrt_pval(cx, addr);
    bool const isBigInt = (0 != addr) && (WIKRT_O == tag) && (wikrt_otag_bigint(*pv));
    if(!isBigInt) { 
        (*positive) = false; 
        (*d0) = 0; (*d1) = 0; (*d2) = 0;
        return WIKRT_TYPE_ERROR; 
    }

    wikrt_size const nDigits = (*pv) >> 9;
    uint32_t const* const d = (uint32_t*)(pv + 1);
    (*positive) = (0 == ((1 << 8) & (*pv)));
    (*d0) = d[0];
    (*d1) = d[1];
    (*d2) = (nDigits > 2) ? d[2] : 0;
    return (nDigits > 3) ? WIKRT_BUFFSZ : WIKRT_OK;
}


wikrt_err _wikrt_alloc_i32(wikrt_cx* cx, wikrt_val* v, int32_t n) 
{
    if((WIKRT_SMALLINT_MIN <= n) && (n <= WIKRT_SMALLINT_MAX)) {
        (*v) = wikrt_i2v(n);
        return WIKRT_OK;
    }

    bool positive;
    uint32_t d0, d1;
    if(n != INT32_MIN) {
        _Static_assert(((INT32_MAX + INT32_MIN) == (-1)), "bad assumption (int32_t)");
        positive = (n >= 0);
        n = positive ? n : -n;
        d0 = n % WIKRT_BIGINT_DIGIT;
        d1 = n / WIKRT_BIGINT_DIGIT;
    } else {
        _Static_assert((-2147483648 == INT32_MIN), "bad INT32_MIN");
        positive = false;
        d1 = 2;
        d0 = 147483648;
    }
    return wikrt_alloc_medint(cx, v, positive, d0, d1, 0);
}


wikrt_err _wikrt_alloc_i64(wikrt_cx* cx, wikrt_val* v, int64_t n) 
{
    if((WIKRT_SMALLINT_MIN <= n) && (n <= WIKRT_SMALLINT_MAX)) {
        (*v) = wikrt_i2v((int32_t)n);
        return WIKRT_OK;
    }

    bool positive;
    uint32_t d0, d1, d2;
    if(n != INT64_MIN) {
        _Static_assert(((INT64_MAX + INT64_MIN) == (-1)), "bad assumption (int64_t)");
        positive = (n >= 0);
        n = positive ? n : -n;
        d0 = n % WIKRT_BIGINT_DIGIT;
        n /= WIKRT_BIGINT_DIGIT;
        d1 = n % WIKRT_BIGINT_DIGIT;
        d2 = n / WIKRT_BIGINT_DIGIT;
    } else {
        // GCC complains with the INT64_MIN constant given directly.
        _Static_assert(((-9223372036854775807 - 1) == INT64_MIN), "bad INT64_MIN");
        positive = false;
        d2 = 9;
        d1 = 223372036;
        d0 = 854775808;
    }
    return wikrt_alloc_medint(cx, v, positive, d0, d1, d2);
}


wikrt_err _wikrt_peek_i32(wikrt_cx* cx, wikrt_val const v, int32_t* i32) 
{
    // small integers (normal case)
    if(wikrt_i(v)) {
        (*i32) = wikrt_v2i(v);
        return WIKRT_OK;
    } 

    bool positive;
    uint32_t d0, d1, d2;
    wikrt_err const st = wikrt_peek_medint(cx, v, &positive, &d0, &d1, &d2);
    if(WIKRT_OK != st) { 
        (*i32) = positive ? INT32_MAX : INT32_MIN;
        return st;
    }
    int32_t const digit = WIKRT_BIGINT_DIGIT;

    if(positive) {
        _Static_assert((2147483647 == INT32_MAX), "bad INT32_MAX");
        uint32_t const d1m = 2;
        uint32_t const d0m = 147483647;
        bool const overflow = (d2 != 0) || (d1 > d1m) || ((d1 == d1m) && (d0 > d0m));
        if(overflow) { (*i32) = INT32_MAX; return WIKRT_BUFFSZ; }
        (*i32) = (d1 * digit) + d0;
        return WIKRT_OK;
    } else {
        _Static_assert((-2147483648 == INT32_MIN), "bad INT32_MIN");
        uint32_t const d1m = 2;
        uint32_t const d0m = 147483648;
        bool const underflow = (d2 != 0) || (d1 > d1m) || ((d1 == d1m) && (d0 > d0m));
        if(underflow) { (*i32) = INT32_MIN; return WIKRT_BUFFSZ; }
        (*i32) = 0 - ((int32_t)d1 * digit) - (int32_t)d0;
        return WIKRT_OK;
    }
}

wikrt_err _wikrt_peek_i64(wikrt_cx* cx, wikrt_val const v, int64_t* i64) 
{
    if(wikrt_i(v)) {
        (*i64) = (int64_t) wikrt_v2i(v);
        return WIKRT_OK;
    }

    bool positive;
    uint32_t d0, d1, d2;
    wikrt_err const st = wikrt_peek_medint(cx, v, &positive, &d0, &d1, &d2);
    if(WIKRT_OK != st) { 
        (*i64) = positive ? INT64_MAX : INT64_MIN;
        return st;
    }
    int64_t const digit = WIKRT_BIGINT_DIGIT;

    if(0 == d2) {
        // nDigits is exactly 2 by construction, no risk of over or underflow
        int64_t const iAbs = ((int64_t)d1 * digit) + d0;
        (*i64) = positive ? iAbs : -iAbs;
        return WIKRT_OK;
    } else if(positive) {
        _Static_assert((9223372036854775807 == INT64_MAX), "bad INT64_MAX");
        uint32_t const d2m = 9;
        uint32_t const d1m = 223372036;
        uint32_t const d0m = 854775807;
        bool const overflow = (d2 > d2m) ||
            ((d2 == d2m) && ((d1 > d1m) || ((d1 == d1m) && (d0 > d0m))));
        if(overflow) { (*i64) = INT64_MAX; return WIKRT_BUFFSZ; }
        (*i64) = ((int64_t)d2 * (digit * digit)) 
               + ((int64_t)d1 * (digit))
               + ((int64_t)d0);
        return WIKRT_OK;
    } else {
        // GCC complains with the INT64_MIN constant given directly.
        _Static_assert(((-9223372036854775807 - 1) == INT64_MIN), "bad INT64_MIN");
        uint32_t const d2m = 9;
        uint32_t const d1m = 223372036;
        uint32_t const d0m = 854775808;
        bool const underflow = (d2 > d2m) ||
            ((d2 == d2m) && ((d1 > d1m) || ((d1 == d1m) && (d0 > d0m))));
        if(underflow) { (*i64) = INT64_MIN; return WIKRT_BUFFSZ; }
        (*i64) = 0 - ((int64_t)d2 * (digit * digit))
                   - ((int64_t)d1 * (digit))
                   - ((int64_t)d0);
        return WIKRT_OK;
    }
}

static inline size_t wikrt_decimal_size(uint32_t n) {
    size_t ct = 0;
    do { ++ct; n /= 10; } while(n > 0);
    return ct;
}


wikrt_err _wikrt_peek_istr(wikrt_cx* cx, wikrt_val const v, char* const buff, size_t* const buffsz)
{
    bool positive;
    uint32_t upperDigit;
    uint32_t innerDigitCt;
    uint32_t const* d;

    size_t const buffsz_avail = (*buffsz);

    if(wikrt_i(v)) {
        _Static_assert((WIKRT_SMALLINT_MIN == (- WIKRT_SMALLINT_MAX)), "negation of smallint must be closed");
        int32_t const i = wikrt_v2i(v);

        positive = (i >= 0);
        upperDigit = (uint32_t)(positive ? i : -i);
        innerDigitCt = 0;
        d = NULL;
    } else {
        wikrt_tag const tag = wikrt_vtag(v);
        wikrt_addr const addr = wikrt_vaddr(v);
        wikrt_val const* const pv = wikrt_pval(cx, addr);
        bool const isBigInt = (WIKRT_O == tag) && (0 != addr) && (wikrt_otag_bigint(*pv));
        if(!isBigInt) { return WIKRT_TYPE_ERROR; }
        
        d = (uint32_t*)(pv + 1);
        positive = (0 == ((1 << 8) & (*pv)));
        innerDigitCt = ((*pv) >> 9) - 1;
        upperDigit = d[innerDigitCt];
    }



    size_t const buffsz_min = (positive ? 0 : 1) // sign
                            + wikrt_decimal_size(upperDigit)
                            + (9 * innerDigitCt);

    (*buffsz) = buffsz_min;
    if(buffsz_min > buffsz_avail) { return WIKRT_BUFFSZ; }

    char* s = buff + buffsz_min;
    #define WD(n) { *(--s) = ('0' + (n % 10)); n /= 10; }
    for(uint32_t ii = 0; ii < innerDigitCt; ++ii) {
        // nine decimal digits per inner digit
        uint32_t n = d[ii];
        WD(n); WD(n); WD(n);
        WD(n); WD(n); WD(n);
        WD(n); WD(n); WD(n);
    }
    do { WD(upperDigit); } while(0 != upperDigit);
    if(!positive) { *(--s) = '-'; }
    assert(buff == s); // assert match expected size
    #undef WD

    return WIKRT_OK;
}

wikrt_err _wikrt_alloc_istr(wikrt_cx* cx, wikrt_val* v, char const* istr, size_t strlen)
{
    (*v) = WIKRT_VOID;
    return WIKRT_IMPL;
}

static inline bool is_deepsum_with_free_space(wikrt_cx* cx, wikrt_val v) 
{
    if(!wikrt_o(v)) { return false; }
    wikrt_val const otag = *(wikrt_pval(cx, wikrt_vaddr(v)));
    // deepsum with free space must have tag bits 30 and 31 free.
    return wikrt_otag_deepsum(otag) && (otag < (1 << 30));
}
 
wikrt_err wikrt_wrap_sum_v(wikrt_cx* cx, bool inRight, wikrt_val* v) 
{
    if(WIKRT_P == wikrt_vtag(*v)) {
        // shallow sum for product or unit values, non-allocating.
        wikrt_tag const newtag = inRight ? WIKRT_PR : WIKRT_PL;
        (*v) = wikrt_tag_addr(newtag, wikrt_vaddr(*v));
        return WIKRT_OK;
    } else if(is_deepsum_with_free_space(cx, (*v))) {
        // extend an existing deepsum without requiring allocation.
        wikrt_val* const pv = wikrt_pval(cx, wikrt_vaddr(*v));
        wikrt_val const s0 = (*pv) >> 8; // chop WIKRT_OTAG_DEEPSUM
        wikrt_val const sf = (s0 << 2) | (inRight ? WIKRT_DEEPSUMR : WIKRT_DEEPSUML);
        wikrt_val const otag = (sf << 8) | WIKRT_OTAG_DEEPSUM;
        (*pv) = otag;
        return WIKRT_OK;
    } else { // wrap value in a new deep sum
        wikrt_val const sf = (inRight ? WIKRT_DEEPSUMR : WIKRT_DEEPSUML);
        wikrt_val const otag = (sf << 8) | WIKRT_OTAG_DEEPSUM;
        if(!wikrt_alloc_cellval(cx, v, WIKRT_O, otag, (*v))) {  return WIKRT_CXFULL; }
        return WIKRT_OK;
    }
}

/* Note: The sum-type data plumbing code assumes that `wikrt_unwrap_sum_v`
 * is non-allocating or at least may be followed by a few wikrt_wrap_sum_v
 * ops without checking for space. If we do allocate (e.g. to unwrap an 
 * array) I need to preserve an extra WIKRT_CELLSIZE for wrap_sum ops. 
 */
wikrt_err wikrt_unwrap_sum_v(wikrt_cx* cx, bool* inRight, wikrt_val* v) 
{
    wikrt_tag const tag = wikrt_vtag(*v);
    wikrt_addr const addr = wikrt_vaddr(*v);
    if((WIKRT_PL == tag) || (WIKRT_PR == tag)) {
        // unwrap shallow sum for pair or unit value
        (*inRight) = (WIKRT_PR == tag);
        (*v) = wikrt_tag_addr(WIKRT_P, addr);
        return WIKRT_OK;
    } else if((WIKRT_O == tag) && (0 != addr)) {
        wikrt_val* const pv = wikrt_pval(cx, addr);
        if(wikrt_otag_deepsum(*pv)) {
            wikrt_val const s0 = ((*pv) >> 8);
            wikrt_val const sf = (s0 >> 2);
            (*inRight) = (3 == (3 & s0));
            if(0 == sf) { // dealloc deepsum wrapper
                (*v) = pv[1];
                wikrt_free(cx, WIKRT_CELLSIZE, addr);
            } else { // keep deepsum, but smaller 
                *pv = (sf << 8) | WIKRT_OTAG_DEEPSUM;
            }
            return WIKRT_OK;
        } 
        // TODO: arrays, texts, binaries
    }
    return WIKRT_TYPE_ERROR;
}

wikrt_err _wikrt_alloc_block(wikrt_cx* cx, wikrt_val* v, char const* abc, size_t len, wikrt_abc_opts opts) 
{
    (*v) = WIKRT_VOID;
    return WIKRT_IMPL;
}


wikrt_err wikrt_wrap_seal_v(wikrt_cx* cx, char const* s, wikrt_val* v)
{
    if(!wikrt_valid_token(s)) { return WIKRT_INVAL; }
    wikrt_size const len = 0x3F & strlen(s);
    if((':' == *s) && (len <= 4)) {
        // WIKRT_OTAG_SEAL_SM: common optimized case, small discretionary tags
        #define TAG(N) ((len > N) ? (((wikrt_val)s[N]) << (8*N)) : 0)
        wikrt_val const otag = TAG(3) | TAG(2) | TAG(1) | WIKRT_OTAG_SEAL_SM;
        if(!wikrt_alloc_cellval(cx, v, WIKRT_O, otag, (*v))) { return WIKRT_CXFULL; }
        return WIKRT_OK;
        #undef TAG
    } else {
        // WIKRT_OTAG_SEAL: rare general case, large arbitrary tags
        wikrt_size const szAlloc = WIKRT_CELLSIZE + len;
        wikrt_addr addr;
        if(!wikrt_alloc(cx, szAlloc, &addr)) { return WIKRT_CXFULL; }
        wikrt_val* const psv = wikrt_pval(cx,addr);
        psv[0] = (len << 8) | WIKRT_OTAG_SEAL;
        psv[1] = (*v);
        memcpy((psv + 2), s, len); // copy of sealer text
        (*v) = wikrt_tag_addr(WIKRT_O, addr);
        return WIKRT_OK;
    }
} 

wikrt_err wikrt_unwrap_seal_v(wikrt_cx* cx, char* buff, wikrt_val* v)
{
    (*buff) = 0;
    if(!wikrt_o(*v)) { return WIKRT_TYPE_ERROR; }
    wikrt_addr const addr = wikrt_vaddr(*v);
    wikrt_val const* const pv = wikrt_pval(cx, addr);
    if(wikrt_otag_seal_sm(*pv)) {
        wikrt_val const otag = *pv;
        (*v) = pv[1];
        buff[0] = ':';
        buff[1] = (char)((otag >> 8 ) & 0xFF);
        buff[2] = (char)((otag >> 16) & 0xFF);
        buff[3] = (char)((otag >> 24) & 0xFF);
        buff[4] = 0;
        wikrt_free(cx, WIKRT_CELLSIZE, addr);
        return WIKRT_OK;
    } else if(wikrt_otag_seal(*pv)) {
        size_t const len = ((*pv) >> 8) & 0x3F;
        size_t const szAlloc = WIKRT_CELLSIZE + len;
        memcpy(buff, (pv + 2), len);
        buff[len] = 0;
        (*v) = pv[1];
        wikrt_free(cx, szAlloc, addr);
        return WIKRT_OK;
    } else { return WIKRT_TYPE_ERROR; }

}

void wikrt_txn_abort(wikrt_cx* cx) 
{
    // clear transaction record
    wikrt_drop_v(cx, cx->txn, NULL);
    cx->txn = WIKRT_VOID;
}


