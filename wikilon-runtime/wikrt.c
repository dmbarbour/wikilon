
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <sys/mman.h>
#include <errno.h>

#include "wikrt.h"

// assumes normal form utf-8 argument, NUL-terminated
bool wikrt_valid_token(char const* cstr) {
    return wikrt_valid_token_l(cstr, strlen(cstr));
}

bool wikrt_valid_token_l(char const* cstr, size_t len) {
    _Static_assert((sizeof(char) == sizeof(uint8_t)), "invalid cast from char* to utf8_t*");
    bool const validLen = ((0 < len) && (len < WIKRT_TOK_BUFFSZ));
    if(!validLen) { return false; }

    uint8_t const* s = (uint8_t const*) cstr;
    while(0 != len) {
        uint32_t cp;
        if(!utf8_step(&s,&len,&cp) || !wikrt_token_char(cp)) {
            return false;
        }
    }
    return true;
}

uint32_t wikrt_api_ver() 
{
    _Static_assert(WIKRT_API_VER < UINT32_MAX, "bad value for WIKRT_API_VER");
    return WIKRT_API_VER;
}

wikrt_ecode wikrt_error(wikrt_cx* cx) { return cx->ecode; }

void wikrt_set_error(wikrt_cx* cx, wikrt_ecode e) {
    if(!wikrt_has_error(cx) && (WIKRT_OK != e)) {
        wikrt_cx_relax(cx); // release resources
        cx->ecode = e;
    }
}


wikrt_env* wikrt_env_create(char const* dirPath, uint32_t dbMaxMB) {
    _Static_assert(WIKRT_CELLSIZE == WIKRT_CELLBUFF(WIKRT_CELLSIZE), "cell size must be a power of two");
    _Static_assert(WIKRT_SMALLINT_MAX >= 0xFF, "smallint should be sufficient for binary values");
    _Static_assert(WIKRT_SMALLINT_MAX >= 0x10FFFF, "smallint should be sufficient for unicode codepoints");
    _Static_assert(WIKRT_SMALLINT_MAX >= OP_COUNT, "smallint should be sufficient for encoding operators");
    _Static_assert(sizeof(uint8_t) == sizeof(char), "in general, assuming uint8_t and char are same size");

    wikrt_env* const e = calloc(1, sizeof(wikrt_env));
    if(NULL == e) { return NULL; }

    e->mutex = (pthread_mutex_t) PTHREAD_MUTEX_INITIALIZER;

    if((NULL == dirPath) || (0 == dbMaxMB)) { 
        e->db = NULL;
    } else if(!wikrt_db_init(&(e->db), dirPath, dbMaxMB)) {
        free(e);
        return NULL;
    }
    // thread pools? etc?
    return e;
}

void wikrt_env_destroy(wikrt_env* e) {
    assert(NULL == e->cxlist);
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

wikrt_cx* wikrt_cx_create(wikrt_env* e, uint32_t cxSizeMB) 
{
    if(NULL == e) { return NULL; }

    size_t const size = ((size_t)cxSizeMB * (1024 * 1024));
    bool const validSize = (0 < size) && (size < (SIZE_MAX / 2));

    if(!validSize) { return NULL; }

    wikrt_cx* const cx = calloc(1, sizeof(wikrt_cx));
    if(NULL == cx) { return NULL; }

    // validate sizes according to API declarations
    static int const prot = PROT_READ | PROT_WRITE | PROT_EXEC;
    static int const flags = MAP_ANONYMOUS | MAP_PRIVATE;
    void* const twospace = mmap(NULL, (2 * size), prot, flags, -1, 0);
    if(NULL == twospace) { free(cx); return NULL; }
    
    void* const mem = twospace;
    void* const ssp = (void*)(size + (char*)mem);

    cx->env     = e;
    cx->size    = (wikrt_size) size; 
    cx->mem     = mem;
    cx->ssp     = ssp;
    cx->ecode   = WIKRT_OK;

    // initialize value registers
    cx->val     = WIKRT_REG_VAL_INIT; 
    cx->pc      = WIKRT_REG_PC_INIT;
    cx->cc      = WIKRT_REG_CC_INIT;
    cx->txn     = WIKRT_REG_TXN_INIT;
    _Static_assert((4 == WIKRT_CX_REGISTER_CT), "todo: missing register initializations"); // maintenance check

    wikrt_add_cx_to_env(cx);
    return cx;
}


void wikrt_add_cx_to_env(wikrt_cx* cx) 
{
    wikrt_env* const e = cx->env;
    wikrt_env_lock(e); {
        cx->cxid = ++(e->cxcount); 
        cx->cxnext = e->cxlist;
        if(NULL != cx->cxnext) { cx->cxnext->cxprev = cx; }
        e->cxlist = cx;
        cx->cxprev = NULL;
    } wikrt_env_unlock(e);
}

void wikrt_remove_cx_from_env(wikrt_cx* cx) 
{
    wikrt_env* const e = cx->env;
    wikrt_env_lock(e); {
        if(NULL != cx->cxnext) { cx->cxnext->cxprev = cx->cxprev; }
        if(NULL != cx->cxprev) { cx->cxprev->cxnext = cx->cxnext; }
        else { assert(e->cxlist == cx); e->cxlist = cx->cxnext; }
        cx->cxnext = NULL;
        cx->cxprev = NULL;
    } wikrt_env_unlock(e);
}

void wikrt_cx_reset(wikrt_cx* cx) 
{
    // drop data
    wikrt_drop_txn(cx); // resets cx->txn
    wikrt_drop_v(cx, cx->val, NULL); cx->val = WIKRT_REG_VAL_INIT;
    wikrt_drop_v(cx, cx->cc, NULL);  cx->cc = WIKRT_REG_CC_INIT;
    wikrt_drop_v(cx, cx->pc, NULL);  cx->pc = WIKRT_REG_PC_INIT;
    _Static_assert((4 == WIKRT_CX_REGISTER_CT), "missing register resets");

    // clear errors, release resources
    cx->ecode = WIKRT_OK;
    wikrt_cx_relax(cx);
}


void wikrt_cx_destroy(wikrt_cx* cx) 
{
    wikrt_cx_reset(cx); // clean up context
    wikrt_remove_cx_from_env(cx);

    // free the trace buffer (if any)
    free(cx->tb.buf);

    // free the memory-mapped region
    errno = 0;
    void* const twospace = (cx->mem < cx->ssp) ? cx->mem : cx->ssp;
    size_t const twospace_size = 2 * ((size_t)cx->size);
    int const unmapStatus = munmap(twospace, twospace_size);
    if(0 != unmapStatus) {
        fprintf(stderr,"Failure to unmap memory (%s) when destroying context.\n", strerror(errno));
        abort(); // this is some sort of OS failure.
    }

    // recover wikrt_cx structure
    free(cx);
}

wikrt_env* wikrt_cx_env(wikrt_cx* cx) {
    return cx->env;
}

static void wikrt_mem_compact(wikrt_cx* cx) 
{

    // It's easiest to think about 'compaction' as just a move operation, 
    // albeit one that we know our destination has sufficient space.
    wikrt_cx cx0 = (*cx);

    // swap semispace and memory; reset allocators 
    cx->ssp = cx0.mem;
    cx->mem = cx0.ssp;
    cx->alloc = cx->size;
    cx->skip = 0;

    // Free lists are used only to recycle memory a bit before compaction
    // occurs. (The benefits of recycling marginal in most cases, but can
    // be significant in some common special cases.) Any free list should 
    // simply be cleared back to zero.
    _Static_assert((0 == WIKRT_FREE_LIST_CT) && (!WIKRT_NEED_FREE_ACTION)
        , "todo: handle free lists");
    
    // Note: ephemerons will require special attention. Either I add cx0 to
    // our environment briefly (I'd prefer to avoid this synchronization)
    // Or I favor a framed mechanism (e.g. a two-frame bloom filter) such
    // that I can write one frame while reading the other. 
    
    // copy roots with assumption of sufficient space.
    wikrt_copy_r(&cx0, cx0.txn, NULL, true, cx, &(cx->txn));
    wikrt_copy_r(&cx0, cx0.cc,  NULL, true, cx, &(cx->cc));
    wikrt_copy_r(&cx0, cx0.pc,  NULL, true, cx, &(cx->pc));
    wikrt_copy_r(&cx0, cx0.val, NULL, true, cx, &(cx->val));
    _Static_assert((4 == WIKRT_CX_REGISTER_CT), "todo: missing register compactions"); // maintenance check

    // Note: I'd prefer to avoid reference counting for shared objects.
    // But if I add that, I'll either need to indicate in copy_r a flag that
    // I'm moving the values OR perform a separate deletion pass.
    _Static_assert(!WIKRT_HAS_SHARED_REFCT_OBJECTS, "todo: figure out refcts during compaction");

    // reset semispace to reduce swap pressure.
    errno = 0;
    if(0 != madvise(cx->ssp, cx->size, MADV_DONTNEED)) {
        fprintf(stderr, "%s: madvise failed (%s)\n", __FUNCTION__, strerror(errno));
        abort();
    }

    // sanity check: compaction must not increase memory usage.
    assert(wikrt_mem_in_use(&cx0) >= wikrt_mem_in_use(cx));

    // keep stats. compaction count is useful for effort quotas. 
    // compaction size is useful for heuristic memory pressure.
    cx->compaction_count += 1;
    cx->compaction_size  = wikrt_mem_in_use(cx);
    cx->bytes_compacted  += cx->compaction_size;
    cx->bytes_collected  += wikrt_mem_in_use(&cx0) - cx->compaction_size;
}

bool wikrt_mem_gc_then_reserve(wikrt_cx* cx, wikrt_sizeb sz)
{
    // If we're in an error state, prevent GC. This will short-circuit
    // further computations.
    if(wikrt_has_error(cx)) { return false; }

    // basic compacting GC
    wikrt_mem_compact(cx);
    if(wikrt_mem_available(cx,sz)) {
        // Success! But large contexts might have too much space! Filling
        // a large volume of memory with mostly dead data is bad for VM
        // pressure. Operating in a smaller volume of memory is good for
        // both VM and cache locality. We'll try to stay within a certain
        // factor of live data (aligned up to a basic page size).

        wikrt_size const inuse = cx->compaction_size + sz;
        wikrt_size const desired = WIKRT_MEM_FACTOR * inuse;
        wikrt_size const pgsz = (WIKRT_MEM_PAGEMB << 20);
        wikrt_size const target = WIKRT_LNBUFF(desired, pgsz); // page aligned

        // Drop memory if appropriate.
        if(cx->alloc > target) {
            cx->skip  = (cx->alloc - target);
            cx->alloc = target;
        }
    
        return true; 
    }


    // TODO: If compaction fails to recover sufficient space,
    // but a sufficient amount of stowage is pending, we can
    // stow this data then retry. 

    wikrt_set_error(cx, WIKRT_CXFULL);
    return false;
} 


void wikrt_cx_relax(wikrt_cx* cx) {
    // If contexts hold onto resources, might need to do something special
    // here. Also consider performing a compaction to reduce virtual memory
    // pressure.
    //
    // For now, just a non-operation.
}



void wikrt_move(wikrt_cx* const lcx, wikrt_cx* const rcx) 
{
    if(lcx == rcx) { wikrt_set_error(lcx, WIKRT_INVAL); return; }
    wikrt_copy_m(lcx, NULL, true, rcx);
    if(wikrt_has_error(lcx)) { return; }

    wikrt_val* const pv = wikrt_pval(lcx, lcx->val);
    lcx->val = pv[1];
    wikrt_drop_v(lcx, pv[0], NULL);
}

void wikrt_copy(wikrt_cx* cx) {
    wikrt_ss ss = 0;
    wikrt_copy_m(cx, &ss, false, cx);
    if(!wikrt_ss_copyable(ss)) {
        wikrt_set_error(cx, WIKRT_ETYPE);
    }
}

void wikrt_copy_move(wikrt_cx* lcx, wikrt_cx* rcx) {
    if(lcx == rcx) { wikrt_set_error(lcx, WIKRT_INVAL); return; }
    wikrt_ss ss = 0;
    wikrt_copy_m(lcx, &ss, false, rcx);
    if(!wikrt_ss_copyable(ss)) {
        wikrt_set_error(lcx, WIKRT_ETYPE);
        wikrt_set_error(rcx, WIKRT_ETYPE);
    }
}


void wikrt_copy_m(wikrt_cx* lcx, wikrt_ss* ss, bool moving_copy, wikrt_cx* rcx)
{
    // NOTE: it might be better to distinguish between 'move' and 'copy'
    //  and drop the `ss`, as some resources truly cannot be copied. But
    //  I'll deal with this issue later.

    // base implementation for both wikrt_copy and wikrt_copy_move.
    // in lcx == rcx case, the copy is stacked above the original.
    if(!wikrt_p(lcx->val)) { 
        wikrt_set_error(lcx, WIKRT_ETYPE); 
        wikrt_set_error(rcx, WIKRT_ETYPE); 
        return; 
    }

    // reserve space in `rcx`. Also, size estimates for validation.
    wikrt_size const max_alloc = WIKRT_CELLSIZE + wikrt_mem_in_use(lcx);
    bool const use_size_bypass = WIKRT_ALLOW_SIZE_BYPASS && wikrt_mem_available(rcx, max_alloc);
    wikrt_size const alloc_est = use_size_bypass ? 0 : (WIKRT_CELLSIZE 
                               + wikrt_vsize(lcx, lcx->ssp, wikrt_pval(lcx, lcx->val)[0]));
    if(!wikrt_mem_reserve(rcx, alloc_est)) { return; }

    // Note: wikrt_mem_reserve may move lcx->val (when lcx == rcx).
    // anyhow, we now have sufficient space to perform our copy!
    wikrt_size const s0 = wikrt_mem_in_use(rcx);
    wikrt_val const copy_src = wikrt_pval(lcx, lcx->val)[0];
    wikrt_val copy_dst = WIKRT_UNIT;
    wikrt_copy_r(lcx, copy_src, ss, moving_copy, rcx, &copy_dst);
    wikrt_intro_r(rcx, copy_dst);
    wikrt_size const sf = wikrt_mem_in_use(rcx);

    // Validate size estimate.
    wikrt_size const alloc_act = sf - s0;
    bool const alloc_est_ok = use_size_bypass || (alloc_est == alloc_act);
    if(!alloc_est_ok) {
        // This is a serious implementation error. Maybe something is missing
        // from wikrt_vsize? Or from wikrt_copy_r?
        fprintf(stderr, "%s: invalid size estimate for copy (est %d, act %d)\n",
            __FUNCTION__, (int)alloc_est, (int)alloc_act);
        abort();
    }
}


wikrt_val_type wikrt_type(wikrt_cx* cx) 
{
    bool const okPeek = wikrt_p(cx->val) && !wikrt_has_error(cx);
    if(!okPeek) { return WIKRT_TYPE_UNDEF; }
    wikrt_val const v = wikrt_pval(cx, cx->val)[0];
    switch(wikrt_vtag(v)) {
        case WIKRT_U: return WIKRT_TYPE_UNIT;
        case WIKRT_P: return WIKRT_TYPE_PROD;
        case WIKRT_UL: // sum
        case WIKRT_UR: // sum
        case WIKRT_PL: // sum
        case WIKRT_PR: return WIKRT_TYPE_SUM;
        case WIKRT_I:  return WIKRT_TYPE_INT;
        case WIKRT_O: {
            wikrt_val const otag = wikrt_pobj(cx, v)[0];
            switch(LOBYTE(otag)) {
                case WIKRT_OTAG_ARRAY:  // list is a sum
                case WIKRT_OTAG_BINARY: // list is a sum
                case WIKRT_OTAG_UTF8:   // list is a sum
                case WIKRT_OTAG_DEEPSUM: return WIKRT_TYPE_SUM;

                case WIKRT_OTAG_TRASH: return WIKRT_TYPE_TRASH;
                case WIKRT_OTAG_BLOCK: return WIKRT_TYPE_BLOCK;

                case WIKRT_OTAG_SEAL_SM: // sealed value
                case WIKRT_OTAG_SEAL: return WIKRT_TYPE_SEAL;

                case WIKRT_OTAG_PEND: return WIKRT_TYPE_PEND;

                default: {
                    fprintf(stderr, "%s: unhandled type %d\n", __FUNCTION__, (int)LOBYTE(otag));
                    abort();
                }
            }
        }
        default: { bool impossible = false; assert(impossible); abort(); }
    }
}

// build stack of items that need at least one cell to allocate
static inline void wikrt_add_size_task(wikrt_val** s, wikrt_val v) {
    if(!wikrt_copy_shallow(v)) { *((*s)++) = v; }
}

wikrt_size wikrt_vsize(wikrt_cx* cx, wikrt_val* const s0, wikrt_val const v0)
{
    wikrt_size result = 0;
    wikrt_val* s = s0;
    wikrt_add_size_task(&s,v0);
    while(s0 != s) {
        wikrt_val const v = *(--s);
        wikrt_val const* const pv = wikrt_pval(cx, v);
        if(WIKRT_O != wikrt_vtag(v)) {
            // WIKRT_P, WIKRT_PL, WIKRT_PR
            wikrt_add_size_task(&s, pv[0]); // first value
            wikrt_add_size_task(&s, pv[1]); // second value
            result += WIKRT_CELLSIZE;
        } else { switch(LOBYTE(*pv)) {
            // simple (otag,value) pairs:
            case WIKRT_OTAG_TRASH:
            case WIKRT_OTAG_BLOCK:  
            case WIKRT_OTAG_OPVAL:  
            case WIKRT_OTAG_UTF8:
            case WIKRT_OTAG_PEND:
            case WIKRT_OTAG_SEAL_SM:
            case WIKRT_OTAG_DEEPSUM: {
                result += WIKRT_CELLSIZE;
                wikrt_add_size_task(&s, pv[1]); // wrapped value
            } break;
            case WIKRT_OTAG_SEAL: {
                wikrt_size const toklen = ((*pv) >> 8);
                wikrt_size const szAlloc = WIKRT_CELLSIZE + toklen;
                result += wikrt_cellbuff(szAlloc);
                wikrt_add_size_task(&s,pv[1]); // sealed value
            } break;
            case WIKRT_OTAG_BINARY: {
                // (hdr, next, size, buffer).
                wikrt_size const bytect = pv[2];
                result += (2 * WIKRT_CELLSIZE) + wikrt_cellbuff(bytect);
                wikrt_add_size_task(&s, pv[1]); // continue list
            } break;
            case WIKRT_OTAG_ARRAY: {
                // (hdr, next, elemct, buffer).
                wikrt_size const elemct = pv[2];
                wikrt_size const buffsz = elemct * sizeof(wikrt_val);
                result += (2 * WIKRT_CELLSIZE) + wikrt_cellbuff(buffsz);
                wikrt_val const* const parray = wikrt_paddr(cx, pv[3]);
                for(wikrt_size ii = 0; ii < elemct; ++ii) {
                    wikrt_add_size_task(&s, parray[ii]);
                }
                wikrt_add_size_task(&s, pv[1]); // continue list
            } break;
            default: {
                fprintf(stderr, "%s: unrecognized tagged value (tag %x)"
                       ,__FUNCTION__, (int)LOBYTE(*pv));
                abort();
            } 
        }} // end else { switch() {
    } // end of loop
    return result;
} 

// Add an address to our copy stack.
static inline void wikrt_add_copy_task(wikrt_cx* rcx, wikrt_addr** s, wikrt_val v, wikrt_addr a) 
{
    *(wikrt_paddr(rcx,a)) = v;
    if(!wikrt_copy_shallow(v)) { *((*s)++) = a; }
}
static inline void wikrt_cpv(wikrt_cx* rcx, wikrt_addr** s
    , wikrt_val const* pv, wikrt_addr addr, wikrt_size ix) 
{
    wikrt_add_copy_task(rcx, s, pv[ix], addr + (ix * sizeof(wikrt_val)));
}

// (for internal use by wikrt_copy_r only)
// mostly this is needed to handle WIKRT_OTAG_OPVAL properly, 
//  to hide `ss` for quoted values constructed by partial evaluation.
static void wikrt_copy_rs(wikrt_cx* const lcx, wikrt_cx* const rcx
                         ,wikrt_ss* const ss, bool const moving_copy, wikrt_addr* const s0
                         ,wikrt_val* dst) 
{
    if(wikrt_copy_shallow(*dst)) { return; }

    wikrt_addr* s = s0;
    do {
        // invariant: `dst` constains non-shallow reference into lcx->mem 
        // Thus WIKRT_U, WIKRT_UL, WIKRT_UR, and WIKRT_I are not found.
        wikrt_val const v = (*dst);
        wikrt_tag const tag = wikrt_vtag(v);
        wikrt_val const* const pv = wikrt_pval(lcx, v);

        if(WIKRT_O != tag) {
            // WIKRT_P, WIKRT_PL, WIKRT_PR
            wikrt_addr const addr = wikrt_alloc_r(rcx, WIKRT_CELLSIZE);
            (*dst) = wikrt_tag_addr(tag, addr);
            wikrt_cpv(rcx, &s, pv, addr, 0);
            wikrt_cpv(rcx, &s, pv, addr, 1);
            // Note: this ordering will copy 'spine' of a stack or list
            // before copying any of the elements.
        } else { switch(LOBYTE(*pv)) {

            // basic (tag, val) pairs
            case WIKRT_OTAG_UTF8:    // (utf8, binary value)
            case WIKRT_OTAG_SEAL_SM: // (sealer tag, value)
            case WIKRT_OTAG_DEEPSUM: {
                wikrt_addr const addr = wikrt_alloc_r(rcx, WIKRT_CELLSIZE);
                (*dst) = wikrt_tag_addr(WIKRT_O, addr);
                *(wikrt_paddr(rcx, addr)) = *pv;
                wikrt_cpv(rcx, &s, pv, addr, 1);
            } break;

            // (tag, val) with WIKRT_SS_PEND
            case WIKRT_OTAG_PEND: {
                if(NULL != ss) { (*ss) |= WIKRT_SS_PEND; }
                wikrt_addr const addr = wikrt_alloc_r(rcx, WIKRT_CELLSIZE);
                (*dst) = wikrt_tag_addr(WIKRT_O, addr);
                *(wikrt_paddr(rcx, addr)) = *pv;
                wikrt_cpv(rcx, &s, pv, addr, 1);
            } break;

            // block is (tag, val) with substructure
            case WIKRT_OTAG_TRASH: // same as WIKRT_OTAG_BLOCK
            case WIKRT_OTAG_BLOCK: {
                wikrt_capture_block_ss(*pv, ss);
                wikrt_addr const addr = wikrt_alloc_r(rcx, WIKRT_CELLSIZE);
                (*dst) = wikrt_tag_addr(WIKRT_O, addr);
                *(wikrt_paddr(rcx, addr)) = *pv;
                wikrt_cpv(rcx, &s, pv, addr, 1);
            } break;

            // opval is special case, may hide substructure
            case WIKRT_OTAG_OPVAL: {
                wikrt_addr const addr = wikrt_alloc_r(rcx, WIKRT_CELLSIZE);
                (*dst) = wikrt_tag_addr(WIKRT_O, addr);
                *(wikrt_paddr(rcx, addr)) = *pv;
                if((NULL != ss) && wikrt_opval_hides_ss(*pv)) {
                    // hide substructure
                    dst = 1 + wikrt_paddr(rcx, addr);
                    (*dst) = pv[1];
                    wikrt_copy_rs(lcx, rcx, NULL, moving_copy, s, dst);
                } else {
                    wikrt_cpv(rcx, &s, pv, addr, 1);
                }
            } break;

            case WIKRT_OTAG_SEAL: {
                wikrt_size const toklen = ((*pv) >> 8);
                wikrt_size const szAlloc = WIKRT_CELLSIZE + toklen;
                wikrt_addr const addr = wikrt_alloc_r(rcx, wikrt_cellbuff(szAlloc));
                (*dst) = wikrt_tag_addr(WIKRT_O, addr);
                memcpy(wikrt_paddr(rcx, addr), pv, szAlloc);
                wikrt_cpv(rcx, &s, pv, addr, 1);
            } break;

            case WIKRT_OTAG_BINARY: {
                // (hdr, next, size, buffer).
                wikrt_size const bytect = pv[2];
                wikrt_addr const buff = wikrt_alloc_r(rcx, wikrt_cellbuff(bytect));
                memcpy(wikrt_paddr(rcx, buff), wikrt_paddr(lcx, pv[3]), bytect);

                wikrt_addr const hdr = wikrt_alloc_r(rcx, (2 * WIKRT_CELLSIZE));
                wikrt_val* const phd = wikrt_paddr(rcx, hdr);
                phd[0] = pv[0]; // tag
                phd[2] = pv[2]; // elemct
                phd[3] = buff;  // array
                wikrt_cpv(rcx, &s, pv, hdr, 1); // continue list
                (*dst) = wikrt_tag_addr(WIKRT_O, hdr);
            } break;

            case WIKRT_OTAG_ARRAY: {
                // (hdr, next, elemct, buffer).
                wikrt_size const elemct = pv[2];
                wikrt_size const buffsz = elemct * sizeof(wikrt_val);
                wikrt_addr const buff = wikrt_alloc_r(rcx, wikrt_cellbuff(buffsz));
                wikrt_val const* const parray = wikrt_paddr(lcx, pv[3]);
                for(wikrt_size ii = 0; ii < elemct; ++ii) {
                    wikrt_cpv(rcx, &s, parray, buff, ii);
                }

                wikrt_addr const hdr  = wikrt_alloc_r(rcx, (2 * WIKRT_CELLSIZE));
                wikrt_val* const phd = wikrt_paddr(rcx, hdr);
                phd[0] = pv[0]; // tag
                phd[2] = pv[2]; // element count
                phd[3] = buff;  // copied buffer
                wikrt_cpv(rcx, &s, pv, hdr, 1); // continue list
                (*dst) = wikrt_tag_addr(WIKRT_O, hdr);
            } break;

            default: {
                fprintf(stderr, "%s: unrecognized tag (%x)"
                    , __FUNCTION__, (unsigned int)(*pv));
                abort();
            }
        }} 

        dst = (s0 == s) ? NULL : wikrt_paddr(rcx, *(--s));
    } while(NULL != dst);
}

void wikrt_copy_r(wikrt_cx* lcx, wikrt_val lval, wikrt_ss* ss, bool moving_copy, wikrt_cx* rcx, wikrt_val* rval)
{
    // Note: I use a stack in rcx->mem (that builds upwards from zero). An invariant
    // is that this stack is always smaller than the space left to be allocated. This
    // is achieved by ensuring only addresses that need to be allocated are recorded
    // within the stack.
    if(NULL != ss) { (*ss) = 0; }
    (*rval) = lval;
    wikrt_copy_rs(lcx, rcx, ss, moving_copy, ((wikrt_addr*)(rcx->mem)), rval);
}


static inline void wikrt_add_drop_task(wikrt_val** s, wikrt_val v) {
    // any copy_shallow values are also shallow for drop.
    if(!wikrt_copy_shallow(v)) { *((*s)++) = v; } 
}

void wikrt_drop_sv(wikrt_cx* cx, wikrt_val* const s0, wikrt_val const v0, wikrt_ss* ss) 
{
    _Static_assert(!WIKRT_NEED_FREE_ACTION, "must update drop to free explicitly");

    // currently don't need to touch anything if not tracking `ss`.
    // This might change if I introduce reference counted value types
    // or free list allocators.
    bool const fast_escape = (NULL == ss);
    if(fast_escape) { return; }

    wikrt_val* s = s0;
    wikrt_add_drop_task(&s,v0);

    while(s0 != s) {
        wikrt_val const v = *(--s);
        wikrt_val const* const pv = wikrt_pval(cx, v);
        if(WIKRT_O != wikrt_vtag(v)) {
            // WIKRT_P, WIKRT_PL, or WIKRT_PR
            wikrt_add_drop_task(&s,pv[0]);
            wikrt_add_drop_task(&s,pv[1]);
        } else { switch(LOBYTE(*pv)) {
            // (tag, val, (... potential binary ...)) 
            case WIKRT_OTAG_SEAL:   
            case WIKRT_OTAG_SEAL_SM:
            case WIKRT_OTAG_UTF8:
            case WIKRT_OTAG_BINARY:
            case WIKRT_OTAG_DEEPSUM: {
                wikrt_add_drop_task(&s,pv[1]);
            } break;

            // (tag, val) with WIKRT_SS_PEND
            case WIKRT_OTAG_PEND: {
                if(NULL != ss) { (*ss) |= WIKRT_SS_PEND; }
                wikrt_add_drop_task(&s, pv[1]);
            } break;

            // block headers are my primary source of substructure.
            case WIKRT_OTAG_TRASH: // same as WIKRT_OTAG_BLOCK
            case WIKRT_OTAG_BLOCK: {
                wikrt_capture_block_ss(*pv, ss);
                wikrt_add_drop_task(&s,pv[1]);
            } break;

            // opval type may hide substructure (for partial eval)
            case WIKRT_OTAG_OPVAL: {
                if((NULL != ss) && wikrt_opval_hides_ss(*pv)) {
                    // drop with ss hidden; preserve stack location
                    wikrt_drop_sv(cx, s, pv[1], NULL);
                } else {
                    wikrt_add_drop_task(&s, pv[1]);
                }
            } break;

            case WIKRT_OTAG_ARRAY: {
                // (hdr, next, elemct, buffer)
                wikrt_size const elemct = pv[2];
                wikrt_val const* const parray = wikrt_paddr(cx, pv[3]);
                for(wikrt_size ii = 0; ii < elemct; ++ii) {
                    wikrt_add_drop_task(&s,parray[ii]);
                }
                wikrt_add_drop_task(&s,pv[1]);
            } break;

            // future notes: stowed value substructure. May
            //  need something special for parallelism (if any).

            default: {
                fprintf(stderr, "%s: unrecognized tag (%x)"
                    , __FUNCTION__, (unsigned int)(*pv));
                abort();
            }
        }} // ends else { switch(*pv) { 
    } // end loop
}

void wikrt_drop(wikrt_cx* cx) 
{
    wikrt_ss ss = 0;
    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }

    _Static_assert(!WIKRT_NEED_FREE_ACTION, "free dropped cell");
    wikrt_val* const pv = wikrt_pval(cx,cx->val);
    cx->val = pv[1];

    wikrt_drop_v(cx, pv[0], &ss);
    if(!wikrt_ss_droppable(ss)) {
        wikrt_set_error(cx, WIKRT_ETYPE);
    }
}

void wikrt_trash(wikrt_cx* cx)
{
    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    _Static_assert(!WIKRT_NEED_FREE_ACTION, "free trashed data");

    wikrt_val* const pv = wikrt_pval(cx, cx->val);
    wikrt_ss ss = 0;
    wikrt_drop_v(cx, *pv, &ss);
    (*pv) = WIKRT_UNIT_INR;

    // use block relevant/affine tags.
    wikrt_otag const otag 
        = WIKRT_OTAG_TRASH
        | (wikrt_ss_copyable(ss)  ? 0 : WIKRT_BLOCK_AFFINE)
        | (wikrt_ss_droppable(ss) ? 0 : WIKRT_BLOCK_RELEVANT);
    
    wikrt_wrap_otag(cx, otag);
}

void wikrt_intro_unit(wikrt_cx* cx) {
    if(!wikrt_mem_reserve(cx, WIKRT_CELLSIZE)) { return; }
    cx->val = wikrt_alloc_cellval_r(cx, WIKRT_P, WIKRT_UNIT, cx->val);
}

void wikrt_intro_unit_r(wikrt_cx* cx) {
    if(!wikrt_mem_reserve(cx, WIKRT_CELLSIZE)) { return; }
    cx->val = wikrt_alloc_cellval_r(cx, WIKRT_P, cx->val, WIKRT_UNIT);
}

void wikrt_elim_unit(wikrt_cx* cx)
{
    _Static_assert(!WIKRT_NEED_FREE_ACTION, "need to recycle cell on elim_unit");
    bool const type_ok = wikrt_p(cx->val) && (WIKRT_UNIT == (wikrt_pval(cx, cx->val)[0]));
    if(!type_ok) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    cx->val = wikrt_pval(cx, cx->val)[1];
}

void wikrt_elim_unit_r(wikrt_cx* cx)
{
    _Static_assert(!WIKRT_NEED_FREE_ACTION, "need to recycle cell on elim_unit_r");
    bool const type_ok = wikrt_p(cx->val) && (WIKRT_UNIT == (wikrt_pval(cx, cx->val)[1]));
    if(!type_ok) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    cx->val = wikrt_pval(cx, cx->val)[0];
}

// non-allocating `w` swap
static inline void wikrt_wswap_v(wikrt_cx* cx, wikrt_val const abc) 
{
    if(wikrt_p(abc)) {
        wikrt_val* const pabc = wikrt_pval(cx,abc);
        wikrt_val  const bc   = pabc[1];
        if(wikrt_p(bc)) {
            wikrt_val* const pbc = wikrt_pval(cx,bc);
            wikrt_pval_swap(pabc, pbc);
            return;
        }
    }
    wikrt_set_error(cx, WIKRT_ETYPE);
}

/** (a*(b*c))→(b*(a*c)). ABC op `w` */
void wikrt_wswap(wikrt_cx* cx)
{
    wikrt_wswap_v(cx, cx->val);
}

/** (a*(b*(c*d)))→(a*(c*(b*d))). ABC op `z` */
void wikrt_zswap(wikrt_cx* cx)
{
    // run wswap on the (b*(c*d)) fragment.
    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    wikrt_wswap_v(cx, wikrt_pval(cx, cx->val)[1]);
}

static void wikrt_assocl_v(wikrt_cx* cx, wikrt_val a_bc) 
{
    // this op must be blazing fast in normal case.
    if(wikrt_p(a_bc)) {
        wikrt_val* const pa_bc = wikrt_pval(cx, a_bc);
        wikrt_val const bc = pa_bc[1];
        if(wikrt_p(bc)) {
            wikrt_val* const pbc = wikrt_pval(cx, bc);
            wikrt_val const a = pa_bc[0];
            pa_bc[0] = bc; // old a → bc
            pa_bc[1] = pbc[1]; // old bc → c
            pbc[1] = pbc[0]; // old c → b
            pbc[0] = a; // old b → a
            return;
        }
    }
    wikrt_set_error(cx, WIKRT_ETYPE);
}
/** (a*(b*c))→((a*b)*c). ABC op `l`. */
void wikrt_assocl(wikrt_cx* cx) { wikrt_assocl_v(cx, cx->val); }
void wikrt_accel_wzlw(wikrt_cx* cx) 
{
    // (a * (b * (c * d))) → (a * ((b * c) * d))
    if(wikrt_p(cx->val)) { wikrt_assocl_v(cx, wikrt_pval(cx, cx->val)[1]); }
    else { wikrt_set_error(cx, WIKRT_ETYPE); }   
}

static void wikrt_assocr_v(wikrt_cx* cx, wikrt_val ab_c) 
{
    // ((a*b)*c) → (a*(b*c))
    // this op must be blazing fast in normal case.
    if(wikrt_p(ab_c)) {
        wikrt_val* const pab_c = wikrt_pval(cx, ab_c);
        wikrt_val const ab = pab_c[0];
        if(wikrt_p(ab)) {
            wikrt_val* const pab = wikrt_pval(cx, ab);
            wikrt_val const c = pab_c[1];
            pab_c[1] = ab;
            pab_c[0] = pab[0];
            pab[0] = pab[1]; // b
            pab[1] = c;
            return;
        }
    }
    wikrt_set_error(cx, WIKRT_ETYPE);
}

/** ((a*b)*c)→(a*(b*c)). ABC op `r`. */
void wikrt_assocr(wikrt_cx* cx) { wikrt_assocr_v(cx, cx->val); }
void wikrt_accel_wrzw(wikrt_cx* cx) 
{
    // (a * ((b*c)*d)) → (a * (b * (c * d))), i.e. `r` on second element
    if(wikrt_p(cx->val)) { wikrt_assocr_v(cx, wikrt_pval(cx, cx->val)[1]); }
    else { wikrt_set_error(cx, WIKRT_ETYPE); }
}

/** (a*b)→(b*a). ABC ops `vrwlc`. */
void wikrt_accel_swap(wikrt_cx* cx)
{
    if(wikrt_p(cx->val)) {
        wikrt_val* const p = wikrt_pval(cx, cx->val);
        wikrt_pval_swap(p, (1 + p));
        return;
    }
    wikrt_set_error(cx, WIKRT_ETYPE);
}

void wikrt_wrap_seal(wikrt_cx* cx, char const* s)
{
    // basic validation of input.
    if(!wikrt_valid_token(s)) { wikrt_set_error(cx, WIKRT_INVAL); return; }
    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }

    // wrap head value in a seal.
    wikrt_size const len = strlen(s);
    if((':' == *s) && (len <= sizeof(wikrt_val))) {

        _Static_assert((':' == WIKRT_OTAG_SEAL_SM), "small seal tag should match token string (if little-endian)");
        wikrt_val otag = 0;
        wikrt_size ix = len;
        do { otag = (otag << 8) | (wikrt_val)s[--ix]; } while(0 != ix);
        wikrt_wrap_otag(cx, otag);

    } else {
        // WIKRT_OTAG_SEAL: general case, large or arbitrary sealers
        assert(len < WIKRT_TOK_BUFFSZ);
        wikrt_size const szAlloc = WIKRT_CELLSIZE + wikrt_cellbuff(len);
        if(!wikrt_mem_reserve(cx, szAlloc)) { return; }

        wikrt_addr const addr = wikrt_alloc_r(cx, szAlloc);
        wikrt_val* const pa = wikrt_paddr(cx, addr);
        wikrt_val* const pv = wikrt_pval(cx, cx->val);
        pa[0] = (len << 8) | WIKRT_OTAG_SEAL;
        pa[1] = (*pv);
        memcpy((2+pa), s, len);
        (*pv) = wikrt_tag_addr(WIKRT_O, addr);
    }
}

void wikrt_unwrap_seal(wikrt_cx* cx, char* buff)
{
    _Static_assert(!WIKRT_NEED_FREE_ACTION, "must free token on unwrap");

    // I'm assuming (via API docs) that `buff` is WIKRT_TOK_BUFFSZ
    (*buff) = 0;
    if(wikrt_p(cx->val)) {
        wikrt_val* const v = wikrt_pval(cx, cx->val);
        if(wikrt_o(*v)) {
            wikrt_val* const pv = wikrt_pobj(cx, (*v));
            if(wikrt_otag_seal_sm(*pv)) {
                _Static_assert((WIKRT_TOK_BUFFSZ > sizeof(wikrt_val)), "don't overflow buffer");
                wikrt_otag otag = (*pv);
                int ix = 0;
                do {
                    buff[ix++] = (char)(otag & 0xFF);
                    otag = (otag >> 8);
                } while(sizeof(wikrt_val) != ix);
                buff[ix] = 0;
                (*v) = pv[1];
                return;
            } else if(wikrt_otag_seal(*pv)) {
                size_t const len = ((*pv) >> 8);
                assert(len < WIKRT_TOK_BUFFSZ);
                memcpy(buff, (2 + pv), len);
                buff[len] = 0;
                (*v) = pv[1];
                return;
            }
        }
    }
    wikrt_set_error(cx, WIKRT_ETYPE);
}

_Static_assert((WIKRT_DEEPSUML == (WIKRT_DEEPSUML & 3)) && // 2 bits
               (WIKRT_DEEPSUMR == (WIKRT_DEEPSUMR & 3)) && // 2 bits
               (WIKRT_DEEPSUML != WIKRT_DEEPSUMR) &&       // distinct
               (WIKRT_DEEPSUML != 0) && (WIKRT_DEEPSUMR != 0) // non-zero
              , "assumptions for deep sum structure");

_Static_assert((WIKRT_PL == (1 + WIKRT_P)) && (WIKRT_PR == (2 + WIKRT_P)) &&
               (WIKRT_UL == (1 + WIKRT_U)) && (WIKRT_UR == (2 + WIKRT_U)) &&
               (WIKRT_USING_MINIMAL_BITREP)
              , "assumptions for shallow sum structures in reference");


static inline bool wikrt_deepsum_with_free_space(wikrt_cx* cx, wikrt_val v) 
{
    if(!wikrt_o(v)) { return false; }
    wikrt_val const otag = *wikrt_pobj(cx, v);
    // need two bits free space to squeeze in another sum step.
    return wikrt_otag_deepsum(otag) && (otag < (WIKRT_VAL_MAX >> 2));
}

// wikrt_wrap_sum, assuming wikrt_p(cx->val). Mostly to ensure optimization
// across multiple calls is viable.
static void wikrt_wrap_sum_p(wikrt_cx* cx, wikrt_sum_tag sum)
{
    _Static_assert(WIKRT_USING_MINIMAL_BITREP, "after any bitrep change, review this function.");
    bool const inL = (WIKRT_INL == sum);
    wikrt_val* const v = wikrt_pval(cx, cx->val);
    if(1 == (3 & (*v))) {
        // WIKRT_P and WIKRT_U have shallow encoding for sums.
        // WIKRT_P → WIKRT_PL or WIKRT_PR. 
        // WIKRT_U → WIKRT_UL or WIKRT_UR.
        (*v) += inL ? 1 : 2;
    } else if(wikrt_deepsum_with_free_space(cx, (*v))) {
        // extend existing deepsum.
        wikrt_val* const pv = wikrt_pobj(cx, (*v));
        wikrt_val const s0 = (*pv) >> 8;
        wikrt_val const sf = (s0 << 2) | (inL ? WIKRT_DEEPSUML : WIKRT_DEEPSUMR); 
        wikrt_val const otag = (sf << 8) | WIKRT_OTAG_DEEPSUM;
        (*pv) = otag;
    } else { 
        // allocating extension of deep sum.
        wikrt_val const sf = inL ? WIKRT_DEEPSUML : WIKRT_DEEPSUMR;
        wikrt_val const otag = (sf << 8) | WIKRT_OTAG_DEEPSUM;
        wikrt_wrap_otag(cx, otag);
    }
}

void wikrt_wrap_sum(wikrt_cx* cx, wikrt_sum_tag sum) 
{
    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    wikrt_wrap_sum_p(cx, sum);
}

// Expansion of a compact sum value. 
// is not a sum that can be expanded.
static void wikrt_expand_sum_p(wikrt_cx* cx) 
{
    if(!wikrt_mem_reserve(cx, WIKRT_CELLSIZE)) { return; }

    wikrt_val* const v = wikrt_pval(cx, cx->val);
    bool const ok_type = wikrt_o(*v);
    if(!ok_type) { wikrt_set_error(cx, WIKRT_ETYPE); }
    wikrt_val* const pv = wikrt_pobj(cx, (*v));

    switch(LOBYTE(*pv)) {
        case WIKRT_OTAG_ARRAY: {
            // pop one element from the array.
            // (hdr, next, size, buffer)
            wikrt_val const* const buff = wikrt_paddr(cx, pv[3]);
            pv[3] += sizeof(wikrt_val);
            pv[2] -= 1;
            wikrt_val const hd = *buff;
            wikrt_val const tl = (0 == pv[2]) ? pv[1] : (*v);
            (*v) = wikrt_alloc_cellval_r(cx, WIKRT_PL, hd, tl);
        } break;
        case WIKRT_OTAG_BINARY: {
            // (hdr, next, size, buffer)
            uint8_t const* const buff = (uint8_t*) wikrt_paddr(cx,pv[3]);
            pv[3] += 1;
            pv[2] -= 1;
            wikrt_val const hd = wikrt_i2v((wikrt_int)(*buff));
            wikrt_val const tl = (0 == pv[2]) ? pv[1] : (*v);
            (*v) = wikrt_alloc_cellval_r(cx, WIKRT_PL, hd, tl);
        } break;
        case WIKRT_OTAG_UTF8: {
            // (utf8, binary). 
            // drop utf8 tag, extract up to four bytes from binary,
            // then read the character. I will try to reuse the `utf8` 
            // tag to minimize allocation.
            _Static_assert(!WIKRT_NEED_FREE_ACTION, "free utf8 tag");
            (*v) = pv[1]; // access the binary.

            uint8_t buff[UTF8_MAX_CP_SIZE];
            size_t readct = 1;
            wikrt_read_binary(cx, buff, &readct);
            if(0 == readct) { return; }

            size_t const bytes_needed = utf8_readcp_size(buff) - 1; // 0-3
            if(0 != bytes_needed) {
                assert(bytes_needed <= 3); 
                readct = bytes_needed;
                wikrt_read_binary(cx, (1 + buff), &readct);
                assert((bytes_needed == readct) || (wikrt_has_error(cx)));
            }

            uint32_t cp;
            utf8_readcp_unsafe(buff, &cp);

            wikrt_wrap_otag(cx, WIKRT_OTAG_UTF8);
            wikrt_intro_i32(cx, (int32_t) cp);
            wikrt_cons(cx);

        } break;
        default: {
            wikrt_set_error(cx, WIKRT_ETYPE);
        }
    } // end switch
}

// unwrap sum but assuming cx->val is a pair. This is more to
// ensure static linking of the implementation.
static void wikrt_unwrap_sum_p(wikrt_cx* cx, wikrt_sum_tag* sum)
{ tailcall: { 
    _Static_assert(WIKRT_USING_MINIMAL_BITREP, "after any bitrep change, review this function.");
    wikrt_val* const v = wikrt_pval(cx, cx->val);

    if(2 & (*v)) { // WIKRT_PL, WIKRT_PR, WIKRT_UL, WIKRT_UR
        if(1 & (*v)) { // WIKRT_PR or WIKRT_UR
            (*sum) = WIKRT_INR;
            (*v) -= 2;
        } else { // WIKRT_PL or WIKRT_UL
            (*sum) = WIKRT_INL;
            (*v) -= 1;
        }
    } else if(wikrt_o(*v)) {
        wikrt_val* const pv = wikrt_pobj(cx, (*v));
        if(wikrt_otag_deepsum(*pv)) {
            wikrt_val const s0 = (*pv) >> 8;
            bool const inL = (WIKRT_DEEPSUML == (3 & s0));
            (*sum) = inL ? WIKRT_INL : WIKRT_INR;
            wikrt_val const sf = s0 >> 2;
            if(0 == sf) { 
                _Static_assert(!WIKRT_NEED_FREE_ACTION, "must free sum on unwrap");
                (*v) = pv[1]; // drop deepsum wrapper
            } else { 
                (*pv) = sf << 8 | WIKRT_OTAG_DEEPSUM;
            }
            
        } else {
            // expand element from array, binary, etc.
            wikrt_expand_sum_p(cx);
            if(!wikrt_has_error(cx)) { goto tailcall; }
        }
    } else { wikrt_set_error(cx, WIKRT_ETYPE); }
}}


void wikrt_unwrap_sum(wikrt_cx* cx, wikrt_sum_tag* sum) 
{
    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    wikrt_unwrap_sum_p(cx, sum);
}

static void wikrt_sum_wswap_p(wikrt_cx* cx) 
{
    wikrt_sum_tag a_bc; // (a + (b + c))
    wikrt_unwrap_sum_p(cx, &a_bc);
    if(WIKRT_INL == a_bc) {
        wikrt_wrap_sum_p(cx, WIKRT_INL); // (a + _)
        wikrt_wrap_sum_p(cx, WIKRT_INR); // (_ + (a + _))
    } else {
        wikrt_sum_tag b_c;
        wikrt_unwrap_sum_p(cx, &b_c);
        if(WIKRT_INL == b_c) { // 'b' → (b + _)
            wikrt_wrap_sum_p(cx, WIKRT_INL); // (b + _)
        } else { // we have 'c'.
            wikrt_wrap_sum_p(cx, WIKRT_INR); // (_ + c)
            wikrt_wrap_sum_p(cx, WIKRT_INR); // (_ + (_ + c))
        }
    }
}

// Thoughts: I'd like to maybe ensure sum manipulations are non-allocating,
// at least on average. One option here might be to use free-lists in some
// very limited capacity, together with reserving enough to guarantee that
// allocation will succeed.
void wikrt_sum_wswap(wikrt_cx* cx)
{
    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    wikrt_sum_wswap_p(cx);
}

void wikrt_sum_zswap(wikrt_cx* cx)
{
    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    wikrt_sum_tag a_bcd;
    wikrt_unwrap_sum_p(cx, &a_bcd);
    if(WIKRT_INR == a_bcd) { wikrt_sum_wswap_p(cx); }
    wikrt_wrap_sum_p(cx, a_bcd);
}

void wikrt_sum_assocl(wikrt_cx* cx) 
{
    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    wikrt_sum_tag a_bc;
    wikrt_unwrap_sum_p(cx, &a_bc);
    if(WIKRT_INL == a_bc) { // a → ((a + _) + _)
        wikrt_wrap_sum_p(cx, WIKRT_INL);
        wikrt_wrap_sum_p(cx, WIKRT_INL);
    } else {
        wikrt_sum_tag b_c;
        wikrt_unwrap_sum_p(cx, &b_c); 
        wikrt_wrap_sum_p(cx, WIKRT_INR); // (_ + b) or (_ + c) (or (_ + ?) on error)
        if(WIKRT_INL == b_c) { 
            wikrt_wrap_sum_p(cx, WIKRT_INL);  // ((_ + b) + _)
        }
    }
}

void wikrt_sum_assocr(wikrt_cx* cx)
{
    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    wikrt_sum_tag ab_c;
    wikrt_unwrap_sum_p(cx, &ab_c);
    if(WIKRT_INL != ab_c) { // 'c' → (_ + (_ + c))
        wikrt_wrap_sum_p(cx, WIKRT_INR);
        wikrt_wrap_sum_p(cx, WIKRT_INR);
    } else { // in (a+b) in left → 'a' in left or 'b' in left of right.
        wikrt_sum_tag a_b;
        wikrt_unwrap_sum_p(cx, &a_b);
        wikrt_wrap_sum_p(cx, WIKRT_INL); // (a + _) or (b + _)
        if(WIKRT_INL != a_b) { 
            wikrt_wrap_sum_p(cx, WIKRT_INR); // (_ + (b + _))
        }
    }
}

void wikrt_accel_sum_swap(wikrt_cx* cx) 
{
    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    wikrt_sum_tag lr;
    wikrt_unwrap_sum_p(cx, &lr);
    wikrt_sum_tag const rl = (WIKRT_INL == lr) ? WIKRT_INR : WIKRT_INL; // swapped tag.
    wikrt_wrap_sum_p(cx, rl);
}

/** (a*((b+c)*e))→(((a*b)+(a*c))*e). ABC op `D`. */
void wikrt_sum_distrib(wikrt_cx* cx)
{
    wikrt_wswap(cx);
    wikrt_sum_tag lr;
    wikrt_unwrap_sum(cx, &lr);
    wikrt_wswap(cx);
    wikrt_assocl(cx);
    wikrt_wrap_sum(cx, lr);
}

/** (((a*b)+(c*d))*e)→((a+c)*((b+d)*e)). ABC op `F`. */
void wikrt_sum_factor(wikrt_cx* cx)
{
    wikrt_sum_tag lr;
    wikrt_unwrap_sum(cx, &lr);
    wikrt_assocr(cx);
    wikrt_wswap(cx);
    wikrt_wrap_sum(cx, lr);
    wikrt_wswap(cx);
    wikrt_wrap_sum(cx, lr);
}

// Allocate WIKRT_OTAG_BINARY, and add it to our stack
void wikrt_intro_binary(wikrt_cx* cx, uint8_t const* data, size_t len)
{
    // to resist overflow with `len` manipulations
    if(len >= cx->size) { wikrt_set_error(cx, WIKRT_CXFULL); return; }
    wikrt_sizeb const szBuff = wikrt_cellbuff((wikrt_size)len);
    wikrt_size const szAlloc = (3 * WIKRT_CELLSIZE) + szBuff;
    if(!wikrt_mem_reserve(cx, szAlloc)) { return; }

    // exit quickly for empty binary
    if(0 == len) { wikrt_intro_r(cx, WIKRT_UNIT_INR); return; }

    // okay, we have sufficient space. Let's do this.
    //   hdr→(otag, next, size, buffer)
    //   buffer→copy of data 
    //   introduce 'hdr' on stack as tagged value
    wikrt_addr const buff = wikrt_alloc_r(cx, szBuff);
    memcpy(wikrt_paddr(cx, buff), data, len);

    wikrt_addr const hdr = wikrt_alloc_r(cx, (2 * WIKRT_CELLSIZE));
    wikrt_val* const phdr = wikrt_paddr(cx, hdr);
    phdr[0] = WIKRT_OTAG_BINARY;
    phdr[1] = WIKRT_UNIT_INR;
    phdr[2] = (wikrt_size) len;
    phdr[3] = buff;
    wikrt_intro_r(cx, wikrt_tag_addr(WIKRT_O, hdr));
}

bool wikrt_valid_text_len(char const* s, size_t* bytes)
{
    _Static_assert((sizeof(char) == sizeof(uint8_t)), "invalid cast from char* to uint8_t*");
    uint8_t const* const utf8 = (uint8_t const*)s;
    size_t const maxlen = (*bytes);
    size_t len = 0;
    do {
        uint32_t cp;
        size_t const k = utf8_readcp(utf8 + len, maxlen - len, &cp);
        if((0 == k) || !wikrt_text_char(cp)) { break; }
        len += k;
    } while(true);
    (*bytes) = len;
    return ((maxlen == len) || (0 == utf8[len]));
}

void wikrt_intro_text(wikrt_cx* cx, char const* s, size_t nBytes)
{
    // Validate text binary. Determine actual size if NUL-terminated.
    if(!wikrt_valid_text_len(s, &nBytes)) { 
        wikrt_set_error(cx, WIKRT_INVAL); 
        return; 
    }
    // Just introduce text as a single binary chunk.
    wikrt_intro_binary(cx, (uint8_t const*)s, nBytes);
    wikrt_wrap_otag(cx, WIKRT_OTAG_UTF8);
}

void wikrt_read_binary(wikrt_cx* cx, uint8_t* buff, size_t* bytes) 
{
    // NOTE: This function must not allocate. It may eliminate data.
    _Static_assert(sizeof(uint8_t) == 1, "assuming sizeof(uint8_t) is 1");
    _Static_assert(((WIKRT_SMALLINT_MIN <= 0) && (255 <= WIKRT_SMALLINT_MAX))
        , "assume bytes are small integers");

    size_t const max_bytes = (*bytes);
    (*bytes) = 0;

    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }

    do {
        wikrt_val* const v = wikrt_pval(cx, cx->val);
        if(wikrt_pl(*v)) { // basic list node
            wikrt_val const* const pnode = wikrt_pval(cx, (*v));
            wikrt_int const byte = wikrt_v2i(*pnode);
            bool const okByte = wikrt_smallint(*pnode) && ((0 <= byte) && (byte <= 255)); 
            if(!okByte) { wikrt_set_error(cx, WIKRT_ETYPE); return; }

            if(max_bytes == (*bytes)) { return; } // output limited
            buff[(*bytes)++] = (uint8_t) byte;
            (*v) = pnode[1]; // step next in list
        } else if(wikrt_value_is_compact_binary(cx, (*v))) {
            // optimize read for WIKRT_OTAG_BINARY
            // (hdr, next, size, buffer)
            wikrt_val* const phd = wikrt_pobj(cx, (*v));
            size_t const output_size_limit = (max_bytes - (*bytes));
            bool const output_limited = phd[2] > output_size_limit; 
            size_t const bytes_read = output_limited ? output_size_limit : phd[2];
            memcpy(buff + (*bytes), wikrt_paddr(cx, phd[3]), bytes_read);
            (*bytes) += bytes_read;
            if(output_limited) { // read as much binary as possible
                phd[2] -= (wikrt_size) bytes_read;
                phd[3] += (wikrt_size) bytes_read;
                return;
            } else { // binary is fully read
                (*v) = phd[1];
            }
        } 
        else { // maybe terminal, maybe expandable
            wikrt_sum_tag lr;
            wikrt_unwrap_sum_p(cx, &lr); 
            wikrt_wrap_sum_p(cx, lr);

            if(WIKRT_INR == lr) { return; } // done reading, at end of list

            // I assume that unwrap/wrap will expand a list node.
            bool const list_node_expanded = wikrt_pl(*wikrt_pval(cx, cx->val));
            if(!list_node_expanded) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
        }
    } while(true);
}

// Since I just dump a big utf-8 string into a binary field, I might have an incomplete
// character at the tail end of the string. I need to ensure the returned bytestring
// only contains complete codepoints. Therefore, I need to put back up to three bytes.
static void wikrt_putback_incomplete_utf8(wikrt_cx* cx, char const* str, size_t* sz)
{
    _Static_assert(sizeof(uint8_t) == sizeof(char), "assuming natural cast between uint8_t and char");

    if(0 == (*sz)) { return; }
    uint8_t const* const buff_end = (uint8_t const*) str + (*sz);
    uint8_t const* cpf = buff_end; 
    while(0x80 == ((*(--cpf)) & 0xC0)) { /* NOP */ }

    bool const cpf_complete = ((cpf + utf8_readcp_size(cpf)) == buff_end);
    if(cpf_complete) { return; }

    // put back one byte at a time.
    uint8_t const* s = buff_end;
    do { 
        wikrt_intro_i32(cx, (int32_t) *(--s)); 
        wikrt_cons(cx); 
    } while(cpf != s);

    // adjust size
    (*sz) = ((char const*) cpf) - str;
}

void wikrt_read_text(wikrt_cx* cx, char* buff, size_t* buffsz)
{
    _Static_assert(sizeof(uint8_t) == sizeof(char), "assuming natural cast between uint8_t and char");
    _Static_assert(((WIKRT_SMALLINT_MIN <= 0) && (0x10FFFF <= WIKRT_SMALLINT_MAX))
        , "assuming unicode codepoints are small integers");

    uint8_t* const dst = (uint8_t*) buff;
    size_t const max_buffsz = (*buffsz);
    (*buffsz) = 0;

    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }

    do {
        wikrt_val const list = wikrt_pval(cx, cx->val)[0];
        if(wikrt_pl(list)) { // basic list cons node
            wikrt_val const* const pnode = wikrt_pval(cx, list);
            wikrt_int const cp = wikrt_v2i(*pnode);
            bool const okTextChar = wikrt_smallint(*pnode) 
                && ((0 <= cp) && (cp <= 0x10FFFF))
                && wikrt_text_char(cp);
            if(!okTextChar) { wikrt_set_error(cx, WIKRT_ETYPE); return; }

            uint32_t const cp32 = (uint32_t) cp;
            size_t const next_buffsz = (*buffsz) + utf8_writecp_size(cp32);
            if(next_buffsz > max_buffsz) { return; } // not enough space in buffer
            utf8_writecp_unsafe(dst + (*buffsz), cp32);
            (*buffsz) = next_buffsz;
            wikrt_pval(cx, cx->val)[0] = pnode[1];
        } else if(wikrt_value_is_utf8(cx, list)) {
            _Static_assert(!WIKRT_NEED_FREE_ACTION, "todo: free OTAG_UTF8 node");
            _Static_assert((sizeof(uint8_t) == sizeof(char)), "cast from char* to uint8_t* for utf8");
            wikrt_pval(cx, cx->val)[0] = wikrt_pobj(cx, list)[1]; // drop the UTF8 tag

            // Read into the buffer.
            size_t const max_bytes_read = max_buffsz - (*buffsz);
            size_t bytes_read = max_bytes_read;
            wikrt_read_binary(cx, (uint8_t*) buff, &bytes_read);
            (*buffsz) += bytes_read;

            // Put some bytes back if necessary. Wrap the remaining text.
            wikrt_putback_incomplete_utf8(cx, buff, buffsz);
            wikrt_wrap_otag(cx, WIKRT_OTAG_UTF8);
            // We have either filled the buffer or finished reading the text.
            return;
        } else { // maybe terminal, maybe expandable
            wikrt_sum_tag lr;
            wikrt_unwrap_sum_p(cx, &lr); 
            wikrt_wrap_sum_p(cx, lr);

            if(WIKRT_INR == lr) { return; } // done reading

            // I assume that unwrap/wrap will expand a list node.
            bool const list_node_expanded = wikrt_pl(*wikrt_pval(cx, cx->val));
            if(!list_node_expanded) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
        }
    } while(true);
}

void wikrt_intro_i32(wikrt_cx* cx, int32_t n) 
{
    _Static_assert(((WIKRT_SMALLINT_MIN <= INT32_MIN) && (INT32_MAX <= WIKRT_SMALLINT_MAX)), 
        "assuming no risk of smallint overflow for introducing i32");
    wikrt_intro_smallval(cx, wikrt_i2v(n));
}

void wikrt_intro_i64(wikrt_cx* cx, int64_t n) 
{
    _Static_assert((INT64_MIN < WIKRT_SMALLINT_MIN), "assuming overflow is possible");
    bool const is_smallint = (WIKRT_SMALLINT_MIN <= n) && (n <= WIKRT_SMALLINT_MAX);
    if(is_smallint) { 
        wikrt_intro_smallval(cx, wikrt_i2v(n));
        return;
    } 
    _Static_assert(!WIKRT_HAS_BIGINT, "todo: large i64 to big integers");
    wikrt_set_error(cx, WIKRT_IMPL);
}

static inline bool wikrt_cx_has_integer(wikrt_cx* cx) {
    return wikrt_p(cx->val) 
        && wikrt_integer(cx, wikrt_pval(cx, cx->val)[0]);
}

bool wikrt_peek_i32(wikrt_cx* cx, int32_t* i32)
{
    _Static_assert(!WIKRT_HAS_BIGINT, "assuming just small integers for now");
    _Static_assert((WIKRT_SMALLINT_MIN <= INT32_MIN), "assuming possible overflow for i32"); 
    if(!wikrt_cx_has_integer(cx)) { (*i32) = 0; return false; }
    wikrt_int const i = wikrt_v2i(wikrt_pval(cx, cx->val)[0]);
    if(i > INT32_MAX) { (*i32) = INT32_MAX; return false; }
    else if(INT32_MIN > i) { (*i32) = INT32_MIN; return false; }
    else { (*i32) = (int32_t)i; return true; }
}

bool wikrt_peek_i64(wikrt_cx* cx, int64_t* i64)
{
    _Static_assert(!WIKRT_HAS_BIGINT, "assuming just small integers for now");
    _Static_assert(((INT64_MIN <= WIKRT_SMALLINT_MIN) && (WIKRT_SMALLINT_MAX <= INT64_MAX))
        , "assuming no overflow for Wikilon integer to i64"); 
    if(!wikrt_cx_has_integer(cx)) { (*i64) = 0; return false; }
    (*i64) = (int64_t) wikrt_v2i(wikrt_pval(cx, cx->val)[0]);
    return true;
}

// count digits to express a number
static inline size_t wikrt_decimal_size(wikrt_int n) {
    size_t ct = 0;
    do { ++ct; n /= 10; } while(n > 0);
    return ct;
}

bool wikrt_peek_istr(wikrt_cx* cx, char* const buff, size_t* const buffsz)
{
    _Static_assert(!WIKRT_HAS_BIGINT, "assuming just small integers for now");
    _Static_assert((WIKRT_SMALLINT_MIN == (- WIKRT_SMALLINT_MAX)), "assuming closed negation of smallint");
    _Static_assert((WIKRT_INT_MAX > WIKRT_SMALLINT_MAX), "potential overflow for peek_istr");

    if(!wikrt_cx_has_integer(cx)) { (*buffsz) = 0; return false; }
    wikrt_int const i = wikrt_v2i(wikrt_pval(cx, cx->val)[0]);
    bool const positive = (i >= 0);
    wikrt_int upperDigit = positive ? i : -i; 

    // Compute output size.
    size_t const buffsz_avail = (*buffsz);
    size_t const buffsz_min = (positive ? 0 : 1) // sign
                            + wikrt_decimal_size(upperDigit);
    (*buffsz) = buffsz_min;
    if(buffsz_min > buffsz_avail) { return false; }

    // Write the integer to the buffer.
    char* s = buff + buffsz_min;
    #define WD(n) { *(--s) = ('0' + (n % 10)); n /= 10; }
    do { WD(upperDigit); } while(0 != upperDigit);
    #undef WD
    if(!positive) { *(--s) = '-'; }
    assert(buff == s); // assert match expected size
    return true;
}

static inline bool wikrt_digit_char(char c) { return (('0' <= c) && (c <= '9')); }

// looking for 0 | (-)?[1-9][0-9]*      optional NUL terminal
static bool wikrt_valid_istr(char const* s, size_t* len) 
{
    size_t const maxlen = (*len);
    char const* const s0 = s;
    char const* const eos = s + maxlen;

    if(eos == s) { return false; }

    // special zero case.
    if('0' == (*s)) { ++s; goto scan_done; } 
    
    // (-)?
    if('-' == (*s)) { ++s; }

    // looking for a positive integer [1-9][0-9]*. At least one char.
    if((eos == s) || ('0' == (*s))) { return false; }
    do { 
        if(!wikrt_digit_char(*s)) { return false; }
        ++s;
    } while((eos != s) && (0 != (*s)));

scan_done:

    (*len) = (s - s0);
    return ((eos == s) || (0 == (*s)));
}

static inline uint32_t wikrt_read_inner_digit(char const* s) 
{
    // read exactly nine decimal digits
    uint32_t d = 0;
    #define RD d = (10 * d) + *(s++) - '0'
    RD; RD; RD;
    RD; RD; RD;
    RD; RD; RD;
    #undef RD
    return d;
}

void wikrt_intro_istr(wikrt_cx* cx, char const* const istr, size_t len)
{
    if(!wikrt_valid_istr(istr, &len)) { wikrt_set_error(cx, WIKRT_INVAL); return; }

    char const* eos = istr + len; // reading backward
    char const* s   = istr;       // reading forward

    // okay, we have a valid input string and string length.    
    bool positive = true;
    if('-' == (*s)) {
        positive = false;
        ++s;
        --len;
    }

    // handle smaller integers by simple translation to int64.
    // this simplifies identification of 'small' integers.
    if(len <= 18) { // int64 robustly supports 18 decimal digits
        int64_t iAbs = 0;
        do { iAbs = (10 * iAbs) + ((*s) - '0'); } while(++s != eos);
        wikrt_intro_i64(cx, (positive ? iAbs : -iAbs));
        return;
    }

    // I'm assuming anything past this point is a big integer.
    _Static_assert((WIKRT_SMALLINT_MAX <= 999999999999999999), 
        "I assume more than 18 digits should encode as a big integer.");
    _Static_assert(!WIKRT_HAS_BIGINT, "need to implement intro_istr for big integers");
    wikrt_set_error(cx, WIKRT_IMPL);
    return;
}

static inline bool wikrt_cx_has_two_ints(wikrt_cx* cx)
{
    // looking for (int * (int * e)). 
    wikrt_val const a = cx->val;
    if(wikrt_p(a)) {
        wikrt_val* const pa = wikrt_pval(cx, a);
        wikrt_val const b = pa[1];
        if(wikrt_p(b)) {
            wikrt_val* const pb = wikrt_pval(cx, b);
            return wikrt_integer(cx, *pa) 
                && wikrt_integer(cx, *pb);
        }
    }
    return false;
}


// Add two integers from stack.
void wikrt_int_add(wikrt_cx* cx)
{
    if(!wikrt_cx_has_two_ints(cx)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    wikrt_val* const pabe = wikrt_pval(cx, cx->val);
    wikrt_val const be = pabe[1];
    wikrt_val* const pbe = wikrt_pval(cx, be);

    _Static_assert(!WIKRT_HAS_BIGINT, "assuming small integers");
    _Static_assert((WIKRT_INT_MAX == INT64_MAX), "assuming 64 bit smallints");
    _Static_assert(((2 * WIKRT_SMALLINT_MAX) < WIKRT_INT_MAX), "overflow for smallint add"); 

    int64_t const sum = wikrt_v2i(*pabe) + wikrt_v2i(*pbe); 
    bool const range_ok = ((WIKRT_SMALLINT_MIN <= sum) && (sum <= WIKRT_SMALLINT_MAX));
    if(!range_ok) { wikrt_set_error(cx, WIKRT_IMPL); return; }
    (*pbe) = wikrt_i2v(sum);
    cx->val = be;
}

// Multiply two integers from stack.
void wikrt_int_mul(wikrt_cx* cx)
{
    if(!wikrt_cx_has_two_ints(cx)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    wikrt_val* const pabe = wikrt_pval(cx,cx->val);
    wikrt_val const be = pabe[1];
    wikrt_val* const pbe = wikrt_pval(cx, be);

    _Static_assert((INT64_MAX == WIKRT_INT_MAX), "assuming 64-bit small integers");
    _Static_assert(!WIKRT_HAS_BIGINT, "assuming no big integers for now, integers are small");

    __int128 const prod = ((__int128) wikrt_v2i(*pabe)) 
                        * ((__int128) wikrt_v2i( *pbe));
    bool const range_ok = ((WIKRT_SMALLINT_MIN <= prod) && (prod <= WIKRT_SMALLINT_MAX));
    if(!range_ok) { wikrt_set_error(cx, WIKRT_IMPL); return; }

    (*pbe) = wikrt_i2v((wikrt_int)prod);
    cx->val = be;
}

// Negate an integer. This operation is non-allocating.
void wikrt_int_neg(wikrt_cx* cx)
{
    if(wikrt_p(cx->val)) { 
        wikrt_val* const v = wikrt_pval(cx, cx->val);
        if(wikrt_smallint(*v)) {
            _Static_assert((WIKRT_SMALLINT_MIN == (- WIKRT_SMALLINT_MAX)), "small integer negation should be closed");
            (*v) = wikrt_i2v(- wikrt_v2i(*v));
            return;
        } 
        _Static_assert(!WIKRT_HAS_BIGINT, "todo: negate a big integer");
    }
    wikrt_set_error(cx, WIKRT_ETYPE);
}


static inline void wikrt_smallint_divmod(wikrt_int dividend, wikrt_int divisor, wikrt_int* quot, wikrt_int* rem)
{
    // I need proper modulus, i.e. where the sign is the same
    // as the divisor. C11 instead guarantees that the % has
    // the same sign as the dividend. 
    //
    //      -11 div  3 → (-3) rem (-2)      BAD
    //       11 div -3 → (-3) rem  (2)      BAD
    //      -11 div -3 →  (3) rem (-2)      OK
    //       11 div  3 →  (3) rem  (2)      OK
    //
    // For now, I'll brute-force a repair where it's needed.
    (*quot) = dividend / divisor;
    (*rem)  = dividend % divisor;
    bool const needs_repair = (divisor > 0) ? ((*rem) < 0) : ((*rem) > 0);
    if(needs_repair) {
        // repair is the same regardless
        (*rem)  += divisor;
        (*quot) -= 1;
    }
}

// (I(divisor) * (I(dividend) * e)) → (I(remainder) * (I(quotient) * e)).
void wikrt_int_div(wikrt_cx* cx) 
{
    if(!wikrt_cx_has_two_ints(cx)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    wikrt_val* const pouter = wikrt_pval(cx, cx->val);
    wikrt_val* const pinner = wikrt_pval(cx, pouter[1]);
    wikrt_val const divisor = pouter[0];
    wikrt_val const dividend = pinner[0];

    if(WIKRT_IZERO == divisor) { wikrt_set_error(cx, WIKRT_EDIV0); return; }
    _Static_assert(!WIKRT_HAS_BIGINT, "assuming integers are small");

    wikrt_int q,r;
    wikrt_smallint_divmod(wikrt_v2i(dividend), wikrt_v2i(divisor), &q, &r);
    pouter[0] = wikrt_i2v(r);
    pinner[0] = wikrt_i2v(q);
}

/** @brief Compare two integers. Non-destructive. (I(a)*(I(b)*e)).
 *
 * This compares `b` to `a`, matching direct allocation order (i.e. if we
 * allocate zero then four, the comparison is `zero is less than four`).
 */
void wikrt_int_cmp(wikrt_cx* cx, wikrt_ord* ord)
{
    if(!wikrt_cx_has_two_ints(cx)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    wikrt_val* const pa = wikrt_pval(cx, cx->val);
    wikrt_val* const pb = wikrt_pval(cx, pa[1]);
    _Static_assert(!WIKRT_HAS_BIGINT, "assuming small integers");

    wikrt_int const a = wikrt_v2i(*pa);
    wikrt_int const b = wikrt_v2i(*pb);
    (*ord) = (b > a) ? WIKRT_GT : (b < a) ? WIKRT_LT : WIKRT_EQ;
}

// Quotation - capturing a value into a block in O(1) time.
void wikrt_quote(wikrt_cx* cx) 
{
    wikrt_size const szAlloc = 3 * WIKRT_CELLSIZE;
    if(!wikrt_mem_reserve(cx, szAlloc)) { return; }
    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }

    wikrt_val* const v = wikrt_pval(cx, cx->val);

    // Allocate three cells: block, cons, and opval. .
    // (block, ((opval, (*v)), end-of-list))
    wikrt_addr const a = wikrt_alloc_r(cx, szAlloc);
    wikrt_val* const pa = wikrt_paddr(cx, a);
    pa[0] = WIKRT_OTAG_BLOCK;
    pa[1] = wikrt_tag_addr(WIKRT_PL, a + WIKRT_CELLSIZE);
    pa[2] = wikrt_tag_addr(WIKRT_O, a + (2 * WIKRT_CELLSIZE));
    pa[3] = WIKRT_UNIT_INR;
    pa[4] = WIKRT_OTAG_OPVAL | WIKRT_OPVAL_LAZYKF;
    pa[5] = (*v);
    (*v) = wikrt_tag_addr(WIKRT_O, a);
}

static void wikrt_block_attrib(wikrt_cx* cx, wikrt_val attrib) 
{
    if(wikrt_p(cx->val)) {
        wikrt_val const v = wikrt_pval(cx, cx->val)[0];
        if(wikrt_o(v)) {
            wikrt_val* const pv = wikrt_pobj(cx, v);
            if(wikrt_otag_block(*pv)) {
                (*pv) |= attrib;
                return;
            }
        }
    }
    wikrt_set_error(cx, WIKRT_ETYPE);
}

void wikrt_block_aff(wikrt_cx* cx) { wikrt_block_attrib(cx, WIKRT_BLOCK_AFFINE); }
void wikrt_block_rel(wikrt_cx* cx) { wikrt_block_attrib(cx, WIKRT_BLOCK_RELEVANT); }

static inline bool wikrt_cx_has_two_blocks(wikrt_cx* cx) {
    wikrt_val const abe = cx->val;
    if(wikrt_p(abe)) {
        wikrt_val* const pa = wikrt_pval(cx, abe);
        wikrt_val const be = pa[1];
        if(wikrt_p(be)) {
            wikrt_val* const pb = wikrt_pval(cx, be);
            return wikrt_blockval(cx, *pa) 
                && wikrt_blockval(cx, *pb);
        }
    }
    return false;
}

// scan to the end of a 'short' operator lists with a limited amount of
// effort. The effort determines how much we'll inline by concatenation
// before we switch to an indirect `[[a→b] inline b→c]` format on compose.
//
// Given the `[]vr$c` overhead when printing, a preliminary recommendation
// is to inline at least 6 ops to limit serialization overheads to 50% for
// this naive inlining method.
static inline wikrt_val* wikrt_end_of_short_opslist(wikrt_cx* cx, wikrt_val* list, uint32_t effort)
{
    do {
        if(wikrt_pl(*list)) { list = 1 + wikrt_pval(cx, (*list)); }
        else if(WIKRT_UNIT_INR == (*list)) { return list; }
        else { return NULL; } // TODO: compact operations lists
    } while(effort-- > 0);
    return NULL;
}

// Compose two blocks in O(1) time. ([a→b]*([b→c]*e))→([a→c]*e).
//
// The most natural approach to composition in ABC is concatenation. However,
// I'm not currently structuring blocks to make this trivial. An alternative
// O(1) implementation involves producing:
//
//      [[a→b] inline b→c] 
//
// Here `inline` refers to ABC sequence `vr$c`. I can leverage EXTOP_INLINE as
// an accelerator with that same behavior. This block may be simplified later
// by proper inlining. 
//
// As a special case, I'll inline short blocks from the `[a→b]` structure more
// directly. This can potentially reduce allocation and improve performance,
// especially for binding of quoted values.
// 
void wikrt_compose(wikrt_cx* cx)
{
    wikrt_size const szAlloc = (2 * WIKRT_CELLSIZE); // + reusing one cell
    if(!wikrt_mem_reserve(cx, szAlloc)) { return; }
    if(!wikrt_cx_has_two_blocks(cx)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }

    wikrt_val  const abe = cx->val;
    wikrt_val* const pabe = wikrt_pval(cx, abe);
    wikrt_val  const be = pabe[1];
    wikrt_val* const pbe = wikrt_pval(cx, be);
    wikrt_val  const fn_ab = pabe[0];
    wikrt_val  const fn_bc = pbe[0];
    wikrt_val* const pfnab = wikrt_pobj(cx, fn_ab);
    wikrt_val* const pfnbc = wikrt_pobj(cx, fn_bc);

    // Optimize for case where `[a→b]` is a 'small' function.
    static uint32_t const small_fn = 6;
    wikrt_val* const eoab = wikrt_end_of_short_opslist(cx, 1+pfnab, small_fn);
    if(NULL != eoab) {
        // Concatenative inlining. Non-allocating.

        _Static_assert(!WIKRT_NEED_FREE_ACTION, "dropping several cells on compose");
        assert(WIKRT_UNIT_INR == *eoab);
        (*eoab)   = pfnbc[1]; // addend the functions
        pfnbc[1]  = pfnab[1]; // `[a→c]` code to `[b→c]` slot
        (*pfnbc) |= (*pfnab); // preserve substructure
        cx->val   = be;       // drop the old `[a→b]` slot
        return;

    } else {

        wikrt_addr const a = wikrt_alloc_r(cx, szAlloc);
        wikrt_val* const pa = wikrt_paddr(cx, a);

        cx->val = be; // drop the `[a→b]` stack element
        pabe[0] = WIKRT_OTAG_OPVAL | WIKRT_OPVAL_LAZYKF; // recycle cell for opval
        pabe[1] = fn_ab;
        pa[0] = wikrt_tag_addr(WIKRT_O, wikrt_vaddr(abe)); 
        pa[1] = wikrt_tag_addr(WIKRT_PL, a + WIKRT_CELLSIZE);
        pa[2] = WIKRT_I2V(ACCEL_INLINE);
        pa[3] = pfnbc[1];
        pfnbc[1] = wikrt_tag_addr(WIKRT_PL, a); // [b→c] ⇒ [[a→b] inline b→c].
    }
}



  /////////////////////
 // DEBUG TRACE OUT //
/////////////////////

bool wikrt_trace_enable(wikrt_cx* cx, size_t bufsz) 
{
    if(0 != cx->tb.writer) { return false; } // don't resize while in use!
    assert(0 == cx->tb.reader); 
    cx->tb.buf = realloc(cx->tb.buf, bufsz);
    if(NULL == cx->tb.buf) {
        cx->tb.size = 0;
        return (0 == bufsz);
    } else {
        cx->tb.size = bufsz;
        return true;
    }
}

void wikrt_trace_write(wikrt_cx* cx) 
{
    // Read into the remaining space.
    // Ensure sufficient space for a NUL terminal.
    size_t const space_avail = cx->tb.size - cx->tb.writer;
    size_t bytes_read = (0 == space_avail) ? 0 : (space_avail - 1);
    char* const trace_buf = cx->tb.buf + cx->tb.writer; 
    wikrt_read_text(cx, trace_buf, &bytes_read);
    trace_buf[bytes_read] = 0;

    // keep message if non-empty and we have full message.
    wikrt_sum_tag lr; wikrt_unwrap_sum(cx, &lr);

    bool const ok_msg = (0 < bytes_read) && (WIKRT_INR == lr);
    if(ok_msg) { cx->tb.writer += (bytes_read + 1); }

    // Since trace annotation destructively modifies input, 
    // annotate whatever remains as trashed. This avoids
    // need for implicit copies of trace strings. 
    wikrt_trash(cx); 
}

char const* wikrt_trace_read(wikrt_cx* cx) 
{
    if(cx->tb.reader == cx->tb.writer) {
        // reset the trace buffer.
        cx->tb.reader = 0;
        cx->tb.writer = 0;
        return NULL;
    } else {
        assert(cx->tb.reader < cx->tb.writer);
        char const* const msg = cx->tb.buf + cx->tb.reader;
        cx->tb.reader += (1 + strlen(msg));
        return msg;
    }
}



















  ///////////////////
 // VALUE STOWAGE //
///////////////////
// stubs for now

void wikrt_stow(wikrt_cx* cx) 
{
    /* NOP for now (annotation) */ 
}
void wikrt_load(wikrt_cx* cx)
{
    /* NOP for now (annotation) */
}
void wikrt_intro_sv(wikrt_cx* cx, char const* resourceId)
{
    wikrt_set_error(cx, WIKRT_IMPL);
}
void wikrt_peek_sv(wikrt_cx* cx, char* resourceId)
{
    (*resourceId) = 0;
    wikrt_set_error(cx, WIKRT_IMPL);
}





  ///////////////////////////
 // TRANSACTION SUBSYSTEM //
///////////////////////////

bool wikrt_valid_key_len(char const* k, size_t* len)
{
    (*len) = 1 + WIKRT_VALID_KEY_MAXLEN;
    return wikrt_valid_text_len(k, len) 
        && (1 <= (*len)) && ((*len) <= WIKRT_VALID_KEY_MAXLEN);
}

bool wikrt_valid_key(char const* k) 
{
    size_t len;
    return wikrt_valid_key_len(k, &len);
}
    
// TODO: move transaction processing to a separate file.

void wikrt_txn_abort(wikrt_cx* cx) 
{
    if(!wikrt_cx_has_txn(cx)) {
        wikrt_set_error(cx, WIKRT_INVAL); 
        return;
    }
    wikrt_drop_txn(cx);
}

void wikrt_drop_txn(wikrt_cx* cx) 
{
    wikrt_drop_v(cx, cx->txn, NULL); 
    cx->txn = WIKRT_REG_TXN_INIT;
}


void wikrt_txn_durable(wikrt_cx* cx)
{
    if(!wikrt_cx_has_txn(cx)) { 
        // must be in a transaction
        wikrt_set_error(cx, WIKRT_INVAL); 
        return;
    }
}

void wikrt_txn_create(wikrt_cx* cx) 
{
    if(wikrt_cx_has_txn(cx)) {
        // prevent hierarchical transactions 
        wikrt_set_error(cx, WIKRT_INVAL); 
        return; 
    }
}

bool wikrt_txn_commit(wikrt_cx* cx) 
{
    // TODO: commit transaction
    wikrt_txn_abort(cx);
    return false;
}

void wikrt_txn_write(wikrt_cx* cx, char const* key)
{
    size_t keylen;
    if(!wikrt_valid_key_len(key,&keylen)) { wikrt_set_error(cx, WIKRT_INVAL); return; }
    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }

    // TODO: write to database
    wikrt_set_error(cx, WIKRT_IMPL);
}

void wikrt_txn_read(wikrt_cx* cx, char const* key) 
{
    size_t keylen;
    if(!wikrt_valid_key_len(key, &keylen)) { wikrt_set_error(cx, WIKRT_INVAL); return; }
    // TODO: lookup in database, and record in transaction state.
    wikrt_set_error(cx, WIKRT_IMPL);
}

