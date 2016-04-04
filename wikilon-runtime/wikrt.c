
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <sys/mman.h>
#include <errno.h>

#include "wikrt.h"

void wikrt_acquire_shared_memory(wikrt_cx* cx, wikrt_sizeb sz); 

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

uint32_t wikrt_api_ver() 
{
    _Static_assert(WIKRT_API_VER < UINT32_MAX, "bad value for WIKRT_API_VER");
    return WIKRT_API_VER;
}

wikrt_err wikrt_env_create(wikrt_env** ppEnv, char const* dirPath, uint32_t dbMaxMB) {
    _Static_assert(WIKRT_SIZE_MAX >= 4294967295, "minimum 32-bit words (for texts, etc.)"); 
    _Static_assert(WIKRT_CELLSIZE == WIKRT_CELLBUFF(WIKRT_CELLSIZE), "cell size must be a power of two");
    _Static_assert(WIKRT_PAGESIZE == WIKRT_PAGEBUFF(WIKRT_PAGESIZE), "page size must be a power of two");

    (*ppEnv) = NULL;

    wikrt_env* const e = calloc(1, sizeof(wikrt_env));
    if(NULL == e) { return WIKRT_NOMEM; }
    e->mutex = (pthread_mutex_t) PTHREAD_MUTEX_INITIALIZER;

    if((NULL == dirPath) || (0 == dbMaxMB)) { 
        e->db = NULL;
    } else if(!wikrt_db_init(&(e->db), dirPath, dbMaxMB)) {
        free(e);
        return WIKRT_DBERR;
    }
    // thread pools? etc?

    (*ppEnv) = e;
    return WIKRT_OK;
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

wikrt_err wikrt_cx_create(wikrt_env* e, wikrt_cx** ppCX, uint32_t cxSizeMB) 
{
    (*ppCX) = NULL;

    // validate sizes according to API declarations
    bool const okSize = (WIKRT_CX_MIN_SIZE <= cxSizeMB) 
                     && (cxSizeMB <= WIKRT_CX_MAX_SIZE);
    if(!okSize) { return WIKRT_INVAL; }

    // also prevent size_t overflows on 32-bit systems
    size_t const mem_size = cxSizeMB << 20;
    if(mem_size > (SIZE_MAX / 2)) { return WIKRT_NOMEM; }

    wikrt_cx* const cx = calloc(1, sizeof(wikrt_cx));
    if(NULL == cx) { goto callocErr; }

    // We'll allocate twice the requested space. We'll use the extra
    // both as a semispace and (when necessary) as a scratch space.
    static int const prot = PROT_READ | PROT_WRITE | PROT_EXEC;
    static int const flags = MAP_ANONYMOUS | MAP_PRIVATE;
    void* const twospace = mmap(NULL, (mem_size * 2), prot, flags, -1, 0);
    if(NULL == twospace) { goto mmapErr; }
    
    wikrt_size const size = (wikrt_size)mem_size;
    void* const mem = twospace;
    void* const ssp = (void*)(size + (char*)mem);

    cx->env     = e;
    cx->size    = size; 
    cx->alloc   = size; // we'll work towards zero 
    cx->mem     = mem;
    cx->ssp     = ssp;

    // initialize registers
    cx->val     = WIKRT_REG_VAL_INIT; 
    cx->pc      = WIKRT_REG_PC_INIT;
    cx->cc      = WIKRT_REG_CC_INIT;
    cx->txn     = WIKRT_REG_TXN_INIT;
    cx->a       = WIKRT_REG_VOL_INIT;
    cx->b       = WIKRT_REG_VOL_INIT;
    cx->c       = WIKRT_REG_VOL_INIT;
    cx->d       = WIKRT_REG_VOL_INIT;
    _Static_assert((8 == WIKRT_CX_REGISTER_CT), "todo: missing register initializations"); // maintenance check

    wikrt_add_cx_to_env(cx);

    (*ppCX) = cx;
    return WIKRT_OK;

mmapErr:
callocErr:
    free(cx);
    return WIKRT_NOMEM;
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
    } wikrt_env_unlock(e);
    cx->cxnext = NULL;
    cx->cxprev = NULL;
}

void wikrt_cx_destroy(wikrt_cx* cx) 
{
    wikrt_remove_cx_from_env(cx);

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

void wikrt_mem_compact(wikrt_cx* cx) 
{
    // It's easiest to think about 'compaction' as just a move operation, 
    // albeit one that we know our destination has sufficient space.
    wikrt_cx cx0 = (*cx);

    // swap semispace and memory; reset allocators 
    cx->ssp = cx0.mem;
    cx->mem = cx0.ssp;
    cx->alloc = cx->size;

    // Note: if ever I introduce free lists, clear them here.
    _Static_assert((0 == WIKRT_FLCT), "todo: handle free lists");
    
    // Note: ephemerons will require special attention. Either I add cx0 to
    // our environment briefly (I'd prefer to avoid this synchronization)
    // Or I favor a framed mechanism (e.g. a two-frame bloom filter) such
    // that I can write one frame while reading the other. 
    
    // copy roots with assumption of sufficient space.
    wikrt_copy_r(&cx0, cx0.txn, NULL, cx, &(cx->txn));
    wikrt_copy_r(&cx0, cx0.cc,  NULL, cx, &(cx->cc));
    wikrt_copy_r(&cx0, cx0.pc,  NULL, cx, &(cx->pc));
    wikrt_copy_r(&cx0, cx0.val, NULL, cx, &(cx->val));
    wikrt_copy_r(&cx0, cx0.d,   NULL, cx, &(cx->d));
    wikrt_copy_r(&cx0, cx0.c,   NULL, cx, &(cx->c));
    wikrt_copy_r(&cx0, cx0.b,   NULL, cx, &(cx->b));
    wikrt_copy_r(&cx0, cx0.a,   NULL, cx, &(cx->a));
    _Static_assert((8 == WIKRT_CX_REGISTER_CT), "todo: missing register compactions"); // maintenance check


    // refresh our semispace as if freshly mmap'd.
    // This should help cut memory requirements, shifting instead to
    // 'mere' address-space requirements.
    errno = 0;
    if(0 != madvise(cx->ssp, cx->size, MADV_DONTNEED)) {
        fprintf(stderr, "%s: madvise failed (%s)\n", __FUNCTION__, strerror(errno));
        abort();
    }

    // keep stats. compaction count is useful for effort quotas. 
    // compaction size is useful for heuristic memory pressure.
    cx->compaction_count += 1;
    cx->compaction_size  = (cx->size - cx->alloc);
}

bool wikrt_alloc_c(wikrt_cx* cx, wikrt_size sz, wikrt_addr* addr)
{
    sz = WIKRT_CELLBUFF(sz);
    if(!wikrt_mem_reserve(cx, sz)) { return false; }
    (*addr) = wikrt_alloc_r(cx, sz);
    return true;
}


wikrt_err wikrt_move(wikrt_cx* const lcx, wikrt_cx* const rcx) 
{
    // this function could feasibly be optimized, especially if
    // I later restore the wikrt_cx_fork() feature. But for now,
    // I'll use a fail-safe moving deep copy followed by drop.
    wikrt_err const st = wikrt_copy_move(lcx, NULL, rcx);
    if(WIKRT_OK == st) { wikrt_drop(lcx, NULL); } 
    return st;
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
    // base implementation for both wikrt_copy and wikrt_copy_move.
    // in lcx == rcx case, the copy is stacked above the original.
    if(!wikrt_p(lcx->val)) { return WIKRT_TYPE_ERROR; }

    // reserve space in `rcx`.
    wikrt_size const max_alloc = WIKRT_CELLSIZE + (lcx->size - lcx->alloc);
    bool const sufficient_space = 
            (wikrt_mem_available(rcx, max_alloc) && WIKRT_ALLOW_SIZE_BYPASS)
        ||  (wikrt_mem_reserve(rcx, wikrt_vsize_ssp(lcx, *(wikrt_pval(lcx, lcx->val)))
                                  + WIKRT_CELLSIZE));
    if(!sufficient_space) { return WIKRT_CXFULL; }

    // note: mem_reserve may move lcx->val if lcx == rcx.
    // anyhow, we now have space to perform our copy!
    wikrt_val const copy_src = *(wikrt_pval(lcx, lcx->val));
    wikrt_val copy_dst = WIKRT_VOID;
    wikrt_copy_r(lcx, copy_src, ss, rcx, &copy_dst);
    wikrt_intro_r(rcx, copy_dst);
    return WIKRT_OK;
}

// stack mirrors copy stack, only carrying elements that need allocation.
static inline void wikrt_add_size_task(wikrt_val** s, wikrt_val v) {
    if(!wikrt_copy_shallow(v)) { *((*s)++) = v; }}

wikrt_size wikrt_vsize_ssp(wikrt_cx* cx, wikrt_val const v0)
{
    wikrt_size result = 0;
    wikrt_val* const s0 = (wikrt_val*)(cx->ssp); // stack of values to be examined.
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
            // simple (tag,value) pairs:
            case WIKRT_OTAG_SEAL_SM:
            case WIKRT_OTAG_BLOCK:  
            case WIKRT_OTAG_OPVAL:  
            case WIKRT_OTAG_DEEPSUM: {
                result += WIKRT_CELLSIZE;
                wikrt_add_size_task(&s, pv[1]); // wrapped value
            } break;
            case WIKRT_OTAG_BIGINT: {
                wikrt_size const nDigits = ((*pv) >> 9);
                wikrt_size const szAlloc = sizeof(wikrt_val) + (nDigits * sizeof(uint32_t));
                result += WIKRT_CELLBUFF(szAlloc);
            } break;
            case WIKRT_OTAG_OPTOK: {
                wikrt_size const toklen = ((*pv) >> 8);
                wikrt_size const szAlloc = sizeof(wikrt_val) + toklen;
                result += WIKRT_CELLBUFF(szAlloc);
            } break;
            case WIKRT_OTAG_SEAL: {
                wikrt_size const toklen = ((*pv) >> 8);
                wikrt_size const szAlloc = WIKRT_CELLSIZE + toklen;
                result += WIKRT_CELLBUFF(szAlloc);
                wikrt_add_size_task(&s,pv[1]); // sealed value
            } break;
            case WIKRT_OTAG_BINARY: {
                // (hdr, next, size, buffer).
                wikrt_size const bytect = pv[2];
                result += (2 * WIKRT_CELLSIZE) + WIKRT_CELLBUFF(bytect);
                wikrt_add_size_task(&s, pv[1]); // continue list
            } break;
            case WIKRT_OTAG_TEXT: {
                // (hdr, next, (size-chars, size_bytes), buffer).
                wikrt_size const bytect = (pv[2] & 0xFFFF);
                result += (2 * WIKRT_CELLSIZE) + WIKRT_CELLBUFF(bytect);
                wikrt_add_size_task(&s,pv[1]); // continue list
            } break;
            case WIKRT_OTAG_ARRAY: {
                // (hdr, next, elemct, buffer).
                wikrt_size const elemct = pv[2];
                wikrt_size const buffsz = elemct * sizeof(wikrt_val);
                result += (2 * WIKRT_CELLSIZE) + WIKRT_CELLBUFF(buffsz);
                wikrt_val const* const parray = wikrt_paddr(cx, pv[3]);
                for(wikrt_size ii = 0; ii < elemct; ++ii) {
                    wikrt_add_size_task(&s, parray[ii]);
                }
                wikrt_add_size_task(&s, pv[1]); // continue list
            } break;
            default: {
                fprintf(stderr, "%s: unrecognized tagged value (tag %x)"
                       ,__FUNCTION__, (unsigned int)(*pv));
                abort();
            } 
        }} // end else { switch() {
    } // end of loop
    return result;
} 

// I wonder how well this will optimize? will rcx->mem be extracted as a loop invariant?
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
static void wikrt_copy_rs(wikrt_cx* const lcx, wikrt_cx* const rcx, wikrt_ss* const ss
                         ,wikrt_addr* const s0, wikrt_val* dst) 
{
    if(wikrt_copy_shallow(*dst)) { return; }

    wikrt_addr* s = s0;
    do {
        // invariant: `dst` constains non-shallow reference into lcx->mem 
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
            case WIKRT_OTAG_SEAL_SM: // same as OTAG_DEEPSUM
            case WIKRT_OTAG_DEEPSUM: {
                wikrt_addr const addr = wikrt_alloc_r(rcx, WIKRT_CELLSIZE);
                (*dst) = wikrt_tag_addr(WIKRT_O, addr);
                *(wikrt_paddr(rcx, addr)) = *pv;
                wikrt_cpv(rcx, &s, pv, addr, 1);
            } break;

            // block is (tag, val) with substructure
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
                    wikrt_copy_rs(rcx, lcx, NULL, s, dst);
                } else {
                    wikrt_cpv(rcx, &s, pv, addr, 1);
                }
            } break;

            case WIKRT_OTAG_OPTOK: {
                wikrt_size const toklen = ((*pv) >> 8);
                wikrt_size const szAlloc = sizeof(wikrt_val) + toklen;
                wikrt_addr const addr = wikrt_alloc_r(rcx, WIKRT_CELLBUFF(szAlloc));
                (*dst) = wikrt_tag_addr(WIKRT_O, addr);
                memcpy(wikrt_paddr(rcx, addr), pv, szAlloc);
            } break;

            case WIKRT_OTAG_BIGINT: {
                wikrt_size const nDigits = ((*pv) >> 9);
                wikrt_size const szAlloc = sizeof(wikrt_val) + (nDigits * sizeof(uint32_t));
                wikrt_addr const addr = wikrt_alloc_r(rcx, WIKRT_CELLBUFF(szAlloc));
                (*dst) = wikrt_tag_addr(WIKRT_O, addr);
                memcpy(wikrt_paddr(rcx, addr), pv, szAlloc);
            } break;

            case WIKRT_OTAG_SEAL: {
                wikrt_size const toklen = ((*pv) >> 8);
                wikrt_size const szAlloc = WIKRT_CELLSIZE + toklen;
                wikrt_addr const addr = wikrt_alloc_r(rcx, WIKRT_CELLBUFF(szAlloc));
                (*dst) = wikrt_tag_addr(WIKRT_O, addr);
                memcpy(wikrt_paddr(rcx, addr), pv, szAlloc);
                wikrt_cpv(rcx, &s, pv, addr, 1);
            } break;

            case WIKRT_OTAG_BINARY: {
                // (hdr, next, size, buffer).
                wikrt_size const bytect = pv[2];
                wikrt_addr const buff = wikrt_alloc_r(rcx, WIKRT_CELLBUFF(bytect));
                memcpy(wikrt_paddr(rcx, buff), wikrt_paddr(lcx, pv[3]), bytect);

                wikrt_addr const hdr = wikrt_alloc_r(rcx, (2 * WIKRT_CELLSIZE));
                wikrt_val* const phd = wikrt_paddr(rcx, hdr);
                phd[0] = pv[0]; // tag
                phd[2] = pv[2]; // elemct
                phd[3] = buff;  // array
                wikrt_cpv(rcx, &s, pv, hdr, 1); // continue list
                (*dst) = wikrt_tag_addr(WIKRT_O, hdr);
            } break;

            case WIKRT_OTAG_TEXT: {
                // (hdr, next, (size-chars, size_bytes), buffer).
                wikrt_size const bytect = (pv[2] & 0xFFFF);
                wikrt_addr const buff = wikrt_alloc_r(rcx, WIKRT_CELLBUFF(bytect));
                memcpy(wikrt_paddr(rcx, buff), wikrt_paddr(lcx, pv[3]), bytect);

                wikrt_addr const hdr = wikrt_alloc_r(rcx, (2 * WIKRT_CELLSIZE));
                wikrt_val* const phd = wikrt_paddr(rcx, hdr);
                phd[0] = pv[0]; // tag
                phd[2] = pv[2]; // sizes (chars and bytes)
                phd[3] = buff;  // copied buffer
                wikrt_cpv(rcx, &s, pv, hdr, 1); // continue list
                (*dst) = wikrt_tag_addr(WIKRT_O, hdr);
            } break;

            case WIKRT_OTAG_ARRAY: {
                // (hdr, next, elemct, buffer).
                wikrt_size const elemct = pv[2];
                wikrt_size const buffsz = elemct + sizeof(wikrt_val);
                wikrt_addr const buff = wikrt_alloc_r(rcx, WIKRT_CELLBUFF(buffsz));
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

void wikrt_copy_r(wikrt_cx* lcx, wikrt_val lval, wikrt_ss* ss, wikrt_cx* rcx, wikrt_val* rval)
{
    // Note: I use a stack in rcx->mem (that builds upwards from zero). This stack
    // only contains addresses in rcx->mem that contain values referencing lcx->mem.
    // Since each such value requires an allocation of at least a full cell, I can
    // guarantee that this stack is smaller than my remaining available space.
    // 
    if(NULL != ss) { (*ss) = 0; }
    (*rval) = lval;
    wikrt_copy_rs(lcx, rcx, ss, ((wikrt_addr*)(rcx->mem)), rval);
}


static inline void wikrt_add_drop_task(wikrt_val** s, wikrt_val v) {
    // any copy_shallow values are also shallow for drop.
    if(!wikrt_copy_shallow(v)) { *((*s)++) = v; } 
}

void wikrt_drop_sv(wikrt_cx* cx, wikrt_val* const s0, wikrt_val const v0, wikrt_ss* ss) 
{
    // currently don't need to touch anything if not tracking `ss`.
    // This might change if I introduce new value types, so I'll
    // only include the escape here and otherwise treat 'drop' as
    // touching everything.  
    if(NULL == ss) { return; }

    wikrt_val* s = s0;
    wikrt_add_drop_task(&s,v0);

    while(s0 != s) {
        wikrt_val const v = *(--s);
        wikrt_val const* const pv = wikrt_pval(cx, v);
        if(WIKRT_O != wikrt_vtag(v)) {
            wikrt_add_drop_task(&s,pv[0]);
            wikrt_add_drop_task(&s,pv[1]);
        } else { switch(LOBYTE(*pv)) {
            // (tag, binary)
            case WIKRT_OTAG_BIGINT:
            case WIKRT_OTAG_OPTOK: {
                // nothing to do here
            } break;

            // (tag, val, (... potential binary ...)) 
            case WIKRT_OTAG_SEAL:   
            case WIKRT_OTAG_SEAL_SM:
            case WIKRT_OTAG_BINARY:
            case WIKRT_OTAG_TEXT:
            case WIKRT_OTAG_DEEPSUM: {
                wikrt_add_drop_task(&s,pv[1]);
            } break;

            // block headers are my primary source of substructure.
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


wikrt_err wikrt_drop(wikrt_cx* cx, wikrt_ss* ss) 
{
    if(NULL != ss) { (*ss) = 0; }
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    wikrt_val* const pv = wikrt_pval(cx,cx->val);
    cx->val = pv[1];
    wikrt_drop_sv(cx, (wikrt_val*)(cx->ssp), pv[0], ss);
    return WIKRT_OK;
}

wikrt_err wikrt_intro_unit(wikrt_cx* cx) {
    return wikrt_alloc_cellval(cx, &(cx->val), WIKRT_P, WIKRT_UNIT, cx->val); 
}

wikrt_err wikrt_intro_unit_r(wikrt_cx* cx) {
    return wikrt_alloc_cellval(cx, &(cx->val), WIKRT_P, cx->val, WIKRT_UNIT); 
}

wikrt_err wikrt_elim_unit(wikrt_cx* cx)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    wikrt_val const* const pv = wikrt_pval(cx, cx->val);
    if(WIKRT_UNIT != pv[0]) { return WIKRT_TYPE_ERROR; }
    cx->val = pv[1];
    return WIKRT_OK;
}

wikrt_err wikrt_elim_unit_r(wikrt_cx* cx)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    wikrt_val const* const pv = wikrt_pval(cx, cx->val);
    if(WIKRT_UNIT != pv[1]) { return WIKRT_TYPE_ERROR; }
    cx->val = pv[0];
    return WIKRT_OK;
}


// non-allocating, data moving, fail-safe wswap
static inline wikrt_err wikrt_wswap_v(wikrt_cx* cx, wikrt_val const abc) 
{
    if(wikrt_p(abc)) {
        wikrt_val* const pabc = wikrt_pval(cx,abc);
        wikrt_val  const bc   = pabc[1];
        if(wikrt_p(bc)) {
            wikrt_val* const pbc = wikrt_pval(cx,bc);
            wikrt_pval_swap(pabc, pbc);
            return WIKRT_OK;
        }
    }
    return WIKRT_TYPE_ERROR;
}

/** (a*(b*c))→(b*(a*c)). ABC op `w` */
wikrt_err wikrt_wswap(wikrt_cx* cx)
{
    return wikrt_wswap_v(cx,cx->val);
}

/** (a*(b*(c*d)))→(a*(c*(b*d))). ABC op `z` */
wikrt_err wikrt_zswap(wikrt_cx* cx)
{
    // hide `a` temporarily; run wswap on what's left.
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_wswap_v(cx, *(wikrt_pval(cx, cx->val)));
}

/** (a*(b*c))→((a*b)*c). ABC op `l`. */
wikrt_err wikrt_assocl(wikrt_cx* cx) 
{
    // this op must be blazing fast in normal case.
    wikrt_val const abc = cx->val;
    if(wikrt_p(abc)) {
        wikrt_val* const pa_bc = wikrt_pval(cx, abc);
        wikrt_val const bc = pa_bc[1];
        if(wikrt_p(bc)) {
            wikrt_val* const pbc = wikrt_pval(cx, bc);
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
    // this op must be blazing fast in normal case.
    wikrt_val const abc = cx->val;
    if(wikrt_p(abc)) {
        wikrt_val* const pab_c = wikrt_pval(cx, abc);
        wikrt_val const ab = pab_c[0];
        if(wikrt_p(ab)) {
            wikrt_val* const pab = wikrt_pval(cx, ab);
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
    if(wikrt_p(cx->val)) {
        wikrt_val* const p = wikrt_pval(cx, cx->val);
        wikrt_pval_swap(p, (1 + p));
        return WIKRT_OK;
    }
    return WIKRT_TYPE_ERROR;
}


wikrt_err wikrt_wrap_seal(wikrt_cx* cx, char const* s)
{
    // Ensure we have sufficient space for a sealer token.
    size_t const worst_case_alloc = WIKRT_CELLSIZE + WIKRT_TOK_BUFFSZ;
    if(!wikrt_mem_reserve(cx, worst_case_alloc)) { return WIKRT_CXFULL; }

    // basic validation of input.
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    if(!wikrt_valid_token(s)) { return WIKRT_INVAL; }

    // wrap head value in a seal.
    wikrt_size const len = strlen(s);
    if((':' == *s) && (len <= 4)) {
        // WIKRT_OTAG_SEAL_SM: optimized case, small discretionary sealers
        #define TAG(N) ((len > N) ? (((wikrt_val)s[N]) << (8*N)) : 0)
        wikrt_val const otag = TAG(3) | TAG(2) | TAG(1) | WIKRT_OTAG_SEAL_SM;
        #undef TAG
        wikrt_val* const v = wikrt_pval(cx, cx->val);
        wikrt_alloc_cellval_r(cx, v, WIKRT_O, otag, (*v));
        return WIKRT_OK;
    } else {
        // WIKRT_OTAG_SEAL: general case, large or arbitrary sealers
        wikrt_size const szData  = WIKRT_CELLSIZE + len;
        wikrt_size const szAlloc = WIKRT_CELLBUFF(szData); 
        wikrt_addr const addr = wikrt_alloc_r(cx, szAlloc);
        wikrt_val* const pa = wikrt_paddr(cx, addr);
        wikrt_val* const pv = wikrt_pval(cx, cx->val);
        pa[0] = (len << 8) | WIKRT_OTAG_SEAL;
        pa[1] = (*pv);
        memcpy((2+pa), s, len);
        (*pv) = wikrt_tag_addr(WIKRT_O, addr);
        return WIKRT_OK;
    }
}

wikrt_err wikrt_unwrap_seal(wikrt_cx* cx, char* buff)
{
    // I'm assuming (via API docs) that `buff` is WIKRT_TOK_BUFFSZ
    (*buff) = 0;
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    wikrt_val* const v = wikrt_pval(cx, cx->val);
    if(!wikrt_o(*v)) { return WIKRT_TYPE_ERROR; }
    wikrt_val* const pv = wikrt_pval(cx, (*v));

    if(wikrt_otag_seal_sm(*pv)) {
        wikrt_val const otag = *pv;
        buff[0] = ':';
        buff[1] = (char)((otag >> 8 ) & 0xFF);
        buff[2] = (char)((otag >> 16) & 0xFF);
        buff[3] = (char)((otag >> 24) & 0xFF);
        buff[4] = 0;
        (*v) = pv[1];
        return WIKRT_OK;
    } else if(wikrt_otag_seal(*pv)) {
        size_t const len = ((*pv) >> 8);
        memcpy(buff, (2 + pv), len);
        buff[len] = 0;
        (*v) = pv[1];
        return WIKRT_OK; 
    } else { return WIKRT_TYPE_ERROR; }
}

wikrt_err wikrt_wrap_sum(wikrt_cx* cx, bool inR) 
{
    if(!wikrt_mem_reserve(cx, WIKRT_WRAP_SUM_RESERVE)) { return WIKRT_CXFULL; }
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    wikrt_wrap_sum_rv(cx, inR, wikrt_pval(cx, cx->val));
    return WIKRT_OK;
}

static inline bool wikrt_deepsum_with_free_space(wikrt_cx* cx, wikrt_val v) 
{
    if(!wikrt_o(v)) { return false; }
    wikrt_val const otag = *(wikrt_pval(cx, v));
    // need two bits free space to squeeze in another sum step.
    return wikrt_otag_deepsum(otag) && (otag < (WIKRT_VAL_MAX >> 2));
}

void wikrt_wrap_sum_rv(wikrt_cx* cx, bool inRight, wikrt_val* v)
{
    if(WIKRT_P == wikrt_vtag(*v)) {
        // non-allocating wrap sum for unit or product. 
        // Useful for compact representation of lists.
        wikrt_tag const tag = inRight ? WIKRT_PR : WIKRT_PL;
        (*v) = wikrt_tag_addr(tag, wikrt_vaddr(*v));
    } else if(wikrt_deepsum_with_free_space(cx, (*v))) {
        // non-allocating deep sum extension.
        wikrt_val* const pv = wikrt_pval(cx, (*v));
        wikrt_val const s0 = (*pv) >> 8;
        wikrt_val const sf = (s0 << 2) | (inRight ? WIKRT_DEEPSUMR : WIKRT_DEEPSUML); 
        wikrt_val const otag = (sf << 8) | WIKRT_OTAG_DEEPSUM;
        (*pv) = otag;
    } else { 
        // allocation of deepsum wrapper
        // Note: I'm thinking about using a dedicated free-list for deepsums.
        // The motivation would be to eliminate average-case allocation for
        // data plumbing with sum types, reusing some temporary locations. 
        wikrt_val const sf = inRight ? WIKRT_DEEPSUMR : WIKRT_DEEPSUML;
        wikrt_val const otag = (sf << 8) | WIKRT_OTAG_DEEPSUM;
        wikrt_alloc_cellval_r(cx, v, WIKRT_O, otag, (*v));
    }
}

wikrt_err wikrt_unwrap_sum(wikrt_cx* cx, bool* inRight) 
{
    // reserve space, move value into cx->a to simplify unwrap
    if(!wikrt_mem_reserve(cx, WIKRT_UNWRAP_SUM_RESERVE)) { return WIKRT_CXFULL; }
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_unwrap_sum_rv(cx, inRight, wikrt_pval(cx, cx->val));
}

wikrt_err wikrt_unwrap_sum_rv(wikrt_cx* cx, bool* inRight, wikrt_val* v)
{ retry: {
    wikrt_val const tag = wikrt_vtag(*v);
    wikrt_val const addr = wikrt_vaddr(*v);
    if(WIKRT_PL == tag) {
        (*inRight) = false;
        (*v) = wikrt_tag_addr(WIKRT_P, addr);
        return WIKRT_OK;
    } else if(WIKRT_PR == tag) {
        (*inRight) = true;
        (*v) = wikrt_tag_addr(WIKRT_P, addr);
        return WIKRT_OK;
    } else if((WIKRT_O == tag) && (0 != addr)) {
        wikrt_val* const pv = wikrt_paddr(cx, addr);
        if(wikrt_otag_deepsum(*pv)) {
            wikrt_val const s0 = (*pv) >> 8;
            (*inRight) = (3 == (3 & s0));
            wikrt_val const sf = s0 >> 2;
            if(0 == sf) { 
                (*v) = pv[1]; // drop deepsum wrapper
                // add deepsum wrapper to dedicated free list?
            } else { 
                (*pv) = sf << 8 | WIKRT_OTAG_DEEPSUM;
            }
        } else {
            // not a deepsum, but might still be array or stream or similar
            wikrt_err const st = wikrt_expand_sum_rv(cx, v);
            if(WIKRT_OK != st) { return st; }
            goto retry;
        }
    } else { return WIKRT_TYPE_ERROR; }
}}

/* expand a sum-type value. */
wikrt_err wikrt_expand_sum_ra(wikrt_cx* cx) 
{
    if(!wikrt_o(cx->a)) { return WIKRT_TYPE_ERROR; }
    fprintf(stderr, "%s: TODO: expand sum object (%x)\n", __FUNCTION__, (int)(wikrt_pval(cx,cx->a)[0]));
    return WIKRT_IMPL;
}

wikrt_err wikrt_sum_wswap(wikrt_cx* cx)
{
    // For now, just brute-force ops like this.
    if(!wikrt_mem_reserve(cx, WIKRT_SUMOP_RESERVE)) { return WIKRT_CXFULL; }
    else if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    else { return wikrt_sum_wswap_rv(cx, wikrt_pval(cx, cx->val)); }
}

wikrt_err wikrt_sum_zswap(wikrt_cx* cx)
{
    if(!wikrt_mem_reserve(cx, WIKRT_SUMOP_RESERVE)) { return WIKRT_CXFULL; }
    else if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    else { return wikrt_sum_zswap_rv(cx, wikrt_pval(cx, cx->val)); }
}

wikrt_err wikrt_sum_assocl(wikrt_cx* cx) 
{
    if(!wikrt_mem_reserve(cx, WIKRT_SUMOP_RESERVE)) { return WIKRT_CXFULL; }
    else if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    else { return wikrt_sum_assocl_rv(cx, wikrt_pval(cx, cx->val)); }
}

wikrt_err wikrt_sum_assocr(wikrt_cx* cx)
{
    if(!wikrt_mem_reserve(cx, WIKRT_SUMOP_RESERVE)) { return WIKRT_CXFULL; }
    else if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    else { return wikrt_sum_assocr_rv(cx, wikrt_pval(cx, cx->val)); }
}

wikrt_err wikrt_sum_swap(wikrt_cx* cx) 
{
    if(!wikrt_mem_reserve(cx, WIKRT_SUMOP_RESERVE)) { return WIKRT_CXFULL; }
    else if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    else { return wikrt_sum_swap_rv(cx, wikrt_pval(cx, cx->val)); }
}

wikrt_err wikrt_sum_wswap_rv(wikrt_cx* cx, wikrt_val* v) 
{
    bool inBC;
    wikrt_err st = wikrt_unwrap_sum_rv(cx, &inBC, v);
    if(WIKRT_OK != st) { return st; }
    else if(!inBC) { // a → (_ + (a + _))
        wikrt_wrap_sum_rv(cx, false, v); 
        wikrt_wrap_sum_rv(cx, true, v);
        return WIKRT_OK;
    } else {
        bool inC;
        st = wikrt_unwrap_sum_rv(cx, &inC, v);
        if(WIKRT_OK != st) { // 
            wikrt_wrap_sum_rv(cx, true, v);
            return st;
        } else if(!inC) { // 'b' → (b + _)
            wikrt_wrap_sum_rv(cx, false, v);
            return WIKRT_OK;
        } else { // we have 'c'.
            wikrt_wrap_sum_rv(cx, true, v);
            wikrt_wrap_sum_rv(cx, true, v);
            return WIKRT_OK;
        }
    }
}

wikrt_err wikrt_sum_zswap_rv(wikrt_cx* cx, wikrt_val* v) 
{
    bool inBCD;
    wikrt_err st = wikrt_unwrap_sum_rv(cx, &inBCD, v);
    if(WIKRT_OK != st) { return st; }
    if(inBCD) { st = wikrt_sum_wswap_rv(cx, v); }
    wikrt_wrap_sum_rv(cx, inBCD, v);
    return st;
}

// (a+(b+c))→((a+b)+c).
wikrt_err wikrt_sum_assocl_rv(wikrt_cx* cx, wikrt_val* v) 
{
    bool inBC;
    wikrt_err st = wikrt_unwrap_sum_rv(cx, &inBC, v);
    if(WIKRT_OK != st) { return st; }
    else if(!inBC) { // a → ((a + _) + _)
        wikrt_wrap_sum_rv(cx, false, v);
        wikrt_wrap_sum_rv(cx, false, v);
        return WIKRT_OK;
    } else {
        bool inC;
        st = wikrt_unwrap_sum_rv(cx, &inC, v);
        wikrt_wrap_sum_rv(cx, true, v);
        if((WIKRT_OK == st) && !inC) { wikrt_wrap_sum_rv(cx, false, v); }
        return st;
    }
}


// ((a+b)+c)→(a+(b+c)).  
wikrt_err wikrt_sum_assocr_rv(wikrt_cx* cx, wikrt_val* v)
{
    bool inC;
    wikrt_err st = wikrt_unwrap_sum_rv(cx, &inC, v);
    if(WIKRT_OK != st) { return st; }
    else if(inC) { // 'c' → (_ + (_ + c))
        wikrt_wrap_sum_rv(cx, true, v);
        wikrt_wrap_sum_rv(cx, true, v);
        return WIKRT_OK;
    } else { // in (a+b) in left → 'a' in left or 'b' in left of right.
        bool inB;
        st = wikrt_unwrap_sum_rv(cx, &inB, v);
        wikrt_wrap_sum_rv(cx, false, v); // (a+b) on failure, or a or b, in left of something.
        if((WIKRT_OK == st) && inB) { wikrt_wrap_sum_rv(cx, true, v); }
        return st;
    }
}

wikrt_err wikrt_sum_swap_rv(wikrt_cx* cx, wikrt_val* v)
{
    bool inR;
    wikrt_err const st = wikrt_unwrap_sum_rv(cx, &inR, v);
    if(WIKRT_OK != st) { return st; }
    wikrt_wrap_sum_rv(cx, !inR, v);
    return WIKRT_OK;
}


/** (a*((b+c)*e))→(((a*b)+(a*c))*e). ABC op `D`. */
wikrt_err wikrt_sum_distrib(wikrt_cx* cx)
{
    // potential allocation only at unwrap of (b+c)
    if(!wikrt_mem_reserve(cx, WIKRT_UNWRAP_SUM_RESERVE)) { return WIKRT_CXFULL; }

    wikrt_val const ase = cx->val;
    if(!wikrt_p(ase)) { return WIKRT_TYPE_ERROR; }
    wikrt_val* const pase = wikrt_pval(cx, ase);

    wikrt_val const se = pase[1];
    if(!wikrt_p(se)) { return WIKRT_TYPE_ERROR; }
    wikrt_val* const pse = wikrt_pval(cx, se);

    bool inR;
    wikrt_err const st = wikrt_unwrap_sum_rv(cx, &inR, pse);
    if(WIKRT_OK != st) { return st; }

    // perform distribution. shifts outer pair without allocation.
    pase[1] = pse[0]; // (a*b) or (a*c)
    pse[0] = wikrt_tag_addr((inR ? WIKRT_PR : WIKRT_PL), wikrt_vaddr(ase));
    cx->val = se;
    return WIKRT_OK;
}


/** (((a*b)+(c*d))*e)→((a+c)*((b+d)*e)). ABC op `F`. */
wikrt_err wikrt_sum_factor(wikrt_cx* cx)
{
    wikrt_size const worst_case_alloc = WIKRT_UNWRAP_SUM_RESERVE 
                                      + (2 * WIKRT_WRAP_SUM_RESERVE);
    if(!wikrt_mem_reserve(cx, worst_case_alloc)) { return WIKRT_CXFULL; }

    wikrt_val const se = cx->val;
    if(!wikrt_p(se)) { return WIKRT_TYPE_ERROR; }
    wikrt_val* const pse = wikrt_pval(cx, se);

    bool inR;
    wikrt_err st = wikrt_unwrap_sum_rv(cx, &inR, pse);
    if(WIKRT_OK != st) { return st; }

    wikrt_val const ab = *pse;
    if(!wikrt_p(ab)) {
        wikrt_wrap_sum_rv(cx, inR, pse); // recover original.
        return WIKRT_TYPE_ERROR; 
    }

    wikrt_val* const pab = wikrt_pval(cx, ab);
    wikrt_wrap_sum_rv(cx, inR, pab); 
    wikrt_wrap_sum_rv(cx, inR, (1 + pab));
    pse[0] = pab[1];
    pab[1] = se;
    cx->val = ab;
    return WIKRT_OK;
}

wikrt_err wikrt_peek_i32(wikrt_cx* cx, int32_t* i32)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_peek_i32_v(cx, wikrt_pval(cx, cx->val)[0], i32);
}

wikrt_err wikrt_peek_i64(wikrt_cx* cx, int64_t* i64)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_peek_i64_v(cx, wikrt_pval(cx, cx->val)[0], i64);
}

wikrt_err wikrt_peek_istr(wikrt_cx* cx, char* buff, size_t* len)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_peek_istr_v(cx, wikrt_pval(cx, cx->val)[0], buff, len);
}


wikrt_err wikrt_intro_binary(wikrt_cx* cx, uint8_t const* data, size_t len)
{
    wikrt_err const st = wikrt_alloc_binary_reg(cx, &(cx->a), data, len);
    if(WIKRT_OK != st) { return st; }
    return wikrt_intro_reg(cx, &(cx->a));
}

wikrt_err wikrt_intro_text(wikrt_cx* cx, char const* s, size_t len)
{
    wikrt_val v = WIKRT_VOID;
    wikrt_err const st = wikrt_alloc_text_reg(cx, &(cx->a), s, len);
    if(WIKRT_OK != st) { return st; }
    return wikrt_intro_reg(cx, &(cx->a));
}

wikrt_err wikrt_read_binary(wikrt_cx* cx, uint8_t* buff, size_t* bytes) 
{
    if(!wikrt_p(cx->val)) { (*bytes) = 0; return WIKRT_TYPE_ERROR; }
    return wikrt_read_binary_v(cx, wikrt_pval(cx, cx->val), buff, bytes);
}

wikrt_err wikrt_read_text(wikrt_cx* cx, char* buff, size_t* bytes, size_t* chars)
{
    size_t chars_alt = SIZE_MAX;
    if(NULL == chars) { chars = &chars_alt; }
    if(!wikrt_p(cx->val)) { 
        (*bytes) = 0;
        (*chars) = 0;
        return WIKRT_TYPE_ERROR;
    }
    return wikrt_read_text_v(cx, wikrt_pval(cx, cx->val), buff, bytes, chars);
}


bool wikrt_valid_text_len(char const* s, size_t* bytes, size_t* chars)
{
    _Static_assert((sizeof(char) == sizeof(uint8_t)), "invalid cast from char* to uint8_t*");
    uint8_t const* const utf8 = (uint8_t const*)s;
    size_t const maxlen = (NULL != bytes) ? (*bytes) : SIZE_MAX;
    size_t len = 0;
    size_t cct = 0;
    do {
        uint32_t cp;
        size_t const k = utf8_readcp(utf8 + len, maxlen - len, &cp);
        if((0 == k) || !wikrt_text_char(cp)) { break; }
        len += k;
        cct += 1;
    } while(true);
    if(NULL != bytes) { (*bytes) = len; }
    if(NULL != chars) { (*chars) = cct; }
    return ((maxlen == len) || (0 == utf8[len]));
}

// Long term, I'll want to use the WIKRT_OTAG_TEXT representation.
// For now, however, I'll allocate a conventional list structure.
// (But I'll allocate it as one larger block for now.)
wikrt_err wikrt_alloc_text_v(wikrt_cx* cx, wikrt_val* v, char const* const s, size_t nBytes) 
{
    size_t nChars;
    if(!wikrt_valid_text_len(s, &nBytes, &nChars)) {
        return WIKRT_INVAL; // invalid text
    }

    if(0 == nChars) { // empty text
        (*v) = WIKRT_UNIT_INR;
        return WIKRT_OK;
    }

    // We need one cell per codepoint.
    if(nChars >= (WIKRT_SIZE_MAX / WIKRT_CELLSIZE)) { return WIKRT_CXFULL; }
    wikrt_size const szAlloc = ((wikrt_size)nChars * WIKRT_CELLSIZE);
    wikrt_addr spine;
    if(!wikrt_alloc(cx, szAlloc, &spine)) { return WIKRT_CXFULL; }
    (*v) = wikrt_tag_addr(WIKRT_PL, spine);

    _Static_assert((WIKRT_SMALLINT_MAX >= 0x10FFFF), "small integers insufficient for unicode codepoints");

    uint8_t const* u = (uint8_t const*) s;
    #define READ_NEXT_CODEPOINT() wikrt_i2v((wikrt_int) utf8_step_unsafe(&u))
    for(size_t ii = nChars; ii > 1; --ii) 
    {
        wikrt_val* const pspine = wikrt_paddr(cx, spine);
        spine += WIKRT_CELLSIZE;
        pspine[0] = READ_NEXT_CODEPOINT();
        pspine[1] = wikrt_tag_addr(WIKRT_PL, spine);
    }
    wikrt_val* const pspine_last = wikrt_paddr(cx, spine);
    pspine_last[0] = READ_NEXT_CODEPOINT();
    pspine_last[1] = WIKRT_UNIT_INR;
    #undef READ_NEXT_CODEPOINT
    assert((s + nBytes) == (char const*)u);
    return WIKRT_OK;
}

/* For the moment, we'll allocate a binary as a plain old list.
 * This results in an WIKRT_CELLSIZE (8x) expansion at this time. I
 * I will support a compact WIKRT_OTAG_BINARY representation later.
 */
wikrt_err wikrt_alloc_binary_v(wikrt_cx* cx, wikrt_val* v, uint8_t const* buff, size_t nBytes) 
{
    if(0 == nBytes) { // empty binary
        (*v) = WIKRT_UNIT_INR;
        return WIKRT_OK;
    }

    // prevent size overflow allocations
    if(nBytes >= (WIKRT_SIZE_MAX / WIKRT_CELLSIZE)) { return WIKRT_CXFULL; }
    wikrt_size const szAlloc = ((wikrt_size)nBytes * WIKRT_CELLSIZE);
    wikrt_addr spine;
    if(!wikrt_alloc(cx, szAlloc, &spine)) { return WIKRT_CXFULL; }
    (*v) = wikrt_tag_addr(WIKRT_PL, spine);

    size_t const intraSpineBytes = nBytes - 1;
    for(size_t ii = 0; ii < intraSpineBytes; ++ii) {
        wikrt_val* const pspine = wikrt_paddr(cx, spine);
        spine += WIKRT_CELLSIZE;
        pspine[0] = wikrt_i2v(buff[ii]);
        pspine[1] = wikrt_tag_addr(WIKRT_PL, spine);
    }
    wikrt_val* const pspine_last = wikrt_paddr(cx, spine);
    pspine_last[0] = wikrt_i2v(buff[intraSpineBytes]);
    pspine_last[1] = WIKRT_UNIT_INR;
    return WIKRT_OK;
} 

static bool wikrt_read_list_byte(wikrt_cx* cx, wikrt_val* v, uint8_t* byte, wikrt_err* st) 
{
    bool inR;
    wikrt_err const stSum = wikrt_unwrap_sum_v(cx, &inR, v);

    if(WIKRT_OK != stSum) { 
        (*st) = WIKRT_TYPE_ERROR; 
        return false; 
    } 

    if(inR) {
        (*st) = WIKRT_OK; // end of input
        wikrt_wrap_sum_v(cx, true, v);
        return false;
    }
 
    if(wikrt_p(*v)) {
        wikrt_addr const addr = wikrt_vaddr(*v);
        wikrt_val* const pv = wikrt_paddr(cx, addr);
        if(wikrt_i(*pv)) {
            wikrt_int const i = wikrt_v2i(*pv);
            if((0 <= i) && (i <= 0xFF)) {
                (*byte) = i;
                (*st) = WIKRT_OK;
                (*v) = pv[1];
                wikrt_free(cx, WIKRT_CELLSIZE, addr);
                return true;
            } // else not a byte
        } // else not a small integer
    } // else not a valid list node

    // our value wasn't of the expected shape (byte * list).
    (*st) = WIKRT_TYPE_ERROR;
    wikrt_wrap_sum_v(cx, false, v);
    return false;
}


wikrt_err wikrt_read_binary_v(wikrt_cx* cx, wikrt_val* v, uint8_t* buff, size_t* size)
{
    // for now, naively read byte-at-a-time. When I have support for
    // WIKRT_OTAG_BINARY, I should try for buffer-copying reads.
    size_t const max_size = (*size);
    (*size) = 0;
    do {
        if(max_size == (*size)) { return WIKRT_BUFFSZ; }
        wikrt_err st;
        if(!wikrt_read_list_byte(cx, v, buff + (*size), &st)) { return st; }
        (*size) += 1;
    } while(true);
}

static bool wikrt_read_list_cp(wikrt_cx* cx, wikrt_val* v, uint32_t* cp, size_t max_utf8, wikrt_err* st) 
{
    bool inR;
    wikrt_err const stSum = wikrt_unwrap_sum_v(cx, &inR, v);

    if(WIKRT_OK != stSum) { 
        (*st) = WIKRT_TYPE_ERROR; 
        return false; 
    } 

    if(inR) {
        (*st) = WIKRT_OK; // end of input
        wikrt_wrap_sum_v(cx, true, v);
        return false;
    }
 
    if(wikrt_p(*v)) {
        wikrt_addr const addr = wikrt_vaddr(*v);
        wikrt_val* const pv = wikrt_paddr(cx, addr);
        _Static_assert((WIKRT_SMALLINT_MAX >= 0x10FFFF), 
            "small integers insufficient for unicode codepoints");
        if(wikrt_i(*pv)) {
            wikrt_int const i = wikrt_v2i(*pv);
            if((0 <= i) && (i <= 0x10FFFF) && wikrt_text_char(i)) {
                (*cp) = i;
                if(utf8_writecp_size(i) > max_utf8) {
                    // character is too large for buffer
                    (*st) = WIKRT_BUFFSZ;
                    wikrt_wrap_sum_v(cx, false, v);
                    return false;
                } else {
                    (*st) = WIKRT_OK;
                    (*v) = pv[1];
                    wikrt_free(cx, WIKRT_CELLSIZE, addr);
                    return true;
                }
            } // else not a valid text codepoint
        } // else not a small integer
    } // else not a valid list node

    // our value wasn't a (codepoint * list) pair.
    (*st) = WIKRT_TYPE_ERROR;
    wikrt_wrap_sum_v(cx, false, v);
    return false;
}

wikrt_err wikrt_read_text_v(wikrt_cx* cx, wikrt_val* v, char* s, size_t* bytes, size_t* chars)
{
    // for now, naively read one character at a time. When I have support for
    // WIKRT_OTAG_TEXT, I should consider buffer-copying reads.
    _Static_assert((sizeof(char) == sizeof(uint8_t)), "dubious cast between char* and uint8_t*");
    uint8_t* buff = (uint8_t*) s;

    size_t const max_chars = (*chars);
    size_t const max_bytes = (*bytes);
    (*chars) = 0;
    (*bytes) = 0;

    do {
        if(max_chars == (*chars)) { return WIKRT_BUFFSZ; }
        uint32_t cp; wikrt_err st;
        if(!wikrt_read_list_cp(cx, v, &cp, (max_bytes - (*bytes)), &st)) { return st; }
        (*bytes) += utf8_writecp_unsafe(buff + (*bytes), cp);
        (*chars) += 1;
    } while(true);
}

// introduce a medium-to-large big-integer by copying.
static wikrt_err wikrt_intro_bigint(wikrt_cx* const cx, bool const positive
    , uint32_t const* const dsrc, wikrt_size nDigits)
{
    assert(nDigits >= 2);
    if(nDigits > WIKRT_BIGINT_MAX_DIGITS) { return WIKRT_IMPL; }
    wikrt_size const isize = sizeof(wikrt_val) + (nDigits * sizeof(uint32_t));
    wikrt_sizeb const iszb = WIKRT_CELLBUFF(isize);

    // I need space for the integer plus one cell for the cx->val stack    
    if(!wikrt_mem_reserve(cx, WIKRT_CELLSIZE + iszb)) { return WIKRT_CXFULL; }

    // copy our integer.
    wikrt_addr const addr = wikrt_alloc_r(cx, iszb);
    wikrt_intro_r(cx, wikrt_tag_addr(WIKRT_O, addr));

    wikrt_val* const p = wikrt_paddr(cx, addr);
    p[0] = wikrt_mkotag_bigint(positive, nDigits);
    uint32_t* const dcpy = (uint32_t*)(1 + p);
    dcpy[0] = dsrc[0];
    dcpy[1] = dsrc[1]; // at least two digits per bigint
    for(wikrt_size ii = 2; ii < nDigits; ++ii) {
        dcpy[ii] = dsrc[ii];
    }
    
    // push it onto our stack, then return
    return WIKRT_OK;
}
    
wikrt_err wikrt_intro_i32(wikrt_cx* cx, int32_t n) 
{
    if((WIKRT_SMALLINT_MIN <= n) && (n <= WIKRT_SMALLINT_MAX)) {
        return wikrt_intro_smallval(cx, wikrt_i2v((wikrt_int)n);
    }

    bool positive;
    uint32_t d[2];
    if(n != INT32_MIN) {
        _Static_assert(((INT32_MAX + INT32_MIN) == (-1)), "bad assumption (int32_t)");
        positive = (n >= 0);
        n = positive ? n : -n;
        d[0] = n % WIKRT_BIGINT_DIGIT;
        d[1] = n / WIKRT_BIGINT_DIGIT;
    } else {
        _Static_assert((-2147483648 == INT32_MIN), "bad INT32_MIN");
        positive = false;
        d[1] = 2;
        d[0] = 147483648;
    }
    return wikrt_intro_bigint(cx, positive, d, 2);
}

wikrt_err wikrt_intro_i64(wikrt_cx* cx, int64_t n) 
{
    if((WIKRT_SMALLINT_MIN <= n) && (n <= WIKRT_SMALLINT_MAX)) {
        return wikrt_intro_smallval(cx, wikrt_i2v((wikrt_int)n);
    }

    bool positive;
    uint32_t d[3];
    if(n != INT64_MIN) {
        _Static_assert(((INT64_MAX + INT64_MIN) == (-1)), "bad assumption (int64_t)");
        positive = (n >= 0);
        n = positive ? n : -n;
        d[0] = n % WIKRT_BIGINT_DIGIT;
        n /= WIKRT_BIGINT_DIGIT;
        d[1] = n % WIKRT_BIGINT_DIGIT;
        d[2] = n / WIKRT_BIGINT_DIGIT;
    } else {
        // GCC complains with the INT64_MIN constant given directly.
        _Static_assert(((-9223372036854775807 - 1) == INT64_MIN), "bad INT64_MIN");
        positive = false;
        d[2] = 9;
        d[1] = 223372036;
        d[0] = 854775808;
    }
    wikrt_size const nDigits = (0 == d[2]) ? 2 : 3;
    return wikrt_intro_bigint(cx, positive, d, nDigits);
}

static inline bool wikrt_bigint(wikrt_cx* cx, wikrt_val v) {
    return wikrt_o(v) && wikrt_otag_bigint(*wikrt_pval(cx, v));
}

wikrt_err wikrt_peek_i32_v(wikrt_cx* cx, wikrt_val const v, int32_t* i32) 
{
    // small integers (normal case)
    if(wikrt_i(v)) {
        (*i32) = wikrt_v2i(v);
        return WIKRT_OK;
    } 

    if(!wikrt_bigint(cx,v)) { return WIKRT_TYPE_ERROR; }
    wikrt_val const* const pv = wikrt_pval(cx, v);
    bool const positive = (0 == ((1<<8) & *pv));
    wikrt_size const nDigits = (*pv) >> 9;
    uint32_t const* const d = (uint32_t const*)(1 + pv);
    int32_t const digit = WIKRT_BIGINT_DIGIT;

    if(positive) {
        _Static_assert((2147483647 == INT32_MAX), "bad INT32_MAX");
        uint32_t const d1m = 2;
        uint32_t const d0m = 147483647;
        bool const overflow = (nDigits > 2) || (d[1] > d1m) || ((d[1] == d1m) && (d[0] > d0m));
        if(overflow) { (*i32) = INT32_MAX; return WIKRT_BUFFSZ; }
        (*i32) = (d[1] * digit) + d[0];
        return WIKRT_OK;
    } else {
        _Static_assert((-2147483648 == INT32_MIN), "bad INT32_MIN");
        uint32_t const d1m = 2;
        uint32_t const d0m = 147483648;
        bool const underflow = (nDigits > 2) || (d[1] > d1m) || ((d[1] == d1m) && (d[0] > d0m));
        if(underflow) { (*i32) = INT32_MIN; return WIKRT_BUFFSZ; }
        (*i32) = 0 - ((int32_t)d[1] * digit) - (int32_t)d[0];
        return WIKRT_OK;
    }
}

wikrt_err wikrt_peek_i64_v(wikrt_cx* cx, wikrt_val const v, int64_t* i64) 
{
    if(wikrt_i(v)) {
        (*i64) = (int64_t) wikrt_v2i(v);
        return WIKRT_OK;
    }

    if(!wikrt_bigint(cx,v)) { return WIKRT_TYPE_ERROR; }
    wikrt_val const* const pv = wikrt_pval(cx, v);
    bool const positive = (0 == ((1<<8) & *pv));
    wikrt_size const nDigits = (*pv) >> 9;
    uint32_t const* const d = (uint32_t const*)(1 + pv);
    int64_t const digit = WIKRT_BIGINT_DIGIT;

    if(2 == nDigits) {
        // no risk of over or underflow
        int64_t const iAbs = ((int64_t)d[1] * digit) + d[0];
        (*i64) = positive ? iAbs : -iAbs;
        return WIKRT_OK;
    } else if(positive) {
        _Static_assert((9223372036854775807 == INT64_MAX), "bad INT64_MAX");
        uint32_t const d2m = 9;
        uint32_t const d1m = 223372036;
        uint32_t const d0m = 854775807;
        bool const overflow = (nDigits > 3) || (d[2] > d2m) ||
            ((d[2] == d2m) && ((d[1] > d1m) || ((d[1] == d1m) && (d[0] > d0m))));
        if(overflow) { (*i64) = INT64_MAX; return WIKRT_BUFFSZ; }
        (*i64) = ((int64_t)d[2] * (digit * digit)) 
               + ((int64_t)d[1] * (digit))
               + ((int64_t)d[0]);
        return WIKRT_OK;
    } else {
        // GCC complains with the INT64_MIN constant given directly.
        _Static_assert(((-9223372036854775807) == (1 + INT64_MIN)), "bad INT64_MIN");
        uint32_t const d2m = 9;
        uint32_t const d1m = 223372036;
        uint32_t const d0m = 854775808;
        bool const underflow = (nDigits > 3) || (d[2] > d2m) ||
            ((d[2] == d2m) && ((d[1] > d1m) || ((d[1] == d1m) && (d[0] > d0m))));
        if(underflow) { (*i64) = INT64_MIN; return WIKRT_BUFFSZ; }
        (*i64) = 0 - ((int64_t)d[2] * (digit * digit))
                   - ((int64_t)d[1] * (digit))
                   - ((int64_t)d[0]);
        return WIKRT_OK;
    }
}

static inline size_t wikrt_decimal_size(uint32_t n) {
    size_t ct = 0;
    do { ++ct; n /= 10; } while(n > 0);
    return ct;
}


wikrt_err wikrt_peek_istr_v(wikrt_cx* cx, wikrt_val const v, char* const buff, size_t* const buffsz)
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
        if(!wikrt_bigint(cx, v)) { return WIKRT_TYPE_ERROR; }
        wikrt_val const* const pv = wikrt_pval(cx, v);
        d = (uint32_t const*)(1 + pv);
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
    uint32_t d = 0;
    #define RD d = (10 * d) + *(s++) - '0'
    RD; RD; RD;
    RD; RD; RD;
    RD; RD; RD;
    #undef RD
    return d;
}

wikrt_err wikrt_intro_istr(wikrt_cx* cx, char const* const istr, size_t len)
{
    if(!wikrt_valid_istr(istr, &len)) { return WIKRT_INVAL; }

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
    if(len <= 18) { // int64 can robustly handle 18 digits
        int64_t iAbs = 0;
        do { iAbs = (10 * iAbs) + ((*s) - '0'); } while(++s != eos);
        return wikrt_alloc_i64_v(cx, v, (positive ? iAbs : -iAbs));
    }

    // otherwise, perform an allocation.
    // divide into an 'upper digit' plus a set of 'inner digits'.
    size_t const innerDigitCt  = ((len - 1) / 9);
    size_t const upperDigitLen = (len - (9 * innerDigitCt));
    assert((innerDigitCt > 0) && (1 <= upperDigitLen) && (upperDigitLen <= 9));

    size_t const nDigits = 1 + innerDigitCt;
    if(nDigits > WIKRT_BIGINT_MAX_DIGITS) { return WIKRT_IMPL; }
    wikrt_size const isize = sizeof(wikrt_val) + (nDigits * sizeof(uint32_t));
    wikrt_sizeb const iszb = WIKRT_CELLBUFF(isize);

    // allocate everything we need
    if(!wikrt_mem_reserve(cx, WIKRT_CELLSIZE + iszb)) { return WIKRT_CXFULL; }
    wikrt_addr const addr = wikrt_alloc_r(cx, iszb);
    wikrt_intro_r(cx, wikrt_tag_addr(WIKRT_O, addr));

    // fill the integer representation.
    wikrt_val* const p = wikrt_paddr(cx, addr);
    p[0] = wikrt_mkotag_bigint(positive, nDigits);
    uint32_t* const d = (uint32_t*)(1 + p);

    for(size_t ii = 0; ii < innerDigitCt; ++ii) {
        eos -= 9; 
        d[ii] = wikrt_read_inner_digit(eos);
    }

    uint32_t ud = 0;
    for(size_t ii = 0; ii < upperDigitLen; ++ii) {
        ud = (10 * ud) + *(s++) - '0';
    }
    d[innerDigitCt] = upperDigit;

    assert(eos == s);
    return WIKRT_OK;
}
    
/** @brief Add two integers. (I(a)*(I(b)*e))→(I(a+b)*e). */
wikrt_err wikrt_int_add(wikrt_cx* cx)
{
    wikrt_val const a = cx->val;
    if(wikrt_p(a)) {
        wikrt_val* const pa = wikrt_pval(cx, a);
        wikrt_val const b = pa[1];
        if(wikrt_p(b)) {
            wikrt_val* const pb = wikrt_pval(cx, b);
            wikrt_val r = WIKRT_VOID;

            // TODO: fix allocation/compaction safety for this.
            //  at the moment, our values can easily move.
            //  would a 'tmp' value register help? or maybe a second stack?

            wikrt_err const st = wikrt_int_add_v(cx, *pb, *pa, &r);
            if(WIKRT_OK != st) { return st; }
            *pb = r;
            cx->val = b;
            return WIKRT_OK;
        }
    }
    return WIKRT_TYPE_ERROR; // need (a*(b*e)) structure.
}

static inline bool wikrt_integer(wikrt_cx* cx, wikrt_val n) {
    return wikrt_i(n) || wikrt_bigint(cx,n); }

wikrt_err wikrt_int_add_v(wikrt_cx* cx, wikrt_val a, wikrt_val b, wikrt_val* r)
{
    // optimally handle the small-integer cases.
    if(wikrt_i(a) && wikrt_i(b)) {
        // two 31-bit ints add to one 32-bit int.
        _Static_assert((INT32_MAX > (WIKRT_SMALLINT_MAX + WIKRT_SMALLINT_MAX)), "smallint add will overflow");
        int32_t const sum = wikrt_v2i(a) + wikrt_v2i(b);
        return wikrt_alloc_i32_v(cx, r, sum);
    }
    bool const okType = wikrt_integer(cx,a) && wikrt_integer(cx,b);
    if(!okType) { return WIKRT_TYPE_ERROR; }

    // Handling big integers is important, but for a spike solution the 
    // small integers are probably sufficient.
    
    return WIKRT_IMPL;
}

/** @brief Multiply two integers. (I(a)*(I(b)*e))→(I(a*b)*e). */
wikrt_err wikrt_int_mul(wikrt_cx* cx)
{
    wikrt_val const a = cx->val;
    if(wikrt_p(a)) {
        wikrt_val* const pa = wikrt_pval(cx, a);
        wikrt_val const b = pa[1];
        if(wikrt_p(b)) {
            wikrt_val* const pb = wikrt_pval(cx, b);
            wikrt_val r = WIKRT_VOID;
            wikrt_err const st = wikrt_int_mul_v(cx, *pb, *pa, &r);
            if(WIKRT_OK != st) { return st; }
            *pb = r;
            cx->val = b;
            wikrt_free(cx, WIKRT_CELLSIZE, wikrt_vaddr(a));
            return WIKRT_OK;
        }
    }
    return WIKRT_TYPE_ERROR; // need (a*(b*e)) structure.
}

wikrt_err wikrt_int_mul_v(wikrt_cx* cx, wikrt_val a, wikrt_val b, wikrt_val* r)
{
    // optimally handle the small-integer cases.
    if(wikrt_i(a) && wikrt_i(b)) {
        // two 31-bit ints should multiply into one 62-bit int.
        _Static_assert((INT64_MAX / WIKRT_SMALLINT_MAX) > WIKRT_SMALLINT_MAX, "smallint mul will overflow");
        int64_t const prod = (int64_t)wikrt_v2i(a) * (int64_t)wikrt_v2i(b);
        return wikrt_alloc_i64_v(cx, r, prod);
    }
    bool const okType = wikrt_integer(cx,a) && wikrt_integer(cx,b);
    if(!okType) { return WIKRT_TYPE_ERROR; }

    // Handling big integers is important, but for a spike solution the 
    // small integers are probably sufficient.

    return WIKRT_IMPL;
}

/** @brief Negate an integer. (I(a)*e)→(I(-a)*e). */
wikrt_err wikrt_int_neg(wikrt_cx* cx)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_int_neg_v(cx, wikrt_pval(cx, cx->val));
}

/* Negation of an integer is O(1) and non-allocating in Wikilon
 * runtime. We either flip a sign bit, or we negate a small int
 * from a known safe range. 
 *
 * Ensuring non-allocating negation is why SMALLINT_MIN is just the
 * negation of SMALLINT_MAX despite this leaving one element unused
 * in the representation.
 */
wikrt_err wikrt_int_neg_v(wikrt_cx* cx, wikrt_val* v)
{
    if(wikrt_i(*v)) { 
        _Static_assert((WIKRT_SMALLINT_MIN == (- WIKRT_SMALLINT_MAX)), "unsafe negation for small integers");
        (*v) = wikrt_i2v(- wikrt_v2i(*v)); 
        return WIKRT_OK;
    } else if(wikrt_bigint(cx,(*v))) {
        wikrt_val* const pv = wikrt_pval(cx,(*v));
        (*pv) ^= (1 << 8); // flip the sign bit.
        return WIKRT_OK;
    } else { return WIKRT_TYPE_ERROR; }
}

/* (I(divisor) * (I(dividend) * e)) → (I(remainder) * (I(quotient) * e)).
 */
wikrt_err wikrt_int_div(wikrt_cx* cx) 
{
    wikrt_val const outer = cx->val;
    if(wikrt_p(outer)) {
        wikrt_val* const pouter = wikrt_pval(cx, outer);
        wikrt_val const inner = pouter[1];
        if(wikrt_p(inner)) {
            wikrt_val* const pinner = wikrt_pval(cx, inner);
            wikrt_val const divisor = pouter[0];
            wikrt_val const dividend = pinner[0];
            wikrt_val remainder = WIKRT_VOID;
            wikrt_val quotient  = WIKRT_VOID;
            wikrt_err const st = wikrt_int_div_v(cx, dividend, divisor, &quotient, &remainder);
            if(WIKRT_OK != st) { return st; }
            pouter[0] = remainder;
            pinner[0] = quotient;
            return WIKRT_OK;
        }
    }
    return WIKRT_TYPE_ERROR;
}

static inline void wikrt_smallnum_divmod(wikrt_int dividend, wikrt_int divisor, wikrt_int* quot, wikrt_int* rem)
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

/* Requires a non-zero divisor. We guarantee that the remainder is  
 * between zero (inclusive) and the divisor (exclusive). So this is
 * not quite identical to C `/` and `%` ops.
 */ 
wikrt_err wikrt_int_div_v(wikrt_cx* cx, wikrt_val dividend, wikrt_val divisor, 
    wikrt_val* quotient, wikrt_val* remainder)
{
    // forbid div-by-zero (treated as a dependent type failure)
    if(WIKRT_IZERO == divisor) { return WIKRT_TYPE_ERROR; }

    // optimally handle the small integer case
    if(wikrt_i(dividend) && wikrt_i(divisor)) {
        wikrt_int q,r;
        wikrt_smallnum_divmod(wikrt_v2i(dividend), wikrt_v2i(divisor), &q, &r);
        (*quotient) = wikrt_i2v(q);
        (*remainder) = wikrt_i2v(r);
        return WIKRT_OK;
    }
    bool const okType = wikrt_integer(cx,dividend) && wikrt_integer(cx,divisor);
    if(!okType) { return WIKRT_TYPE_ERROR; }

    // TODO: support division with big numbers.
    return WIKRT_IMPL;
}

/** @brief Compare two integers. Non-destructive. (I(a)*(I(b)*e)).
 *
 * This compares `b` to `a`, matching direct allocation order (i.e. if we
 * allocate zero then four, the comparison is `zero is less than four`).
 */
wikrt_err wikrt_int_cmp(wikrt_cx* cx, wikrt_ord* ord)
{
    wikrt_val const a = cx->val;
    if(wikrt_p(a)) {
        wikrt_val const* const pa = wikrt_pval(cx, a);
        wikrt_val const b = pa[1];
        if(wikrt_p(b)) {
            wikrt_val const* const pb = wikrt_pval(cx, b);
            return wikrt_int_cmp_v(cx, *pb, ord, *pa);
        }
    }
    return WIKRT_TYPE_ERROR;
}

//typedef enum wikrt_ord { WIKRT_LT = -1, WIKRT_EQ = 0, WIKRT_GT = 1 } wikrt_ord;
wikrt_err wikrt_int_cmp_v(wikrt_cx* cx, wikrt_val a, wikrt_ord* ord, wikrt_val b)
{
    // fast handling for common smallnum case
    if(wikrt_i(a) && wikrt_i(b)) {
        wikrt_int const ia = wikrt_v2i(a);
        wikrt_int const ib = wikrt_v2i(b);
        (*ord) = (ia > ib) ? WIKRT_GT : (ia < ib) ? WIKRT_LT : WIKRT_EQ;
        return WIKRT_OK; 
    }
    bool const okType = wikrt_integer(cx,a) && wikrt_integer(cx,b);
    if(!okType) { return WIKRT_TYPE_ERROR; }

    // TODO: big number comparisons
    return WIKRT_IMPL;
}


wikrt_err wikrt_quote(wikrt_cx* cx) 
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_quote_v(cx, wikrt_pval(cx, cx->val));
}

wikrt_err wikrt_quote_v(wikrt_cx* cx, wikrt_val* v)
{
    // O(1) quotation via value capture:
    //
    //  (block, list) where:
    //     list  = (opval, end-of-list)
    //     opval = (tag, val)
    //
    // we need three pairs for this. 
    // allocating in one block.
    wikrt_size const szAlloc = 3 * WIKRT_CELLSIZE;
    
    wikrt_addr addr;
    if(!wikrt_alloc(cx, szAlloc, &addr)) { return WIKRT_CXFULL; }

    wikrt_addr const opval = addr;
    wikrt_addr const list  = addr + WIKRT_CELLSIZE;
    wikrt_addr const block = addr + (2 * WIKRT_CELLSIZE);

    wikrt_val* const popval = wikrt_paddr(cx, opval);
    popval[0] = WIKRT_OTAG_OPVAL | WIKRT_OPVAL_LAZYKF;
    popval[1] = (*v);

    wikrt_val* const plist = wikrt_paddr(cx, list);
    plist[0]  = wikrt_tag_addr(WIKRT_O, opval);
    plist[1]  = WIKRT_UNIT_INR;

    wikrt_val* const pblock = wikrt_paddr(cx, block);
    pblock[0] = WIKRT_OTAG_BLOCK;
    pblock[1] = wikrt_tag_addr(WIKRT_PL, list);

    (*v) = wikrt_tag_addr(WIKRT_O, block);
    return WIKRT_OK; 
}

wikrt_err wikrt_block_aff(wikrt_cx* cx)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_block_attrib_v(cx, wikrt_pval(cx, cx->val), WIKRT_BLOCK_AFFINE);
}

wikrt_err wikrt_block_rel(wikrt_cx* cx)
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_block_attrib_v(cx, wikrt_pval(cx, cx->val), WIKRT_BLOCK_RELEVANT);
}

wikrt_err wikrt_block_par(wikrt_cx* cx) 
{
    if(!wikrt_p(cx->val)) { return WIKRT_TYPE_ERROR; }
    return wikrt_block_attrib_v(cx, wikrt_pval(cx, cx->val), WIKRT_BLOCK_PARALLEL);
}

wikrt_err wikrt_block_attrib_v(wikrt_cx* cx, wikrt_val* v, wikrt_val attrib) 
{
    if(!wikrt_o(*v)) { return WIKRT_TYPE_ERROR; }
    wikrt_val* const pv = wikrt_pval(cx, (*v));
    if(wikrt_otag_block(*pv)) { 
        (*pv) |= attrib;
        return WIKRT_OK;
    } else { return WIKRT_TYPE_ERROR; }
}

/** Compose two blocks. ([a→b]*([b→c]*e))→([a→c]*e). */
wikrt_err wikrt_compose(wikrt_cx* cx)
{
    wikrt_val const a = cx->val;
    if(wikrt_p(a)) {
        wikrt_val* const pa = wikrt_pval(cx, a);
        wikrt_val const b = pa[1];
        if(wikrt_p(b)) {
            wikrt_val* const pb = wikrt_pval(cx, b);
            wikrt_val r = WIKRT_VOID;
            wikrt_err const st = wikrt_compose_v(cx, *pa, *pb, &r);
            if(WIKRT_OK != st) { return st; }
            wikrt_free(cx, WIKRT_CELLSIZE, wikrt_vaddr(a));
            pb[0] = r;
            cx->val = b;
            return WIKRT_OK;
        }
    }
    return WIKRT_TYPE_ERROR;
}

static inline bool wikrt_blockval(wikrt_cx* cx, wikrt_val b) {
    return wikrt_o(b) && wikrt_otag_block(*wikrt_pval(cx, b)); }

/* At the moment, I want to ensure O(1) composition of blocks. 
 *
 * An easy technique is essentially to construct `[(a→b)]vr$c(b→c)`. This
 * leverages the WIKRT_OTAG_OPVAL type, and thus does not need to inspect
 * the main structure of either block. 
 *
 * I might be able to optimize a little by inspecting for composition of
 * a simple quotation, or track composition of 'small' blocks by inlining.
 * OTOH, I could also leave that for an annotation-driven simplification 
 * process.
 *
 * I don't want to be clever about this, so for now I'll favor composition
 * that doesn't require inspection.
 */
wikrt_err wikrt_compose_v(wikrt_cx* cx, wikrt_val const ab, wikrt_val const bc, wikrt_val* out)
{
    bool const okType = wikrt_blockval(cx, ab) && wikrt_blockval(cx, bc);
    if(!okType) { return WIKRT_TYPE_ERROR; }

    wikrt_val* const pbc = wikrt_pval(cx, bc);
    
    wikrt_addr addr;
    wikrt_size const szAlloc = 3 * WIKRT_CELLSIZE;
    if(!wikrt_alloc(cx, szAlloc, &addr)) { return WIKRT_CXFULL; }

    wikrt_addr const addr_ap_bc = addr;
    wikrt_addr const addr_ab_val = addr + WIKRT_CELLSIZE;
    wikrt_addr const addr_ab_ap_bc = addr + (2 * WIKRT_CELLSIZE);

    wikrt_val* const pap_bc = wikrt_paddr(cx, addr_ap_bc);
    pap_bc[0] = wikrt_i2v(ABCD_INLINE); // apply a block inline
    pap_bc[1] = pbc[1];                 // followd by bc ops (list)

    wikrt_val* const pab_val = wikrt_paddr(cx, addr_ab_val);
    pab_val[0] = WIKRT_OTAG_OPVAL | WIKRT_OPVAL_LAZYKF; // quoting
    pab_val[1] = ab; // the ab block

    wikrt_val* const pab_ap_bc = wikrt_paddr(cx, addr_ab_ap_bc);
    pab_ap_bc[0] = wikrt_tag_addr(WIKRT_O, addr_ab_val);
    pab_ap_bc[1] = wikrt_tag_addr(WIKRT_PL, addr_ap_bc);
    
    pbc[1] = wikrt_tag_addr(WIKRT_PL, addr_ab_ap_bc);

    (*out) = bc;
    return WIKRT_OK;     
}
































  ///////////////////////////
 // TRANSACTION SUBSYSTEM //
///////////////////////////

bool wikrt_valid_key_len(char const* k, size_t* len)
{
    (*len) = 1 + WIKRT_VALID_KEY_MAXLEN;
    return wikrt_valid_text_len(k, len, NULL) 
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
    // transactions are trivially recorded as values (for now).
    
    wikrt_drop_v(cx, cx->txn, NULL);
    cx->txn = WIKRT_REG_TXN_INIT;
}

void wikrt_txn_durable(wikrt_cx* cx)
{

}

wikrt_err wikrt_txn_create(wikrt_cx* cx) 
{
    if(WIKRT_VOID != cx->txn) { 
        // no hierarchical transactions
        return WIKRT_IMPL;
    }
    return WIKRT_IMPL;
}

wikrt_err wikrt_txn_commit(wikrt_cx* cx) 
{
    // TODO: commit transaction
    wikrt_txn_abort(cx);
    return WIKRT_IMPL;
}

wikrt_err wikrt_txn_write(wikrt_cx* cx, char const* key)
{
    size_t keylen;
    if(!wikrt_valid_key_len(key,&keylen)) { return WIKRT_INVAL; }
    return WIKRT_IMPL;
}

wikrt_err wikrt_txn_read(wikrt_cx* cx, char const* key) 
{
    size_t keylen;
    if(!wikrt_valid_key_len(key, &keylen)) { return WIKRT_INVAL; }
    // TODO: lookup in database, and record in transaction state.

    return WIKRT_IMPL;
}

