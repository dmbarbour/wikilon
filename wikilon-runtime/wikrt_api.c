
/* wikrt_api.c serves as an indirection layer between implementation
 * and external clients. The main task of this indirection is to track
 * the root-set for each context, but also to provide a separation to
 * ensure freedom of implementation later.
 */
#include "wikilon-runtime.h"

#include "wikrt.h"

#include <string.h>
#include <stdio.h>
#include <sys/mman.h>
#include <errno.h>
#include <assert.h>

static void wikrt_rs_init(wikrt_rs*);
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
    case WIKRT_IMPL:            return "reached limit of current implementation";
    case WIKRT_DBERR:           return "filesystem or database layer error";
    case WIKRT_NOMEM:           return "out of memory (malloc or mmap failure)";
    case WIKRT_CXFULL:          return "context full, size quota reached";
    case WIKRT_BUFFSZ:          return "target buffer too small";
    case WIKRT_TXN_CONFLICT:    return "transaction conflict";
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


// trivial implementation 
void wikrt_env_sync(wikrt_env* e) {
    if(NULL != e->db) { 
        wikrt_db_flush(e->db); 
    }
}

static void wikrt_cx_init(wikrt_cxm* cxm, wikrt_cx* cx) {
    cx->cxm = cxm;
    cx->memory = cxm->memory;
    wikrt_rs_init(&(cx->rs));

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

void wikrt_cx_destroy(wikrt_cx* cx) {
    
    // drop bound vrefs
    wikrt_rs* const rs = &(cx->rs);
    for(int ix = 0; ix < WIKRT_ROOTSET_SIZE; ++ix) {
        _wikrt_drop(cx, rs->ls[ix], true);
    }

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

  ///////////////////////////////////////////// 
 // CONVERSION FROM WIKRT_VREF TO WIKRT_VAL //
/////////////////////////////////////////////

typedef wikrt_size wikrt_rsi;
static inline void wikrt_rsi_free(wikrt_rs* rs, wikrt_rsi ix) 
{
    rs->ls[ix] = rs->fl;
    rs->fl = wikrt_i2v(ix + 1);
}
static inline bool wikrt_rsi_alloc(wikrt_rs* rs, wikrt_rsi* ix) 
{
    if(0 == rs->fl) { return false; }
    (*ix) = wikrt_i2v(rs->fl) - 1;
    assert((*ix) < WIKRT_ROOTSET_SIZE); // sanity check against non-linear use
    rs->fl = rs->ls[(*ix)];
    return true;
}
static void wikrt_rs_init(wikrt_rs* rs)
{
    rs->fl = 0;
    for(wikrt_rsi ix = 0; ix < WIKRT_ROOTSET_SIZE; ++ix) {
        wikrt_rsi_free(rs, ix);
    }
}
static inline wikrt_val* wikrt_pvref(wikrt_vref* vref) 
{ 
    return ((wikrt_val*)vref); 
}
static inline void wikrt_vref_free(wikrt_cx* cx, wikrt_vref* vref) 
{
    wikrt_rsi_free(&(cx->rs), ((wikrt_val*)vref) - (cx->rs.ls));
}
static inline wikrt_val wikrt_vref_peek(wikrt_vref const* vref) 
{
    return *((wikrt_val const*) vref);
}
static inline wikrt_val wikrt_vref_take(wikrt_cx* cx, wikrt_vref* vref) 
{
    wikrt_val const r = *(wikrt_pvref(vref));
    wikrt_vref_free(cx, vref);
    return r;
}

static bool wikrt_vref_alloc(wikrt_cx* cx, wikrt_vref** pvref) 
{
    wikrt_rsi ix;
    if(!wikrt_rsi_alloc(&(cx->rs), &ix)) {
        (*pvref) = NULL;
        return false;
    }
    wikrt_val* const pv = (cx->rs.ls + ix);
    (*pvref) = (wikrt_vref*)pv;
    *pv = WIKRT_VOID;
    return true;
}
static inline bool wikrt_vref_valid(wikrt_cx const* const cx, wikrt_vref const* const vref) 
{
    // currently just checks whether a vref is in the correct context. 
    wikrt_vref const* const lb = (wikrt_vref const*)(cx->rs.ls);
    wikrt_vref const* const ub = (wikrt_vref const*)(cx->rs.ls + WIKRT_ROOTSET_SIZE);
    return ((lb <= vref) && (vref < ub)); 
}

wikrt_err wikrt_peek_type(wikrt_cx* cx, wikrt_vtype* out, wikrt_vref const* v)
{
    if(!wikrt_vref_valid(cx,v)) { return WIKRT_INVAL; }
    return _wikrt_peek_type(cx, out, wikrt_vref_peek(v));
}

wikrt_err wikrt_alloc_text(wikrt_cx* cx, wikrt_vref** txt, char const* cstr, size_t len) 
{
    if(!wikrt_vref_alloc(cx, txt)) { return WIKRT_CXFULL; }
    return _wikrt_alloc_text(cx, wikrt_pvref(*txt), cstr, len);
}

wikrt_err wikrt_alloc_binary(wikrt_cx* cx, wikrt_vref** binary, uint8_t const* buff, size_t nBytes) 
{
    if(!wikrt_vref_alloc(cx, binary)) { return WIKRT_CXFULL; }
    return _wikrt_alloc_binary(cx, wikrt_pvref(*binary), buff, nBytes);
}

wikrt_err wikrt_alloc_i32(wikrt_cx* cx, wikrt_vref** i32, int32_t n) 
{
    if(!wikrt_vref_alloc(cx, i32)) { return WIKRT_CXFULL; }
    return _wikrt_alloc_i32(cx, wikrt_pvref(*i32), n);
}

wikrt_err wikrt_alloc_i64(wikrt_cx* cx, wikrt_vref** i64, int64_t n) 
{
    if(!wikrt_vref_alloc(cx, i64)) { return WIKRT_CXFULL; }
    return _wikrt_alloc_i64(cx, wikrt_pvref(*i64), n);
}

wikrt_err wikrt_peek_i32(wikrt_cx* cx, wikrt_vref const* v, int32_t* i32) 
{
    if(!wikrt_vref_valid(cx, v)) { return WIKRT_INVAL; }
    return _wikrt_peek_i32(cx, wikrt_vref_peek(v), i32);
}

wikrt_err wikrt_peek_i64(wikrt_cx* cx, wikrt_vref const* v, int64_t* i64) 
{
    if(!wikrt_vref_valid(cx, v)) { return WIKRT_INVAL; }
    return _wikrt_peek_i64(cx, wikrt_vref_peek(v), i64);
}

wikrt_err wikrt_peek_istr(wikrt_cx* cx, wikrt_vref const* v, char* buff, size_t* buffsz)
{
    if(!wikrt_vref_valid(cx, v)) { return WIKRT_INVAL; }
    return _wikrt_peek_istr(cx, wikrt_vref_peek(v), buff, buffsz);
}

wikrt_err wikrt_alloc_istr(wikrt_cx* cx, wikrt_vref** v, char const* istr, size_t strlen) 
{
    if(!wikrt_vref_alloc(cx, v)) { return WIKRT_CXFULL; }
    return _wikrt_alloc_istr(cx, wikrt_pvref(*v), istr, strlen);
}

wikrt_err wikrt_alloc_prod(wikrt_cx* cx, wikrt_vref** p, wikrt_vref* fst, wikrt_vref* snd) 
{
    if(!wikrt_vref_valid(cx, fst) || !wikrt_vref_valid(cx, snd)) { return WIKRT_INVAL; }
    (*p) = snd; // reuse vref allocation
    return _wikrt_alloc_prod(cx, wikrt_pvref(*p), wikrt_vref_take(cx, fst), wikrt_vref_peek(snd));
}

wikrt_err wikrt_split_prod(wikrt_cx* cx, wikrt_vref* p, wikrt_vref** fst, wikrt_vref** snd) 
{
    if(!wikrt_vref_valid(cx, p)) { return WIKRT_INVAL; }
    if(!wikrt_vref_alloc(cx, fst)) { return WIKRT_CXFULL; }
    (*snd) = p; // reuse vref allocation
    return _wikrt_split_prod(cx, wikrt_vref_peek(p), wikrt_pvref(*fst), wikrt_pvref(*snd));
}
 
wikrt_err wikrt_alloc_sum(wikrt_cx* cx, wikrt_vref** c, bool inRight, wikrt_vref* v)
{
    if(!wikrt_vref_valid(cx, v)) { return WIKRT_INVAL; }
    (*c) = v; // reuse vref allocation
    return _wikrt_alloc_sum(cx, wikrt_pvref(*c), inRight, wikrt_vref_peek(v));
}

wikrt_err wikrt_split_sum(wikrt_cx* cx, wikrt_vref* c, bool* inRight, wikrt_vref** v) 
{
    if(!wikrt_vref_valid(cx, c)) { return WIKRT_INVAL; }
    (*v) = c; // reuse vref allocation
    return _wikrt_split_sum(cx, wikrt_vref_peek(c), inRight, wikrt_pvref(*v));
}

wikrt_err wikrt_alloc_block(wikrt_cx* cx, wikrt_vref** v, char const* abc, size_t len, wikrt_abc_opts opts) 
{
    if(!wikrt_vref_alloc(cx, v)) { return WIKRT_CXFULL; }
    return _wikrt_alloc_block(cx, wikrt_pvref(*v), abc, len, opts);
}


wikrt_err wikrt_alloc_seal(wikrt_cx* cx, wikrt_vref** sv, char const* s, size_t len, wikrt_vref* v)
{
    if(!wikrt_vref_valid(cx, v)) { return WIKRT_INVAL; }
    (*sv) = v; // reuse vref allocation
    return _wikrt_alloc_seal(cx, wikrt_pvref(*sv), s, len, wikrt_vref_peek(v));
}

wikrt_err wikrt_split_seal(wikrt_cx* cx, wikrt_vref* sv, char* buff, wikrt_vref** v)
{
    if(!wikrt_vref_valid(cx, sv)) { return WIKRT_INVAL; }
    (*v) = sv; // reuse vref allocation
    return _wikrt_split_seal(cx, wikrt_vref_peek(sv), buff, wikrt_pvref(*v));
}

wikrt_err wikrt_copy(wikrt_cx* cx, wikrt_vref** dst, wikrt_vref const* origin, bool const bCopyAff) 
{
    return wikrt_remote_copy(cx, dst, cx, origin, bCopyAff);
}

wikrt_err wikrt_remote_copy(wikrt_cx* dcx, wikrt_vref** dv, wikrt_cx const* ocx, wikrt_vref const* ov, bool const bCopyAff)
{
    if(!wikrt_vref_valid(ocx, ov)) { return WIKRT_INVAL; }
    if(!wikrt_vref_alloc(dcx, dv)) { return WIKRT_CXFULL; }
    return _wikrt_remote_copy(dcx, wikrt_pvref(*dv), ocx, wikrt_vref_peek(ov), bCopyAff);
}

wikrt_err wikrt_drop(wikrt_cx* cx, wikrt_vref* v, bool const bDropRel) 
{
    if(!wikrt_vref_valid(cx, v)) { return WIKRT_INVAL; }
    return _wikrt_drop(cx, wikrt_vref_take(cx, v), bDropRel);
}

wikrt_err wikrt_stow(wikrt_cx* cx, wikrt_vref** v)
{
    if(!wikrt_vref_valid(cx, (*v))) { return WIKRT_INVAL; }
    return _wikrt_stow(cx, wikrt_pvref(*v));
}



