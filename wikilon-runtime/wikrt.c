
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/mman.h>
#include <assert.h>

#include "wikrt.h"

void wikrt_cx_resetmem(wikrt_cx*); 


wikrt_err wikrt_env_create(wikrt_env** ppEnv, char const* dirPath, uint32_t dbMaxMB) {
    wikrt_env* const e = calloc(1, sizeof(wikrt_env));
    if(NULL == e) return WIKRT_NOMEM;

    e->mutex = (pthread_mutex_t) PTHREAD_MUTEX_INITIALIZER;

    // use of key-value database and stowage is optional
    if((NULL == dirPath) || (0 == dbMaxMB)) { 
        e->db_env = NULL;
    } else if(!wikrt_db_init(e, dirPath, dbMaxMB)) {
        free(e);
        return WIKRT_DBERR;
    } 

    // maybe create thread pool or task list, etc.?

    (*ppEnv) = e;
    return WIKRT_OK;
}

void wikrt_env_destroy(wikrt_env* e) {
    assert(NULL == e->cxhd);
    wikrt_db_destroy(e);
    pthread_mutex_destroy(&(e->mutex));
    free(e);
}

void wikrt_env_lock(wikrt_env* e) {
    pthread_mutex_lock(&(e->mutex));
}

void wikrt_env_unlock(wikrt_env* e) {
    pthread_mutex_unlock(&(e->mutex));
}

// trivial implementation 
void wikrt_env_sync(wikrt_env* e) {
    if(e->db_enable) {
        int const force_flush = 1;
        mdb_env_sync(e->db_env, force_flush);
    }
}

size_t cx_size_bytes(uint32_t sizeMB) { 
    return ((size_t) sizeMB) * (1024 * 1024); 
}

wikrt_err wikrt_cx_create(wikrt_env* e, wikrt_cx** ppCX, uint32_t sizeMB) {
    bool const bSizeValid = (WIKRT_CX_SIZE_MIN <= sizeMB) 
                         && (sizeMB <= WIKRT_CX_SIZE_MAX);
    if(!bSizeValid) return WIKRT_INVAL;
    size_t const sizeBytes = cx_size_bytes(sizeMB);

    wikrt_cx* const cx = calloc(1,sizeof(wikrt_cx));
    if(NULL == cx) return WIKRT_NOMEM;


    static int const prot = PROT_READ | PROT_WRITE | PROT_EXEC;
    static int const flags = MAP_ANONYMOUS | MAP_PRIVATE;

    errno = 0;
    void* const pMem = mmap(NULL, sizeBytes, prot, flags, -1, 0); 
    if(NULL == pMem) {
        free(cx);
        return WIKRT_NOMEM;
    }

    cx->env    = e;
    cx->sizeMB = sizeMB;
    cx->memory = (wikrt_val*) pMem;
    //cx->mutex = (pthread_mutex_t) PTHREAD_MUTEX_INITIALIZER;

    // set initial memory before adding context to global list
    // (e.g. to ensure empty stowage lists)
    wikrt_cx_resetmem(cx);

    // add to global context list
    wikrt_env_lock(e); {
        wikrt_cx* const hd = e->cxhd;
        cx->next = hd;
        cx->prev = NULL;
        if(NULL != hd) { hd->prev = cx; }
        e->cxhd = cx;
    } wikrt_env_unlock(e);
    
    (*ppCX) = cx;
    return WIKRT_OK;
}

void wikrt_cx_destroy(wikrt_cx* cx) {
    wikrt_env* const e = cx->env;

    // remove context from global context list
    wikrt_env_lock(e); {
        if(NULL != cx->next) { 
            cx->next->prev = cx->prev; 
        }
        if(NULL != cx->prev) { 
            cx->prev->next = cx->next; 
        } else { 
            assert(e->cxhd == cx);
            e->cxhd = cx->next; 
        }
    } wikrt_env_unlock(e);

    // free memory associated with the context
    size_t const sizeBytes = cx_size_bytes(cx->sizeMB);
    bool const context_unmapped = (0 == munmap(cx->memory, sizeBytes));
    assert(context_unmapped);
    free(cx);
}

wikrt_env* wikrt_cx_env(wikrt_cx* cx) {
    return cx->env;
}

void wikrt_cx_reset(wikrt_cx* cx) {
    // At the moment, contexts don't have any external metadata.
    // I'd prefer to keep it that way, if feasible. Anyhow, this
    // means a reset is a trivial update to a context's internal
    // memory. 
    wikrt_env_lock(cx->env); {
        wikrt_cx_resetmem(cx);
    } wikrt_env_unlock(cx->env);
}

void wikrt_cx_resetmem(wikrt_cx* cx) {
    wikrt_cx_hdr* hdr = (wikrt_cx_hdr*) cx->memory;
    (*hdr) = (wikrt_cx_hdr){ 0 }; // clear root memory

    wikrt_addr const hdrEnd = (wikrt_val) WIKRT_PAGEBUFF(sizeof(wikrt_cx_hdr));
    wikrt_val const szRem = (wikrt_val) cx_size_bytes(cx->sizeMB) - hdrEnd;
    #undef HDRSZ

    // we'll simply 'free' our chunk of non-header memory.
    wikrt_free(cx, &(hdr->flmain), hdrEnd, szRem);
}

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
    case ABC_INEW:        return "#";
    case ABC_ID1:         return "1";
    case ABC_ID2:         return "2";
    case ABC_ID3:         return "3";
    case ABC_ID4:         return "4";
    case ABC_ID5:         return "5";
    case ABC_ID6:         return "6";
    case ABC_ID7:         return "7";
    case ABC_ID8:         return "8";
    case ABC_ID9:         return "9";
    case ABC_ID0:         return "0";
    case ABC_IADD:        return "+";
    case ABC_IMUL:        return "*";
    case ABC_INEG:        return "-";
    case ABC_IDIV:        return "Q";
    case ABC_IGT:         return "G";
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
    case WIKRT_DBERR:           return "filesystem or database layer error";
    case WIKRT_NOMEM:           return "out of memory (malloc or mmap failure)";
    case WIKRT_CXFULL:          return "context full, size quota reached";
    case WIKRT_BUFFSZ:          return "target buffer too small";
    case WIKRT_TXN_CONFLICT:    return "transaction conflict";
    case WIKRT_STREAM_WAIT:     return "open binary stream needs data";
    case WIKRT_QUOTA_STOP:      return "evaluation effort quota reached";
    case WIKRT_ASSERT_FAIL:     return "assertion failure";
    case WIKRT_TYPE_ERROR:      return "type mismatch";
    default:                    return "unrecognized error code";
}}

// assuming valid utf-8 input
inline uint32_t utf8_readc(uint8_t const** s) {
    uint8_t const* p = *s;
    uint32_t const c0 = p[0];
    if(c0 < 0x80) {
        (*s) = (p+1);
        return c0;
    } else if(c0 < 0xE0) {
        // two bytes
        uint32_t const c1 = (uint32_t) p[1];
        (*s) = (p+2);
        return ((0x1F & c0) << 6) | ((0x3F & c1));
    } else if(c0 < 0xF0) {
        uint32_t const c1 = (uint32_t) p[1];
        uint32_t const c2 = (uint32_t) p[2];
        (*s) = (p+3);
        return ((0x0F & c0) << 12)
             | ((0x3F & c1) << 6)
             | ((0x3F & c2));
    } else {
        uint32_t const c1 = (uint32_t) p[1];
        uint32_t const c2 = (uint32_t) p[2];
        uint32_t const c3 = (uint32_t) p[3];
        (*s) = (p+4); 
        return ((0x07 & c0) << 18)
             | ((0x3F & c1) << 12)
             | ((0x3F & c2) << 6)
             | ((0x3F & c3));
    }
}

// C0, DEL, C1
inline bool isControlChar(uint32_t c) {
    return (c <= 0x1F) || ((0x7F <= c) && (c <= 0x9F));
}

// 0xD800..0xDFFF
inline bool isSurrogateCodepoint(uint32_t c) { 
    return (0xD800 <= c) && (c <= 0xDFFF);
}

// 0xFFFD
inline bool isReplacementChar(uint32_t c) {
    return (0xFFFD == c);
}

inline bool isValidTokChar(uint32_t c) {
    bool const bInvalidChar =
        ('{' == c) || ('}' == c) ||
        isControlChar(c) ||
        isSurrogateCodepoint(c) ||
        isReplacementChar(c);
    return !bInvalidChar;
}

// assumes normal form utf-8 argument, NUL-terminated
bool wikrt_valid_token(char const* s) {
    // valid size is 1..63 bytes
    size_t const len = strlen(s);
    bool const bValidSize = (0 < len) && (len < 64);
    if(!bValidSize) return false;

    uint8_t const* p = (uint8_t const*) s;
    while(1) {
        uint32_t c = utf8_readc(&p);
        if(0 == c) return true;
        if(!isValidTokChar(c)) return false;
    }
}


wikrt_err wikrt_alloc_text(wikrt_cx* cx, wikrt_val* v, char const* s) 
{
    // for now, just construct a naive list.
    return WIKRT_INVAL;
}

wikrt_err wikrt_alloc_block(wikrt_cx* cx, wikrt_val* v, char const* abc, wikrt_abc_opts opts) 
{
    return WIKRT_INVAL;
}

wikrt_err wikrt_alloc_i32(wikrt_cx* _cx, wikrt_val* v, int32_t n) 
{
    // small integers?
    if((WIKRT_SMALLINT_MIN <= n) && (n <= WIKRT_SMALLINT_MAX)) {
        (*v) = (wikrt_val)(2 * n);
        return WIKRT_OK;
    }

    // TODO: Larger integers.
    return WIKRT_INVAL;
}

wikrt_err wikrt_alloc_i64(wikrt_cx* cx, wikrt_val* v, int64_t n) 
{
    if((WIKRT_SMALLINT_MIN <= n) && (n <= WIKRT_SMALLINT_MAX)) {
        (*v) = (wikrt_val)(2 * n);
        return WIKRT_OK;
    }

    // TODO: larger integers
    return WIKRT_INVAL;
}

wikrt_err wikrt_peek_i32(wikrt_cx* cx, wikrt_val const v, int32_t* i32) 
{
    return WIKRT_INVAL;
}

wikrt_err wikrt_peek_i64(wikrt_cx* cx, wikrt_val const v, int64_t* i64) 
{
    return WIKRT_INVAL;
}

wikrt_err wikrt_alloc_prod(wikrt_cx* cx, wikrt_val* p, wikrt_val fst, wikrt_val snd)
{
    return WIKRT_INVAL;
}

wikrt_err wikrt_split_prod(wikrt_cx* cx, wikrt_val p, wikrt_val* fst, wikrt_val* snd)
{
    return WIKRT_INVAL;
}

wikrt_err wikrt_alloc_sum(wikrt_cx* cx, wikrt_val* c, bool inRight, wikrt_val v)
{
    return WIKRT_INVAL;
}

wikrt_err wikrt_split_sum(wikrt_cx* cx, wikrt_val c, bool* inRight, wikrt_val* v)
{
    return WIKRT_INVAL;
}

