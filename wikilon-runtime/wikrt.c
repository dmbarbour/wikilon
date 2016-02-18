
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/mman.h>
#include <assert.h>

#include "wikrt.h"

void wikrt_cx_resetmem(wikrt_cx*); 

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



wikrt_err wikrt_cx_create(wikrt_env* e, wikrt_cx** ppCX, uint32_t sizeMB) 
{
    (*ppCX) = NULL;

    bool const bSizeValid = (WIKRT_CX_SIZE_MIN <= sizeMB) 
                         && (sizeMB <= WIKRT_CX_SIZE_MAX);
    if(!bSizeValid) return WIKRT_INVAL;
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

    cx->cxm = cxm;
    cx->memory = memory;

    // Address zero is reserved to represent 'unit'. 
    // But everything else may be allocated normally.
    wikrt_addr const allocStart = WIKRT_CELLSIZE;
    wikrt_size const allocSize = (sizeBytes - WIKRT_CELLSIZE);
    wikrt_fl_free(memory, &(cxm->fl), allocSize, allocStart);

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
    wikrt_cx* fork = calloc(1, sizeof(wikrt_cx));
    if(NULL == fork) {
        (*pfork) = NULL;
        return WIKRT_NOMEM;
    }

    fork->cxm = cxm;
    fork->memory = cxm->memory;

    wikrt_cxm_lock(cxm); {
        fork->next = cxm->cxlist;
        if(NULL != fork->next) { fork->next->prev = fork; }
        cxm->cxlist = fork;
    } wikrt_cxm_unlock(cxm);

    (*pfork) = fork;
    return WIKRT_OK;
}

void wikrt_cx_destroy(wikrt_cx* cx) {
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

wikrt_err wikrt_peek_type(wikrt_cx* cx, wikrt_vtype* out, wikrt_val const v)
{
    if(wikrt_i(v)) { 
        (*out) = WIKRT_VTYPE_INTEGER; 
    } else {
        wikrt_tag const vtag = wikrt_vtag(v);
        wikrt_addr const vaddr = wikrt_vaddr(v);
        if(WIKRT_P == vtag) {
            if(0 == vaddr) { (*out) = WIKRT_VTYPE_UNIT; }
            else { (*out) = WIKRT_VTYPE_PRODUCT; }
        } else if((WIKRT_PL == vtag) || (WIKRT_PR == vtag)) {
            (*out) = WIKRT_VTYPE_SUM;
        } else if((WIKRT_O == vtag) && (0 != vaddr)) {
            wikrt_val const* const pv = wikrt_pval(cx, vaddr);
            wikrt_val const otag = pv[0];
            if(wikrt_otag_bigint(otag)) { 
                (*out) = WIKRT_VTYPE_INTEGER; 
            } else if(wikrt_otag_deepsum(otag) || wikrt_otag_array(otag)) { 
                (*out) = WIKRT_VTYPE_SUM; 
            } else if(wikrt_otag_block(otag)) { 
                (*out) = WIKRT_VTYPE_BLOCK; 
            } else if(wikrt_otag_stowage(otag)) { 
                (*out) = WIKRT_VTYPE_STOWED; 
            } else if(wikrt_otag_seal(otag) || wikrt_otag_seal_sm(otag)) { 
                (*out) = WIKRT_VTYPE_SEALED; 
            } else { return WIKRT_INVAL; }
        } else { return WIKRT_INVAL; }
    }
    return WIKRT_OK;
}

// assumes normal form utf-8 argument, NUL-terminated
bool wikrt_valid_token(char const* s) {
    // valid size is 1..63 bytes
    size_t len = strlen(s);
    bool const bValidSize = (0 < len) && (len < 64);
    if(!bValidSize) return false;

    uint32_t cp;
    while(len != 0) {
        if(!utf8_step(&s,&len,&cp) || !wikrt_token_char(cp))
            return false;
    }
    return true;
}

/* Currently allocating as a normal list. This means we allocate one
 * full cell (WIKRT_CELLSIZE) per character, usually an 8x increase.
 * Yikes! But I plan to later tune this to a dedicated structure.
 */
wikrt_err wikrt_alloc_text(wikrt_cx* cx, wikrt_val* txt, char const* s, size_t len) 
{ 
    wikrt_err r = WIKRT_IMPL;
    wikrt_val* tl = txt;
    uint32_t cp;
    while((len != 0) && utf8_step(&s, &len, &cp)) {
        if(!wikrt_text_char(cp)) { 
            r = WIKRT_INVAL; 
            goto e;
        }
        wikrt_addr dst;
        if(!wikrt_alloc(cx, WIKRT_CELLSIZE, &dst)) {
            r = WIKRT_CXFULL;
            goto e;
        }
        (*tl) = wikrt_tag_addr(WIKRT_PL, dst);
        wikrt_val* const pv = wikrt_pval(cx, dst);
        pv[0] = wikrt_i2v(cp);
        tl = (pv + 1);
    }
    (*tl) = WIKRT_UNIT_INR;
    return WIKRT_OK;
e: // error; need to free allocated data
    (*tl) = WIKRT_UNIT_INR;
    wikrt_drop(cx, (*txt), true);
    (*txt) = WIKRT_UNIT;
    return r;
}

/* For the moment, we'll allocate a binary as a plain old list.
 * This results in an WIKRT_CELLSIZE (8x) expansion at this time. I
 * I will support more compact representations later.
 */
wikrt_err wikrt_alloc_binary(wikrt_cx* cx, wikrt_val* v, uint8_t const* buff, size_t nBytes) 
{
    wikrt_err r = WIKRT_IMPL;
    wikrt_val* tl = v;
    uint8_t const* const buffEnd = buff + nBytes;
    while(buff != buffEnd) {
        uint8_t const e = *(buff++);
        wikrt_addr dst;
        if(!wikrt_alloc(cx, WIKRT_CELLSIZE, &dst)) { 
            r = WIKRT_CXFULL;
            goto e;
        } 
        (*tl) = wikrt_tag_addr(WIKRT_PL, dst);
        wikrt_val* const pdst = wikrt_pval(cx, dst);
        pdst[0] = wikrt_i2v((int32_t)e); 
        tl = (pdst + 1);
    }
    (*tl) = WIKRT_UNIT_INR;
    return WIKRT_OK;
e:
    (*tl) = WIKRT_UNIT_INR;
    wikrt_drop(cx, (*v), true);
    (*v) = WIKRT_UNIT;
    return r;
}

wikrt_err wikrt_alloc_i32(wikrt_cx* cx, wikrt_val* v, int32_t n) 
{
    bool const isSmallInt = ((WIKRT_SMALLINT_MIN <= n) && (n <= WIKRT_SMALLINT_MAX));
    if(isSmallInt) { 
        (*v) = wikrt_i2v(n); 
        return WIKRT_OK; 
    }

    bool const sign = (n < 0);
    if(sign) { n = -n; }

    wikrt_size const nDigits = 2;
    wikrt_size const allocSz = sizeof(wikrt_val) + (nDigits * sizeof(uint32_t));

    wikrt_addr dst;
    if(!wikrt_alloc(cx, allocSz, &dst)) {
        (*v) = WIKRT_UNIT;
        return WIKRT_CXFULL;
    }
    (*v) = wikrt_tag_addr(WIKRT_O, dst);

    wikrt_val* const p = wikrt_pval(cx, dst);
    p[0] = wikrt_mkotag_bigint(sign, nDigits);
    uint32_t* const d = (uint32_t*)(p+1);
    d[0] = (uint32_t) (n % WIKRT_BIGINT_DIGIT);
    d[1] = (uint32_t) (n / WIKRT_BIGINT_DIGIT); 

    return WIKRT_OK;
}

wikrt_err wikrt_peek_i32(wikrt_cx* cx, wikrt_val const v, int32_t* i32) 
{
    // small integers (normal case)
    if(wikrt_i(v)) {
        (*i32) = wikrt_v2i(v);
        return WIKRT_OK;
    }

    // TODO: big integers, overflow calculations.
    return WIKRT_IMPL;
}

wikrt_err wikrt_alloc_i64(wikrt_cx* cx, wikrt_val* v, int64_t n) 
{
    bool const isSmallInt = ((WIKRT_SMALLINT_MIN <= n) && (n <= WIKRT_SMALLINT_MAX));
    if(isSmallInt) {
        (*v) = wikrt_i2v((int32_t)n);
        return WIKRT_OK;
    }

    bool const sign = (n < 0);
    if(sign) { n = -n; }
    int64_t const dmax = (int64_t)WIKRT_BIGINT_DIGIT;
    int64_t const d2max = ((dmax-1)*(dmax+1));
    wikrt_size const nDigits = (n > d2max) ? 3 : 2;
    wikrt_size const allocSz = sizeof(wikrt_val) + (nDigits * sizeof(uint32_t));

    wikrt_addr dst;
    if(!wikrt_alloc(cx, allocSz, &dst)) {
        (*v) = WIKRT_UNIT;
        return WIKRT_CXFULL;
    }

    (*v) = wikrt_tag_addr(WIKRT_O, dst);
    wikrt_val* const p = wikrt_pval(cx, dst);
    p[0] = wikrt_mkotag_bigint(sign, nDigits);
    uint32_t* const d = (uint32_t*)(p+1);

    if(2 == nDigits) {
        d[0] = (uint32_t) (n % WIKRT_BIGINT_DIGIT);
        d[1] = (uint32_t) (n / WIKRT_BIGINT_DIGIT);
    } else { // 3 big digits
        d[0] = (uint32_t) (n % WIKRT_BIGINT_DIGIT);
        n /= WIKRT_BIGINT_DIGIT;
        d[1] = (uint32_t) (n % WIKRT_BIGINT_DIGIT);
        d[2] = (uint32_t) (n / WIKRT_BIGINT_DIGIT);
    }

    return WIKRT_OK;
}

wikrt_err wikrt_peek_i64(wikrt_cx* cx, wikrt_val const v, int64_t* i64) 
{
    if(wikrt_i(v)) {
        (*i64) = (int64_t) wikrt_v2i(v);
        return WIKRT_OK;
    }

    // TODO: big integers, simple overflow calculations.
    //  this will wait until after spike solution.
    return WIKRT_IMPL;
}

wikrt_err wikrt_alloc_prod(wikrt_cx* cx, wikrt_val* p, wikrt_val fst, wikrt_val snd) 
{
    wikrt_addr dst;
    if(!wikrt_alloc(cx, WIKRT_CELLSIZE, &dst)) {
        (*p) = WIKRT_UNIT;
        return WIKRT_CXFULL;
    }
    (*p) = wikrt_tag_addr(WIKRT_P, dst);
    wikrt_val* const pv = wikrt_pval(cx, dst);
    pv[0] = fst;
    pv[1] = snd;
    return WIKRT_OK;
}

wikrt_err wikrt_split_prod(wikrt_cx* cx, wikrt_val p, wikrt_val* fst, wikrt_val* snd) 
{
    wikrt_tag const ptag = wikrt_vtag(p);
    wikrt_addr const paddr = wikrt_vaddr(p);
    if((WIKRT_P == ptag) && (0 != paddr)) {
        wikrt_val* const pv = wikrt_pval(cx, paddr);
        (*fst) = pv[0];
        (*snd) = pv[1];
        wikrt_free(cx, WIKRT_CELLSIZE, paddr);
        return WIKRT_OK;
    } else {
        (*fst) = WIKRT_UNIT;
        (*snd) = WIKRT_UNIT;
        return WIKRT_TYPE_ERROR;
    }
}


wikrt_err wikrt_alloc_sum(wikrt_cx* cx, wikrt_val* c, bool inRight, wikrt_val v) 
{
    wikrt_tag const vtag = wikrt_vtag(v);
    wikrt_addr const vaddr = wikrt_vaddr(v);
    wikrt_val* const pv = wikrt_pval(cx, vaddr);
    if(WIKRT_P == vtag) {
        // shallow sum on product, pointer manipulation, no allocation
        wikrt_tag const newtag = inRight ? WIKRT_PR : WIKRT_PL;
        (*c) = wikrt_tag_addr(newtag, vaddr);
        return WIKRT_OK;
    } else if((WIKRT_O == vtag) && wikrt_otag_deepsum(*pv) && ((*pv) < (1 << 30))) {
        // deepsum has space if bits 30 and 31 are available, i.e. if tag less than (1 << 30).
        // In this case, no allocation is required. We can update the existing deep sum in place.
        wikrt_val const sumtag = ((*pv) >> 6) | (inRight ? WIKRT_DEEPSUMR : WIKRT_DEEPSUML);
        wikrt_val const otag = (sumtag << 8) | WIKRT_OTAG_DEEPSUM;
        (*pv) = otag;
        (*c) = v;
        return WIKRT_OK;
    } else { // need to allocate space
        wikrt_addr dst;
        if(!wikrt_alloc(cx, WIKRT_CELLSIZE, &dst)) {
            (*c) = WIKRT_UNIT;
            return WIKRT_CXFULL;
        }
        wikrt_val const sumtag = (inRight ? WIKRT_DEEPSUMR : WIKRT_DEEPSUML);
        wikrt_val const otag = (sumtag << 8) | WIKRT_OTAG_DEEPSUM;
        wikrt_val* const pv = wikrt_pval(cx, dst);
        pv[0] = otag;
        pv[1] = v;
        (*c) = wikrt_tag_addr(WIKRT_O, dst);
        return WIKRT_OK;
    }
}

wikrt_err wikrt_split_sum(wikrt_cx* cx, wikrt_val c, bool* inRight, wikrt_val* v) 
{
    wikrt_tag const tag = wikrt_vtag(c);
    wikrt_addr const addr = wikrt_vaddr(c);
    if(WIKRT_PL == tag) {
        (*inRight) = false;
        (*v) = wikrt_tag_addr(WIKRT_P, addr);
        return WIKRT_OK;
    } else if(WIKRT_PR == tag) {
        (*inRight) = true;
        (*v) = wikrt_tag_addr(WIKRT_P, addr);
        return WIKRT_OK;
    } else if(WIKRT_O == tag) {
        wikrt_val* const pv = wikrt_pval(cx, addr);
        wikrt_val const otag = pv[0];
        if(wikrt_otag_deepsum(otag)) {
            wikrt_val const s0 = (otag >> 8);
            (*inRight) = (3 == (3 & s0));
            wikrt_val const sf = (s0 >> 2);
            if(0 == sf) { // dealloc deepsum
                (*v) = pv[1];
                wikrt_free(cx, WIKRT_CELLSIZE, addr);
            } else { // keep value, reduce one level 
                (*v) = c;
                pv[0] = (sf << 8) | WIKRT_OTAG_DEEPSUM;
            }
            return WIKRT_OK;
        } else if(wikrt_otag_array(otag)) {
            // probably expand head of array then retry...
            return WIKRT_IMPL;
        } else { return WIKRT_TYPE_ERROR; }
    } else { return WIKRT_TYPE_ERROR; }
}

wikrt_err wikrt_alloc_block(wikrt_cx* cx, wikrt_val* v, char const* abc, size_t len, wikrt_abc_opts opts) 
{
    return WIKRT_IMPL;
}


wikrt_err wikrt_alloc_seal(wikrt_cx* cx, wikrt_val* sv, char const* s, wikrt_val v)
{
    return WIKRT_IMPL;
}

/** deep copy a structure
 *
 * It will be important to control how much space is used when copying,
 * i.e. to avoid busting the thread stack. I might need to model the
 * copy stack within the context itself, albeit with reasonably large
 * blocks to reduce fragmentation.
 */
wikrt_err wikrt_copy(wikrt_cx* cx, wikrt_val* copy, wikrt_val const src, bool bCopyAff) {
    return WIKRT_IMPL;
}

/** delete a large structure
 *
 * Similar to 'copy', I need some way to track progress for deletion of 
 * deep structures in constant extra space. 
 */
wikrt_err wikrt_drop(wikrt_cx* cx, wikrt_val v, bool bDropRel) 
{
    return WIKRT_IMPL;
}


wikrt_err wikrt_stow(wikrt_cx* cx, wikrt_val* out)
{
    return WIKRT_IMPL;
}

