
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "wikilon-runtime.h"

#define TESTCX_SIZE (WIKRT_CX_SIZE_MIN)
#define TESTENV_SIZE (5 * TESTCX_SIZE)

void run_tests(wikrt_cx* cx, int* runct, int* passct); 
int fillct(wikrt_cx* cx); // exercise memory management code

int main(int argc, char const** argv) {
    // return values
    int const ok  = 0;
    int const err = (-1);

    wikrt_env* e;
    wikrt_err const env_created = wikrt_env_create(&e,"testdir/db", TESTENV_SIZE);
    if(WIKRT_OK != env_created) {
        fprintf(stderr, "env create: %s\n", wikrt_strerr(env_created));
        return err;
    }

    wikrt_cx* cx;
    wikrt_err const cx_created = wikrt_cx_create(e, &cx, TESTCX_SIZE);
    if(WIKRT_OK != cx_created) {
        fprintf(stderr, "cx create: %s\n", wikrt_strerr(cx_created));
        return err;
    }

    int const fct0 = fillct(cx);
    int tests_run = 0;
    int tests_passed = 0;
    run_tests(cx, &tests_run, &tests_passed);
    int const fctf = fillct(cx);

    wikrt_cx_destroy(cx);
    wikrt_env_destroy(e);

    fprintf(stdout, u8"Available memcells: %d → %d (%s)\n"
        , fct0, fctf
        , ((fct0 == fctf) ? "ok" : "memleak"));
    fprintf(stdout, u8"Passed %d of %d Tests\n", tests_passed, tests_run);
    return ((tests_run == tests_passed) ? ok : err);
}

int fillct(wikrt_cx* cx) 
{
    // fill entire context with pairs, then free them.
    // goal here is to exercise memory management code (coalesce, etc.)
    // which might otherwise never trigger during testing.
    int ct = 0;
    wikrt_val p = WIKRT_UNIT;
    wikrt_val v;
    wikrt_err st;
    do {
        ++ct;
        v = p;
        st = wikrt_alloc_prod(cx, &p, WIKRT_UNIT, v);
    } while(WIKRT_OK == st);
    wikrt_drop(cx, v, false);

    // force coalesce: large numbers represented by multi-cell objects
    wikrt_alloc_i64(cx, &v, INT64_MIN);
    wikrt_drop(cx, v, false);
    
    return (ct - 1);
}

bool test_tcx(wikrt_cx* cx) { return true; }

bool test_alloc_true(wikrt_cx* cx) 
{
    wikrt_val uR;
    return (WIKRT_OK == wikrt_alloc_sum(cx, &uR, true, WIKRT_UNIT)) &&
        (WIKRT_UNIT_INR == uR);
}

bool test_alloc_false(wikrt_cx* cx) 
{
    wikrt_val uL;
    return (WIKRT_OK == wikrt_alloc_sum(cx, &uL, false, WIKRT_UNIT)) &&
        (WIKRT_UNIT_INL == uL);
}

bool test_split_true(wikrt_cx* cx)
{
    bool b; wikrt_val u;
    return (WIKRT_OK == wikrt_split_sum(cx, WIKRT_UNIT_INR, &b, &u)) &&
        (b) && (WIKRT_UNIT == u);
}

bool test_split_false(wikrt_cx* cx) 
{
    bool b; wikrt_val u;
    return (WIKRT_OK == wikrt_split_sum(cx, WIKRT_UNIT_INL, &b, &u)) &&
        (!b) && (WIKRT_UNIT == u);
}

bool test_vtype_unit(wikrt_cx* cx) 
{
    wikrt_vtype t;
    return (WIKRT_OK == wikrt_peek_type(cx, &t, WIKRT_UNIT)) &&
        (WIKRT_VTYPE_UNIT == t);
}

bool test_vtype_false(wikrt_cx* cx) 
{
    wikrt_vtype t;
    return (WIKRT_OK == wikrt_peek_type(cx, &t, WIKRT_UNIT_INL)) &&
        (WIKRT_VTYPE_SUM == t);
}

bool test_vtype_true(wikrt_cx* cx) 
{
    wikrt_vtype t;
    return (WIKRT_OK == wikrt_peek_type(cx, &t, WIKRT_UNIT_INR)) &&
        (WIKRT_VTYPE_SUM == t);
}

bool test_i32(wikrt_cx* cx, int32_t const iTest) 
{
    wikrt_val v; wikrt_vtype t; int32_t i;
    int const stAlloc = wikrt_alloc_i32(cx, &v, iTest);
    int const stType = wikrt_peek_type(cx, &t, v);
    int const stPeek = wikrt_peek_i32(cx, v, &i);
    int const stDrop = wikrt_drop(cx, v, false);

    bool const ok = 
        (WIKRT_OK == stAlloc) && (WIKRT_OK == stType) &&
        (WIKRT_OK == stPeek) && (WIKRT_OK == stDrop) &&
        (WIKRT_VTYPE_INTEGER == t) && (iTest == i);

    return ok;
}

static inline bool test_alloc_i32_max(wikrt_cx* cx) {
    return test_i32(cx, INT32_MAX); }
static inline bool test_alloc_i32_zero(wikrt_cx* cx) {
    return test_i32(cx, 0); }
static inline bool test_alloc_i32_min(wikrt_cx* cx) {
    return test_i32(cx, INT32_MIN); }
static inline bool test_alloc_i32_nearmin(wikrt_cx* cx) {
    return test_i32(cx, (-INT32_MAX)); }

// uses knowledge of internal representation
static inline bool test_alloc_i32_smallint_min(wikrt_cx* cx) {
    return test_i32(cx, 0 - ((1<<30) - 1) ); }
static inline bool test_alloc_i32_smallint_max(wikrt_cx* cx) {
    return test_i32(cx, ((1 << 30) - 1) ); }
static inline bool test_alloc_i32_largeint_minpos(wikrt_cx* cx) {
    return test_i32(cx, (1 << 30)); }
static inline bool test_alloc_i32_largeint_maxneg(wikrt_cx* cx) {
    return test_i32(cx, 0 - (1 << 30)); }

bool test_i64(wikrt_cx* cx, int64_t const iTest) 
{
    wikrt_val v; wikrt_vtype t; int64_t i;
    int const stAlloc = wikrt_alloc_i64(cx, &v, iTest);
    int const stType = wikrt_peek_type(cx, &t, v);
    int const stPeek = wikrt_peek_i64(cx, v, &i);
    int const stDrop = wikrt_drop(cx, v, false);

    bool const ok = 
        (WIKRT_OK == stAlloc) && (WIKRT_OK == stType) &&
        (WIKRT_OK == stPeek) && (WIKRT_OK == stDrop) &&
        (WIKRT_VTYPE_INTEGER == t) && (iTest == i);

    return ok;
}

static inline bool test_alloc_i64_max(wikrt_cx* cx) {
    return test_i64(cx, INT64_MAX); }
static inline bool test_alloc_i64_zero(wikrt_cx* cx) {
    return test_i64(cx, 0); }
static inline bool test_alloc_i64_min(wikrt_cx* cx) {
    return test_i64(cx, INT64_MIN); }
static inline bool test_alloc_i64_nearmin(wikrt_cx* cx) {
    return test_i64(cx, (-INT64_MAX)); }

// using knowledge of internal representations
static inline bool test_alloc_i64_2digit_min(wikrt_cx* cx) {
    return test_i64(cx,  -999999999999999999); }
static inline bool test_alloc_i64_2digit_max(wikrt_cx* cx) {
    return test_i64(cx,   999999999999999999); }
static inline bool test_alloc_i64_3digit_minpos(wikrt_cx* cx) {
    return test_i64(cx,  1000000000000000000); }
static inline bool test_alloc_i64_3digit_maxneg(wikrt_cx* cx) {
    return test_i64(cx, -1000000000000000000); }

/* grow a simple stack of numbers (count .. 1) for testing purposes. */
void numstack(wikrt_cx* cx, wikrt_val* stack, int32_t count) 
{
    for(int32_t ii = 1; ii <= count; ++ii) {
        wikrt_val num;
        wikrt_alloc_i32(cx, &num, ii);
        wikrt_alloc_prod(cx, stack, num, (*stack));
    }
}

/* destroy a stack and compute its sum. */
void sumstack(wikrt_cx* cx, wikrt_val* stack, int64_t* sum)
{
    wikrt_vtype type;
    while((WIKRT_OK == wikrt_peek_type(cx, &type, *stack)) &&
          (WIKRT_VTYPE_PRODUCT == type))
    {
        wikrt_val elem;  wikrt_split_prod(cx, (*stack), &elem, stack);
        int32_t elemVal; wikrt_peek_i32(cx, elem, &elemVal);
        wikrt_drop(cx, elem, false);
        *sum += elemVal;
    }
}


bool test_alloc_prod(wikrt_cx* cx) 
{
    int32_t const ct = 111111;
    wikrt_val stack = WIKRT_UNIT;
    numstack(cx, &stack, ct);

    int64_t const expected_sum = (ct * (int64_t)(ct + 1)) / 2;
    int64_t actual_sum = 0;
    sumstack(cx, &stack, &actual_sum);

    bool const ok = (expected_sum == actual_sum) && (WIKRT_UNIT == stack);
    return ok;
}

bool test_copy_prod(wikrt_cx* cx)
{
    int32_t const ct = 77777;
    int64_t const expected_sum = (ct * (int64_t)(ct + 1)) / 2;
    int64_t sumA = 0;
    int64_t sumB = 0;
    int64_t sumC = 0;
    wikrt_val a, b, c;
    a = WIKRT_UNIT_INR;
    numstack(cx, &a, ct);
    wikrt_copy(cx, &b, a, false);
    wikrt_copy(cx, &c, b, false);
    sumstack(cx, &a, &sumA);
    sumstack(cx, &b, &sumB);
    sumstack(cx, &c, &sumC);
    bool const ok = (sumA == sumB) && (sumB == sumC) &&
                    (sumC == expected_sum) &&
                    (WIKRT_UNIT_INR == a) &&
                    (WIKRT_UNIT_INR == b) &&
                    (WIKRT_UNIT_INR == c);
    return ok;
}

/** Create a deep sum from a string of type (L|R)*. */
static inline void deepsum_path(wikrt_cx* cx, char const* s, wikrt_val* v)
{

    size_t len = strlen(s);
    while(len > 0) {
        char const c = s[--len];
        wikrt_alloc_sum(cx, v, ('R' == c), *v);
    }
}

// destroys val
bool dismantle_deepsum_path(wikrt_cx* cx, char const* const sumstr, wikrt_val* v) 
{
    bool ok = true;
    char const* ss = sumstr;
    while(ok && *ss) {
        char const c = *(ss++);
        bool const expected_inR = ('R' == c);
        bool actual_inR;
        wikrt_err const st = wikrt_split_sum(cx, (*v), &actual_inR, v);
        ok = (WIKRT_OK == st) && (actual_inR == expected_inR);
    }
    return ok;
}

bool test_deepsum_str(wikrt_cx* cx, char const* const sumstr) 
{
    wikrt_val v = WIKRT_UNIT;
    deepsum_path(cx, sumstr, &v);
    bool const ok = dismantle_deepsum_path(cx, sumstr, &v) && (WIKRT_UNIT == v);
    return ok;
}

bool test_alloc_deepsum_L(wikrt_cx* cx)   { return test_deepsum_str(cx, "L");   }
bool test_alloc_deepsum_R(wikrt_cx* cx)   { return test_deepsum_str(cx, "R");   }
bool test_alloc_deepsum_LL(wikrt_cx* cx)  { return test_deepsum_str(cx, "LL");  }
bool test_alloc_deepsum_LR(wikrt_cx* cx)  { return test_deepsum_str(cx, "LR");  }
bool test_alloc_deepsum_RL(wikrt_cx* cx)  { return test_deepsum_str(cx, "RL");  }
bool test_alloc_deepsum_RR(wikrt_cx* cx)  { return test_deepsum_str(cx, "RR");  }
bool test_alloc_deepsum_LLL(wikrt_cx* cx) { return test_deepsum_str(cx, "LLL"); }
bool test_alloc_deepsum_LLR(wikrt_cx* cx) { return test_deepsum_str(cx, "LLR"); }
bool test_alloc_deepsum_LRL(wikrt_cx* cx) { return test_deepsum_str(cx, "LRL"); }
bool test_alloc_deepsum_LRR(wikrt_cx* cx) { return test_deepsum_str(cx, "LRR"); }
bool test_alloc_deepsum_RLL(wikrt_cx* cx) { return test_deepsum_str(cx, "RLL"); }
bool test_alloc_deepsum_RLR(wikrt_cx* cx) { return test_deepsum_str(cx, "RLR"); }
bool test_alloc_deepsum_RRL(wikrt_cx* cx) { return test_deepsum_str(cx, "RRL"); }
bool test_alloc_deepsum_RRR(wikrt_cx* cx) { return test_deepsum_str(cx, "RRR"); }

void deepsum_prng_string(char* buff, unsigned int seed, size_t const nChars) {
    for(size_t ii = 0; ii < nChars; ++ii) {
        buff[ii] = (rand_r(&seed) & (1<<9)) ? 'R' : 'L';
    }
    buff[nChars] = 0;
}

bool test_deepsum_prng(wikrt_cx* cx, unsigned int seed, size_t const nChars) 
{
    char buff[nChars + 1];
    deepsum_prng_string(buff, seed, nChars);
    return test_deepsum_str(cx, buff);
}

bool test_alloc_deepsum_large(wikrt_cx* cx) 
{
    int const count = 4000;
    int passed = 0;
    for(int ii = 0; ii < count; ++ii) {
        if(test_deepsum_prng(cx, ii, 70)) {
            ++passed;
        }
    }
    return (passed == count);
}

bool test_copy_deepsum(wikrt_cx* cx) 
{
    size_t const nChars = 8000;
    char buff[nChars + 1];
    deepsum_prng_string(buff, 0, nChars);
    
    wikrt_val v = WIKRT_UNIT;
    deepsum_path(cx, buff, &v);
    wikrt_val vcpy = WIKRT_UNIT;
    bool ok = (WIKRT_OK == wikrt_copy(cx, &vcpy, v, false))
            && dismantle_deepsum_path(cx, buff, &v)
            && dismantle_deepsum_path(cx, buff, &vcpy)
            && (WIKRT_UNIT == v) && (WIKRT_UNIT == vcpy);
    return ok;
}

bool test_pkistr_s(wikrt_cx* cx, int64_t n, char const* const nstr) 
{
    wikrt_val v;
    wikrt_alloc_i64(cx, &v, n);
    size_t len = 0;
    wikrt_peek_istr(cx, v, NULL, &len); // obtain string size
    bool const okSize = (len == strlen(nstr));

    char buff[len+1]; buff[len] = 0;
    wikrt_peek_istr(cx, v, buff, &len); // print integer into buffer
    bool const okBuff = (0 == strcmp(nstr, buff));

    wikrt_drop(cx, v, false);
    bool const ok = (okBuff && okSize);
    return ok;
}

bool test_pkistr_small(wikrt_cx* cx)
{
    int runct = 0;
    int passct = 0;
    #define TEST(N) { \
        ++runct;\
        if(test_pkistr_s(cx, N, #N)) { ++passct; } \
    }
    TEST(0);
    TEST(1);
    TEST(-1);
    TEST(-1073741824);
    TEST(-1073741823);
    TEST(1073741823);
    TEST(1073741824);
    TEST(-2147483649);
    TEST(-2147483648);
    TEST(-2147483647);
    TEST(2147483647);
    TEST(2147483648);
    TEST(2147483649);
    TEST(999999999999999999);
    TEST(1000000000000000000);
    TEST(9223372036854775807);
    TEST(-999999999999999999);
    TEST(-1000000000000000000);
    TEST(-9223372036854775807);
    
    #undef TEST

    return ((runct > 0) && (runct == passct));

}

bool test_copy_i64(wikrt_cx* cx, int64_t const test) {
    wikrt_val i1, i2;
    wikrt_alloc_i64(cx, &i1, test);
    wikrt_copy(cx, &i2, i1, false);
    int64_t n;
    wikrt_peek_i64(cx, i2, &n);
    wikrt_drop(cx, i1, false);
    wikrt_drop(cx, i2, false);
    bool const ok = (test == n);
    return ok;
}

bool test_copy_num(wikrt_cx* cx) 
{
    unsigned int r = 0;
    int testCt = 1000;
    bool ok = test_copy_i64(cx, INT64_MIN) 
           && test_copy_i64(cx, INT64_MAX)
           && test_copy_i64(cx, 0);
    while(testCt-- > 0) {
        int64_t testVal = ((int64_t)rand_r(&r) * RAND_MAX) + rand_r(&r);
        ok = test_copy_i64(cx, testVal) && ok;
    }
    return ok;
}

bool test_valid_token_str(char const* s, bool expected) {
    bool const ok = (expected == wikrt_valid_token(s));
    if(!ok) { fprintf(stderr, "token validation failed for: %s\n", s); }
    return ok; 
}

bool test_valid_token(wikrt_cx* cx)
{
    #define ACCEPT(S) test_valid_token_str(S, true)
    #define REJECT(S) test_valid_token_str(S, false)
    return ACCEPT("foo")
        && ACCEPT("hello world")
        && ACCEPT("<>")
        && ACCEPT(".:,;|")
        && ACCEPT("\"")
        && ACCEPT("@")
        && ACCEPT("'")
        && ACCEPT(u8"x→y→z.κλμνξοπρς") // utf-8 okay
        && REJECT("{foo}") // no curly braces
        && REJECT("foo\nbar") // no newlines
        && REJECT("") // too small
        && ACCEPT("123456789012345678901234567890123456789012345678901234567890123") // max len
        && REJECT("1234567890123456789012345678901234567890123456789012345678901234") // too large
        && ACCEPT(u8"←↑→↓←↑→↓←↑→↓←↑→↓←↑→↓←") // max len utf-8
        && REJECT(u8"←↑→↓←↑→↓←↑→↓←↑→↓←↑→↓←z"); // too large
    #undef ACCEPT
    #undef REJECT
}

static inline size_t strct(char const* const* ps) {
    size_t ct = 0;
    while(NULL != (*ps++)) { ++ct; }
    return ct;
}

bool test_sealers(wikrt_cx* cx) 
{
    char const* const lSeals[] = { ":", "abracadabra", ":m", u8"←↑→↓←↑→↓←↑→↓←↑→↓←↑→↓←"
                                 , ":cx", ":foobar", ":env", ":xyzzy" };
    size_t const nSeals = sizeof(lSeals) / sizeof(char const*);
    assert(nSeals >= 8);

    wikrt_val v = WIKRT_UNIT;
    for(size_t ii = 0; ii < nSeals; ++ii) {
        char const* s = lSeals[ii];
        wikrt_alloc_seal(cx, &v, s, strlen(s), v);
    }

    // validate copy and drop of sealed values
    for(size_t ii = 0; ii < 100; ++ii) {
        wikrt_val tmp = v;
        wikrt_copy(cx, &v, tmp, false);
        wikrt_drop(cx, tmp, false);
    }

    for(size_t ii = nSeals; ii > 0; --ii) {
        char const* s = lSeals[ii - 1];
        char buff[WIKRT_TOK_BUFFSZ];
        wikrt_split_seal(cx, v, buff, &v);
        if(0 != strcmp(s, buff)) {
            fprintf(stderr, "expected seal %s, got %s\n", s, buff);
            return false;
        }
    }
    
    return (WIKRT_UNIT == v);
}

void run_tests(wikrt_cx* cx, int* runct, int* passct) {
    char const* errFmt = "test #%d failed: %s\n";

    #define TCX(T)                          \
    {                                       \
        ++(*runct);                         \
        if(T(cx)) { ++(*passct); }          \
        else {                              \
            char const* name = #T ;         \
            fprintf(stderr, errFmt, *runct, name);    \
        }                                   \
    }
    
    TCX(test_tcx);
    TCX(test_alloc_false);
    TCX(test_alloc_true);
    TCX(test_split_false);
    TCX(test_split_true);
    TCX(test_vtype_unit);
    TCX(test_vtype_true);
    TCX(test_vtype_false);

    TCX(test_alloc_i32_min);
    TCX(test_alloc_i32_nearmin);
    TCX(test_alloc_i32_zero);
    TCX(test_alloc_i32_max);
    TCX(test_alloc_i32_smallint_min);
    TCX(test_alloc_i32_smallint_max);
    TCX(test_alloc_i32_largeint_minpos);
    TCX(test_alloc_i32_largeint_maxneg);
    TCX(test_alloc_i64_min);
    TCX(test_alloc_i64_nearmin);
    TCX(test_alloc_i64_zero);
    TCX(test_alloc_i64_max);
    TCX(test_alloc_i64_2digit_min);
    TCX(test_alloc_i64_2digit_max);
    TCX(test_alloc_i64_3digit_minpos);
    TCX(test_alloc_i64_3digit_maxneg);

    TCX(test_pkistr_small);
    TCX(test_copy_num);

    // TODO: test istr, isz.

    TCX(test_alloc_prod);
    TCX(test_copy_prod);

    TCX(test_alloc_deepsum_L);
    TCX(test_alloc_deepsum_R);
    TCX(test_alloc_deepsum_LL);
    TCX(test_alloc_deepsum_LR);
    TCX(test_alloc_deepsum_RL);
    TCX(test_alloc_deepsum_RR);
    TCX(test_alloc_deepsum_LLL);
    TCX(test_alloc_deepsum_LLR);
    TCX(test_alloc_deepsum_LRL);
    TCX(test_alloc_deepsum_LRR);
    TCX(test_alloc_deepsum_RLL);
    TCX(test_alloc_deepsum_RLR);
    TCX(test_alloc_deepsum_RRL);
    TCX(test_alloc_deepsum_RRR);
    TCX(test_alloc_deepsum_large);
    TCX(test_copy_deepsum);

    TCX(test_valid_token);
    TCX(test_sealers);


    // TODO test: texts, binaries.
    // TODO test: math.
    // TODO test: evaluations.
    // TODO test: stowage.
    // TODO test: transactions.

    #undef TCX
}

