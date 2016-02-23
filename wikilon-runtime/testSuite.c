
#include <stdio.h>
#include "wikilon-runtime.h"

void run_tests(wikrt_cx* cx, int* runct, int* passct); 

int main(int argc, char const** argv) {
    // return values
    int const ok  = 0;
    int const err = (-1);

    wikrt_env* e;
    wikrt_err const env_created = wikrt_env_create(&e,"testdir/db", 200);
    if(WIKRT_OK != env_created) {
        fprintf(stderr, "env create: %s\n", wikrt_strerr(env_created));
        return err;
    }

    wikrt_cx* cx;
    wikrt_err const cx_created = wikrt_cx_create(e, &cx, 200);
    if(WIKRT_OK != cx_created) {
        fprintf(stderr, "cx create: %s\n", wikrt_strerr(cx_created));
        return err;
    }


    int tests_run = 0;
    int tests_passed = 0;
    run_tests(cx, &tests_run, &tests_passed);

    wikrt_cx_destroy(cx);
    wikrt_env_destroy(e);

    fprintf(stdout, "Passed %d of %d Tests\n", tests_passed, tests_run);
    return ((tests_run == tests_passed) ? ok : err);
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

bool test_alloc_i32(wikrt_cx* cx, int32_t const iTest) 
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
    return test_alloc_i32(cx, INT32_MAX); }
static inline bool test_alloc_i32_zero(wikrt_cx* cx) {
    return test_alloc_i32(cx, 0); }
static inline bool test_alloc_i32_min(wikrt_cx* cx) {
    return test_alloc_i32(cx, INT32_MIN); }

// uses knowledge of internal representation
static inline bool test_alloc_i32_smallint_min(wikrt_cx* cx) {
    return test_alloc_i32(cx, 0 - ((1<<30) - 1) ); }
static inline bool test_alloc_i32_smallint_max(wikrt_cx* cx) {
    return test_alloc_i32(cx, ((1 << 30) - 1) ); }
static inline bool test_alloc_i32_largeint_minpos(wikrt_cx* cx) {
    return test_alloc_i32(cx, (1 << 30)); }
static inline bool test_alloc_i32_largeint_maxneg(wikrt_cx* cx) {
    return test_alloc_i32(cx, 0 - (1 << 30)); }

bool test_alloc_i64(wikrt_cx* cx, int64_t const iTest) 
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
    return test_alloc_i64(cx, INT64_MAX); }
static inline bool test_alloc_i64_zero(wikrt_cx* cx) {
    return test_alloc_i64(cx, 0); }
static inline bool test_alloc_i64_min(wikrt_cx* cx) {
    return test_alloc_i64(cx, INT64_MIN); }

// using knowledge of internal representations
static inline bool test_alloc_i64_2digit_min(wikrt_cx* cx) {
    return test_alloc_i64(cx,  -999999999999999999); }
static inline bool test_alloc_i64_2digit_max(wikrt_cx* cx) {
    return test_alloc_i64(cx,   999999999999999999); }
static inline bool test_alloc_i64_3digit_minpos(wikrt_cx* cx) {
    return test_alloc_i64(cx,  1000000000000000000); }
static inline bool test_alloc_i64_3digit_maxneg(wikrt_cx* cx) {
    return test_alloc_i64(cx, -1000000000000000000); }


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
    TCX(test_alloc_i32_zero);
    TCX(test_alloc_i32_max);
    TCX(test_alloc_i32_smallint_min);
    TCX(test_alloc_i32_smallint_max);
    TCX(test_alloc_i32_largeint_minpos);
    TCX(test_alloc_i32_largeint_maxneg);
    TCX(test_alloc_i64_min);
    TCX(test_alloc_i64_zero);
    TCX(test_alloc_i64_max);
    TCX(test_alloc_i64_2digit_min);
    TCX(test_alloc_i64_2digit_max);
    TCX(test_alloc_i64_3digit_minpos);
    TCX(test_alloc_i64_3digit_maxneg);

    #undef TCX
}

