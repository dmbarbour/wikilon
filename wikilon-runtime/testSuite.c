
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

bool test_alloc_sum_right_unit(wikrt_cx* cx) 
{
    wikrt_val uR;
    return (WIKRT_OK == wikrt_alloc_sum(cx, &uR, true, WIKRT_UNIT)) &&
        (WIKRT_UNIT_INR == uR);
}

bool test_alloc_sum_left_unit(wikrt_cx* cx) 
{
    wikrt_val uL;
    return (WIKRT_OK == wikrt_alloc_sum(cx, &uL, false, WIKRT_UNIT)) &&
        (WIKRT_UNIT_INL == uL);
}

bool test_split_sum_right_unit(wikrt_cx* cx)
{
    bool b; wikrt_val u;
    return (WIKRT_OK == wikrt_split_sum(cx, WIKRT_UNIT_INR, &b, &u)) &&
        (b) && (WIKRT_UNIT == u);
}

bool test_split_sum_left_unit(wikrt_cx* cx) 
{
    bool b; wikrt_val u;
    return (WIKRT_OK == wikrt_split_sum(cx, WIKRT_UNIT_INL, &b, &u)) &&
        (!b) && (WIKRT_UNIT == u);
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
    TCX(test_alloc_sum_left_unit);
    TCX(test_alloc_sum_right_unit);
    TCX(test_split_sum_left_unit);
    TCX(test_split_sum_right_unit);
    #undef TCX
}

