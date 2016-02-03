
#include <stdio.h>
#include "wikilon-runtime.h"

int tests_run = 0;
int tests_passed = 0;

void run_tests(wikrt_cx* cx); 

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
    wikrt_err const cx_created = wikrt_cx_create(e, &cx, 100);
    if(WIKRT_OK != cx_created) {
        fprintf(stderr, "cx create: %s\n", wikrt_strerr(cx_created));
        return err;
    }

    run_tests(cx);

    wikrt_cx_destroy(cx);
    wikrt_env_destroy(e);

    return ((tests_run == tests_passed) ? ok : err);
}

void run_tests(wikrt_cx* cx) {
    
}

