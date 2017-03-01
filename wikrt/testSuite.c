
#include <stdio.h>
#include <assert.h>
#include <time.h>
#include "wikrt.h"

#define MEGABYTES (1000 * 1000)

int tests_performed;
int tests_passed;

int main(int argc, char const* const* args) 
{
    if(WIKRT_API_VER != wikrt_api_ver()) {
        printf("tests compiled to wrong API\n");
        return -1;
    }

    wikrt_env* const e = wikrt_env_create();
    if(NULL == e) {
        printf("could not allocate wikrt_env\n");
        return -1;
    }
    wikrt_env_threadpool(e, 2);
    wikrt_db_open(e, "./testdir" , (32 * MEGABYTES));
    
    wikrt_cx* const cx = wikrt_cx_create(e, "t/e/s/t", (4 * MEGABYTES));
    assert(e == wikrt_cx_env(cx));

    struct timespec tm = { .tv_sec = 30, .tv_nsec = 0 };
    nanosleep(&tm, NULL);

    // todo: 
    //  parses
    //  write and read binaries
    //  simple evaluations

    wikrt_cx_destroy(cx);
    wikrt_env_destroy(e);
    
    printf("tests passed: %d of %d\n"
        , tests_passed, tests_performed ); 
    return (tests_performed - tests_passed);
}
