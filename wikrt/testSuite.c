
#include <stdio.h>
#include <time.h>
#include "wikrt.h"

int tests_performed;
int tests_passed;

void microsleep(uint32_t time) 
{
    struct timespec tm;
    tm.tv_sec = (time / 1000000);
    tm.tv_nsec = ((time % 1000000) * 1000);
    nanosleep(&tm, NULL);
}

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
    wikrt_env_destroy(e);
    
    printf("tests passed: %d of %d\n"
        , tests_passed, tests_performed ); 
    return (tests_performed - tests_passed);
}
