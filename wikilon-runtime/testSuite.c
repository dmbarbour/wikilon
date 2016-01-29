
#include <stdio.h>
#include "wikilon-runtime.h"

// todo: leverage a proper unit testing framework

//bool test_data_rep(wikrt_env*);


int main(int argc, char const** argv) {
    wikrt_env* e;
    wikrt_err const created = wikrt_env_create(&e,"testdir/db", 200);
    if(WIKRT_OK != created) {
        fprintf(stderr, "env creation: %s\n", wikrt_strerr(created));
        return (-1);
    }

#if 0
    if(!test_data_rep(e))
        return (-1);
#endif

    wikrt_env_destroy(e);
    return 0;
}


