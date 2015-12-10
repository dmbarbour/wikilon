#include <stdio.h>
#include "wikrt.h"



// testing integration
void wikrt_hello(char const * s) {
    printf("Hello, %s!\n", (NULL == s) ? "World" : s);
}




