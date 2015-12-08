#include <stdio.h>
#include "wikilon-runtime.h"

// testing integration
void wikrt_hello(char const * s) {
    printf("Hello, %s!", (NULL == s) ? "World" : s);
}

