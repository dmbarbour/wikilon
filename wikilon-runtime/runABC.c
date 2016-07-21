
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "wikilon-runtime.h"

#define APP_VER 20160721

// To avoid redundancy, just writing the program description as a string.
static char const* runABC_helpMsg() { return u8""
 "USAGE: runABC [-m memoryMB] [-e effort] [-rt directory] [-?] [-v]\n"
 "\n"
 "By default, runABC will evaluate an ABC stream of type ∀e.e→(v*e),\n"
 "evaluate the value `v`, and generate a stream that represents the\n"
 "given value. E.g. `#7 #6 *` should result in output `#42`.\n"
 "\n"
 "Input is from STDIN and output printed to STDOUT. This is currently\n"
 "not incremental. The full input must be provided before any output.\n"
 "In general, the input should be provided via an upstream process\n"
 "such as linkABC.\n"
 "\n"
 "All command line arguments are optional. What we have:\n"
 "  -m memoryMB\n"
 "      Control memory available for evaluation, in megabytes.\n"
 "      The default is unspecified but relatively small.\n"
 "  -e effort\n"
 "      Heuristic effort for evaluation. An incomplete evaluation\n"
 "      is feasible and may still return a useful output.\n"
 "  -rt directory\n"
 "      Resources for large value stowage, persistent sessions.\n"
 "  -?  (this message)\n"
 "  -v  (version information)\n"
 "\n"
 "\n"
 "In the future, runABC may support a persistent sessions concept\n"
 "enabling clients to continue with a value already computed. Also\n"
 "alternative IO concepts might be enabled, e.g. to generate a binary\n"
 "from a stream, or interpret the computed value with monadic effects.\n"
 ;
}


typedef struct { 
    int  memoryMB;
    int  effort;
    char const* rtdir;
    bool help;
    bool version;
    char const* badArg;
} runABC_args;

#define default_memory 16
#define default_effort 1000

bool match(char const* a, char const* b) { return (0 == strcmp(a,b)); }

runABC_args parseArgs(char const* const* argv)
{
    runABC_args a = { 0 };
    while(*argv) {
        char const* const arg = *(argv++);
        if(match(arg, "-?")) { a.help = true; }
        else if(match(arg, "-v")) { a.version = true; }
        else if(match(arg, "-rt") && *argv && !a.rtdir) { a.rtdir = *(argv++); }
        else if(match(arg, "-m") && *argv) { a.memoryMB = atoi(*(argv++)); }
        else { a.badArg = arg; }
    }
    return a;
}

void printVersion() 
{
    fprintf(stdout, "runABC version: %d\n", (int) APP_VER);
    fprintf(stdout, "Wikilon runtime version: API %d, SO %d\n"
        , (int) WIKRT_API_VER, (int) wikrt_api_ver());
}

void printHelp(FILE* out) { fprintf(out, "%s", runABC_helpMsg()); }

int main(int argc, char const* const argv[]) 
{
    if(wikrt_api_ver() != WIKRT_API_VER) {
        fprintf(stderr, "WARNING: linked wikilon runtime and API header have different versions\n"); 
    }

    runABC_args a = parseArgs(1+argv); // skip argv[0], program name
    if(a.badArg) { 
        fprintf(stderr, "unhandled argument: %s\n", a.badArg);
        printHelp(stderr);
        return -1;
    }
    else if(a.help) { printHelp(stdout); return 0; }
    else if(a.version) { printVersion(); return 0; }
    else {
        
    }
    return 0;
}

