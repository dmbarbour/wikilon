#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include <errno.h>
#include "AO/aofile.h"

static char const* linkAO_helpMsg() { return u8""
 "USAGE: linkAO yourFile.ao [-w word | -d definition] \n"
 "  unless provided as argument, read definition from STDIN\n"
 "  output is Awelon Bytecode (ABC) stream printed to STDOUT\n"
 "\n"
 "An AO file represents a program dictionary, a set of `@word definition`\n"
 "pairs with each word on a new line. Definitions are Awelon Bytecode with\n"
 "{%word} tokens representing acyclic graph dependencies. All dependencies\n"
 "must be defined within the same dictionary, the same AO file. AO files\n"
 "are intended primarily for import/export, not hand editing, but they can\n"
 "work in a pinch.\n"
 "\n"
 "linkAO takes an AO file together with a word or definition, and produces\n"
 "an ABC stream. Linking is trivial: every {%word} token is replaced by that\n"
 "word's definition. Minimal validation is performed.\n"
 ;
}

// TODO: If this program sees much use outside of testing, it might be worthwhile
// to support use of persistent index files.

typedef struct 
{
    bool help;           
    char const* aoFile;  // reference into filesystem
    char const* program; // use stdin iff not set in command line
    bool programIsWord;  // single word program (foo, not {%foo}).
    char const* badArg;
} linkAO_args;

bool match(char const* arg, char const* action) { return (0 == strcmp(arg, action)); }
bool matchSuffix(char const* arg, char const* suf, size_t sl) {
    size_t const al = strlen(arg);
    if(al < sl) { return false; }
    return (0 == memcmp((arg + (al - sl)), suf, sl));
}

linkAO_args parseArgs(char const* const* argv) 
{
    linkAO_args a = { 0 };
    while(*argv) {
        char const* const arg = *(argv++);
        if(match(arg, "-?")) { 
            a.help = true; 
        } else if(!a.program && *argv && match(arg, "-w")) {
            a.program = *(argv++); 
            a.programIsWord = true; 
        } else if(!a.program && *argv && match(arg, "-d")) {
            a.program = *(argv++);
        } else if(!a.aoFile && matchSuffix(arg, ".ao", 3)) {
            a.aoFile = arg;
        } else { a.badArg = arg; }
    }
    return a;
} 

void printHelp(FILE* out) { fprintf(out, "%s", linkAO_helpMsg()); }

/* Obtain the root program as a NUL-terminated C string, 
 * representing an AO definition. I might eventually want
 * stream processing of STDIN, but it isn't a critical
 * feature at this time.
 */
char* linkAO_program(linkAO_args const* args) 
{
    if(NULL == args->program) {

        // load all of STDIN
        size_t max_buffsz = 10000;
        size_t bytes_read = 0;
        char* buff = malloc(max_buffsz);
        if(!buff) { abort(); }
        do {
            FILE* const source = stdin;
            bytes_read += fread((buff + bytes_read), 1, (max_buffsz - (1 + bytes_read)), source);
            buff[bytes_read] = 0;
            bool const done = feof(source) || ferror(source);
            if(done) { return buff; }
            assert(bytes_read == (max_buffsz - 1));
            max_buffsz *= 2; 
            buff = realloc(buff, max_buffsz);
            if(!buff) { abort(); }
        } while(1);

    } else if(args->programIsWord) {

        size_t const len = 3 + strlen(args->program); // {%word}
        char* const buff = malloc(1 + len);           // +1 for NUL
        if(NULL == buff) { abort(); }
        sprintf(buff, "{%%%s}", args->program);
        return buff;

    } else { 

        char* result = strdup(args->program); 
        if(NULL == result) { abort(); }
        return result;

    }
}

int main(int argc, char const* const argv[])
{
    linkAO_args a = parseArgs(1 + argv);

    errno = 0;
    if(a.help) { 
        printHelp(stdout); 
        return 0; 
    } else if(a.badArg) { 
        fprintf(stderr, "unhandled argument (-? for help): %s\n", a.badArg); 
        return -1;
    } else if(!a.aoFile) {
        fprintf(stderr, "please specify the `.ao` source file (-? for help)\n");
        return -1;
    } else if(0 != access(a.aoFile, R_OK)) {
        fprintf(stderr, "File `%s` cannot be read. %s.\n", a.aoFile, strerror(errno));
        return -1;
    }

    AOFile* const aofile = AOFile_load(a.aoFile);
    if(NULL == aofile) {
        char const* const error = (0 != errno) ? strerror(errno) : "Unspecified error";
        fprintf(stderr, "Could not load AO source `%s`. %s.\n", a.aoFile, error);
        return -1;
    }

    char* const prog = linkAO_program(&a);
    // TODO: process the program!
    fprintf(stderr, "TODO: process program `%s`\n", prog);
    free(prog);
    AOFile_unload(aofile);
    return 0;
}

