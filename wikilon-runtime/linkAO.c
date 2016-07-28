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
 "USAGE: linkAO yourFile.ao [-w word | -d definition]\n"
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
 "word's definition.\n"
 "\n"
 "Minimal validation is performed: words have valid size, cycles are detected,\n"
 "and balance of brackets is tested within each word. linkAO will exit with a\n"
 "non-zero result upon error.\n"
 ;
}

// TODO: 
//  * Potentially support a persistent index for large AO files. 
//  * Support append-only edits, maybe via separate editAO tool.

typedef struct 
{
    bool help;           
    char const* aoFile;  // reference into filesystem
    char const* word;    // word to use
    char const* def;     // definition to use
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
        if(match(arg, "-?")) { a.help = true; }
        else if(!a.word && *argv && match(arg, "-w")) { a.word = *(argv++); }
        else if(!a.def && *argv && match(arg, "-d")) { a.def = *(argv++); }
        else if(!a.aoFile && matchSuffix(arg, ".ao", 3)) { a.aoFile = arg; }
        else { a.badArg = arg; }
    }
    return a;
} 

void printHelp(FILE* out) { fprintf(out, "%s", linkAO_helpMsg()); }

char* streamToString(FILE* source) 
{
    // load all of STDIN
    size_t max_buffsz = 1000;
    size_t bytes_read = 0;
    char* buff = malloc(max_buffsz);
    if(!buff) { abort(); }
    do {
        bytes_read += fread((buff + bytes_read), 1, (max_buffsz - (1 + bytes_read)), source);
        buff[bytes_read] = 0;
        bool const done = feof(source) || ferror(source);
        if(done) { return buff; }
        assert(bytes_read == (max_buffsz - 1));
        max_buffsz *= 2; 
        buff = realloc(buff, max_buffsz);
        if(!buff) { abort(); }
    } while(1);
}

/* Obtain the root program as a NUL-terminated C string, 
 * representing an AO definition. I might eventually want
 * stream processing of STDIN, but it isn't a critical
 * feature at this time.
 */
char* linkAO_program(linkAO_args const* args) 
{
    if(args->word) {
        size_t const len = 3 + strlen(args->word); // {%word}
        char* const buff = malloc(1 + len);           // +1 for NUL
        if(NULL == buff) { abort(); }
        sprintf(buff, "{%%%s}", args->word);
        return buff;
    } else if(args->def) {
        char* result = strdup(args->def); 
        if(NULL == result) { abort(); }
        return result;
    } else {
        return streamToString(stdin);
    }
}

/* Linking the AO program is trivial. Essentially, I just need to
 * find every `{%word}` token and replace it by the definition at
 * `word`. However, this simple idea is complicated a little: 
 *
 *  - must detect cycles (eventually)
 *  - must parse for tokens (so we don't link within text)
 *  - validate balance of `[]` (because cannot validate later) 
 */
typedef struct {
    char const* s; // current location in source
    char const* e; // end of current source segment
    size_t      b; // count of `[` not yet balanced
} prog;

// a contiguous stack that might be resized
typedef struct {
    size_t space;  // available size for stack
    size_t depth;  // current size of stack 
    prog*  data;   // start of stack
} pstack;

/* Detect a cycle on the call stack. This can only detect a cycle if we're
 * already in one, i.e. when the top of the stack is part of the cycle.
 * But it's a trivial test, and may be performed at any time we suspect a
 * cycle (e.g. before resizing the stack).
 */
size_t detect_cycle(pstack const* stack)
{
    size_t ix = stack->depth;
    if(0 == ix) { return 0; }
    char const* const ce = stack->data[--ix].e;
    while(0 != ix) 
    {
        char const* const se = stack->data[--ix].e;
        if(se == ce) { return (1 + ix); }
    }
    return 0;
}

// Assume we're pointing to just after a `{%word}` token.
// Print that word. This is meant for debugging output.
void print_prior_word(FILE* out, char const* s) 
{
    --s; // back up in stream
    assert('}' == (*s)); // should be at end of token
    do { --s; } while ((*s) != '{'); // scan to start of token
    ++s; // drop start of token
    assert('%' == (*s)); // should be a {%word} token
    while('}' != (*s)) { fputc(*(s++), out); } // print the word
}

void print_cycle(FILE* out, pstack const* stack, size_t ix)
{
    print_prior_word(out, stack->data[ix++].e);
    while(ix < stack->depth) {
        fputc(' ', out); // separator
        print_prior_word(out, stack->data[ix++].e);
    }
}

void assert_no_cycles(pstack* stack) 
{
    size_t const cycle_start = detect_cycle(stack);
    if(0 != cycle_start) {
        size_t const cycle_size = stack->depth - cycle_start;
        fprintf(stderr, "Cycle of %d word(s): ", (int) cycle_size);
        print_cycle(stderr, stack, cycle_start);
        fputc('\n', stderr);
        exit(-1);
    }
}

void push_stack(pstack* stack, prog p)
{
    if(stack->depth == stack->space) {
        assert_no_cycles(stack);
        stack->space = (2 * (8 + stack->space));
        stack->data = realloc(stack->data, (sizeof(prog) * stack->space));
        if(NULL == stack->data) { abort(); }
    }
    stack->data[(stack->depth)++] = p;
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
    } else if(a.word && a.def) {
        fprintf(stderr, "link a word or link a definition, but not both!\n");
        return -1;
    } else if(0 != access(a.aoFile, R_OK)) {
        fprintf(stderr, "File `%s` cannot be read. %s.\n", a.aoFile, strerror(errno));
        return -1;
    } 

    AOFile* const ao = AOFile_load(a.aoFile);
    if(NULL == ao) {
        char const* const error = (0 != errno) ? strerror(errno) : "Unspecified error";
        fprintf(stderr, "Could not load AO source `%s`. %s.\n", a.aoFile, error);
        return -1;
    } else {
        fprintf(stderr, "File `%s` loaded. %d definitions.\n"
            , a.aoFile, (int) AOFile_size(ao));
    }

    char* const prog = linkAO_program(&a);
    // TODO: process the program!
    fprintf(stderr, "TODO: process program `%s`\n", prog);
    free(prog);
    AOFile_unload(ao);
    return 0;
}

