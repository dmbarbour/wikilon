// runABC is intended mostly to simplify testing and benchmarking
// of the Wikilon C runtime. It receives an ABC string on STDIN, 
// performs basic evaluation, and generates a string on STDOUT.
//
// In the future, there may be some support for 'sessions', i.e.
// for manipulating a persistent value in the runtime. 
#include "wikilon-runtime.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define APP_VER 20160721

// To avoid redundancy, just writing the program description as a string.
static char const* runABC_helpMsg() { return u8""
 "USAGE: runABC [options]\n"
 "\n"
 "The primary input is a stream of ABC on STDIN, of (default) type `∀e.e→(v*e)`.\n"
 "The stream is evaluated, and the value is serialized as another stream of ABC\n"
 "on STDOUT.\n"
 "\n"
 "Options:\n"
 "  -mem megabytes  Memory quota for evaluation, default 32.\n"
 "  -quota effort   Heuristic CPU effort quota, default 100.\n"
 "  -trace          Print available trace messages to STDERR.\n"
 "  -prof           Print available profiling output to STDERR.\n"
 "  -rt directory   Necessary for stowage or persistence.\n"
 "  -rtdb gigabytes Maximum persistent database, default 32.\n"
 "  -?              Print this help message.\n"
 "\n"
 "Alternative output models may be supported in the future, e.g. for\n"
 "extracting a binary or text stream. Leveraging runtime persistence\n"
 "so we don't require ongoing computations is also viable.\n";
}

typedef struct { 
    int  mem;
    int  quota;
    bool trace;
    bool prof;
    char const* rt;
    int  rtdb;
    bool help;
    bool version;
    char const* badArg;
} runABC_args;
#define default_args (runABC_args){ .mem = 32, .quota = 1000, .rtdb = 32, 0 }

bool match(char const* a, char const* b) { return (0 == strcmp(a,b)); }

runABC_args parseArgs(char const* const* argv)
{
    runABC_args a = default_args;

    while(*argv) {
        char const* const arg = *(argv++);
        if(match(arg, "-?")) { a.help = true; }
        else if(match(arg, "-trace")) { a.trace = true; }
        else if(*argv && match(arg, "-mem")) { a.mem = atoi(*(argv++)); }
        else if(*argv && match(arg, "-quota")) { a.quota = atoi(*(argv++)); }
        else if(*argv && match(arg, "-rt")) { a.rt = *(argv++); }
        else if(*argv && match(arg, "-rtdb")) { a.rtdb = atoi(*(argv++)); }
        else { a.badArg = arg; }
    }

    return a;
}

void printHelp(FILE* out) { fprintf(out, "%s", runABC_helpMsg()); }

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

void intro_block(wikrt_cx* cx, char const* s) 
{
    wikrt_intro_text(cx, s, SIZE_MAX);
    wikrt_text_to_block(cx);
}

void print_trace_messages(wikrt_cx* cx) 
{
    do {
        char const* const msg = wikrt_trace_read(cx);
        if(NULL == msg) { break; }
        fprintf(stderr, "[{&trace}]%%\n%s\n", msg);
    } while(1);
}

void print_text(wikrt_cx* cx, FILE* out)
{
    size_t const buff_size = 60000;
    char buff[buff_size];
    do {
        size_t bytes_read = buff_size;
        wikrt_read_text(cx, buff, &bytes_read);
        if(0 == bytes_read) { break; }
        fwrite(buff, 1, bytes_read, out);
    } while(1);
    wikrt_drop(cx);
}

void print_value(wikrt_cx* cx, FILE* out)
{
    wikrt_quote(cx);
    wikrt_block_to_text(cx);
    print_text(cx, out);
    fputc('\n', out);
    fflush(out);
}

int main(int argc, char const* const argv[]) 
{
    runABC_args a = parseArgs(1+argv); // skip argv[0], program name
    if(a.help) { printHelp(stdout); return 0; }
    else if(a.badArg) { 
        fprintf(stderr, "unhandled argument: %s (see -? for help)\n", a.badArg);
        return -1;
    } 

    wikrt_env* env = wikrt_env_create(a.rt, 1024 * (uint32_t) a.rtdb);
    wikrt_cx* cx = wikrt_cx_create(env, (uint32_t) a.mem);
    if(NULL == cx) { 
        fprintf(stderr, "failed to create runtime environment\n");
        return -1; 
    }

    // enable a record of {&trace} messages (if requested)
    if(a.trace) { 
        size_t const trace_buff_size = 60 * 1000;
        wikrt_trace_enable(cx, trace_buff_size); 
    }

    // model our `∀e` by use of a randomly sealed linear value.
    char runABC_seal[WIKRT_TOK_BUFFSZ];
    unsigned int sealerId = ((unsigned int)rand()) % 1000000;
    sprintf(runABC_seal, ":runABC-%06u", sealerId);
    intro_block(cx, "");
    wikrt_block_aff(cx);
    wikrt_block_rel(cx);
    wikrt_wrap_seal(cx, runABC_seal);

    // I'd like to do some streaming computations. ABC is designed for
    // it, after all. But for now, just grab entire STDIN as one large
    // program and process it.
    char* prog = streamToString(stdin);
    intro_block(cx, prog);
    free(prog);

    if(wikrt_error(cx)) {
        char const* const e = (WIKRT_CXFULL == wikrt_error(cx)) ? "too large" : "bad parse";
        fprintf(stderr, "error loading ABC program: %s\n", e);
        return -1;
    }

    wikrt_apply(cx);
    int effort = 0;
    bool needs_more_work = true;
    while((effort++ < a.quota) && needs_more_work) {
        needs_more_work = wikrt_step_eval(cx);
        print_trace_messages(cx);
    }

    // ensure type of comptuation.
    wikrt_assocr(cx); wikrt_wswap(cx); // ((v*e)*1) → (e*(v*1))
    char envSeal[WIKRT_TOK_BUFFSZ];
    wikrt_unwrap_seal(cx, envSeal); 
    if(!match(runABC_seal, envSeal)) { 
        // we expected`:runABC-999999 (or similar)
        wikrt_set_error(cx, WIKRT_ETYPE); 
    }
    wikrt_wswap(cx); // (v * (e * 1))

    // print our text output. 
    print_value(cx, stdout);

    if(wikrt_error(cx)) {
        char const* const e = (WIKRT_CXFULL == wikrt_error(cx)) ? "out of memory" 
                            : needs_more_work ? "insufficient effort" 
                            : "runtime type error";
        fprintf(stderr, "error evaluating ABC program: %s\n", e);
        return -1;
    } 

    wikrt_cx_destroy(cx);
    wikrt_env_destroy(env);
    return 0;
}

