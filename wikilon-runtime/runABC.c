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

#include <time.h>
#include <sys/types.h>
#include <unistd.h>

#define APP_VER 20160721
#define TRACE_LOG_SIZE (64 * 1024)

// To avoid redundancy, just writing the program description as a string.
static char const* runABC_helpMsg() { return u8""
 "USAGE: runABC [options]\n"
 "\n"
 "The primary input is a stream of ABC on STDIN, of (default) type `∀e.e→(v*e)`.\n"
 "The stream is evaluated, and the value is serialized as another stream of ABC\n"
 "on STDOUT. Errors and trace messages print to STDERR.\n"
 "\n"
 "Options:\n"
 "  -m megabytes    Memory quota for evaluation. (default 32)\n"
 "  -q(t) effort    Heuristic CPU effort quota, of type (t).\n"
 "      -qm         Effort is megabytes allocated. (default 1000)\n"
 "      -qb         Effort is blocks evaluated.\n"
 "      -qt         Effort is milliseconds elapsed.\n"
 "      -qp         Effort is processor time milliseconds.\n"
 "      -qg         Effort is garbage collector passes.\n"
 "  -rt directory   Necessary for stowage or persistence.\n"
 "  -rtdb gigabytes Maximum persistent database. (default 32)\n"
 "  -?              Print this help message.\n"
 "\n"
 "Alternative output models may be supported in the future, e.g. for\n"
 "extracting a binary or text stream. Leveraging runtime persistence\n"
 "so we don't require ongoing computations is also viable.\n";
}

typedef struct { 
    int  mem;
    wikrt_effort_model effort_model;
    int effort_value;
    bool prof;
    char const* rt;
    int  rtdb;
    bool help;
    bool version;
    char const* badArg;
} runABC_args;
#define default_args (runABC_args){ .mem = 32, .rtdb = 32,        \
    .effort_model = WIKRT_EFFORT_MEGABYTES, .effort_value = 1000, \
    0 }

bool match(char const* a, char const* b) { return (0 == strcmp(a,b)); }
bool is_quota_type(char c, wikrt_effort_model* m)
{ switch(c) {
    case 'm': (*m) = WIKRT_EFFORT_MEGABYTES; return true;
    case 'b': (*m) = WIKRT_EFFORT_BLOCKS;    return true;
    case 't': (*m) = WIKRT_EFFORT_MILLISECS; return true;
    case 'p': (*m) = WIKRT_EFFORT_CPU_TIME;  return true;
    case 'g': (*m) = WIKRT_EFFORT_GC_CYCLES; return true;
    default: return false;
}}
bool quota_model_arg(char const* arg, wikrt_effort_model* m) {
    return ('-' == arg[0])
        && ('q' == arg[1])
        && is_quota_type(arg[2],m)
        && (0 == arg[3]);
}

runABC_args parseArgs(char const* const* argv)
{
    runABC_args a = default_args;
    wikrt_effort_model m;
    while(*argv) {
        char const* const arg = *(argv++);
        if(match(arg, "-?")) { a.help = true; }
        else if(*argv && match(arg, "-m")) { a.mem = atoi(*(argv++)); }
        else if(*argv && quota_model_arg(arg, &m)) {
            a.effort_value = atoi(*(argv++));
            a.effort_model = m;
        } else if(*argv && match(arg, "-rtdb")) { a.rtdb = atoi(*(argv++)); }
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
        fprintf(stderr, "[{&trace}]%% %s\n", msg);
    } while(1);
    fflush(stderr);
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

void print_mem_stats(wikrt_cx* cx)
{
    wikrt_mem_stats m;
    wikrt_peek_mem_stats(cx, &m);
    fprintf(stderr, "GC  cycle count:      %llu\n"
                    "    bytes processed:  %llu\n"
                    "    bytes collected:  %llu\n"
                    "Memory in use:        %llu\n"
        , (unsigned long long) m.gc_cycle_count
        , (unsigned long long) m.gc_bytes_processed
        , (unsigned long long) m.gc_bytes_collected
        , (unsigned long long) m.memory_current
        );
}

int main(int argc, char const* const argv[]) 
{
    unsigned int seed = (17 * (unsigned int)time(NULL))
                      + (7919 * (unsigned int)getpid());

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

    // Configure context.
    wikrt_trace_enable(cx, TRACE_LOG_SIZE); 
    wikrt_set_step_effort(cx, a.effort_model, (uint32_t) a.effort_value);

    // model `∀e` by use of a randomly sealed linear value.
    char runABC_seal[WIKRT_TOK_BUFFSZ];
    unsigned int sealerId = ((unsigned int)rand_r(&seed)) % 1000000;
    sprintf(runABC_seal, ":%06u", sealerId);
    intro_block(cx, ""); 
    wikrt_block_aff(cx); // cannot copy e
    wikrt_block_rel(cx); // cannot drop e
    wikrt_wrap_seal(cx, runABC_seal);

    // When done constructing the value, destroy the `∀e` environment.
    // I.e. compose the `∀e.e→(v*e)` with a `∀v.(v*e)→v` for the
    // specific initial environment we provide. 
    char elim_e[WIKRT_TOK_BUFFSZ + 8];
    sprintf(elim_e, "vrw{.%s}$c", (1 + runABC_seal));
    intro_block(cx, elim_e);

    // grab entire program from STDIN.
    char* prog = streamToString(stdin);
    intro_block(cx, prog);
    free(prog);

    // at this point we have
    //    ([∀e.e→(v*e)] * ([∀v.(v*e)→v] * (e * 1))).
    wikrt_compose(cx); // ([e→v]*(e*1))
    wikrt_apply(cx);   // ((pending v) * 1)

    if(wikrt_error(cx)) {
        wikrt_ecode const e = wikrt_error(cx);
        char const* const emsg = (WIKRT_CXFULL == e) ? "input too large" 
                                                     : "bad parse";
        fprintf(stderr, "error loading ABC program: %s\n", emsg);
        return -1;
    }

    // Perform our evaluation. 
    wikrt_step_eval(cx);  // (future result) → ((future result) + result)
    print_trace_messages(cx);
    wikrt_sum_tag lr; 
    wikrt_unwrap_sum(cx, &lr);
    wikrt_quote(cx);
    bool const incomplete = !wikrt_error(cx) && (WIKRT_INL == lr);

    // Either we have (future a) or `a` on stack.
    if(incomplete) {
        // addend `{&join}` to unwrap incomplete future
        intro_block(cx, "{&join}");
        wikrt_wswap(cx);
        wikrt_compose(cx);
    }
    wikrt_block_to_text(cx);

    char const* const attrib_origin = "{&runABC}";
    char const* const attrib_incomplete = incomplete ? "{&incomplete}" : "";
    char const* const attrib_error = wikrt_error(cx) ? "{&error}" : "";
    fprintf(stdout, "[%s%s%s]%% ", attrib_origin, attrib_incomplete, attrib_error);
    print_text(cx, stdout);
    fflush(stdout);
    fputc('\n', stderr); // in case of console output

    //print_mem_stats(cx);

    if(wikrt_error(cx)) {
        wikrt_ecode const e = wikrt_error(cx);
        char const* const emsg = 
            (WIKRT_CXFULL == e) ? "out of memory" :
            (WIKRT_IMPL == e)   ? "impl error" :
                                  "type error"; 
        fprintf(stderr, "evaluation failure: %s\n", emsg);
        return -1;
    }

    wikrt_cx_destroy(cx);
    wikrt_env_destroy(env);
    return 0;
}

