
#include <stdio.h>
#include <assert.h>
#include <time.h>
#include <string.h>
#include "wikrt.h"

#define MEGABYTES (1000 * 1000)
#define TESTDIR "./testdir"

int tests_run = 0;
int tests_pass = 0;

// runTCX is intended for single-context tests.
void runTCX(char const* testName, bool (*test)(wikrt_cx*), wikrt_cx* cx);
#define TCX(T) runTCX( #T , &(T) , cx )

bool test_hash(wikrt_cx*);
bool test_parse_check(wikrt_cx*);
bool test_write_read_id(wikrt_cx*);
bool test_parse(wikrt_cx*);

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
    wikrt_db_open(e, TESTDIR , (32 * MEGABYTES));
    
    wikrt_cx* const cx = wikrt_cx_create(e, NULL, (4 * MEGABYTES));
    assert(e == wikrt_cx_env(cx));

    TCX(test_hash);
    TCX(test_parse_check);
    TCX(test_write_read_id);
    TCX(test_parse);
    

    // todo: 
    //  trivial binary IO
    //  primitive evaluations
    //   primitive loops
    //  definitions and lazy linking
    //  write and read binaries
    //  incremental evaluations
    //  update of definition during evaluation

    wikrt_cx_destroy(cx);
    wikrt_env_destroy(e);

    // todo:
    //   persistence tests
    //   shared environments
    //   transaction conflict tests
    
    printf("tests passed: %d of %d\n", tests_pass, tests_run); 
    return (tests_pass - tests_run);
}

void runTCX(char const* testName, bool (*test)(wikrt_cx*), wikrt_cx* cx)
{
    wikrt_cx_reset(cx, NULL); // clear context, empty dictionary
    ++tests_run;
    if(test(cx)) { 
        ++tests_pass; 
    } else {
        fprintf(stderr, "test #%d failed: %s\n", tests_run, testName);
    }
}


bool match_hash(char const* s, char const* h_expected)
{
    char h[WIKRT_HASH_SIZE + 1];
    wikrt_hash(h, (uint8_t const*)s, strlen(s));
    h[WIKRT_HASH_SIZE] = 0;
    bool const ok = (0 == strcmp(h, h_expected));
    if(!ok) {    
        fprintf(stderr, "hash of `%s` is `%s` not `%s`\n", s, h, h_expected);
    }
    return ok;
}

// six point test for the BLAKE2b hash (45 bytes) and base64 encoding.
bool test_hash(wikrt_cx* _unused) 
{
    return match_hash("t/e/s/t",    "QgZDyYYGYV57hLkAEqUMVM6qESxuN6QpM1ekCHv9Yi59SirYaXcH0FdSNN9T")
        && match_hash("test",       
                      "J7URBffnfK_NVVcQNQ6D21k5A7J8Zhhwb2Ry3WLYfFc7Vy1TiE01Q4H7duKE")
        && match_hash("J7URBffnfK_NVVcQNQ6D21k5A7J8Zhhwb2Ry3WLYfFc7Vy1TiE01Q4H7duKE"
                     ,"Kve-Zbz23Zz28x0tTsmnuJv8dj0YGvwEVVWCbxLkAM7S6FLp6gCA0M2n_Nee")
        && match_hash("Kve-Zbz23Zz28x0tTsmnuJv8dj0YGvwEVVWCbxLkAM7S6FLp6gCA0M2n_Nee"
                     ,"MnrYTJyeGxLz5OSGwTwW7WAiC9alwYaOBFuu2_flmK1LGCCMqEDjkzPDL-Rl")
        && match_hash("",           "-p2eN9b-CeuBFlEPrbnGHMWeMy1GzEo2XnLtxzMYjwi-nAiUttuwYCP_MSUG")
        ;
}

static inline bool check_parse(char const* s, bool const e) {
    bool const r = wikrt_parse_check((uint8_t const*)s, strlen(s), NULL);
    if(e != r) {
        fprintf(stderr, "expected parse of `%s` to %s\n"
            , s, (e ? "pass" : "fail"));
    }
    return (e == r);
}
static inline bool accept_parse(char const* s) { return check_parse(s, true); }
static inline bool reject_parse(char const* s) { return check_parse(s, false); }

// lightweight parse checks
bool test_parse_check(wikrt_cx* _unused)
{
    return accept_parse("") // empty program is okay
        // accept SP and LF, reject tabs
        && accept_parse(" ") 
        && accept_parse("\n") 
        && reject_parse("\t")
        // Require non-empty program ends in whitespace 
        // to simplify streaming and concatenative composition
        && reject_parse("h e l l o") 
        && accept_parse("h e l l o ") 
        && accept_parse("h e l l o\n")
        && reject_parse("[  \n\nhello]") 
        && accept_parse("[  \n\nhello]\n")
        && reject_parse(" \"hello\"")
        && accept_parse(" \"hello\" ")
        // block balance
        && accept_parse("[] ")
        && accept_parse("[[][][[]]] ")
        && reject_parse("][ ")
        && reject_parse("[][ ")
        // inline texts
        && accept_parse(" \"hello, world!\" ")
        && reject_parse(" \"hello,\n world!\" ")
        && reject_parse(" \"hello,\" world!\" ")
        && reject_parse(" \"hello ")
        // multi-line texts
        && accept_parse(" \"\n\" ") // empty text
        && accept_parse(" \"\n\n\n\" ") // text with blank lines
        && accept_parse(" \"\n hello\n\" ") // single line of text
        && reject_parse(" \"\nhello\n\" ")  // missing SP indent
        && reject_parse(" \"\n hello\n ")   // missing text terminal
        && accept_parse("\"\n hello\n multi-line\n text\n\" ")
        && accept_parse("[\"\n hello\n multi-line\n text\n\"] ")
        && accept_parse("[\"\n hello\n multi-line text\n\"]")
        && accept_parse("\"\n hello\n multi-line text\n\"@d")
        && accept_parse("\"\n\n\n hello\n\n\n\" ")
        // hierarchical namespace qualifiers
        && accept_parse(" 42@baz ")
        && accept_parse(" 42@baz@qux ")
        && reject_parse(" 42@baz @qux ")
        && accept_parse("[ 42@baz@qux foo@dict ]@xy@zzy ")
        && accept_parse("  \"hello\"@x@y@z  ")
        && accept_parse("\"\n hello,\n multi-line\n world!\n\"@foo ")
        // annotations
        && accept_parse("(a2) ")
        && accept_parse("(foo)@d ")
        && reject_parse("() ")
        && reject_parse("( ) ")
        && reject_parse("([]) ")
        && reject_parse("(@d) ")
        && reject_parse("(foo@d) ")
        && accept_parse("[foo](a2)(par)i ")
        && accept_parse("[foo]@d(a2)(par)i ")
        && accept_parse(" (~z)@d(par)@y(foo)x ")
        && reject_parse("(a2 ")
        && reject_parse("a2) ")
        ;

}

bool test_rw(wikrt_cx* cx, char const* s)
{
    _Static_assert((sizeof(uint8_t) == sizeof(char)), 
        "cast between char* and uint8_t*");
 
    wikrt_r const r = 1;
    size_t const len = strlen(s);

    if(!wikrt_is_empty(cx, r)) { 
        fprintf(stderr, "%s: expecting empty stream\n", __FUNCTION__);
        return false;
    }

    // split the writes to better stress context write buffering
    size_t const split = len / 3;
    wikrt_write(cx, r, (uint8_t const*)s, split);
    wikrt_write(cx, r, (uint8_t const*)s + split, len - split);

    if(wikrt_is_empty(cx, r) && (0 != len)) { 
        fprintf(stderr, "%s: write failed for `%s`\n", __FUNCTION__, s);
        return false;
    }
    
    char buff[len+1]; buff[len] = 0;

    // split reads to better stress context read buffering
    size_t const rd1 = wikrt_read(cx, r, (uint8_t*)buff, (len-split));
    size_t const rd2 = wikrt_read(cx, r, (uint8_t*)(buff + (len - split)), (split + 1));
    if(len != (rd1 + rd2)) { 
        fprintf(stderr, "%s: read failed\n", __FUNCTION__);
        return false;
    }

    if(0 != strcmp(s, buff)) {
        fprintf(stderr, "%s: read and write not equal (`%s` != `%s`)\n"
            , __FUNCTION__, s, buff);
        return false;
    }

    wikrt_clear(cx, r);
    if(!wikrt_is_empty(cx, r)) {
        fprintf(stderr, "%s: clear failed\n", __FUNCTION__);
        return false;
    }

    return true;
}

bool test_write_read_id(wikrt_cx* cx)
{
    return test_rw(cx,"")
        && test_rw(cx,"h")
        && test_rw(cx,"hello")
        && test_rw(cx,"hello, world! this is a test!")
        && test_rw(cx,"    \n\n\n\n    \n\n\n\n    ")
        // test at least one large input
        && test_rw(cx,wikrt_prelude()) 
        ;
}


// basic tests
bool test_parse(wikrt_cx* _unused)
{
    return false;
}


