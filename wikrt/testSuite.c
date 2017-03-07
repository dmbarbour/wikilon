
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
void runTCX(char const* testName, wikrt_cx* cx, bool (*test)(wikrt_cx*));
#define TCX(T,CX) runTCX( #T , cx, &(T) )

bool test_hash(wikrt_cx*);
bool test_parse_check(wikrt_cx*);

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

    TCX(test_hash, cx);
    TCX(test_parse_check, cx);

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

void runTCX(char const* testName, wikrt_cx* cx, bool (*test)(wikrt_cx*))
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
        && match_hash("t/e/s/t\n",  "dbNAGEG-F9ElYjNw4T4qI1A7o9clXiDRSs1hAEYJdu3BWAd5W3NDUHmki60s")
        && match_hash("test",       "J7URBffnfK_NVVcQNQ6D21k5A7J8Zhhwb2Ry3WLYfFc7Vy1TiE01Q4H7duKE")
        && match_hash("test\n",     "FNn9bEhfDqPpswCc36-GcJn42xZ5Bc-qaaylGefS2ystQ0ksVU9bpDqypG46")
        && match_hash("",           "-p2eN9b-CeuBFlEPrbnGHMWeMy1GzEo2XnLtxzMYjwi-nAiUttuwYCP_MSUG")
        && match_hash("\n",         "-vnxnuU93oPpZ1al_J_Gj9GkrLyLM0l7vAmVyIHcA5yH7UL77ukXKcq8fpG8");
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
    return accept_parse("")
        && accept_parse("hello")
        && accept_parse("  hello  ")
        && accept_parse("[12345   ~\n\n\n :]\n")
        && accept_parse("[foo](a2)(par)i")
        && accept_parse("[ 42@baz@qux foo@dict ]@xy@zzy")
        && accept_parse("[\"hello\"]")
        && accept_parse("\"hello\"@d")
        && accept_parse("[\"\n hello\n multi-line text\n\"]")
        && accept_parse("\"\n hello\n multi-line text\n\"@d")
        && accept_parse("\"\n\n\n hello\n\n\n\"")
        && reject_parse("[0 ~")
        && reject_parse("~  ]  ")
        && reject_parse("(a2")
        && reject_parse("a2)")
        && reject_parse("()")
        && reject_parse("\"hello ")
        && reject_parse("\"\n hello ")
        && reject_parse("\"\nhello\n\"")
        && reject_parse("hello\"")
        && reject_parse("\t")
        && reject_parse("foo @dict")
        && reject_parse("0 1 2 3 [ 4 5 6");
}


// basic tests
bool test_parse(wikrt_cx* _unused)
{
    return false;
}


