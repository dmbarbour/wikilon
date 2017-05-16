
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "wikilon-runtime.h"
#include "utf8.h"
#include "testAO.h"

#define TESTCX_SIZE 8
#define TESTENV_SIZE (4 * TESTCX_SIZE)

#define B32 "bdfghjkmnpqstxyzBDFGHJKMNPQSTXYZ"


char const* const valid_abc_strings[] =
 { "", " ", "\n", "\n\n\n\n\n", "      "
 , "vrlwcz", "VRWLCZ", "%^", " \n", "$", "mkf'", "#9876543210-", "+*-Q", "G", "DFMK"
 , "[  ]", "[vc]", "[v[vc]lc]", "[v[v[vc]lc]lc]", "[v[v[v[vc]lc]lc]lc]"
 , "[]", "[[]]", "[[[]]]", "[[[[]]]]"
 , "[ ]", "[ [ ] ]", "[ [ [ ] ] ]", "[ [ [ [ ] ] ] ]"
 , " [] [[]] [[] [[]]] [[] [[]] [[] [[]]]] [[] [[]] [[] [[]]] [[] [[]] [[] [[]]]]] \n"
   "[[] [[]] [[] [[]]] [[] [[]] [[] [[]]]] [[] [[]] [[] [[]]] [[] [[]] [[] [[]]]]]]  " 
 , "$c", "vr$c"
 , "wrzl", "rwrzwll", "{%p}{:ratio}", "vvrwlcl"
 , "[^'m]m^'m", "'[^'mw^'zmwvr$c]^'mwm", "rwrzvrwr$wlcl"
 , "{x}", "{x}{y}{xyzzy}", "{%word}", "{:seal}", "{token with space}", "{$@#:._-/\\!}", "{←↖↑↗→↘↓↙←}"
 , "{" B32 B32 B32 B32 B32 B32 B32 "bdfghjkmnpqstxyzBDFGHJKMNPQSTXY}" // new size limit (255 bytes)
 , "[{hello world}]kf"
 , "{empty string} \"\n~"
 , "{space string} \" \n~"
 , "\"hello, world!\n"
    " this text has two lines\n"
    "~"
 , "[\" \n~]", "[\"\n~]", "[\"hello, world!\n~]"
   // ensure recognized annotations will round-trip.
 , "{&trace}{&trash}{&stow}{&load}"
 , "{&lazy}{&fork}{&join}{&asynch}"
 , "{&text}{&binary}"
 , NULL
 };


char const* const invalid_abc_strings[] = 
 { "a", "e", "i", "o", "u"
 , "\a", "\t", "\r"
 , "[", "]", "[c[v]", "[v]c]"
 , "{", "}", "{}", "{\n}", "{x{y}"
 , "{" B32 B32 B32 B32 B32 B32 B32 B32 "}" // oversized (256 bytes)
 , "\"", "~", "\"\n.\n~"
 , "\"\a\n~", "\"\t\n~", "\"\r\n~"
 , NULL // terminate list 
 };   



void run_tests(wikrt_cx* cx, int* runct, int* passct); 
int fillcount(wikrt_cx* cx); // exercise memory management code

int main(int argc, char const** argv) {
    assert(WIKRT_API_VER == wikrt_api_ver());

    // return values
    int const ok  = 0;
    int const err = (-1);

    wikrt_env* const e = wikrt_env_create("testdir/db", TESTENV_SIZE);
    wikrt_cx* const cx = wikrt_cx_create(e, TESTCX_SIZE);
    if(NULL == cx) { 
        fprintf(stderr, "failed to create wikilon runtime environment or context\n");
        return err;
    }

    int tests_run = 0;
    int tests_passed = 0;
    run_tests(cx, &tests_run, &tests_passed);
    fprintf(stdout, u8"Passed %d of %d Tests\n", tests_passed, tests_run);

    wikrt_cx_destroy(cx);
    wikrt_env_destroy(e);

    return ((tests_run == tests_passed) ? ok : err);
}


void read_sum(wikrt_cx* cx, wikrt_sum_tag e) 
{
    wikrt_sum_tag lr;
    wikrt_unwrap_sum(cx, &lr);
    if(lr != e) { wikrt_set_error(cx, WIKRT_ETYPE); }
}
void elim_list_end(wikrt_cx* cx) 
{
    read_sum(cx, WIKRT_INR);
    wikrt_elim_unit(cx);
}


void val2txt(wikrt_cx* cx) { wikrt_quote(cx); wikrt_block_to_text(cx); }
void print_text(wikrt_cx* cx) {
    if(wikrt_error(cx)) { fprintf(stderr, "{error}"); return; }

    size_t const max_len = 800;
    char buff[max_len];
    do {
        size_t len = max_len;
        wikrt_read_text(cx, buff, &len);
        if(0 == len) { return; }
        fwrite(buff, len, 1, stderr);
    } while(true);
    elim_list_end(cx);
    assert(!wikrt_error(cx));
}
void print_val(wikrt_cx* cx) { val2txt(cx); print_text(cx); }


void test_unit(wikrt_cx* cx) 
{
    wikrt_intro_unit(cx);
    wikrt_elim_unit(cx);
}

void test_bool(wikrt_cx* cx, bool const bTest) 
{
    wikrt_sum_tag const t = bTest ? WIKRT_INR : WIKRT_INL;
    wikrt_sum_tag b; 

    wikrt_intro_unit(cx);
    wikrt_wrap_sum(cx, t);
    wikrt_unwrap_sum(cx, &b);
    wikrt_elim_unit(cx);

    if(t != b) { wikrt_set_error(cx, WIKRT_ETYPE); }
}

void test_true(wikrt_cx* cx) { test_bool(cx, true); }
void test_false(wikrt_cx* cx) { test_bool(cx, false); }

void test_i32(wikrt_cx* cx, int32_t const iTest) 
{
    int32_t i;
    wikrt_intro_i32(cx, iTest);
    wikrt_peek_i32(cx, &i);
    wikrt_drop(cx);
    if(i != iTest) { wikrt_set_error(cx, WIKRT_ETYPE); }
}

void test_i32_zero(wikrt_cx* cx) { test_i32(cx, 0); }

void test_i32_misc(wikrt_cx* cx) 
{
    test_i32(cx,1);
    test_i32(cx,42);
    test_i32(cx, 12345678);
    test_i32(cx,-12345678);
    test_i32(cx,-42);
    test_i32(cx,-1);
}

/* grow a simple stack of numbers (count .. 1) for testing purposes. */
void numstack(wikrt_cx* cx, int32_t count) 
{
    wikrt_intro_unit(cx);
    for(int32_t ii = 1; ii <= count; ++ii) {
        wikrt_intro_i32(cx, ii);
        wikrt_assocl(cx);
    }
}

/* destroy a stack of count elements and compute its sum. */
int64_t sumstack(wikrt_cx* cx, int32_t count)
{   
    int64_t sum = 0;
    for(int32_t ii = 0; ii < count; ++ii) {
        int32_t elem = INT32_MIN;
        wikrt_assocr(cx);
        wikrt_peek_i32(cx, &elem);
        wikrt_drop(cx);
        sum += elem;
    }
    wikrt_elim_unit(cx);
    return sum;
}

void test_alloc_prod(wikrt_cx* cx) 
{
    int32_t const ct = 111111;
    int64_t const expected_sum = (ct * (int64_t)(ct + 1)) / 2;

    numstack(cx, ct);
    int64_t actual_sum = sumstack(cx, ct);

    if(expected_sum != actual_sum) { wikrt_set_error(cx, WIKRT_ETYPE); }
}

void test_copy_prod(wikrt_cx* cx)
{
    int32_t const ct = 77777;
    int64_t const expected_sum = (ct * (int64_t)(ct + 1)) / 2;

    numstack(cx, ct);
    wikrt_copy(cx);
    wikrt_copy(cx);

    int64_t const sumA = sumstack(cx,ct);
    int64_t const sumB = sumstack(cx,ct);
    int64_t const sumC = sumstack(cx,ct);

    bool const ok = (sumA == sumB) 
                 && (sumB == sumC) 
                 && (sumC == expected_sum);
    if(!ok) { wikrt_set_error(cx, WIKRT_ETYPE); }
}

/** Create a deep sum from a string of type (L|R)*. */
void deepsum_path(wikrt_cx* cx, char const* s)
{
    wikrt_intro_unit(cx);
    size_t len = strlen(s);
    while(len > 0) {
        char const c = s[--len];
        wikrt_sum_tag const lr = ('R' == c) ? WIKRT_INR : WIKRT_INL;
        wikrt_wrap_sum(cx, lr);
    }
}

// destroys val
bool dismantle_deepsum_path(wikrt_cx* cx, char const* const sumstr) 
{
    bool ok = true;
    char const* ss = sumstr;
    while(ok && *ss) {
        char const c = *(ss++);
        wikrt_sum_tag lr;
        wikrt_unwrap_sum(cx, &lr);
        bool const tagMatched = 
            ((WIKRT_INL == lr) && ('L' == c)) ||
            ((WIKRT_INR == lr) && ('R' == c));
        ok = tagMatched;
    }
    if(!ok) {
        fprintf(stderr, "sum mismatch - %s at char %d\n", sumstr, (int)(ss - sumstr));
    }
    wikrt_elim_unit(cx);
    return ok;
}

bool test_deepsum_str(wikrt_cx* cx, char const* const sumstr) 
{
    deepsum_path(cx, sumstr);
    bool const ok = dismantle_deepsum_path(cx, sumstr);
    return ok;
}

bool test_alloc_deepsum_L(wikrt_cx* cx)   { return test_deepsum_str(cx, "L");   }
bool test_alloc_deepsum_R(wikrt_cx* cx)   { return test_deepsum_str(cx, "R");   }
bool test_alloc_deepsum_LL(wikrt_cx* cx)  { return test_deepsum_str(cx, "LL");  }
bool test_alloc_deepsum_LR(wikrt_cx* cx)  { return test_deepsum_str(cx, "LR");  }
bool test_alloc_deepsum_RL(wikrt_cx* cx)  { return test_deepsum_str(cx, "RL");  }
bool test_alloc_deepsum_RR(wikrt_cx* cx)  { return test_deepsum_str(cx, "RR");  }
bool test_alloc_deepsum_LLL(wikrt_cx* cx) { return test_deepsum_str(cx, "LLL"); }
bool test_alloc_deepsum_LLR(wikrt_cx* cx) { return test_deepsum_str(cx, "LLR"); }
bool test_alloc_deepsum_LRL(wikrt_cx* cx) { return test_deepsum_str(cx, "LRL"); }
bool test_alloc_deepsum_LRR(wikrt_cx* cx) { return test_deepsum_str(cx, "LRR"); }
bool test_alloc_deepsum_RLL(wikrt_cx* cx) { return test_deepsum_str(cx, "RLL"); }
bool test_alloc_deepsum_RLR(wikrt_cx* cx) { return test_deepsum_str(cx, "RLR"); }
bool test_alloc_deepsum_RRL(wikrt_cx* cx) { return test_deepsum_str(cx, "RRL"); }
bool test_alloc_deepsum_RRR(wikrt_cx* cx) { return test_deepsum_str(cx, "RRR"); }

void deepsum_prng_string(char* buff, unsigned int seed, size_t const nChars) {
    for(size_t ii = 0; ii < nChars; ++ii) {
        buff[ii] = (rand_r(&seed) & (1<<9)) ? 'R' : 'L';
    }
    buff[nChars] = 0;
}

bool test_deepsum_prng(wikrt_cx* cx, unsigned int seed, size_t const nChars) 
{
    char buff[nChars + 1];
    deepsum_prng_string(buff, seed, nChars);
    return test_deepsum_str(cx, buff);
}

bool test_alloc_deepsum_large(wikrt_cx* cx) 
{
    int const count = 400;
    int passed = 0;
    for(int ii = 0; ii < count; ++ii) {
        if(test_deepsum_prng(cx, ii, ii)) {
            ++passed;
        }
    }
    return (passed == count);
}

bool test_copy_deepsum(wikrt_cx* cx) 
{
    size_t const nChars = 8000;
    char buff[nChars + 1];
    deepsum_prng_string(buff, 0, nChars);
    deepsum_path(cx, buff);
    wikrt_copy(cx);
    bool ok = dismantle_deepsum_path(cx, buff)
           && dismantle_deepsum_path(cx, buff);
    return ok;
}

bool test_pkistr_s(wikrt_cx* cx, int64_t n, char const* const nstr) 
{
    wikrt_intro_i64(cx, n);
    size_t len = 0;
    wikrt_peek_istr(cx, NULL, &len); // obtain string size
    bool const okSize = (len == strlen(nstr));

    char buff[len+1]; buff[len] = 0;
    wikrt_peek_istr(cx, buff, &len); // print integer into buffer
    bool const okBuff = (0 == strcmp(nstr, buff));
    wikrt_drop(cx);

    // also try opposite direction
    wikrt_intro_istr(cx, buff, len);
    int64_t i;
    wikrt_peek_i64(cx, &i);
    wikrt_drop(cx);
    bool const okRev = (n == i);

    bool const ok = (okBuff && okSize && okRev);
    return ok;
}


bool test_pkistr_small(wikrt_cx* cx)
{
    int runct = 0;
    int passct = 0;
    #define TEST(N) { \
        ++runct;\
        if(test_pkistr_s(cx, N, #N)) { ++passct; } \
    }
    TEST(0);
    TEST(1);
    TEST(-1);
    TEST(12345678);
    TEST(-12345678);

    #if 0
    // Need support for big integers
    TEST(-1073741824);
    TEST(-1073741823);
    TEST(1073741823);
    TEST(1073741824);
    TEST(-2147483649);
    TEST(-2147483648);
    TEST(-2147483647);
    TEST(2147483647);
    TEST(2147483648);
    TEST(2147483649);
    TEST(999999999999999999);
    TEST(-999999999999999999);
    #endif

    #undef TEST

    return ((runct > 0) && (runct == passct));

}

bool test_copy_i64(wikrt_cx* cx, int64_t const test) {
    wikrt_intro_i64(cx, test);
    wikrt_copy(cx);
    int64_t n1, n2;
    wikrt_peek_i64(cx, &n1);
    wikrt_drop(cx);
    wikrt_peek_i64(cx, &n2);
    wikrt_drop(cx);
    bool const ok = (test == n1) && (n1 == n2);
    return ok;
}

bool test_valid_token_str(char const* s, bool expected) {
    bool const ok = (expected == wikrt_valid_token(s));
    if(!ok) { fprintf(stderr, "token validation failed for: %s\n", s); }
    return ok; 
}

bool test_valid_token(wikrt_cx* cx)
{
    #define ACCEPT(S) test_valid_token_str(S, true)
    #define REJECT(S) test_valid_token_str(S, false)
    return ACCEPT("foo")
        && ACCEPT("hello world")
        && ACCEPT("<>")
        && ACCEPT(".:,;|")
        && ACCEPT("\"")
        && ACCEPT("@")
        && ACCEPT("'")
        && ACCEPT(u8"x→y→z.κλμνξοπρς") // utf-8 okay
        && REJECT("{foo}") // no curly braces
        && REJECT("foo\nbar") // no newlines
        && REJECT("") // too small
        && ACCEPT(u8"←↑→↓←↑→↓←↑→↓←↑→↓←↑→↓←") 
        && REJECT(B32 B32 B32 B32 B32 B32 B32 B32); // oversized
    #undef ACCEPT
    #undef REJECT
}

size_t strct(char const* const* ps) {
    size_t ct = 0;
    while(NULL != (*ps++)) { ++ct; }
    return ct;
}

bool test_sealers(wikrt_cx* cx) 
{
    char const* const lSeals[] = { ":", "abracadabra", ":m", u8"←↑→↓←↑→↓←↑→↓←↑→↓←↑→↓←"
                                 , ":cx", ":foobar", ":env", ":xyzzy" };
    size_t const nSeals = sizeof(lSeals) / sizeof(char const*);
    assert(nSeals > 4);

    wikrt_intro_unit(cx);
    for(size_t ii = 0; ii < nSeals; ++ii) {
        char const* s = lSeals[ii];
        wikrt_wrap_seal(cx, s);
    }

    // validate copy and drop of sealed values
    for(size_t ii = 0; ii < 12; ++ii) {
        wikrt_copy(cx);
        if(ii & 1) { wikrt_wswap(cx); }
        wikrt_drop(cx);
    }

    for(size_t ii = nSeals; ii > 0; --ii) {
        char const* s = lSeals[ii - 1];
        char buff[WIKRT_TOK_BUFFSZ];
        wikrt_unwrap_seal(cx, buff);
        if(0 != strcmp(s, buff)) {
            fprintf(stderr, "expected seal %s, got %s\n", s, buff);
            return false;
        }
    }
    wikrt_elim_unit(cx);
    return true;
}

int32_t pop_list_i32(wikrt_cx* cx)
{
    int32_t a = INT32_MIN;
    read_sum(cx, WIKRT_INL);
    wikrt_assocr(cx);
    wikrt_peek_i32(cx, &a);
    wikrt_drop(cx);
    return a;
}

void elim_list_i32(wikrt_cx* cx, int32_t e) {
    if(pop_list_i32(cx) != e) { wikrt_set_error(cx, WIKRT_ETYPE); }
}

void checkbuff(wikrt_cx* cx, uint8_t const* buff, size_t ct)
{
    for(size_t ii = 0; ii < ct; ++ii) {
        elim_list_i32(cx, buff[ii]);
    }
    elim_list_end(cx);
}

void fillbuff(uint8_t* buff, size_t ct, unsigned int seed) 
{
    for(size_t ii = 0; ii < ct; ++ii) {
        buff[ii] = (rand_r(&seed) & 0xFF);
    }
}

void test_alloc_binary(wikrt_cx* cx) 
{
    int const nLoops = 1;
    for(int ii = 0; ii < nLoops; ++ii) {
        size_t const buffsz = (2000 * ii);
        uint8_t buff[buffsz];
        fillbuff(buff, buffsz, ii);
        wikrt_intro_binary(cx, buff, buffsz);
        checkbuff(cx, buff, buffsz);
    }
}

void elim_cstr(wikrt_cx* cx, char const* cstr) 
{
    if(NULL == cstr) { cstr = ""; }
    uint8_t const* s = (uint8_t const*) cstr;
    do {
        int32_t const cp = (int32_t) utf8_step_unsafe(&s);
        if(0 == cp) { break; }
        elim_list_i32(cx, cp);
    } while(!wikrt_error(cx));
    elim_list_end(cx);
}

void test_alloc_text(wikrt_cx* cx) 
{
    char const* const helloWorld = "hello, world!";
    wikrt_intro_text(cx, helloWorld, 0);
    elim_cstr(cx, "");

    wikrt_intro_text(cx, helloWorld, SIZE_MAX);
    elim_cstr(cx, helloWorld);

    wikrt_intro_text(cx, u8"→", 3);
    elim_cstr(cx, u8"→");

    wikrt_intro_text(cx, u8"abc←↑→↓", 12);
    elim_cstr(cx, u8"abc←↑→");

    wikrt_intro_text(cx, u8"abc←↑", SIZE_MAX);
    elim_cstr(cx, u8"abc←↑");
}

void intro_empty_list(wikrt_cx* cx) { 
    wikrt_intro_unit(cx);
    wikrt_wrap_sum(cx, WIKRT_INR);
}
void push_list_i32(wikrt_cx* cx, int32_t i32) 
{
    wikrt_intro_i32(cx, i32);
    wikrt_assocl(cx);
    wikrt_wrap_sum(cx, WIKRT_INL);
}


void read_binary_chunks(wikrt_cx* cx, uint8_t const* buff, size_t buffsz, size_t const read_chunk) 
{
    uint8_t buff_read[read_chunk];
    size_t bytes_read;
    do {
        bytes_read = read_chunk;
        wikrt_read_binary(cx, buff_read, &bytes_read);
        if(0 != memcmp(buff_read, buff, bytes_read)) { 
            wikrt_set_error(cx, WIKRT_ETYPE); 
            return;
        }
        buff += bytes_read;
    } while(0 != bytes_read);
    elim_list_end(cx);
}


void test_read_binary(wikrt_cx* cx) 
{
    size_t const buffsz = 12345;
    uint8_t buff[buffsz];
    fillbuff(buff, buffsz, buffsz);
    wikrt_intro_binary(cx, buff, buffsz); // first copy
    // need a total seven copies of the binary, for seven tests
    wikrt_copy(cx); wikrt_copy(cx); wikrt_copy(cx);
    wikrt_copy(cx); wikrt_copy(cx); wikrt_copy(cx);

    read_binary_chunks(cx, buff, buffsz, buffsz);
    read_binary_chunks(cx, buff, buffsz, (buffsz - 1));
    read_binary_chunks(cx, buff, buffsz, (buffsz + 1));
    read_binary_chunks(cx, buff, buffsz, (buffsz / 3));
    read_binary_chunks(cx, buff, buffsz, (buffsz / 3) + 1);
    read_binary_chunks(cx, buff, buffsz, (buffsz / 3) - 1);
    read_binary_chunks(cx, buff, buffsz, (buffsz / 2));
}

void read_text_chunks(wikrt_cx* cx, char const* s, size_t const chunk_bytes)
{
    char buff_read[chunk_bytes];
    size_t bytes_read;
    do {
        bytes_read = chunk_bytes;
        wikrt_read_text(cx, buff_read, &bytes_read);
        if(0 != memcmp(buff_read, s, bytes_read)) { 
            wikrt_set_error(cx, WIKRT_ETYPE);
            return;
        }
        s += bytes_read;
    } while(0 != bytes_read);
    elim_list_end(cx);
}

void read_text_cstr(wikrt_cx* cx, char const* s) 
{
    size_t const len = strlen(s);
    wikrt_intro_text(cx, s, SIZE_MAX); // first copy
    // need four copies of the text for four tests
    wikrt_copy(cx); 
    wikrt_copy(cx); 
    read_text_chunks(cx, s, len);
    read_text_chunks(cx, s, 11);
}

void test_read_text(wikrt_cx* cx) 
{
    read_text_cstr(cx,"Hello, world! This is a test string.");
    read_text_cstr(cx,u8"←↖↑↗→↘↓↙←↖↑↗→↘↓↙←↖↑↗→↘↓↙←↖↑↗→↘↓↙←↖↑↗→");
    read_text_cstr(cx,u8"★★★☆☆");
    read_text_cstr(cx,u8"μL.((α*L)+β)");
    read_text_cstr(cx,"");
}

void test_big_text(wikrt_cx* cx) 
{
    // I need a test for texts of more than 64kB to cover the split case.
    // I'll use a 250kB text for this test.
    size_t const txt_maxlen = 250 * 1000;
    char txtbuff[txt_maxlen];

    char const* const src = u8"←↖↑↗→↘↓↙";
    size_t const src_len = strlen(src);

    char* wbuff = txtbuff;
    char* const wbuff_max = wbuff + txt_maxlen - (src_len + 1);
    while(wbuff < wbuff_max) { memcpy(wbuff, src, src_len); wbuff += src_len; }
    (*wbuff) = 0;
    //size_t const txtlen = (wbuff - txtbuff);

    wikrt_intro_text(cx, txtbuff, SIZE_MAX); 
    wikrt_copy(cx);
    wikrt_copy(cx);
    read_text_chunks(cx, txtbuff, 30002);
    read_text_chunks(cx, txtbuff, 3001);
    read_text_chunks(cx, txtbuff, 304);
}



void read_istr(wikrt_cx* cx, char const* expecting) 
{
    size_t len = 0;
    wikrt_peek_istr(cx, NULL, &len);
    char buff[len+1]; 
    buff[len] = 0;
    wikrt_peek_istr(cx, buff, &len);
    wikrt_drop(cx);
    if(0 != strcmp(buff, expecting)) {
        wikrt_set_error(cx, WIKRT_ETYPE);
        fprintf(stderr, "integer match failed: got %s, expected %s\n", buff, expecting);
    }
}

void test_add1(wikrt_cx* cx, char const* a, char const* b, char const* expected) {
    wikrt_intro_istr(cx, a, SIZE_MAX);
    wikrt_intro_istr(cx, b, SIZE_MAX);
    wikrt_int_add(cx);
    read_istr(cx, expected);
}
void test_add(wikrt_cx* cx, char const* a, char const* b, char const* expected) {
    test_add1(cx, a, b, expected);
    test_add1(cx, b, a, expected);
}

void test_mul1(wikrt_cx* cx, char const* a, char const* b, char const* expected) {
    wikrt_intro_istr(cx, a, SIZE_MAX);
    wikrt_intro_istr(cx, b, SIZE_MAX);
    wikrt_int_mul(cx);
    read_istr(cx, expected);
}
void test_mul(wikrt_cx* cx, char const* a, char const* b, char const* expected) {
    test_mul1(cx, a, b, expected);
    test_mul1(cx, b, a, expected);
}

void test_neg1(wikrt_cx* cx, char const* a, char const* expected) {
    wikrt_intro_istr(cx, a, SIZE_MAX);
    wikrt_int_neg(cx);
    read_istr(cx, expected);
}
void test_neg(wikrt_cx* cx, char const* a, char const* b) {
    test_neg1(cx, a, b);
    test_neg1(cx, b, a);
}

void test_div(wikrt_cx* cx, char const* dividend, char const* divisor, char const* quotient, char const* remainder)
{
    wikrt_intro_istr(cx, dividend, SIZE_MAX);
    wikrt_intro_istr(cx, divisor, SIZE_MAX);
    wikrt_int_div(cx);
    read_istr(cx, remainder);
    read_istr(cx, quotient);
}


void test_smallint_math(wikrt_cx* cx)
{
    // testing by string comparisons.
    test_add(cx,"1","2","3");
    test_add(cx,"60","-12","48");
    test_neg(cx,"0","0");
    test_neg(cx,"1","-1");
    test_neg(cx,"42","-42");
    test_mul(cx,"1","1044","1044");
    test_mul(cx,"129","0","0");
    test_mul(cx,"13","12","156");
    test_mul(cx,"19","-27","-513");
    test_div(cx, "11", "3", "3", "2");
    test_div(cx,"-11", "3","-4", "1");
    test_div(cx, "11","-3","-4","-1");
    test_div(cx,"-11","-3", "3","-2");
}

void test_bigint_math(wikrt_cx* cx)
{
    wikrt_set_error(cx, WIKRT_IMPL);
#if 0
    return test_add(cx, "10000000000", "0", "10000000000")
        && test_add(cx, "10000000000", "20000000000", "30000000000")
        && test_add(cx, "123456789", "9876543210", "9999999999")
        && test_add(cx, "-123456789", "9876543210", "9753086421")
        && test_mul(cx, "123456789", "42", "5185185138")
        && test_mul(cx, 
    return false;
#endif
}

void test_sum_distrib_b(wikrt_cx* cx, bool inR) {
    char const * const a = "42";
    char const * const b = "11";
    wikrt_sum_tag const lr = inR ? WIKRT_INR : WIKRT_INL;
    wikrt_intro_istr(cx, a, SIZE_MAX);
    wikrt_wrap_sum(cx, lr);
    wikrt_intro_istr(cx, b, SIZE_MAX);
    wikrt_sum_distrib(cx);
    read_sum(cx, lr);
    wikrt_assocr(cx); // ((42 * 11) * e) → (42 * (11 * e)) 
    read_istr(cx, b);
    read_istr(cx, a);
}
void test_sum_distrib(wikrt_cx* cx) {
    test_sum_distrib_b(cx, true);
    test_sum_distrib_b(cx, false);
}


void test_sum_factor_b(wikrt_cx* cx, bool inR) 
{
    char const* const a = "42";
    char const* const b = "11";
    wikrt_sum_tag const lr = inR ? WIKRT_INR : WIKRT_INL;

    wikrt_intro_istr(cx, a, SIZE_MAX);
    wikrt_intro_istr(cx, b, SIZE_MAX);
    wikrt_assocl(cx);
    wikrt_wrap_sum(cx, lr);
    wikrt_sum_factor(cx);

    read_sum(cx, lr); read_istr(cx, b);
    read_sum(cx, lr); read_istr(cx, a);
}

void test_sum_factor(wikrt_cx* cx) {
    test_sum_factor_b(cx, true);
    test_sum_factor_b(cx, false);
}

#if 0
bool test_serialize_cstr(wikrt_cx* cx, char const* abc)
{
    size_t const len = strlen(abc);
    wikrt_intro_text(cx, abc, len);
    wikrt_text_to_block(cx);

    wikrt_block_to_text(cx);
    char buff[len + 1];
    size_t bufflen = len + 1;
    wikrt_read_text(cx, buff, &bufflen);
    buff[len] = 0;
    elim_list_end(cx);

    bool const ok = (len == bufflen) && (0 == strcmp(abc, buff)) && !wikrt_error(cx);
    return ok;
}
#endif

void test_arg_list(wikrt_cx* cx, char const* const* ss, void (*test)(wikrt_cx*, char const* s)) 
{
    bool ok = true;
    while(NULL != *ss) { 
        char const* s = *(ss++);
        (*test)(cx, s);
        ok = !wikrt_error(cx) && ok;
        wikrt_cx_reset(cx);
    }
    if(!ok) { wikrt_set_error(cx, WIKRT_ETYPE); }
}

void test_parse_abc_str(wikrt_cx* cx, char const* abc)
{
    wikrt_intro_text(cx, abc, SIZE_MAX);
    wikrt_text_to_block(cx);
    wikrt_drop(cx);
    if(WIKRT_OK != wikrt_error(cx)) {
        fprintf(stderr, "%s: failed to introduce ABC: %s\n", __FUNCTION__, abc);
    } 
}

void test_write_abc_str(wikrt_cx* cx, char const* abc)
{
    // fprintf(stderr, "%s: arg `%s`\n", __FUNCTION__, abc);
    wikrt_intro_text(cx, abc, SIZE_MAX); 
    wikrt_text_to_block(cx);
    if(wikrt_error(cx)) { wikrt_cx_reset(cx); return; } // shows up as failure in test_parse_abc_str

    wikrt_block_to_text(cx); 
    if(wikrt_error(cx)) { // failed to write ABC at all
        fprintf(stderr, "%s: failed to write ABC to text: `%s`\n", __FUNCTION__, abc);
        return;
    }

    elim_cstr(cx, abc);
    if(WIKRT_OK != wikrt_error(cx)) { // text→block→text was not exact identity function
        fprintf(stderr, "%s: failed to write and recover ABC: `%s`\n", __FUNCTION__, abc);
    }
}

void test_reject_parse_str(wikrt_cx* cx, char const* s) 
{
    wikrt_intro_text(cx, s, SIZE_MAX);
    wikrt_text_to_block(cx);
    if(!wikrt_error(cx)) {
        fprintf(stderr, "%s: accepted invalid ABC: %s\n", __FUNCTION__, s);
        wikrt_set_error(cx, WIKRT_ETYPE);
    } else {
        wikrt_cx_reset(cx);
    }
}

void test_parse_abc(wikrt_cx* cx) { test_arg_list(cx, valid_abc_strings, test_parse_abc_str); }
void test_write_abc(wikrt_cx* cx) { test_arg_list(cx, valid_abc_strings, test_write_abc_str); }
void test_reject_parse(wikrt_cx* cx) { test_arg_list(cx, invalid_abc_strings, test_reject_parse_str); }



void test_quote_unit(wikrt_cx* cx) 
{ 
    wikrt_intro_unit(cx); val2txt(cx); elim_cstr(cx, "vvrwlc"); 
}

void test_quote_int(wikrt_cx* cx)
{
    wikrt_intro_i32(cx, 0); val2txt(cx);  elim_cstr(cx, "#");
    wikrt_intro_i32(cx, -7); val2txt(cx); elim_cstr(cx, "#7-");
    wikrt_intro_i32(cx, 42); val2txt(cx); elim_cstr(cx, "#42");
}

void test_quote_pair(wikrt_cx* cx)
{
    wikrt_intro_i32(cx, 42);
    wikrt_intro_i32(cx, -7);
    wikrt_wswap(cx);
    wikrt_assocl(cx);
    val2txt(cx);

    size_t len = 79;
    char buff[len+1];
    wikrt_read_text(cx, buff, &len);
    buff[len] = 0;

    bool const ok = (0 == strcmp(buff, "#7-#42l")) 
                 || (0 == strcmp(buff, "#42#7-wl"));
    if(!ok) { wikrt_set_error(cx, WIKRT_ETYPE); }
}

void test_quote_sum(wikrt_cx* cx)
{
    wikrt_intro_i32(cx, -7); wikrt_wrap_sum(cx, WIKRT_INL); val2txt(cx); elim_cstr(cx, "#7-V");
    wikrt_intro_i32(cx, 42); wikrt_wrap_sum(cx, WIKRT_INR); val2txt(cx); elim_cstr(cx, "#42VVRWLC");
}

void test_quote_seal(wikrt_cx* cx)
{
    wikrt_intro_i32(cx, 108);
    wikrt_wrap_seal(cx, ":this");
    wikrt_wrap_seal(cx, ":is");
    wikrt_wrap_seal(cx, "a");
    wikrt_wrap_seal(cx, "test");
    val2txt(cx);
    elim_cstr(cx, "#108{:this}{:is}{a}{test}");
}

void test_quote_text(wikrt_cx* cx) 
{
    wikrt_intro_text(cx, "Hello, World!", SIZE_MAX); val2txt(cx); elim_cstr(cx, "\"Hello, World!\n~");
    wikrt_intro_text(cx, "multi\nline", SIZE_MAX); val2txt(cx); elim_cstr(cx, "\"multi\n line\n~");
    wikrt_intro_text(cx, u8"★★★ → ★★", SIZE_MAX); val2txt(cx); elim_cstr(cx, u8"\"★★★ → ★★\n~");
}

void test_quote_empty_text(wikrt_cx* cx) 
{
    wikrt_intro_text(cx, "", SIZE_MAX); 
    val2txt(cx);
    size_t len = 79;
    char buff[len+1];
    wikrt_read_text(cx, buff, &len);
    buff[len] = 0;
    bool const ok = (0 == strcmp(buff, "vvrwlcVVRWLC"))
                 || (0 == strcmp(buff, "\"\n~"));
    if(!ok) { wikrt_set_error(cx, WIKRT_ETYPE); }

}

void test_quote_block(wikrt_cx* cx)
{
    wikrt_intro_i32(cx, -57); 
    wikrt_quote(cx); 
    val2txt(cx); 
    elim_cstr(cx, "[#57-]");

    wikrt_intro_text(cx, u8"{hello}{★}{world}", SIZE_MAX); 
    wikrt_text_to_block(cx);
    val2txt(cx); 
    elim_cstr(cx, u8"[{hello}{★}{world}]");
}

void test_quote_block_ss(wikrt_cx* cx)
{
    wikrt_intro_unit(cx);
    wikrt_wrap_sum(cx, WIKRT_INR);

    wikrt_text_to_block(cx);
    wikrt_copy(cx);
    wikrt_copy(cx);
    wikrt_copy(cx);

    wikrt_block_rel(cx); 
    val2txt(cx); 
    elim_cstr(cx, "[]k");

    wikrt_block_aff(cx);
    val2txt(cx);
    elim_cstr(cx, "[]f");

    wikrt_block_rel(cx);
    wikrt_block_aff(cx);
    val2txt(cx);
    elim_cstr(cx, "[]kf");

    wikrt_block_aff(cx);
    wikrt_block_rel(cx);
    val2txt(cx);
    elim_cstr(cx, "[]kf"); // give no `fk`s. 
}

void deep_wrap_val(wikrt_cx* cx) 
{
    wikrt_intro_unit(cx); wikrt_assocl(cx);
    wikrt_quote(cx);
    wikrt_wrap_seal(cx, ":s");
    wikrt_intro_unit(cx); wikrt_wswap(cx); wikrt_assocl(cx);
    wikrt_wrap_sum(cx, WIKRT_INL);
    wikrt_wrap_sum(cx, WIKRT_INR);
    wikrt_wrap_sum(cx, WIKRT_INL);
    wikrt_quote(cx);
    wikrt_wrap_sum(cx, WIKRT_INR);
    wikrt_wrap_seal(cx, "deep wrapped value");
}

void test_aff(wikrt_cx* cx) 
{
    wikrt_intro_i32(cx, 0);
    wikrt_quote(cx);
    wikrt_block_aff(cx);
    deep_wrap_val(cx);
    wikrt_drop(cx);  // this is okay (drop affine value)

    wikrt_intro_i32(cx, 1);
    wikrt_quote(cx);
    wikrt_block_aff(cx);
    deep_wrap_val(cx);
    
    if(wikrt_error(cx)) { return; }

    wikrt_copy(cx); // this is an error (copy affine value)
    if(!wikrt_error(cx)) {
        wikrt_set_error(cx, WIKRT_ETYPE);
    } else { wikrt_cx_reset(cx); }
}

void test_rel(wikrt_cx* cx)
{
    wikrt_intro_i32(cx, 0); 
    wikrt_quote(cx);
    wikrt_block_rel(cx);
    deep_wrap_val(cx);
    wikrt_copy(cx);  // this is okay (copy of relevant value)
    if(wikrt_error(cx)) { return; }

    wikrt_drop(cx); // this is an error (drop relevant value)
    if(!wikrt_error(cx)) {
        wikrt_set_error(cx, WIKRT_ETYPE);
    } else { wikrt_cx_reset(cx); }
}

void test_trash(wikrt_cx* cx) 
{
    wikrt_intro_i32(cx, 99);
    wikrt_quote(cx);
    wikrt_block_rel(cx);
    wikrt_wrap_seal(cx, ":test");
    wikrt_copy(cx);
    val2txt(cx); elim_cstr(cx, "[#99]k{:test}");
    wikrt_trash(cx);
    val2txt(cx); elim_cstr(cx, "[]k{&trash}");

    wikrt_intro_unit(cx);
    wikrt_quote(cx);
    wikrt_block_aff(cx);
    wikrt_wrap_seal(cx, ":affineTest");
    wikrt_trash(cx);
    val2txt(cx); elim_cstr(cx, "[]f{&trash}");

    wikrt_intro_i32(cx, -12345);
    wikrt_quote(cx);
    wikrt_block_aff(cx);
    wikrt_block_rel(cx);
    wikrt_wrap_seal(cx, ":relevantAndAffine");
    wikrt_trash(cx);
    val2txt(cx); elim_cstr(cx, "[]kf{&trash}");

}

void intro_block(wikrt_cx* cx, char const* abc) 
{
    if(wikrt_error(cx)) { return; }
    wikrt_intro_text(cx, abc, SIZE_MAX);
    wikrt_text_to_block(cx);
    if(wikrt_error(cx)) {
        fprintf(stderr, "%s: Could not introduce ABC code `%s`\n", __FUNCTION__, abc);
    }
}

void run_block(wikrt_cx* cx, int effort) 
{
    wikrt_apply(cx);
    while((effort--) > 0) {
        wikrt_step_eval(cx);
        wikrt_sum_tag lr; 
        wikrt_unwrap_sum(cx, &lr);
        bool const done = (WIKRT_INR == lr) || wikrt_error(cx);
        if(done) { return; }
    }
    wikrt_set_error(cx, WIKRT_CXFULL);
}

void run_block_inline(wikrt_cx* cx, int effort)
{
    // vr$c
    wikrt_intro_unit_r(cx);
    wikrt_assocr(cx);
    run_block(cx, effort);
    wikrt_elim_unit_r(cx);
}

void test_eval_id(wikrt_cx* cx)
{
    char const* const a = "-7";
    char const* const b = "49";
    wikrt_intro_istr(cx, a, SIZE_MAX);
    wikrt_intro_istr(cx, b, SIZE_MAX);
    wikrt_assocl(cx);
    intro_block(cx, "");
    run_block(cx, 1);
    wikrt_assocr(cx);
    read_istr(cx, b);
    read_istr(cx, a);
}

void test_eval_vrwlc(wikrt_cx* cx)
{
    char const* const a = "-7";
    char const* const b = "49";
    wikrt_intro_istr(cx, a, SIZE_MAX);
    wikrt_intro_istr(cx, b, SIZE_MAX);
    wikrt_assocl(cx); // (b, a)
    intro_block(cx, "vrwlc");
    run_block(cx,1); // should have (a, b)

    wikrt_assocr(cx); 
    read_istr(cx, a);
    read_istr(cx, b);
}

// input: a block of type `∀a.a→(Int*a)` and an expected result. Limited effort.
void test_eval_abc2i(wikrt_cx* cx, char const* const abc, int64_t const expected) 
{
    if(wikrt_error(cx)) { return; }
    int const max_effort = 4;
    char const* const seal = u8"∀";
    wikrt_intro_unit(cx);
    wikrt_wrap_seal(cx, seal);
    intro_block(cx, abc);
    run_block(cx, max_effort);
    wikrt_assocr(cx);
    wikrt_wswap(cx);
    char sealbuff[WIKRT_TOK_BUFFSZ];
    wikrt_unwrap_seal(cx, sealbuff);
    wikrt_elim_unit(cx);
    int64_t actual;
    wikrt_peek_i64(cx, &actual);

    bool const okResult = (0 == strcmp(seal, sealbuff)) && (expected == actual);
    if(!okResult) { 
        fprintf(stderr, "%s: failed evaluation of `%s` (expecting %lld, got %lld)\n", 
            __FUNCTION__, 
            abc, 
            (long long int) expected,
            (long long int) actual);
        wikrt_set_error(cx, WIKRT_ETYPE);
    }
}

void test_eval_num(wikrt_cx* cx)
{
    test_eval_abc2i(cx, "#", 0);
    test_eval_abc2i(cx, "#0", 0);
    test_eval_abc2i(cx, "#7 -", -7);
    test_eval_abc2i(cx, "# 8\n675 309", 8675309);
    //test_eval_abc2i(cx, "#09876543210", 9876543210);
}

void test_eval_math(wikrt_cx* cx) 
{
    test_eval_abc2i(cx, "#7-^^*+", 42);
    test_eval_abc2i(cx, "#6^^*+", 42);

    test_eval_abc2i(cx, "#11#3  Q%",  3); test_eval_abc2i(cx, "#11#3  Qw%",  2);
    test_eval_abc2i(cx, "#11-#3 Q%", -4); test_eval_abc2i(cx, "#11-#3 Qw%",  1);
    test_eval_abc2i(cx, "#11#3- Q%", -4); test_eval_abc2i(cx, "#11#3- Qw%", -1);
    test_eval_abc2i(cx, "#11-#3-Q%",  3); test_eval_abc2i(cx, "#11-#3-Qw%", -2);
}

void test_eval_comparison(wikrt_cx* cx)
{
    test_eval_abc2i(cx, "#7#7 G VRWLC Krw%", 7);
    test_eval_abc2i(cx, "#7#7 G VRWLC Kr %", 7);
    test_eval_abc2i(cx, "#6#7 G VRWLC Krw%", 6);  
    test_eval_abc2i(cx, "#6#7 G VRWLC Kr %", 7);
    test_eval_abc2i(cx, "#7#6 G       Krw%", 6);
    test_eval_abc2i(cx, "#7#6 G       Kr %", 7);
}

void test_eval_compose(wikrt_cx* cx) {
    test_eval_abc2i(cx, "#7-[*+][^^]m vr$c", 42);
    test_eval_abc2i(cx, "#6 [       ^       ^][       *       +]wm vr$c", 42);
}

void test_eval_quote(wikrt_cx* cx) {
    char const* const a = "42";
    wikrt_intro_istr(cx, a, SIZE_MAX);
    wikrt_quote(cx);
    run_block_inline(cx, 1);
    read_istr(cx, a);

    test_eval_abc2i(cx, "#7-'[^^*+]wm vr$c", 42);
    test_eval_abc2i(cx, "#6'[vr$c^^*+] vr$c", 42);
}


void test_eval_fixpoint(wikrt_cx* cx) 
{
    // minimal fixpoint function: [^'m]m^'m
    //  (not a constant overhead variant)
    // This doubles the argument function size,
    // but is okay for small functions.
    test_eval_abc2i(cx, "#[w#2*#1+w] [^'m]m^'m vr$c vr$c vr$c vr$c vr$c %", 31);
}

void test_eval_fixpoint_sto(wikrt_cx* cx)
{
    // a more sophisticated fixpoint function: 
    //  '[^'mw^'zmwvr$c]^'mwm
    //
    // Example: [{%foo}] '[^'mw^'zmwvr$c]^'mwm
    //        = [[{%foo}]] [^'mw^'zmwvr$c] ^'mwm
    //        = [[{%foo}]] [^'mw^'zmwvr$c] [[^'mw^'zmwvr$c]] mwm
    //        = [[{%foo}]] [[^'mw^'zmwvr$c]^'mw^'zmwvr$c] wm
    //        = [[{%foo}][^'mw^'zmwvr$c]^'mw^'zmwvr$c]
    //
    // Applied: [{%foo}][^'mw^'zmwvr$c]^'mw^'zmwvr$c
    //        = [{%foo}][[^'mw^'zmwvr$c]^'mw^'zmwvr$c]w^'zmwvr$c
    //        = [[^'mw^'zmwvr$c]^'mw^'zmwvr$c][{%foo}][[{%foo}]]zmwvr$c
    //        = [[{%foo}][^'mw^'zmwvr$c]^'mw^'zmwvr$c][{%foo}]vr$c
    //
    // Anyhow, the important point is that we only have one `[{%foo}]` within
    // the fixpoint block. We briefly have two, but only when we're about to
    // apply one.
    test_eval_abc2i(cx, "#[w#100+w] '[^'mw^'zmwvr$c]^'mwm  vr$c vr$c vr$c vr$c %", 400);
}

void test_eval_loop(wikrt_cx* cx)
{
    test_eval_abc2i(cx, "#[#4+]#100" ABC_REPEAT, 400);
}

void test_quote_apply(wikrt_cx* cx) 
{
    // also tests that `wikrt_apply` performs no immediate evaluation.
    wikrt_intro_istr(cx, "-7", SIZE_MAX);
    intro_block(cx, "v^^*+");
    wikrt_apply(cx);
    val2txt(cx);
    elim_cstr(cx, "#7-[v^^*+]{&lazy}$");
}

void test_quote_compose(wikrt_cx* cx)
{
    // Testing for aesthetics of composition!
    // k before f.
    intro_block(cx, ""); wikrt_block_aff(cx); 
    intro_block(cx, ""); wikrt_block_rel(cx);
    wikrt_compose(cx); val2txt(cx); elim_cstr(cx, "[]kf");

    intro_block(cx, ""); wikrt_block_rel(cx);
    intro_block(cx, ""); wikrt_block_aff(cx); 
    wikrt_compose(cx); val2txt(cx); elim_cstr(cx, "[]kf");

    // Small blocks: simple concatenation should be the case. 
    // I'm going to assume that 4 ops is a 'small' block. 
    intro_block(cx, "cccc"); 
    intro_block(cx, "vvvv"); 
    wikrt_compose(cx);
    val2txt(cx);
    elim_cstr(cx, "[vvvvcccc]");

    // Blocks with lazy/fork decorators must also use [[block]inline] form.
    intro_block(cx, "{bar}"); wikrt_block_lazy(cx);
    intro_block(cx, "{foo}"); wikrt_block_fork(cx);
    wikrt_compose(cx);
    val2txt(cx);
    elim_cstr(cx, "[[{foo}]{&fork}vr$c[{bar}]{&lazy}vr$c]");
}

void test_quote_compose_large_block(wikrt_cx* cx)
{
    // Large blocks: first block should use [[block]inline] form
    // when composing, to ensure O(1) composition. But this test
    // might need some updating if I use compact bytecode on
    // parse in the future (which is quite possible).  
    intro_block(cx, "cccccccccccccccccccccccccccccccc"); 
    intro_block(cx, "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv"); 
    wikrt_compose(cx);
    val2txt(cx);

    char const* const ok1 = "[[vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv]vr$ccccccccccccccccccccccccccccccccc]";
    char const* const ok2 = "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvcccccccccccccccccccccccccccccccc";

    size_t len = 4 + strlen(ok1);
    char buff[len + 1];
    wikrt_read_text(cx, buff, &len);
    buff[len] = 0;


    if(0 == strcmp(buff, ok1)) {
        // expected
    } else if(0 == strcmp(buff, ok2)) {
        // not a failure per se, but not testing what I'm hoping to test.
        fprintf(stderr, "%s: a correct result, but fails to test for O(1) compose.", __FUNCTION__);
    } else {
        wikrt_set_error(cx, WIKRT_ETYPE);
    }
}

void test_block_dec(wikrt_cx* cx) 
{
    // block 'decorators' that affect their evaluation mode.. 
    // Evaluation of lazy-lazy, fork-fork, lazy-fork, fork-lazy.
    //
    // This is effectively a stability test for now, so I get a warning
    // if the representation changes. In practice, multiple decorators
    // on a block (modulo special composition options) should be extremely
    // rare so I'm unlikely to see this outside of explicit testing.
    intro_block(cx, "{test}");
    wikrt_block_lazy(cx);
    wikrt_block_lazy(cx);
    wikrt_block_fork(cx);
    wikrt_block_fork(cx);
    wikrt_block_lazy(cx);
    val2txt(cx);
    elim_cstr(cx, "[[[[[{test}]{&lazy}vr$c]{&lazy}vr$c]{&fork}vr$c]{&fork}vr$c]{&lazy}");

}

void test_eval_anno(wikrt_cx* cx)
{
    test_eval_abc2i(cx, "#7-{&testAnno}^^*+", 42);
}

void test_trace(wikrt_cx* cx) 
{
    do {} while(NULL != wikrt_trace_read(cx)); // clear trace buff
    char const* const testMsg1 = u8"★★★This is a\n\ntest trace message.★★★";
    int64_t testMsg2 = -8675309;
    wikrt_trace_enable(cx, 1000);

    // msg{&trace}%. Test for both text and an integer.
    wikrt_intro_text(cx, testMsg1, SIZE_MAX); wikrt_trace_write(cx); wikrt_drop(cx);
    wikrt_intro_i64(cx, testMsg2); wikrt_trace_write(cx); wikrt_drop(cx);

    // Compare to quoted values. Also tests order of trace messages.
    wikrt_intro_text(cx, testMsg1, SIZE_MAX); val2txt(cx); 
    elim_cstr(cx, wikrt_trace_read(cx));

    wikrt_intro_i64(cx, testMsg2); val2txt(cx); 
    elim_cstr(cx, wikrt_trace_read(cx));

    // Since we only traced two messages, the buffer should be empty.
    if(NULL != wikrt_trace_read(cx)) { wikrt_set_error(cx, WIKRT_ETYPE); }
}

void test_anno_text(wikrt_cx* cx)
{
    intro_empty_list(cx);
    val2txt(cx);
    elim_cstr(cx, "vvrwlcVVRWLC");

    intro_empty_list(cx);
    wikrt_anno_text(cx);
    val2txt(cx);
    elim_cstr(cx, "\"\n~");

    intro_empty_list(cx);
    push_list_i32(cx, 'b');
    push_list_i32(cx, 0x2192);
    push_list_i32(cx, 'a');
    wikrt_anno_text(cx);
    val2txt(cx);
    elim_cstr(cx, u8"\"a→b\n~");
}



void run_tests(wikrt_cx* cx, int* runct, int* passct) {
    char const* errFmt = "test #%d failed: %s\n";
    wikrt_cx_reset(cx);

    #define TCX(TEST)                               \
    do {                                            \
        wikrt_cx_reset(cx);                         \
        char const* name = #TEST ;                  \
        ++(*runct);                                 \
        TEST(cx);                                   \
        if(!wikrt_error(cx)) { ++(*passct); }       \
        else {                                      \
            fprintf(stderr, errFmt, *runct, name);  \
        }                                           \
    } while(0)

    TCX(test_unit);
    TCX(test_false);
    TCX(test_true);

    TCX(test_i32_zero);
    TCX(test_i32_misc);
    TCX(test_pkistr_small);

    TCX(test_alloc_prod);
    TCX(test_copy_prod);

    TCX(test_alloc_deepsum_L);
    TCX(test_alloc_deepsum_R);
    TCX(test_alloc_deepsum_LL);
    TCX(test_alloc_deepsum_LR);
    TCX(test_alloc_deepsum_RL);
    TCX(test_alloc_deepsum_RR);
    TCX(test_alloc_deepsum_LLL);
    TCX(test_alloc_deepsum_LLR);
    TCX(test_alloc_deepsum_LRL);
    TCX(test_alloc_deepsum_LRR);
    TCX(test_alloc_deepsum_RLL);
    TCX(test_alloc_deepsum_RLR);
    TCX(test_alloc_deepsum_RRL);
    TCX(test_alloc_deepsum_RRR);
    TCX(test_alloc_deepsum_large);
    TCX(test_copy_deepsum);
    TCX(test_sum_distrib);
    TCX(test_sum_factor);

    // TODO: test data plumbing functions
    //   vrwlcz, VRWLCZ, swaps, etc.

    TCX(test_valid_token);
    TCX(test_sealers);
    TCX(test_alloc_binary);
    TCX(test_alloc_text);
    TCX(test_read_binary);
    TCX(test_read_text);
    TCX(test_big_text);
    TCX(test_anno_text);


    TCX(test_smallint_math);
    //TCX(test_bigint_math);

    TCX(test_reject_parse);
    TCX(test_parse_abc);
    TCX(test_write_abc);

    TCX(test_quote_unit);
    TCX(test_quote_int);
    TCX(test_quote_pair);
    TCX(test_quote_sum);
    TCX(test_quote_text);
    TCX(test_quote_empty_text);
    TCX(test_quote_seal);
    TCX(test_quote_block);
    TCX(test_quote_block_ss);

    TCX(test_aff);
    TCX(test_rel);
    TCX(test_trash);

    TCX(test_quote_apply);
    TCX(test_quote_compose);
    TCX(test_quote_compose_large_block);
    TCX(test_block_dec);

    // TODO: evaluations
    TCX(test_eval_id);
    TCX(test_eval_vrwlc);
    TCX(test_eval_num);
    TCX(test_eval_math);
    TCX(test_eval_comparison);
    // Test comparisons.
    TCX(test_eval_compose);
    TCX(test_eval_quote);
    TCX(test_eval_fixpoint);
    TCX(test_eval_fixpoint_sto);
    TCX(test_eval_loop);
    TCX(test_eval_anno);

    TCX(test_trace);
    // TODO: simple loops. E.g. a fibonacci function.
    // TODO: evaluation with ad-hoc annotations
    // TODO: evaluation with value sealers
    // TODO: test infinite evaluation returns
    // TODO: bignum math


    // TODO: 
    //  comput
    //   serialization for pending computations.
    // 



    // TODO test: stowage.
    //   serialization for stowed values
    //   test of structure sharing for stowed values
    //   test of rel/affine substructure for stowed values
    // TODO test: transactions.
    //   and persistence of values, count testSuite runs. 

    #undef TCX
}

