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

#define AO_MAX_WORDSIZE 60

static char const* linkAO_helpMsg() { return u8""
 "USAGE: linkAO yourFile.ao [-w word | -d definition]\n"
 "  unless provided as argument, read definition from STDIN\n"
 "  output is Awelon Bytecode (ABC) stream printed to STDOUT\n"
 "\n"
 "Any AO file consists of a set of `@word definition` pairs, each word\n"
 "on a new line. Definitions are Awelon Bytecode with {%word} tokens\n"
 "representing dependency. Dependencies must form a directed acyclic\n"
 "graph, local to the dictionary provided by the file.\n"
 "\n"
 "linkAO will take an AO file together with a word or definition, and\n"
 "produces an ABC stream. The linking is trivial: each {%word} token\n"
 "is replaced by the definition for that word from the AO file.\n"
 ;
}


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
        } else if(match(arg, "-w") && *argv && !a.program) {
            a.program = *(argv++); 
            a.programIsWord = true; 
        } else if(match(arg, "-d") && *argv && !a.program) {
            a.program = *(argv++);
        } else if(matchSuffix(arg, ".ao", 3)) {
            a.aoFile = arg;
        } else { a.badArg = arg; }
    }
    return a;
} 

void printHelp(FILE* out) { fprintf(out, "%s", linkAO_helpMsg()); }

// just read the entire input into a string.
char* readToString(FILE* f) {
    size_t max_buffsz = 1000;
    size_t bytes_read = 0;
    char* buff = malloc(max_buffsz);
    if(!buff) { abort(); }
    buff[0] = 0;
    do {
        bytes_read += fread((buff + bytes_read), 1, (max_buffsz - (1 + bytes_read)), f);
        buff[bytes_read] = 0;
        if(feof(f) || ferror(f)) { return buff; }
        max_buffsz *= 2;
        buff = realloc(buff, max_buffsz);
        if(!buff) { abort(); }
    } while(1);
}

// word → {%word}, for uniform input processing.
char* aoWrapWord(char const* word) 
{
    size_t const len = strlen(word);
    char* const buff = malloc(len + 4);
    if(NULL == buff) { abort(); }
    char* s = buff;
    (*s++) = '{';
    (*s++) = '%';
    memcpy(s, word, len); s += len;
    (*s++) = '}';
    (*s++) = 0;
    return buff;
}

size_t fstatSize(int fd) { struct stat st; fstat(fd, &st); return (size_t) st.st_size; }

/* Indexing the AO file: I need a very fast lookup, given a word, to find its
 * definition in memory. This will be the core of the fast linker. A linear 
 * collision hash is a reasonable option. I don't need deletion - I assume
 * the AO file will remain constant during evaluation.
 *
 * Since I know that the word and definition are separated by a single character
 * (either SP or LF), it is sufficient that I know where to find the pointer to
 * the `word def` pointer, and the word size. 
 */

typedef struct wordDef {
    char const* where; // ref to `word (SP|LF) def` in a file.
    size_t      sizes; // size for word (low 6 bits) and definition.
        // sizes omits the separator byte (SP or LF).
} wordDef;

static inline bool wd_undefined(wordDef const* wd)   { return (NULL == wd->where); }
_Static_assert((AO_MAX_WORDSIZE < 64), "assuming word sizes under 64 bytes");
static inline uint8_t wd_wordSize(wordDef const* wd) { return (0x3F & wd->sizes);  }
static inline char const* wd_word(wordDef const* wd) { return wd->where;           }
static inline size_t wd_defSize(wordDef const* wd)   { return (wd->sizes >> 6);    }
static inline char const* wd_def(wordDef const* wd)  { return ((1 + wd_wordSize(wd)) + wd->where); } // skip word + SP|LF


typedef struct wdTable {
    size_t      size; // total number of available slots
    size_t      fill; // how many filled slots (for collision hash management)
    wordDef*    item; // the `where` field is NULL if unfilled.
} wdTable;

// a simple, public domain hash for use with hashtables
static inline uint64_t hash_fnv64(uint8_t const* data, size_t size) {
    uint64_t hash = (uint64_t)14695981039346656037ull;
    while(size > 0) {
        hash ^= data[--size];
        hash *= 1099511628211;
    }
    return hash;
}

// Find writable location in a hashtable. Assume hashtable has empty cells.
wordDef* wdTable_lookup(wdTable const* tbl, char const* const word, uint8_t const wordSize) {
    _Static_assert(sizeof(uint8_t) == sizeof(char), "casting from char* to uint8_t*");
    uint64_t h = hash_fnv64((uint8_t const*) word, (size_t) wordSize);
    size_t ix = h % tbl->size;
    do {
        wordDef* const r = tbl->item + ix;
        if(wd_undefined(r)) { return r; }
        bool const matchWord = (wordSize == wd_wordSize(r))
                            && (0 == memcmp(word, wd_word(r), wordSize));
        if(matchWord) { return r; }
        ix = (ix + 1) % tbl->size; // linear collision search.        
    } while(1);
}

void wdTable_drop(wdTable* tbl) { free(tbl->item); (*tbl) = (wdTable){0}; }
void wdTable_resize(wdTable* tbl, size_t targetSize) 
{
    assert(targetSize > tbl->fill);

    wdTable old = (*tbl);
    tbl->size = targetSize;   
    tbl->item = calloc(tbl->size, sizeof(wordDef));
    tbl->fill = 0;
    if(NULL == tbl->item) { abort(); }

    for(size_t ix = 0; ix < old.size; ++ix) {
        wordDef const* oldWD = old.item + ix;
        if(NULL == oldWD->where) { continue; }
        wordDef* newWD = wdTable_lookup(tbl, wd_word(oldWD), wd_wordSize(oldWD));
        assert(NULL == newWD->where);
        (*newWD) = (*oldWD); // same memory reference
        ++(tbl->fill);
    }

    assert(old.fill == tbl->fill);
    wdTable_drop(&old);
}

// insert a word, possibly overwrite
void wdTable_insert(wdTable* tbl, wordDef const* wd) 
{
    assert(NULL != wd->where);

    // c. ~70% fill for efficient linear collision hash
    bool const grow = ((tbl->fill * 10) >= (tbl->size * 7)); 
    if(grow) { wdTable_resize(tbl, ((1 + tbl->size) * 2)); } // 0,2,6,14,30... (2^K - 2)

    // insert the data.
    wordDef* tgt = wdTable_lookup(tbl, wd_word(wd), wd_wordSize(wd));
    if(NULL == tgt->where) { ++(tbl->fill); }
    (*tgt) = (*wd);
}

// Given a definition `word def`, return the size of the word. The
// word and definition are divided by SP or LF. If there is no 
// division, we'll return defSize.
size_t findWordSize(char const* fullDef, size_t defSize) 
{
    for(size_t ix = 0; ix < defSize; ++ix) {
        char const c = fullDef[ix];
        bool const split = (' ' == c) || ('\n' == c);
        if(split) { return ix; }
    }
    return defSize;
}

// Return pointer to start of the next definition. 
// Or to memEnd no such definition is found. 
// Also, count lines for debugging purposes.
static void scanToAODef(char const** mem, char const* const memEnd, size_t* lnct) 
{
    bool const spaceToScan = (*mem) < memEnd;
    if(!spaceToScan) { return; }
    char const* const searchEnd = memEnd - 1;
    do {
        char const* const nextLF = memchr((*mem), '\n', searchEnd - (*mem));
        if(NULL == nextLF) { (*mem) = memEnd; return; }
        ++(*lnct);
        (*mem) = 1 + nextLF;
    } while((memEnd != *mem) && ('@' != **mem));
}

void fillTableFromAOFile(wdTable* tbl, char const* mem, size_t size)
{
    char const* const memEnd = mem + size;
    size_t lfct = 0;
    size_t defct = 0;

    // Skip the header if necessary.
    if((size > 0) && ('@' != *mem)) { 
        scanToAODef(&mem, memEnd, &lfct); 
    }

    while(memEnd != mem) 
    {
        size_t const ln_debug = lfct + 1;
        assert('@' == *mem); ++mem; // drop the `@`
        ++defct;
        char const* const wdRef = mem;
        scanToAODef(&mem, memEnd, &lfct);
        bool const finalDef = (mem == memEnd);
        size_t const wdRefSize = (mem - wdRef) - (finalDef ? 1 : 0); // drop trailing LF from size.
        size_t const wordSize = findWordSize(wdRef, wdRefSize);
        size_t const defSize = (wordSize < wdRefSize) ? ((wdRefSize - wordSize) - 1) : 0;
        bool const okSizes = (wordSize <= AO_MAX_WORDSIZE) && (defSize < (SIZE_MAX >> 6));
        if(!okSizes) {
            fprintf(stderr, "Error processing definition %lld (line %lld)\n"
                   , (long long int) defct
                   , (long long int) ln_debug
                   );
        } else {
            wordDef const wd = { .where = wdRef, .sizes = ((defSize << 6) | wordSize) };
            wdTable_insert(tbl, &wd);
        }
    }
} 







int main(int argc, char const* const argv[])
{
    linkAO_args a = parseArgs(1 + argv);
    if(a.help) { printHelp(stdout); return 0; }
    else if(a.badArg) { 
        fprintf(stderr, "unhandled argument (-? for help): %s\n", a.badArg); 
        return -1;
    } else if(!a.aoFile) {
        fprintf(stderr, "please specify your `.ao` source file (-? for help)\n");
        return -1;
    }


    // Load our AO file into memory (via mmap)
    int const aoFile = open(a.aoFile, O_RDONLY);
    if(-1 == aoFile) { 
        fprintf(stderr, "Could not open file `%s` for reading.\n", a.aoFile);
        return -1;
    }
    size_t const memFileSize = fstatSize(aoFile);
    char const* const memFile 
        = mmap(NULL, memFileSize, PROT_READ, MAP_PRIVATE, aoFile, 0);

    // Index our memory file.
    if(NULL == memFile) {
        fprintf(stderr, "Could not map file `%s` (size %lld) into memory\n"
            , a.aoFile, (long long int) memFileSize);
        return -1;
    }

    wdTable table = (wdTable){ 0 };
    fillTableFromAOFile(&table, memFile, memFileSize);

    fprintf(stderr, "File `%s` loaded. Size %lld bytes, %lld definitions.\n"
        , a.aoFile, (long long int) memFileSize, (long long int) table.fill);

    char* const prog = (NULL == a.program) ? readToString(stdin)
                     : a.programIsWord     ? aoWrapWord(a.program)
                     :                       strdup(a.program);

    if(NULL == prog) {
        fprintf(stderr, "Could not obtain input program.\n");
        return -1;
    }

    free(prog);
    wdTable_drop(&table);

    return 0;
}

