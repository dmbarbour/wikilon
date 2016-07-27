#include "aofile.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>


#define AO_WORDSIZE_MIN  1
#define AO_WORDSIZE_MAX  60

// note: I only index words of valid size.
typedef struct wdef {
    uint64_t where; // offset into the AO file
    uint64_t sizes; // (definition size << 6) | wordSize
} wdef;

// A linear collision hash table. Monotonic. (No deletion.)
// Undefined slots use `sizes = 0`.
typedef struct wdef_table {
    size_t   space;
    size_t   elems;
    wdef*    table;
} wdef_table;

struct AOFile {
    // index points into idxfile_mmap if defined.
    void*       src_file_mmap;
    size_t      src_file_size;
    wdef_table  index; 
};

static uint64_t hash_fnv64(uint8_t const* data, size_t size) {
    uint64_t hash = (uint64_t)14695981039346656037ull;
    while(size > 0) {
        hash ^= data[--size];
        hash *= 1099511628211;
    }
    return hash;
}

static inline char const* src(AOFile const* ao) { return (char const*)(ao->src_file_mmap); }
static inline AOWord wd2w(AOFile const* ao, wdef wd) 
{
    return (AOWord){ .str = (src(ao) + wd.where)
                   , .len = (0x3F & wd.sizes)
                   };
}
static inline AODef wd2d(AOFile const* ao, wdef wd)
{
    return (AODef){ .str = (src(ao) + wd.where + (0x3F & wd.sizes) + 1) // offset by word + SP/LF sep
                  , .len = (wd.sizes >> 6)
                  };
}

// Find the hash index for the given word, or the blank space
// in the index where we'd insert said word. This assumes that
// the element either exists or there is sufficient space to
// add it.
static size_t AOFile_word_index(AOFile const* ao, AOWord const w) 
{
    _Static_assert((sizeof(uint8_t) == sizeof(char)), "casting from char* to uint8_t*");
    uint64_t const h = hash_fnv64((uint8_t const*)w.str, w.len);
    size_t const hmax = ao->index.space;
    size_t ix = h % hmax;
    do {
        wdef const wd = ao->index.table[ix];
        if(0 == wd.sizes) { return ix; }
        AOWord const cw = wd2w(ao, wd);
        bool const match = (cw.len == w.len) 
                        && (0 == memcmp(cw.str, w.str, w.len));
        if(match) { return ix; }
        ix = (1 + ix) % hmax;
    } while(1);
}

// Resize the index to a given target size.
static void AOFile_index_resize(AOFile* ao, size_t newSize)
{
    wdef_table const oldIndex = ao->index;
    assert(newSize > oldIndex.elems);
    ao->index = (wdef_table){ .space = newSize
                            , .elems = 0
                            , .table = calloc(newSize, sizeof(wdef))
                            };
    if(NULL == ao->index.table) { abort(); }
    for(size_t ix = 0; ix < oldIndex.space; ++ix) 
    {
        wdef const wd = oldIndex.table[ix];
        if(0 == wd.sizes) { continue; }
        AOWord const w = wd2w(ao, wd);
        size_t const newIx = AOFile_word_index(ao, w);
        ao->index.table[newIx] = wd;
        ++(ao->index.elems);
    }
    assert(ao->index.elems == oldIndex.elems);
    free(oldIndex.table);
}

static void AOFile_index_insert(AOFile* ao, wdef wd)
{
    assert(0 != wd.sizes);
    bool const grow = (ao->index.elems * 10) >= (ao->index.space * 7);
    if(grow) { AOFile_index_resize(ao, (2 * (1 + ao->index.space))); }
    size_t const insIx = AOFile_word_index(ao, wd2w(ao, wd));
    wdef* const slot = insIx + ao->index.table;
    if(0 == slot->sizes) { ++(ao->index.elems); }
    (*slot) = wd;
}

AODef AOFile_lookup(AOFile* ao, AOWord w) 
{
    size_t const ix = AOFile_word_index(ao, w);
    wdef const wd = ao->index.table[ix];
    if(0 == wd.sizes) { return (AODef){0}; }
    return wd2d(ao,wd);
}

// I'll iterate through the hashtable. This means I look up
// the given word in the hashtable, find the next table index
// in use, then return the word associated with that index.
AOWord AOFile_iterate(AOFile* ao, AOWord w)
{
    size_t const ix0 = (NULL == w.str) ? 0 : (1 + AOFile_word_index(ao, w));
    size_t const ixf = ao->index.space;
    for(size_t ix = ix0; ix < ixf; ++ix) 
    {
        wdef const wd = ao->index.table[ix];
        if(0 == wd.sizes) { continue; }
        return wd2w(ao,wd);
    }
    return (AOWord){0}; // end of iteration
}

    


static void AOFile_build_index(AOFile* ao)
{
    

    // TODO: build the index
}


static size_t readFileSize(int fd)
{
    struct stat st;
    return (0 == fstat(fd, &st)) ? (size_t) st.st_size : 0;
}


AOFile* AOFile_load(char const* source) 
{

    AOFile* ao = calloc(1, sizeof(AOFile));
    if(!ao) { return NULL; }
    
    assert(NULL != source);
    int const aofd = open(source, O_RDONLY);
    if((-1) == aofd) { free(ao); return NULL; }

    ao->src_file_size = readFileSize(aofd);
    ao->src_file_mmap = mmap(NULL, ao->src_file_size, PROT_READ, MAP_PRIVATE, aofd, 0);
    close(aofd);
    if(MAP_FAILED == ao->src_file_mmap) { free(ao); return NULL; }

    AOFile_build_index(ao);
    return ao;
}

size_t AOFile_size(AOFile* ao) 
{
    return (ao->index.elems);
}


void AOFile_unload(AOFile* ao) 
{
    free(ao->index.table);
    munmap(ao->src_file_mmap, ao->src_file_size);
    free(ao);
}

#if 0

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
#endif