
/** Utilities for indexing and interacting with an AO file.
 *
 * USAGE: Load the file, lookup any number of words, then unload.
 * You may also iterate through words in the file.  
 *
 * Notes: This code assumes our file will NOT be modified concurrently.
 * Validity of the AO code is not tested. The only check on words is
 * that they are 1..60 bytes and end in SP or LF. (Invalid words will
 * simply be ignored.)
 *
 * I might eventually want to support a persistent index. It would 
 * make a difference when working with large AO files, enabling reuse
 * of the index. However, it isn't really critical for existing use
 * cases.
 */

struct AOFile;
typedef struct AOFile AOFile;

/* An AOFile_String is basically a sized string. This is necessary
 * because we're returning pointers directly into an mmap'd file.
 * Note that all AO strings returned from this API become invalid
 * upon unload.
 *
 * There are two strings with length zero. The 'undefined' string
 * has NULL for `str`. The empty string is valid, and is not NULL.
 */
struct AOFile_String { 
    char const* str; 
    size_t      len;
};
typedef struct AOFile_String AODef;
typedef struct AOFile_String AOWord;

/** Load an AO file. 
 *
 * The main task this performs is to construct an in-memory for the
 * file, and memory-map the file for efficient reference.
 */
AOFile* AOFile_load(char const* aofile);

/** Lookup a given word.
 * 
 * The returned definition is not necessarily valid AO code. If the
 * word is not defined, we'll return the undefined AODef (NULL str).
 */
AODef AOFile_lookup(AOFile*, AOWord);

/** Iterate through defined words in the file.
 *
 * Use the undefined word, `(AOWord){0}`, to get started. Then use each
 * returned word to access the next one. An undefined word is returned
 * as the final sentinel to indicate there are no words to look up.
 *
 * NOTE: Iteration has no well defined ordering to it. You're currently
 * stepping through a hashtable. If definitions are small, this might
 * be a bit slower than scanning the file.
 */
AOWord AOFile_iterate(AOFile*, AOWord);

/** Return count of defined words. 
 * 
 * When a word is defined more than once in the AO file, only the final
 * definition is used. There is no guarantee that words are well defined.
 */
size_t AOFile_size(AOFile*);

/** Release memory resources associated with the AOFile. 
 *
 * Note that all AODef and AOWord strings returned from the AOFile
 * API will be released by this. 
 */
void AOFile_unload(AOFile*);


// TODO? I'm interested in perhaps supporting:
//
//   validation of AO (guarantee against cycles)
//   fast 'reverse lookup' of arbitrary tokens
//
// But such indices can be separated, easily enough.

