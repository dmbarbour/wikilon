/** Utilities for interacting with an AO File.
 *
 * Features: lookup definitions by word, iterate through
 * words in the file.
 *
 * Potential future features include append-only edits and 
 * persistent indexes (with maintenance over edits). These
 * could enable command-line processes to use the AO file
 * as a lightweight filesystem or database.
 */

#pragma once
#include <stddef.h>

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

/** Load an AO file
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
 * The argument is the 'current' word, and the return value is the 'next'
 * word. The undefined word `(AOWord){0}` is the alpha and the omega here,
 * i.e. serving as both an initial sentinel and a final result once all
 * words have been processed. (Effectively, iteration performs a cycle.)
 *
 * The iteration may be messed up if the file is modified.
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
