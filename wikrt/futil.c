
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include "futil.h"

bool mkdirpath(char const* dirPath, mode_t mode)
{
    errno = 0;
    if(0 == mkdir(dirPath, mode)) { 
        // Directory created!
        return true;
    }

    int const e = errno; 
    if(EEXIST == e) {
        // Directory already exists. This is okay, too.
        return true;  
    }
    if(ENOENT != e) { 
        // Unrecoverable error, e.g. EPERM or ENOTDIR
        return false;
    }

    // the parent directory doesn't exist
    // compute parent directory name
    size_t const dplen = strlen(dirPath) + 1;
    char parentDir[dplen];
    strcpy(parentDir, dirPath);
    char *psep = parentDir;
    char *p = parentDir;
    while(*p) {
        // this is probably less portable than ideal
        if('/' == *p) { psep = p; }
        ++p;
    }
    (*psep) = 0;

    if(!mkdirpath(parentDir, mode)) { 
        // failed to create parent directory.
        return false; 
    }
    
    // final attempt to create directory 
    return (0 == mkdir(dirPath, mode));
}

