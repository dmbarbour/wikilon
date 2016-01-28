
#include <string.h>
#include <errno.h>
#include "futil.h"

// lockfile to prevent multi-process collisions
bool lockfile(int* pfd, char const* fn, mode_t mode) 
{
    errno = 0;
    int const fd = open(fn, O_CREAT | O_RDONLY, mode);
    if ((-1) == fd) {
        return false;
    }  
    if(0 != flock(fd, LOCK_EX|LOCK_NB)) {
        close(fd);
        return false;
    }
    (*pfd) = fd;
    return true;
}

bool mkdirpath(char const* dirPath, mode_t mode)
{
    errno = 0;
    if(0 == mkdir(dirPath, mode)) return true;

    int const e = errno; 
    if(EEXIST == e) return true;  // directory exists already
    if(ENOENT != e) return false; // problem other than missing directory

    // compute parent directory name
    size_t const dplen = strlen(dirPath) + 1;
    char parentDir[dplen];
    strcpy(parentDir, dirPath);
    char *psep = parentDir;
    char *p = parentDir;
    while(*p) {
        if('/' == *p) { psep = p; }
        ++p;
    }
    (*psep) = 0;

    if(0 == parentDir[0]) return false; // base case: no parent 
    if(!mkdirpath(parentDir, mode)) return false; // parent construction failure

    bool const bRecursiveSuccess = (0 == mkdir(dirPath, mode));
    return bRecursiveSuccess;
}

