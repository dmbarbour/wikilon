
#pragma once
#include <stdbool.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/types.h>

// construct a lockfile
bool lockfile(int* pfd, char const* fn, mode_t);

// roughly 'mkdir -p'
bool mkdirpath(char const* dirPath, mode_t);
