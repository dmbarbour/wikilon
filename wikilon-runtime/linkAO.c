#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

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
    size_t const fileSize = fstatSize(aoFile);
    void* const memFile = mmap(NULL, fileSize, PROT_READ, MAP_PRIVATE, aoFile, 0);

    // Index our memory file.
    if(NULL == memFile) {
        fprintf(stderr, "Could not map file `%s` (size %lld) into memory\n"
            , a.aoFile, (long long int) fileSize);
        return -1;
    } else {
        fprintf(stderr, "`%s` loaded, size %lld\n", a.aoFile, (long long int) fileSize);
    }

    char const* const prog = (NULL == a.program) ? readToString(stdin)
                           : a.programIsWord     ? aoWrapWord(a.program)
                           :                       strdup(a.program);

    if(NULL == prog) {
        fprintf(stderr, "Could not obtain input program.\n");
        return -1;
    }

    return 0;
}

