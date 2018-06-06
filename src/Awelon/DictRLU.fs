namespace Awelon

// The DictRLU module augments a dictionary with a reverse lookup
// index, such that we can find the clients for each word in the 
// dictionary. This does require parsing every definition.
//
// This is represented using a companion dictionary, with a :word.client
// entry for each client of a word. This allows for stripping the prefix
// to find all clients.
//
// Hierarchical dictionaries require special attention. We want distinct
// prefixes for internal vs. external clients of a word, such that we can
// index internal clients independently of context. Proposal: 
//
// - dict/word.client   is internal, dict/client depends on dict/word
// - ^dict/word.client  is external, client depends on dict/word
//
// In this manner the ^ carrot acts as a cursor, indicating where our
// client is in context of the hierarchical namespace. We could make
// this more uniform, using a :^word.client entry in the base case. 
// To find all clients of `foo/bar/baz` requires looking at prefixes
// `^foo/bar/baz.`, `foo/^bar/baz.`, and `foo/bar/^baz.`. 
//
// Besides words, we'll also want to index annotations like (par), 
// record and accessor labels, and potentially data - natural numbers
// and words within text. Errors might also be tracked, e.g. treating
// undefined words as clients of `ERROR`. Secure hash resources also
// should be tracked, with special handling for GC when there are no
// more clients.
//
// 
//
// Secure hash resources can also be tracked, but may need special
// attention to erase the entry when no more clients exist. 
//
// A reverse lookup index can be computed incrementally with updates
// to a dictionary. In general, the total size of the index is not 
// much larger or smaller than the original dictionary including its
// secure hash resource dependencies.
//
// 




