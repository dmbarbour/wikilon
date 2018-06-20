namespace Awelon

// The DictRLU module augments a dictionary with a reverse lookup
// index, such that we can find the clients for each word in the 
// dictionary. This does require parsing every definition.
//
// Basically, this is represented using a meta dictionary with one
// :word.client entry for each client of a word. This allows for 
// `extractPrefix` to find all clients of a word.
//
// To deal with the hierarchical dictionaries, it's sufficient to 
// include the entries at the appropriate hierarchical layer. Like:
//
// - RLU/word.client        word has client in same dictionary
// - dict/RLU/word.client   dict internally has word as client
// - RLU/dict/word.client   dict/word has an external client
//
// To find all clients of foo/bar/baz, we look at three prefixes:
// 
// - foo/bar/RLU/baz.       internal clients of baz
// - foo/RLU/bar/baz.       clients of bar/baz within foo
// - RLU/foo/bar/baz.       clients of foo/bar/baz
//
// For implementations, we could either try to maintain our RLU index
// within our primary Awelon dictionary, or separately as a companion
// dictionary. The former option is convenient for sharing, but also
// introduces risk that there are errors in the index, and hinders
// some extensions of the reverse lookup.
//
// The `RLU/` could be represented as another `.` without ambiguity.
//
// Besides words, we can index annotations like (par), natural numbers,
// text fragments or words within texts, and so on. With some care, we
// can also provide fast lookup for words that are referenced but not
// defined, and words whose definitions fail to parse. 
//
// 
// 
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




