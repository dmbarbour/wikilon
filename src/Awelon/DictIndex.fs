namespace Awelon

// These modules augment an Awelon Dictionary with two primary 
// indices:
//
//  reverse lookup index: symbol → client words
//  version index: word/resource → deep version
//
// A reverse lookup index can be incrementally computed for each
// dictionary. A version index may further be computed, but may
// cascade in some cases (e.g. update initial state of a command
// pattern or REPL session). It might be worthwhile to simply 
// compute 
//
// These indices can be maintained incrementally with dictionary 
// updates, and have a predictable overhead (in space and time).
// The reverse lookup is very useful: it can also be applied to
// find erroneous (undefined, cyclic, unparseable) words, and as
// a way for developers to add key-word or tag search.
//
// The version identifier can then be used for secondary indices
// valid across dictionaries, e.g. mapping a version to evaluated
// definition (perhaps indirectly: version to evaluated version,
// version to definition). Observations indexed on version would
// be valid across multiple similar dictionaries, and could also
// be incremental.
//
// Incremental indexing could be made asynchronous by seperately
// tracking a dictionary and its recent (not yet indexed) updates.
// This would be valuable for ensuring efficient imports and that
// 
// 
//
//
// The two indices chosen would be the most useful for common processing
// of the dictionaries. Computing a unique version for each definition is
// a useful basis for cached evaluations, for example.
//
// A reverse lookup index is not constrained to finding words. We could
// include annotations, numbers, and even full-text fragments. Erroneous
// definitions (cycles, undefined symbols, etc.) may also be recorded for
// easier search and debugging.
//
// 

//module DictX 
