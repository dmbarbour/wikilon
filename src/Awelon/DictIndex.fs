namespace Awelon

// DictIndex associates an Awelon Dictionary with two indices:
//
//  version index: Word → Version
//  reverse lookup index: Symbol → Clients
//
// These two indices will be maintained together with every update
// to the dictionary. Although, I'm interested in asynchronous or 
// lazy maintenance of these indices where we can get away with it.
// So we might further track unprocessed updates.
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
