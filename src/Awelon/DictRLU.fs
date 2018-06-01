namespace Awelon

// Given a dictionary, find all clients of a word (or annotation, etc.)
//
// Maintaining this index together with a dictionary will approximately
// double the storage costs for the dictionary. But ideally, we could 
// compute this index incrementally. 
//
// Special consideration for hierarchical dictionaries.
// 
// 
