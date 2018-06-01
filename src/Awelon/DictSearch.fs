namespace Awelon


// The goal for DictSearch is to model an index data structure
// with the following features:
//
// - we can index each dictionary node independently
// - we can efficiently merge indices on Dict nodes
// - we can store computed values - types, versions
//
// Ideally, we can achieve these features using one index model.
// This would allow efficient indexing across multiple versions
// of a dictionary.
//
// Computing values is difficult because the value computed at
// a node may depend on other nodes in its context. 
// 
// If we achieve these features together, we can maintain our
// indices across multiple versions of a dictionary, and produce
// a new index as needed. 
//
// One viable option is to maintain global index on secure hash
// resources
// hash resources to their 
//   
//
// Besides search, I also want effective caching of Dict 

