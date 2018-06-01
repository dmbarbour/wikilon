namespace Awelon

// Goal: Compute a deep, unique version identifier for each word.
// Motive: A deep version for a word can be associated with its:
//
//  - definition 
//  - evaluation
//  - function type
//  - optimized forms (link optimized)
//  - compiled forms
//
// Additionally, we can feasibly index dictionaries based on the
// specific versions of words within the dictionary. That is, we
// can have a reverse lookup that is version-specific. 

