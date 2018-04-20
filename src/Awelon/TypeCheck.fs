namespace Awelon

// Awelon language can support static typing based on arities
// and annotations. However, Awelon does not specify any type
// model. This is because we might wish to type some programs
// using simple static types, and others using sophisticated
// dependent types, and some might require dynamic analysis.
//
// Ultimately, I'd like for most type checking to be modeled
// within the dictionary. But in the short term, this module
// can provide ad-hoc static type analysis for Awelon code.
//
// My primary concerns for type analysis will be:
//
// - static analysis of arities
// - accelerated data types
// - simple type annotations 
// - dynamic types for macros
//  


