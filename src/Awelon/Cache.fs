namespace Awelon

// Caching for Awelon
//
// Caching is a non-trivial consideration, and I'm not certain how
// to best approach it. But to start I consider two kinds of cache:
//
// - Memory Cache: track external resources loaded into memory and
//   perhaps processed to some degree. This allows us to avoid
//   rework, but has small global capacity set via Stowage.Cache. 
//
// - Durable Cache: a large-scale durable, external memory for
//   computed results, potentially maintained over time.
//
// I'll need two different kinds of data structures to support these.
// But I think a common API could probably work well for both.
// 


