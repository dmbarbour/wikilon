namespace Awelon

// Caching for Awelon
//
// Caching is a non-trivial consideration, and I'm not certain how
// to best approach it. But to start I consider two kinds of cache:
//
// - Memory Cache: tracking data resources loaded into memory,
//   avoiding rework from load+parse operations.
//
// - Durable Cache: large scale observations such as evaluated
//   definitions, memoized computations, separate compilations.
//   Mostly held on disk rather than in memory.
//
// I'll need two different kinds of data structures to support these.
// But I think a common API could probably work well for both.
// 
observations on large data

For now, I'm splitting the
// responsibility into two variants:
//
// - Memory Cache, for local items in memory.
// 
module MemoryCache =

