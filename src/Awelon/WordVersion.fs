namespace Awelon

// We can associate an Awelon program with a deep, behavioral version
// by taking the secure hash of the program together with versions of
// its dependencies. Doing so is rather useful, and the result is easy
// to cache and use for memoization of other details (such as inferred
// types or evaluations).
//
// Although we could keep a permanent index of versions, it might prove
// simpler to recompute versions as needed. The cost is predictable and
// we can still benefit from temporary caching. 

