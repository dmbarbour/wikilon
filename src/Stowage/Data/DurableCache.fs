namespace Stowage
open Data.ByteString

/// Durable Resource Cache
///
/// The motive with DCache is to support large-scale memoization,
/// for incremental compilation or monotonic indices. The cached
/// data is held on disk and accessed through ByteString keys. If
/// the cache has reached its quota limits, we'll gradually erase
/// entries using exponential decay techniques.
module DCache =

    /// Keys must be ByteStrings. These might represent URL paths or
    /// computations. A cache can be associated with mutable data, 
    /// but it must be data where lost keys can be recomputed or the
    /// loss is a minor concern.
    type Key = ByteString

    // For simplicity, I rewrite durable keys using a secure hash.
    // This ensures data has uniform depth and distribution, which
    // in turn simplifies the exponential decay implementation. It
    // also guards against timing attacks on a shared cache.
    let inline private mangleKey k = RscHash.hash k |> BS.drop 4

    // Each element will remember its given "memory" size estimate, 
    // which may be different from encoded size. Items with large
    // encodings will be referenced indirectly.
    type private E<'V> = (struct(SizeEst * CVRef<'V>))

    // Our basic storage representation - a sized tree. We need the
    // sizes to help drive GC for quota management. 
    type private Rep<'V> =
        { data  : LSMTrie<E<'V>>
          size  : uint64            // how much data (total of SizeEst)
          count : uint64            // how many keys
        }

    // thoughts: should I favor an LSM trie or a plain trie? A normal
    // trie has the advantage that our size estimate is more accurate
    // a reflection of actual storage costs, but has higher costs for
    // adding new elements. Although, this isn't too bad because we'd
    // still buffer updates in memory.
    //
    // LSM trie has advantage of better long-term use of nodes, and it
    // avoids loading remote values permanently into memory while we
    // await compaction. Importantly, it keeps adds relatively cheap
    // since it doesn't rewrite every node. W

    // TODO: the durable representation should update independently
    // of the underlying database, i.e. such that we don't synch with
    // durable transactions on the database. It's assumed that cache
    // is not fully durable.



