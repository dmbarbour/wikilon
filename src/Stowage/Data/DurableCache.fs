namespace Stowage
open Data.ByteString

/// Durable Resource Cache
///
/// The motive with DCache is to support large-scale memoization
/// for incremental compilation or monadic views. The cached data
/// is held on disk (rather than in memory), and is accessed via
/// ByteString keys. If the cache has reached its quota limits, 
/// we'll gradually erase entries using heuristic techniques.
module DCache =

    /// Keys must be ByteStrings. Typically, these represent names or
    /// computations (with version information or secure hashes).
    type Key = ByteString

    // For simplicity, I rewrite the keys using a secure hash.
    //
    // This ensures data has uniform depth and distribution, which
    // in turn simplifies the exponential decay implementation. It
    // also guards against timing attacks on a shared cache.
    let inline private mangleKey k = RscHash.hash k |> BS.drop 4

    // Each element will remember its given "memory" size estimate, 
    // which may be different from encoded size. Clients should use
    // use CVRef or other reference type explicitly for larger values.
    type private E<'V> = (struct(SizeEst * 'V))

    // How should we represent the cache?
    //
    // I'm leaning towards an LSM Trie, to support efficient updates
    // and lazy deletions. A normal trie would also be acceptable. 
    //
    // Delayed parse might be useful - we could use an intermediate 
    // MCache for stored data. But it might be better to make this
    // explicit in our value types, e.g. using a lazy value encoder.


    // Our basic storage representation - a sized tree. We need the
    // sizes to help drive GC for quota management. 
    type private StowageRep<'V> =
        { data  : LSMTrie<E<'V>>
          size  : uint64            // how much data (total of SizeEst)
          count : uint64            // how many keys
        }

    let private cRep cV =
        let cT = LSMTrie.codec (EncPair.codec' (EncVarNat.codec) cV)
        { new Codec<StowageRep<'V>> with
            member cR.Write r dst =
                EncVarNat.write (r.count) dst
                EncVarNat.write (r.size) dst
                Codec.write cT (r.data) dst
            member cR.Read db src =
                let count = EncVarNat.read src
                let size = EncVarNat.read src
                let data = Codec.read cT db src
                { count = count; size = size; data = data }
            member cR.Compact db r =
                let struct(data',szData) = Codec.compactSz cT db (r.data)
                let szEst = EncVarNat.size (r.count) 
                          + EncVarNat.size (r.size)
                          + szData
                let r' = { count = (r.count); size = (r.size); data = data' }
                struct(r', szEst)
        }


    // thoughts: should I favor an LSM trie or a plain trie? A plain
    // trie has advantages of more precise size estimates, but for a
    // cache the update performance and working sets from LSM may be
    // more valuable.  
    // 
    // If I don't secure-hash keys, an IntMap based hashmap may be wise.
    //
    // I don't want to use a DB TVar directly because I don't want to
    // sync every update with other durable DB updates. Instead, I might
    // want to write after some threshold is buffered in memory, or after
    // several seconds delay. Or some other heuristic. Loss is acceptable.
    //
    // An interesting point: if Codec changes, I can presumably handle
    // this by simply erasing the keys that raised ByteStream.ReadError.
    // So cache is highly resilient to changes in type or codec. Similar
    // for missing a hash resource.

