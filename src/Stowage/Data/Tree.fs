namespace Stowage
open Data.ByteString

/// A key-value lookup tree above Stowage.
///
/// By modeling a key-value tree above Stowage, we essentially get first
/// class database values with a lot of nice properties: data persistence,
/// structure sharing, lightweight serialization, efficient diffs, etc..
///
/// The KVT module provides a simple history-independent tree, a variant of
/// a critbit tree. Because its structure is independent of history, it is
/// easy to reason about structure sharing. On the other hand, updates will
/// often require writing many new nodes. To mitigate the latter issue, this
/// tree requires explicit compaction, such that we can update the tree many
/// times then compact it as one larger batch.
///
/// Aside: it may be worthwhile to introduce a history-dependent tree, such
/// as an LSM-tree, to improve insert/delete performance.
module KVT =
    type Key = VRef
    type Val = VRef
    type Rsc = VRef

    // ByteString encoding, used for Key/Val/Rsc.
    //
    // We need to protect any resource hashes, so we'll use the following
    // encoding:
    //   (size)(bytes)~ 
    //     when size >= rscHashLen and (bytes) ends with rscHashByte
    //   (size)(bytes) otherwise
    // (size) is a VarNat

    type Node =
        | Leaf of Val                          // value in leaf
        | Inner of int * Node * Key * Node     // critbit * key * left * right
        | Remote of Rsc                        // just a RscHash

    type Root = List<struct(Key * Node)>
    



//type KVTree = KVT.Tree
        







