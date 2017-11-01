namespace Stowage
open Data.ByteString

/// Trie with bytestring keys, above Stowage. 
///
/// This is an implementation of a prefix-sharing radix tree with 
/// ability to move large subtrees into a Stowage database via a
/// compaction operation. Essentially, this allows representation
/// of large key-value databases as first-class values in limited
/// memory.
///
/// Compaction is explicit, enabling multiple updates on the tree
/// to be batched without constructing intermediate stowage nodes.
/// Decision to compact is made at bit level in a key, via IntMap.
/// Without compaction, this is essentially an in-memory trie. 
///
/// Prefix sharing for keys is useful for efficient serialization,
/// but does imply overheads for reconstructing keys on iteration.
///
/// For write-heavy workloads, try LSMTrie.
module Trie =

    /// Keys in our Trie should be short, simple bytestrings. Even
    /// with prefix sharing, try to keep keys under a few kilobytes.
    type Key = ByteString

    /// The Tree has a prefix key fragment, a possible value at that
    /// key, and a sparse array of children indexed 0..255. Large 
    /// values will be represented 
    ///
    /// This isn't a precise representation. Only the root tree
    /// may be empty, and the child array should not contain any
    /// keys outside the range 0..255. Take care to not break 
    /// tree invariants if you must use this type directly.
    type Tree<'V> = 
        { prefix    : ByteString 
          size      : uint64
          value     : CVRef<'V> option
          children  : IntMap<Tree<'V>>
        }  
 
    let inline size (t:Tree<_>) : uint64 = t.size

    let empty : Tree<_> = 
        { prefix = BS.empty 
          size = 0UL
          value = None
          children = IntMap.empty 
        }

    let isEmpty (t:Tree<_>) : bool = (0UL = size t)

    let singleton (k:Key) (v:'V) : Tree<'V> = 
        { prefix = k
          size = 1UL
          value = Some v
          children = IntMap.empty
        }

    let inline private matchPrefix prefix k = 
        (prefix = (BS.take (prefix.Length) k))

    let rec tryFind (k:Key) (t:Tree<'V>) : 'V option =
        if not (matchPrefix (t.prefix) k) then None else
        let k' = BS.drop (t.prefix.Length) k
        if (BS.isEmpty k') then (t.value) else
        let ixChild = uint64 (BS.unsafeHead k')
        match IntMap.tryFind ixChild (t.children) with
        | Some t' -> tryFind (BS.unsafeTail k') t'
        | None -> None 

    /// Check for whether a tree contains a specific key.
    let inline containsKey k t = Option.isSome (tryFind k t)

    /// Find or raise `System.Collection.Generic.KeyNotFoundException`
    let find (k:Key) (t:Tree<'V>) : 'V =
        match tryFind k t with
        | Some v -> v
        | None -> raise (System.Collections.Generic.KeyNotFoundException())

    /// Rewrite tree to ensure a specific key and its value is in 
    /// memory. This undoes compaction for a minimal subset of the
    /// tree. 
    ///
    /// Normally, touch isn't required because LVRef already will
    /// cache nodes upon lookups.
    let rec touch (k:Key) (t:Tree<'V>) : Tree<'V> =
        if not (matchPrefix (t.prefix) k) then t else
        let k' = BS.drop (t.prefix.Length) k
        if (BS.isEmpty k') then t else
        let ixChild = uint64 (BS.unsafeHead k')
        match IntMap.tryFind ixChild (t.children) with
        | None -> t
        | Some tC ->
            let tC' = touch (BS.unsafeTail k') tC
            let children' = IntMap.add ixChild tC' (t.children)
            { t with children = children' }

    /// Move entire tree from Stowage into memory.
    let rec expand (t:Tree<'V>) : Tree<'V> =
        if 
    

    /// Remove a key from the tree.
    /// 
    /// Note: This will touch  tree
    let remove (k:Key) (t:Tree<'V>) : Tree<'V> =


    /// 

    let rec add (k:Key) (v:'V) (t:Tree<'V>) : Tree<'V> =




type Trie<'V> = Trie.Tree<'V>






