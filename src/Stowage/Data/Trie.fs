namespace Stowage
open Data.ByteString

/// Trie with bytestring keys, above Stowage. 
///
/// This is an implementation of a radix tree with ability to push 
/// subtrees into a Stowage database. This enables first class key
/// value databases to be modeled. These trees can be larger than
/// memory, durable, potentially even distributed (depending on the
/// Stowage implementation).
///
/// For write-heavy workloads, consider the LSMTrie instead.
///
/// The implementation here pushes the complex logic to the IntMap
/// used to represent a sparse array of children. This use of IntMap
/// enables very fine-grained fanout when pushing data into stowage
/// while using a simple byte-level trie. (Otherwise, a fanout of
/// up to 256 would make for problematic compaction decisions.)
module Trie =

    /// Keys in our Trie should be short, simple bytestrings.
    /// Even with prefix sharing, try to keep keys under a few
    /// kilobytes.
    type Key = ByteString

    /// Our tree structure has a prefix, a value right at that
    /// prefix, and a sparse array of up to 256 child trees. Only
    /// the root tree should be empty since an empty child shall 
    /// be represented by absence of an entry in the child table.
    ///
    /// The Tree's implementation should be treated as internal 
    /// for most use cases, since clients can break invariants.
    /// But it's exposed if you need it.
    type Tree<'V> = 
        { prefix    : ByteString 
          size      : uint64
          value     : 'V option
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

    let singleton (k:Key) (v:'V) = 
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
        let child = uint64 (BS.unsafeHead k')
        match IntMap.tryFind child (t.children) with
        | Some t' -> tryFind (BS.unsafeTail k') t'
        | None -> None 

    /// Check for whether a tree contains a specific key.
    let inline containsKey k t = Option.isSome (tryFind k t)

    /// Find or raise `System.Collection.Generic.KeyNotFoundException`
    let find (k:Key) (t:Tree<'V>) : 'V =
        match tryFind k t with
        | Some v -> v
        | None -> raise (System.Collections.Generic.KeyNotFoundException())

    //let rec add (k:Key) (v:'V) (t:Tree<'V>) : Tree<'V> =
        



    // function to combine two keys plus the intermediate byte
    let private joinKeys (prefix:ByteString) (b:byte) (suffix:ByteString) =
        let szTotal = prefix.Length + 1 + suffix.Length
        let mem = Array.zeroCreate szTotal
        Array.blit (prefix.UnsafeArray) (prefix.Offset) mem 0 prefix.Length
        mem.[prefix.Length] <- b
        Array.blit (suffix.UnsafeArray) (suffix.Offset) mem (1 + prefix.Length) suffix.Length
        BS.unsafeCreateA mem

type Trie<'V> = Trie.Tree<'V>






