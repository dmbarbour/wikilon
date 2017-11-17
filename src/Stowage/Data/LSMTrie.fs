namespace Stowage
open Data.ByteString

/// A Log Structured Merge (LSM) Tree above Stowage, based on Tries.
///
/// An LSM tree is an associative structure with built-in update buffers.
/// Recent updates aggregate near the tree root. This results in efficient
/// writes and longer life cycle for Stowage resources. However, it also
/// slows down lookups and results in history-dependent tree structure. 
///
/// LSM trees are suitable for write-heavy loads, or cases where reads and
/// writes tend to operate on a small working set.
/// 
/// Representing these trees in stowage allows for first-class, immutable,
/// persistent, larger-than-memory key-value databases with structured data.
/// It is easy to keep histories or model forks and branches. But performance
/// will hurt a little due to some indirection and GC overheads.
module LSMTrie =

    /// Keys in our Trie should be short, simple bytestrings. Even
    /// with prefix sharing, try to keep keys under a few kilobytes.
    type Key = ByteString

    /// Each tree node has a key fragment, a value at that key, a sparse
    /// array of children indexed 0..255, and buffered updates. 
    ///
    /// Updates are buffered for child nodes in Stowage. When we compact,
    /// we make heuristic decisions whether to flush buffered updates based
    /// on sizes.
    type Tree<'V> =
        { prefix    : ByteString
          value     : 'V option         // value, if any.
          children  : IntMap<Tree<'V>>  // tree child array (uses stowage)
          updates   : IntMap<Trie<'V option>> // updates buffered in memory
        }
        // note: compaction for updates should be performed with large
        // thresholds, which allows us to cache size estimates and avoid
        // repeated recompaction for multiple values.

    let empty : Tree<_> =
        { prefix = BS.empty
          value = None
          children = IntMap.empty
          updates = IntMap.empty
        }

    let singleton (k:Key) (v:'V) : Tree<'V> =
        { prefix = k
          value = Some v
          children = IntMap.empty
          updates = IntMap.empty
        }

    /// Test whether LSMTrie is empty. (Assuming valid structure.)
    let isEmpty (t:Tree<_>) : bool =
        Option.isNone (t.value) && IntMap.isEmpty (t.children)

    let rec private validChild k (t:Tree<_>) : bool =
        if (k > 255UL) then false else
        let validUpdate ix u = IntMap.isKeyRemote ix (t.children)
                            && not (Trie.isEmpty u) 
                            && Trie.validate u
        let validUpdates = IntMap.forall validUpdate (t.updates)
        if not validUpdates then false else
        match t.children with
        | None -> Option.isSome (t.value) // child must be non-empty
        | Some(IntMap.Leaf(k,c)) ->
            // singleton child requires value at node
            Option.isSome (t.value) && validChild k c
        | cs -> IntMap.forall validChild cs

    /// Validate tree structural invariants.
    let validate (t:Tree<_>) : bool =
        (empty = t) || (validChild 0UL t)

    // compute size of shared prefix for two strings.
    let private bytesShared (a:ByteString) (b:ByteString) : int =
        let limit = min (a.Length) (b.Length)
        let rec loop ix =
            if ((ix = limit) || (a.[ix] <> b.[ix])) then ix else
            loop (ix + 1)
        loop 0

    let rec tryFind (k:Key) (t:Tree<'V>) : 'V option =
        let n = bytesShared k (t.prefix)
        if (n <> t.prefix.Length) then None else
        if (n = k.Length) then (t.value) else
        let ix = uint64 (k.[n])
        let k' = BS.drop (n+1) k
        // Search update buffer before child nodes.
        let keyUpdate =
            match IntMap.tryFind ix (t.updates) with
            | Some u -> Trie.tryFind k' u
            | None -> None
        match keyUpdate with
        | Some vUpd -> vUpd // is `None` if removed.
        | None -> // search children recursively.
            match IntMap.tryFind ix (t.children) with
            | Some c -> tryFind k' c
            | None -> None

    


    let private updatesTrie (t:Tree<'V>) : Trie<'V option> =
        { prefix = BS.empty
          value = None
          children = t.updates
        }



type LSMTrie<'V> = LSMTrie.Tree<'V>


