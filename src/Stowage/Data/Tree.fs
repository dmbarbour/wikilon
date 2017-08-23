namespace Stowage
open Data.ByteString

/// A key-value lookup tree above Stowage.
///
/// By modeling a key-value tree above Stowage, we essentially get first
/// class database values with a lot of nice properties: data persistence,
/// structure sharing, lightweight serialization, efficient diffs, etc..
/// Both keys and values may have further structure.
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
    
    /// KVTree keys are structured binaries with a maximum size. Keys
    /// are oft replicated between trees, so it's best to keep them
    /// small. 
    type Key = Binary
    let maxKeyLen = 1000
    let inline isValidKey (k:Key) : bool = (maxKeyLen >= k.Length)
        
    /// KVTree values are structured binaries, limited only by the
    /// maxValLength of stowage. Larger values will automatically
    /// be stowed upon compaction.
    type Val = Binary
    let maxValInlineLen = 500

    type Node =
        | ILeaf of Val                      // leaf value inline
        | RLeaf of Rsc                      // leaf value reference
        | INode of int * Node * Key * Node  // critbit * left * keyRight * right
        | RNode of Rsc                      // inner node reference

    // Node encoding:
    //  L(val)   - ILeaf
    //  l(rsc)   - RLeaf
    //  N(inode) - INode
    //  n(rsc)   - RNode
    module EncNode =

        let rec size (n : Node) = 
            let szData =
                match n with 
                | ILeaf v -> EncBin.size v
                | RLeaf _ -> EncRsc.size
                | INode (n,l,k,r) ->
                    assert(n >= 0)
                    EncVarNat.size (uint64 n)
                      + size l
                      + EncBin.size k
                      + size r
                | RNode _ -> EncRsc.size
            (1 + szData)

        let rec write (o : System.IO.Stream) (n : Node) : unit =
            raise (System.NotImplementedException())

        let rec read (db:DB) (i:System.IO.Stream) : Node =
            raise (System.NotImplementedException())

    type Tree =
        | Empty
        | Root of Key * Node

    // E | R(key)(node)
    module EncTree =
        let size (t : Tree) : int = 
            let szData =
                match t with
                | Empty -> 0
                | Root (k, n) -> EncBin.size k + EncNode.size n
            (1 + szData)

        let write (o : System.IO.Stream) (t : Tree) : unit =
            match t with
            | Empty -> o.WriteByte(69uy)
            | Root (k,n) -> 
                o.WriteByte(82uy)
                EncBin.write o k
                EncNode.write o n
        
        let read (db : DB) (i : System.IO.Stream) : Tree =
            raise (System.NotImplementedException())



    let empty : Tree = Empty
    let singleton (k : Key) (v : Val) : Tree = 
        assert(k.DB = v.DB)
        Root (k, ILeaf v)

    let isEmpty (t : Tree) : bool =
        match t with
        | Empty -> true
        | _ -> false

    /// assumes non-empty tree
    let leastKey (t : Tree) : Key =
        match t with
        | Empty -> invalidOp "empty tree"
        | Root (k, _) -> k

(*
    let greatestKey (t : Tree) : Key =
        let rec loop k n = 
            match n with
            | Leaf _ -> k
            | Inner (_,_,k',n') -> loop k' n'
            | Remote ref -> loop (loadNode ref)
*)

    // compact trees

    // merge trees
    // diff trees

    // let inline mergeWith (merge : Val -> Val -> Val) : Tree -> Tree -> Tree


type KVTree = KVT.Tree
        







