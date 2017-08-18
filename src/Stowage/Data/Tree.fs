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
    type Key = Binary
    type Val = Binary
    type Node =
        | Leaf of Val                          // value in leaf
        | Inner of int * Node * Key * Node     // critbit * left * keyRight * right
        | Remote of Rsc                        // inner node in stowage
    type Tree = Option<struct(Key * Node)>

    //module EncNode =
        // Node encoding:
        //   'L'(Val) - Leaf
        //   'N'(Inner) - Inner node, inline
        //   'R'(Rsc) - Remote

       // let size (n : Node) =

    // encoder/decoder for node and tree

    /// internals are exposed, but stability is not assured
    //module Internal =
        // encode and decode nodes




    let empty : Tree = None
    let singleton (k : Key) (v : Val) : Tree = 
            assert(k.DB = v.DB)
            Some(struct(k,Leaf v))

    let isEmpty (t : Tree) : bool = 
            Option.isNone t

    /// assumes non-empty tree
    let leastKey (t : Tree) : Key =
        match t with
        | Some (struct(k,_)) -> k
        | None -> invalidOp "empty tree has no least key"

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
        







