namespace Stowage
open Data.ByteString

/// A key-value lookup tree above Stowage.
///
/// By modeling a key-value tree above Stowage, we essentially get first
/// class database values with a lot of nice properties: data persistence,
/// structure sharing, lightweight serialization, efficient diffs, etc..
/// Values may be structured, containing ad-hoc references.
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
    
    /// KVTree keys should be relatively small, flat bytestrings.
    /// Keys within a tree will be ordered lexicographically. 
    ///
    /// Keys larger than a kilobyte cause runtime exceptions. But
    /// you could easily apply a secure hash to such keys before
    /// adding them, resulting in a hash lookup tree.
    type Key = ByteString
    let maxKeyLen = 1024
    let inline isValidKey (k:Key) : bool = (maxKeyLen >= k.Length)

    /// KVTree values are structured binaries, limited only by the
    /// maxValLength of stowage. Larger values will automatically 
    /// be stowed upon compaction to improve sharing, while smaller
    /// values will be inlined.
    type Val = Binary
    let maxValInlineLen = 480


    type Node =
        | ILeaf of Val     // leaf value inline
        | RLeaf of Rsc     // leaf value reference
        | INode of int * Node * Key * Node  // critbit * left * keyRight * right
        | RNode of Rsc     // inner node reference

    type Tree =
        | Empty
        | Root of Key * Node

    module EncNode =
        // note that encoding is separate from compaction
        // so be careful to compact before writing...

        //  L(val)   - ILeaf
        //  l(rsc)   - RLeaf
        //  N(inode) - INode
        //  n(rsc)   - RNode
        let cILeaf : byte = byte 'L'
        let cRLeaf : byte = byte 'l'
        let cINode : byte = byte 'N'
        let cRNode : byte = byte 'n'

        let rec size (n : Node) = 
            match n with 
            | ILeaf v -> 1 + EncBin.size v
            | RLeaf r -> 1 + EncRsc.size r
            | RNode r -> 1 + EncRsc.size r
            | INode (n,l,k,r) ->
                assert(n >= 0)
                1 + EncVarNat.size (uint64 n)
                  + size l
                  + EncBytes.size k
                  + size r

        let rec write (o : System.IO.Stream) (node : Node) : unit =
            match node with
            | ILeaf v -> o.WriteByte(cILeaf); EncBin.write o v
            | RLeaf r -> o.WriteByte(cRLeaf); EncRsc.write o r
            | RNode r -> o.WriteByte(cRNode); EncRsc.write o r
            | INode (n,l,k,r) ->
                assert(n >= 0)
                o.WriteByte(cINode)
                EncVarNat.write o (uint64 n)
                write o l
                EncBytes.write o k
                write o r

        let rec read (db:DB) (i:System.IO.Stream) : Node =
            let b0 = EncByte.read i
            if(b0 = cILeaf) then ILeaf (EncBin.read db i)
            elif (b0 = cRLeaf) then RLeaf (EncRsc.read db i)
            elif (b0 = cRNode) then RNode (EncRsc.read db i)
            elif (b0 <> cINode) then failwith (sprintf "unrecognized Node %A" b0)
            else
                let n = EncVarNat.read i
                let l = read db i
                let k = EncBytes.read i
                let r = read db i
                INode (int n,l,k,r)

        // obtain DB associated with a node.
        let rec nodeDB (node : Node) : DB =
            match node with
            | ILeaf v -> v.DB
            | RLeaf r -> r.DB
            | INode (n,l,k,r) -> nodeDB l
            | RNode r -> r.DB

        let stow (db:DB) (node:Node) : Rsc =
            let o = new System.IO.MemoryStream()
            write o node
            let b = Data.ByteString.unsafeCreateA (o.ToArray())
            Rsc.Stow db b

        /// Heuristic threshold for node compaction (approx bytes)
        ///
        /// A larger compaction threshold results in less sharing
        /// but should be more efficient for most other use cases.
        /// Sharing should still be pretty good when the scale is
        /// much larger than the threshold.
        let compactThreshold = 10000

        /// Compact a node, potentially keeping many nodes inline.
        let compact (node : Node) : Node =
            let db = nodeDB node
            let rsz = 60
            let rec compact (node : Node) : struct (Node * int) = 
                match node with
                | INode (n,l,k,r) ->
                    let struct (l',szL) = compact l
                    let struct (r',szR) = compact r
                    let node' = INode (n, l', k, r')
                    let szApprox = 12 + szL + szR + k.Length  
                    if(szApprox > compactThreshold)
                        then struct (RNode (stow db node'), rsz)
                        else struct (node', szApprox)
                | ILeaf v ->
                    assert(v.DB = db)
                    if (v.Length > maxValInlineLen)
                        then struct (RLeaf (Rsc.Stow db v.Bytes), rsz)
                        else struct (node, 6 + v.Length) 
                | RNode r -> assert(r.DB = db); struct (node, rsz)
                | RLeaf r -> assert(r.DB = db); struct (node, rsz)
            let struct (node', szApprox) = compact node
            node'

        let parse (db:DB) (v:Stowage.Val) : Node =
            read db (EncBytes.toInputStream v)

        let expand (r:Rsc) : Node =
            let result = parse (r.DB) (r.Load())
            System.GC.KeepAlive(r)
            result

    // E | R(key)(node)
    module EncTree =
        let cEmpty : byte = byte 'E'
        let cRoot : byte = byte 'R'

        let size (t : Tree) : int = 
            match t with
            | Empty -> 1
            | Root (k, n) -> 1 + EncBytes.size k + EncNode.size n

        let write (o : System.IO.Stream) (t : Tree) : unit =
            match t with
            | Empty -> o.WriteByte(cEmpty)
            | Root (k,n) -> 
                o.WriteByte(cRoot)
                EncBytes.write o k
                EncNode.write o n
        
        let read (db : DB) (i : System.IO.Stream) : Tree =
            let b0 = EncByte.read i
            if(b0 = cEmpty) then Empty 
            elif(b0 <> cRoot) then failwith (sprintf "unrecognized Tree %A" b0)
            else
                let k = EncBytes.read i
                let n = EncNode.read db i
                Root (k, n)

        let compact (tree : Tree) : Tree =
            match tree with
            | Empty -> Empty
            | Root (k,v) -> Root (k, EncNode.compact v)

    
        


    let empty : Tree = Empty
    let singleton (k : Key) (v : Val) : Tree = 
        if(not (isValidKey k)) 
            then invalidArg "k" "key too large"
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
        







