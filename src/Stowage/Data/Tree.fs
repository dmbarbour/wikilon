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
    
    /// KVTree keys are flat bytestrings up to a few kilobytes. 
    /// Ideally, most keys should be much smaller than this. No
    /// structure sharing is performed for keys.
    type Key = ByteString
    let maxKeyLen = 4096
    let isValidKey (k:Key) : bool = (maxKeyLen >= k.Length) 

    /// KVTree values are structured binaries, limited only by the
    /// maxValLength of stowage. Larger values will automatically 
    /// be stowed upon compaction to improve sharing, while smaller
    /// values will be inlined.
    type Val = Binary
    let maxValInlineLen = 480

    /// A Critbit indicates a bit-level offset into a key. 
    ///
    /// To simplify some logic, we logically inject an extra 1 bit
    /// before each byte within the key. Hence bit 9 indicates if a
    /// key has two bytes, and bit 10 is the high bit of the second
    /// byte. 
    type Critbit = int

    let inline private keyElem (k : Key) (ix : int) : uint16 =
        if (ix >= k.Length) then 0us else (256us ||| uint16 k.[ix])

    let testCritbit (cb : Critbit) (k : Key) : bool =
        assert(cb >= 0)
        let byteOff = cb / 9
        let bitOff = 8 - (cb % 9)
        (0us <> (1us &&& ((keyElem k byteOff) >>> bitOff)))

    // returns high bit for a uint16
    let private highBitIndex (x0:uint16) : int =
        let r0 = 0
        let (r1,x1) = if (0us <> (0xFF00us &&& x0)) then (r0+8, x0>>>8) else (r0,x0)
        let (r2,x2) = if (0us <> (0x00F0us &&& x1)) then (r1+4, x1>>>4) else (r1,x1)
        let (r3,x3) = if (0us <> (0x000Cus &&& x2)) then (r2+2, x2>>>2) else (r2,x2)
        if(0us <> (0x02us &&& x3)) then (1+r3) else r3

    /// Find first critbit between keys equal or greater to specified critbit.
    /// May return None if there are no differences after the specified bit.
    let findCritbit (cbMin : Critbit) (a:Key) (b:Key) : Critbit option =
        assert(cbMin >= 0)
        let len = max a.Length b.Length
        let off = cbMin / 9
        if (off >= len) then None else
        let mask0 = (0x1FFus >>> (cbMin % 9)) // hide high bits in first byte
        let x0 = mask0 &&& ((keyElem a off) ^^^ (keyElem b off))
        if (0us <> x0) then Some((9 * off) + (8 - highBitIndex x0)) else
        let rec loop ix =
            if(ix = len) then None else
            let x = ((keyElem a ix) ^^^ (keyElem b ix))
            if(0us <> x) then Some((9 * ix) + (8 - highBitIndex x)) else
            loop (1 + ix)
        loop (1+off)

    /// KVTree nodes are based on a modified critbit tree. The main
    /// difference is that the least key is held in the parent to
    /// support efficient tree-level merge actions.
    type Node =
        | ILeaf of Val     // leaf value inline
        | RLeaf of Rsc     // leaf value reference
        | INode of Critbit * Node * Key * Node  // critbit * left * keyRight * right
        | RNode of Rsc     // inner node reference

    /// A non-empty tree is simply the least key and the root node.
    type Tree =
        | Empty
        | Root of Key * Node

    module EncNode =
        // encoding is separated from compaction, and multiple nodes
        // may freely be inlined into one binary.

        //  L(val)   - ILeaf
        //  l(rsc)   - RLeaf
        //  N(inode) - INode
        //  n(rsc)   - RNode
        let cILeaf : byte = byte 'L'
        let cRLeaf : byte = byte 'l'
        let cINode : byte = byte 'N'
        let cRNode : byte = byte 'n'

        let rec size (node : Node) = 
            match node with 
            | ILeaf v -> 1 + EncBin.size v
            | RLeaf r -> 1 + EncRsc.size r
            | RNode r -> 1 + EncRsc.size r
            | INode (n,l,k,r) ->
                1 + EncVarNat.size (uint64 n)
                  + size l
                  + EncBytes.size k
                  + size r

        let rec write (o : System.IO.Stream) (node : Node) : unit =
            match node with
            | ILeaf v -> EncByte.write o cILeaf; EncBin.write o v
            | RLeaf r -> EncByte.write o cRLeaf; EncRsc.write o r
            | RNode r -> EncByte.write o cRNode; EncRsc.write o r
            | INode (n,l,k,r) ->
                assert(isValidKey k)
                EncByte.write o cINode
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
                assert(isValidKey k)
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
        /// A large compaction threshold results in less sharing
        /// of leaf nodes, but should be more efficient in most 
        /// other use cases. Sharing should still be pretty good
        /// if the scale is large and most updates are localized
        /// to a key prefix.
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

        // zero-copy access? maybe later.

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


    /// Lookup value (if any) associated with a key.
    let tryFind (t : Tree) (k : Key) : Val option =
        //let rec fn kMin node =
        //    rais
        raise (System.NotImplementedException())   

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
        







