namespace Stowage
open Data.ByteString

/// A key-value critbit tree above Stowage.
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
    
    /// KVTree keys and values are potentially structured binaries.
    ///
    /// Note: keys and values mustn't be too large or compaction will
    /// fail. A few megabytes each is safe but inefficient. Keep keys
    /// and values under a few kilobytes each for best performance.
    type Key = Binary
    type Val = Binary

    /// A Critbit indicates a bit-level offset into a key. 
    ///
    /// To simplify some logic, we logically inject an extra 1 bit
    /// before each byte within the key. Hence bit 9 indicates if a
    /// key has two bytes, and bit 10 is the high bit of the second
    /// byte. 
    type Critbit = int

    let inline private keyElem (k : Key) (ix : int) : uint16 =
        if (ix >= k.Length) then 0us else (0x100us ||| uint16 k.[ix])

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
    ///
    /// Although the Node type is exposed, the expectation is that
    /// clients shouldn't directly construct nodes (otherwise, it is
    /// possible to create ill-formed trees). 
    type Node =
        | Leaf of Val     // leaf value inline
        | INode of Critbit * Node * Key * Node  // critbit * left * keyRight * right
        | RNode of Rsc     // inner node reference

    /// A non-empty tree is simply the least key and the root node.
    type Tree =
        | Empty
        | Root of Key * Node

    module EncNode =
        // encoding is separated from compaction, and multiple nodes
        // may freely be inlined into one binary.

        //  L(val)   - Leaf
        //  N(inode) - INode
        //  R(rsc)   - RNode
        let cLeaf  : byte = byte 'L'
        let cINode : byte = byte 'N'
        let cRNode : byte = byte 'R'

        let rec size (node : Node) = 
            match node with 
            | Leaf v -> 1 + EncBin.size v
            | RNode r -> 1 + EncRsc.size
            | INode (cb,l,k,r) ->
                1 + EncVarNat.size (uint64 cb)
                  + size l
                  + EncBin.size k
                  + size r

        let rec write (o : System.IO.Stream) (node : Node) : unit =
            match node with
            | Leaf v -> EncByte.write o cLeaf; EncBin.write o v
            | RNode r -> EncByte.write o cRNode; EncRsc.write o r
            | INode (cb,l,k,r) ->
                EncByte.write o cINode
                EncVarNat.write o (uint64 cb)
                write o l
                EncBin.write o k
                write o r

        let rec read (db:DB) (i:System.IO.Stream) : Node =
            let b0 = EncByte.read i
            if(b0 = cLeaf) then Leaf (EncBin.read db i)
            elif (b0 = cRNode) then RNode (EncRsc.read db i)
            elif (b0 <> cINode) then failwith (sprintf "unrecognized Node %A" b0)
            else
                let cb = EncVarNat.read i
                if(cb > uint64 System.Int32.MaxValue) then failwith "critbit overflow"
                let l = read db i
                let k = EncBin.read db i
                let r = read db i
                INode (int cb,l,k,r)

        let stow (db:DB) (node:Node) : Rsc =
            use o = new System.IO.MemoryStream()
            write o node
            Rsc.Stow db (Data.ByteString.unsafeCreateA (o.ToArray()))

        /// Compaction threshold determines a serialization size above
        /// which we compact our tree nodes. A larger compaction threshold
        /// reduces sharing for small subtrees, but should be more efficient
        /// when working with very small keys and values.
        let compactThreshold : int = 10000

        let rec compactSz (db:DB) (node:Node) : struct (Node * int) =
            match node with
            | RNode rsc -> assert(db = rsc.DB); struct(node, 1 + EncRsc.size)
            | Leaf v -> assert(db = v.DB); struct(node, 1 + EncBin.size v)
            | INode (cb, l, kr, r) ->
                let struct (l', szL) = compactSz db l
                let struct (r', szR) = compactSz db r
                let node' = INode (cb, l', kr, r')
                let szNode' = 1 + EncVarNat.size (uint64 cb) + szL + EncBin.size kr + szR
                if (szNode' > compactThreshold)
                    then struct (RNode (stow db node'), 1 + EncRsc.size)
                    else struct (node', szNode')

        let inline compact (db:DB) (node:Node) : Node =
            let struct(node', szNode') = compactSz db node
            node'

        let inline parse (db:DB) (v:Stowage.Val) : Node =
            read db (EncBytes.toInputStream v)

        let expand (r:Rsc) : Node =
            let result = parse (r.DB) (loadRscDB r.DB r.ID)
            System.GC.KeepAlive(r)
            result

        // zero-copy access? maybe later. I could potentially use
        // an UnmanagedMemoryStream, and potential savings would be
        // reduced parsing and construction overheads on tryFind.

    let rec private validateN (mcb:Critbit) (kl:Key) (node:Node) =
        match node with
        | Leaf v -> (kl.DB = v.DB)
        | RNode rsc -> (kl.DB = rsc.DB) && (validateN mcb kl (EncNode.expand rsc))
        | INode (cb, l, kr, r) ->
            (kl.DB = kr.DB) 
                && ((Some cb) = findCritbit mcb kl kr)
                && (testCritbit cb kr)
                && (validateN (1+cb) kl l)
                && (validateN (1+cb) kr r)

    /// validate structural invariants of tree
    let validate (t:Tree) : bool =
        match t with
        | Empty -> true
        | Root(kl,n) -> validateN 0 kl n

    /// Compact tree in memory. This can be delayed until after many
    /// tree updates to avoid stowage of intermediate nodes.
    let compact (t : Tree) : Tree =
        match t with
        | Empty -> Empty
        | Root (k,n) -> Root(k, EncNode.compact (k.DB) n)

    /// Write tree to 'nullable' resource. This only writes to disk
    /// for non-empty tree. Implicitly compacts the tree to control
    /// maximum node sizes.
    let stow (t : Tree) : Rsc option =
        match compact t with
        | Empty -> None
        | Root(kl,node) as tc ->
            use o = new System.IO.MemoryStream()
            EncBin.write o kl
            EncNode.write o node
            let r = Rsc.Stow (kl.DB) (Data.ByteString.unsafeCreateA (o.ToArray()))
            System.GC.KeepAlive tc
            Some r

    /// Load tree (partially) into memory from a nullable resource.
    let load (rt : Rsc option) : Tree =
        match rt with
        | None -> Empty
        | Some r ->
            let i = EncBytes.toInputStream (loadRscDB r.DB r.ID)
            let k = EncBin.read (r.DB) i
            let v = EncNode.read (r.DB) i
            System.GC.KeepAlive r
            Root (k,v)

    let empty : Tree = Empty
    let singleton (k : Key) (v : Val) : Tree = Root (k, Leaf v)
    let isEmpty (t : Tree) : bool =
        match t with
        | Empty -> true
        | Root _ -> false

    // update existing least-key value
    let rec private setLKV (v:Val) (node:Node) : Node =
        match node with
        | INode (cb, l, k, r) -> INode (cb, setLKV v l, k, r)
        | RNode rsc -> setLKV v (EncNode.expand rsc)
        | Leaf _ -> Leaf v

    // insert a new least-key value, at given critbit
    let rec private addLKV (ncb:Critbit) (oldLK:Key) (v:Val) (node:Node) : Node =
        match node with
        | INode (cb, l, kr, r) when (cb < ncb) ->
            assert(not (testCritbit cb oldLK))
            INode (cb, addLKV ncb oldLK v l, kr, r) 
        | RNode rsc -> addLKV ncb oldLK v (EncNode.expand rsc)
        | _ -> 
            assert(testCritbit ncb oldLK) 
            INode (ncb, Leaf v, oldLK, node) 

    // add or update key-value element somewhere to right of least-key
    //  uses a critbit relative to least-key for efficient insertion
    let rec private addRKV (mcb:Critbit) (k:Key) (v:Val) (node:Node) : Node =
        match node with
        | INode (cb, l, kr, r) ->
            if (cb < mcb) then // diff after node
                assert(not (testCritbit cb k)) 
                INode (cb, addRKV mcb k v l, kr, r) 
            elif (cb > mcb) then // diff before node
                assert(not (testCritbit mcb kr))
                INode (mcb, node, k, Leaf v)
            else // diff aligns with kr
                match findCritbit (1+cb) k kr with
                | Some ncb ->
                    if testCritbit ncb k
                        then INode (cb, l, kr, addRKV ncb k v r)
                        else INode (cb, l, k, addLKV ncb kr v r)
                | None -> INode (cb, l, kr, setLKV v r) // update at kr
        | RNode rsc -> addRKV mcb k v (EncNode.expand rsc)
        | Leaf _ -> INode (mcb, node, k, Leaf v)

    /// Add key-value to the tree, or update existing value.
    let add (nk : Key) (nv : Val) (t : Tree) : Tree =
        match t with
        | Root (tk, tn) ->
            match findCritbit 0 nk tk with
            | Some cb -> 
                if testCritbit cb nk
                    then Root (tk, addRKV cb nk nv tn) // add to right of least-key
                    else Root (nk, addLKV cb tk nv tn) // new least-key
            | None -> Root (tk, setLKV nv tn) // update least-key
        | Empty -> Root (nk, Leaf nv)

    // TODO: efficient union of trees

    // remove least-key value for a node, return a tree.
    let rec private removeLKV (node:Node) : Tree =
        match node with
        | INode (cb, l, kr, r) ->
            match removeLKV l with
            | Empty -> Root(kr, r)
            | Root(kl', l') -> Root(kl', INode(cb, l', kr, r))
        | RNode rsc -> removeLKV (EncNode.expand rsc)
        | Leaf _  -> Empty // key removed

    // remove a key to right of least-key (if present). 
    //  Use critbit relative to least-key for efficient deletion.
    let rec private removeRKV (mcb:Critbit) (k:Key) (node:Node) : Node =
        match node with
        | INode (cb, l, kr, r) ->
            if (cb < mcb) then
                assert(not (testCritbit cb k))
                INode(cb, removeRKV mcb k l, kr, r)
            elif (cb > mcb) then node // key not present
            else match findCritbit (1+cb) k kr with
                 | Some ncb -> 
                    if not (testCritbit ncb k) then node else
                    INode (cb, l, kr, removeRKV ncb k r)
                 | None -> // remove leftmost node
                    match removeLKV r with
                    | Empty -> l 
                    | Root(kr',r') -> INode (cb, l, kr', r')
        | RNode rsc -> removeRKV mcb k (EncNode.expand rsc)
        | Leaf _ -> node // key not present

    /// Remove key from tree if present. 
    let remove (k : Key) (t:Tree) : Tree =
        match t with
        | Root (kl, node) -> 
            match findCritbit 0 k kl with
            | Some cb -> 
                if not (testCritbit cb k) then t else // less than kl
                Root (kl, removeRKV cb k node) // greater than kl
            | None -> removeLKV node // equal to kl, remove least key
        | Empty -> Empty

    // recursive search for a key
    let rec private tryFindN (k:Key) (kl:Key) (node:Node) : Val option =
        match node with
        | INode (cb, l, kr, r) -> 
            if testCritbit cb k
                then tryFindN k kr r
                else tryFindN k kl l
        | RNode r when (k >= kl) -> tryFindN k kl (EncNode.expand r)
        | Leaf v when (k = kl) -> Some v
        | _ -> None

    /// Lookup value (if any) associated with a key. This may load
    /// tree nodes into memory, but doesn't hold onto them. So if
    /// you need to lookup a value many times, or plan to modify it
    /// afterwards, consider use of `touch` to pre-load the data.
    let tryFind (k : Key) (t : Tree) : Val option =
        match t with
        | Root (kl,node) -> tryFindN k kl node
        | Empty -> None

    /// Lookup value associated with key. 
    /// If no binding, raises System.Collections.Generic.KeyNotFoundException.
    let find (k : Key) (t : Tree) : Val =
        match tryFind k t with
        | Some v -> v
        | None -> raise (System.Collections.Generic.KeyNotFoundException())

    let rec private touchN (k : Key) (kl : Key) (node : Node) : Node =
        match node with
        | INode (cb, l, kr, r) ->
            if testCritbit cb k
                then INode(cb, l, kr, (touchN k kr r))
                else INode(cb, (touchN k kl l), kr, r)
        | RNode r when (k >= kl) -> touchN k kl (EncNode.expand r)
        | _ -> node

    /// Touch a key, loading into RAM everything needed for value lookup.
    let touch (t : Tree) (k : Key) : Tree =
        match t with
        | Empty -> Empty
        | Root (kl,node) -> Root(kl, touchN k kl node)

    let rec private fullyExpandN (node : Node) : Node =
        match node with
        | INode (cb, l, k, r) -> INode (cb, fullyExpandN l, k, fullyExpandN r)
        | RNode r -> fullyExpandN (EncNode.expand r)
        | Leaf _ -> node

    /// Load entire tree into RAM. 
    let fullyExpand (t : Tree) : Tree =
        match t with
        | Empty -> Empty
        | Root (k, n) -> Root (k, fullyExpandN n)

    /// leastKey (leftmost key) requires non-empty tree, O(1)
    let leastKey (t : Tree) : Key =
        match t with
        | Empty -> invalidArg "t" "empty tree"
        | Root (k, _) -> k

    /// Remove least-key from tree.
    /// (Slightly more efficient than `remove (leastKey t) t`.)
    let removeLeastKey (t:Tree) : Tree =
        match t with
        | Root (_, node) -> removeLKV node
        | Empty -> Empty

    let rec private greatestKeyN (k : Key) (n : Node) : Key =
        match n with
        | INode (_, _, k', r) -> greatestKeyN k' r
        | RNode rsc -> greatestKeyN k (EncNode.expand rsc)
        | _ -> k

    /// greatestKey requires non-empty tree, O(KeyLength)
    let greatestKey (t : Tree) : Key =
        match t with
        | Empty -> invalidArg "t" "empty tree"
        | Root (k, n) -> greatestKeyN k n


    // Enumeration of Tree Elements
    module private EnumNode =
        type Stack = List<(Key * Node)>
        type Elem = (Key * Val)
        type EnumState = (Elem * Stack)
        
        let rec enterL (s : Stack) (k:Key) (n:Node) : (Elem * Stack) =
            match n with
            | INode (_, l, kr, r) -> enterL ((kr,r)::s) k l
            | RNode rsc -> enterL s k (EncNode.expand rsc)
            | Leaf v -> ((k,v),s)
            
        let rec enterR (s : Stack) (k:Key) (n:Node) : (Elem * Stack) =
            match n with
            | INode (_, l, kr, r) -> enterR ((k,l)::s) kr r
            | RNode rsc -> enterR s k (EncNode.expand rsc)
            | Leaf v -> ((k,v),s)

        type EnumeratorL = // enumerates left to right
            val mutable private st : EnumState
            member e.Elem with get() = fst e.st
            member e.Stack with get() = snd e.st
            new (st0:EnumState) = { st = st0 }
            interface System.Collections.Generic.IEnumerator<(Key * Val)> with
                member e.Current with get() = e.Elem
            interface System.Collections.IEnumerator with
                member e.Current with get() = upcast e.Elem
                member e.MoveNext() =
                    match e.Stack with
                    | ((kr,r)::sr) ->
                        e.st <- enterL sr kr r
                        true
                    | _ -> false
                member e.Reset() = raise (System.NotSupportedException())
            interface System.IDisposable with
                member e.Dispose() = ()

        type EnumerableL =
            val private k : Key
            val private n : Node
            new(k:Key, n:Node) = { k = k; n = n }
            member e.GetEnum() = new EnumeratorL(enterL List.empty e.k e.n)
            interface System.Collections.Generic.IEnumerable<(Key*Val)> with
                member e.GetEnumerator() = upcast e.GetEnum()
            interface System.Collections.IEnumerable with
                member e.GetEnumerator() = upcast e.GetEnum()

        type EnumeratorR = // enumerates right to left
            val mutable private st : EnumState
            member e.Elem with get() = fst e.st
            member e.Stack with get() = snd e.st
            new (st0:EnumState) = { st = st0 }
            interface System.Collections.Generic.IEnumerator<(Key * Val)> with
                member e.Current with get() = e.Elem
            interface System.Collections.IEnumerator with
                member e.Current with get() = upcast e.Elem
                member e.MoveNext() =
                    match e.Stack with
                    | ((kl,l)::sl) ->
                        e.st <- enterR sl kl l
                        true
                    | _ -> false
                member e.Reset() = raise (System.NotSupportedException())
            interface System.IDisposable with
                member e.Dispose() = ()

        type EnumerableR =
            val private k : Key
            val private n : Node
            new(k:Key, n:Node) = { k = k; n = n }
            member e.GetEnum() = new EnumeratorR(enterR List.empty e.k e.n)
            interface System.Collections.Generic.IEnumerable<(Key*Val)> with
                member e.GetEnumerator() = upcast e.GetEnum()
            interface System.Collections.IEnumerable with
                member e.GetEnumerator() = upcast e.GetEnum()

    /// sequence ordered from least key to greatest key
    let toSeq (t : Tree) : seq<(Key * Val)> =
        match t with
        | Empty -> Seq.empty
        | Root(k,n) -> upcast EnumNode.EnumerableL(k,n)

    /// reverse-ordered sequence, greatest key to least key
    let toSeqR (t : Tree) : seq<(Key * Val)> =
        match t with
        | Empty -> Seq.empty
        | Root(k,n) -> upcast EnumNode.EnumerableR(k,n)

    let inline fold (fn : 'St -> Key -> Val -> 'St) (s0 : 'St) (t : Tree) : 'St =
        Seq.fold (fun s (k,v) -> fn s k v) s0 (toSeq t)

    let inline foldBack (fn : Key -> Val -> 'St -> 'St) (t : Tree) (s0 : 'St) : 'St =
        Seq.fold (fun s (k,v) -> fn k v s) s0 (toSeqR t)

    let inline private matchPrefix (prefix : ByteString) (key : Key) =
        (prefix = (Data.ByteString.take prefix.Length key.Bytes))

    // union after filtering trees, assumes in-order elements, preserves critbit 
    let private unionF (cb : int) (a:Tree) (b:Tree) : Tree = 
        match a with
        | Empty -> b
        | Root(ak,an) -> 
            match b with
            | Empty -> a 
            | Root(bk, bn) -> Root(ak, INode (cb, an, bk, bn))
         
    /// Filter by Prefix.
    let rec selectPrefix (prefix : ByteString) (t : Tree) : Tree =
        match t with
        | Empty -> Empty
        | Root(kl, node) ->
            if (prefix < kl.Bytes) then Empty else
            match node with
            | INode (cb, l, kr, r) ->
                let tInPrefix = (cb >= (9 * prefix.Length)) && 
                                (matchPrefix prefix kl)
                if tInPrefix then t else
                let a = selectPrefix prefix (Root(kl,l))
                let b = selectPrefix prefix (Root(kr,r))
                unionF cb a b
            | RNode rsc -> selectPrefix prefix (Root(kl, EncNode.expand rsc))
            | leaf -> if matchPrefix prefix kl then t else Empty
    // TODO: I think selectPrefix could be heavily optimized...

    /// Right-biased union of trees. 
    /// 
    /// Here the 'right bias' means we'll keep the value from the
    /// right hand tree whenever both trees have a value for a key.
    (*
    let union (a:Tree) (b:Tree) : Tree =
        match (a,b) with
        | Empty -> b
        | Root(ak,an) ->
            match b with
            | Empty -> a
            | Root(bk, bn) ->
                let cb 
                unionR 
    *)          



    /// Partition tree on key. This returns two trees, such that
    /// all keys less than the requested key are in the left tree
    /// and all keys greater are in the right.


    /// Merge of Trees.
    ///   goal is flexible merges with deletion
    /// 


    // Key range selection
    //  by key prefix
    //  by minimum key
    //  by maximum key

    // merge trees
    // diff trees

    // let inline mergeWith (merge : Val -> Val -> Val) : Tree -> Tree -> Tree


type KVTree = KVT.Tree
        







