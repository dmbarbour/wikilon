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
    
    // thoughts: it might be useful to combine Rsc and Binary such that
    // we can semi-transparently store a large binary to stowage.

    /// KVTree keys are flat bytestrings up to a few kilobytes. 
    /// Ideally, keys should be much smaller than this limit.
    ///
    /// Structure sharing is not performed for keys. But it is
    /// possible to model tries with shared key structure in a
    /// KVTree by storing another KVTree at the value.
    type Key = ByteString
    let maxKeyLen = 8192
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
            | RLeaf r -> 1 + EncRsc.size
            | RNode r -> 1 + EncRsc.size
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
            use o = new System.IO.MemoryStream()
            write o node
            Rsc.Stow db (Data.ByteString.unsafeCreateA (o.ToArray()))

        /// Compaction threshold determines an approximate size above
        /// which we compact our nodes. A larger threshold has some
        /// benefits for efficiency, though it's sufficient for larger
        /// structures.
        let compactThreshold : int = 10000

        /// Compact a node, potentially keeping many nodes inline.
        /// (This also asserts the node is within one database.)
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
                        then struct (RLeaf (Rsc.StowBin v), rsz)
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

        // zero-copy access? maybe later. I could potentially use
        // an UnmanagedMemoryStream, but the main savings would be
        // reduced parsing and construction overheads upon lookup.

    /// Compact tree in memory. This can be delayed until after many
    /// tree updates to avoid stowage of intermediate nodes.
    let compact (t : Tree) : Tree =
        match t with
        | Empty -> Empty
        | Root (k,v) -> Root (k, EncNode.compact v)

    /// Write tree to 'nullable' resource. This only writes to disk
    /// for non-empty tree. It implicitly compacts the tree to control
    /// maximum node sizes.
    let stow (t : Tree) : Rsc option =
        match compact t with
        | Empty -> None
        | Root (k,v) ->
            use o = new System.IO.MemoryStream()
            EncBytes.write o k
            EncNode.write o v
            let r = Rsc.Stow (EncNode.nodeDB v) (Data.ByteString.unsafeCreateA (o.ToArray()))
            System.GC.KeepAlive v
            Some r

    /// Load tree (partially) into memory from a nullable resource.
    let load (rt : Rsc option) : Tree =
        match rt with
        | None -> Empty
        | Some r ->
            let i = EncBytes.toInputStream (r.Load())
            let k = EncBytes.read i
            let v = EncNode.read (r.DB) i
            System.GC.KeepAlive r
            Root (k,v)

    let empty : Tree = Empty

    let singleton (k : Key) (v : Val) : Tree = 
        if(not (isValidKey k)) 
            then invalidArg "k" "key too large"
        Root (k, ILeaf v)

    let isEmpty (t : Tree) : bool =
        match t with
        | Empty -> true
        | _ -> false

    let rec private tryFindN (kseek : Key) (kleast : Key) (node : Node) : Val option =
        match node with
        | INode (cb, l, kright, r) -> 
            if testCritbit cb kseek 
                then tryFindN kseek kright r
                else tryFindN kseek kleast l
        | ILeaf v when (kseek = kleast) -> Some v
        | RLeaf r when (kseek = kleast) -> Some (r.LoadBin())
        | RNode r -> tryFindN kseek kleast (EncNode.expand r)
        | _ -> None

    /// Lookup value (if any) associated with a key. 
    ///
    /// This is O(KeyLength) in general, like any trie. But in most
    /// cases, the number of nodes touched will be O(lg(N)).
    let tryFind (t : Tree) (k : Key) : Val option =
        match t with
        | Empty -> None
        | Root (lk,node) -> tryFindN k lk node

    /// leastKey (leftmost key) requires non-empty tree, O(1)
    let leastKey (t : Tree) : Key =
        match t with
        | Empty -> invalidArg "t" "empty tree"
        | Root (k, _) -> k

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
            | ILeaf v -> ((k,v),s)
            | RLeaf rsc -> ((k, rsc.LoadBin()),s)
            
        let rec enterR (s : Stack) (k:Key) (n:Node) : (Elem * Stack) =
            match n with
            | INode (_, l, kr, r) -> enterR ((k,l)::s) kr r
            | RNode rsc -> enterR s k (EncNode.expand rsc)
            | ILeaf v -> ((k,v),s)
            | RLeaf rsc -> ((k, rsc.LoadBin()),s)

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



    /// Merge of Trees.
    /// 
    /// 


    // Key range selection
    //  by key prefix
    //  by minimum key
    //  by maximum key

    // merge trees
    // diff trees

    // let inline mergeWith (merge : Val -> Val -> Val) : Tree -> Tree -> Tree


type KVTree = KVT.Tree
        







