namespace Stowage
open Data.ByteString

/// A key-value critbit tree modeled above Stowage.
///
/// By modeling a key-value tree above Stowage, we essentially have first
/// class database values with a lot of nice properties: data persistence,
/// structure sharing, lightweight serialization, efficient diffs, etc..
/// The keys and values may be further structured, e.g. storing a tree as
/// a value under another tree.
///
/// The critbit tree is history-independent after compaction, which makes
/// it easy to reason about structure sharing. But a KVTree uses explicit
/// compaction to support efficient batched updates.
module KVTree =
    
    /// KVTree keys and values are potentially structured binaries,
    /// meaning they may reference other binaries in the underlying 
    /// stowage database via secure hashes. All keys and values in
    /// a specific tree must be associated with the same database!
    ///
    /// Although adding a key requires a Binary, lookup or removal 
    /// accepts a ByteString (corresponding to key.Bytes) to avoid
    /// unnecessary incref/decref operations on secure hashes.
    ///
    /// NOTE: Keys and values should be at most a few kilobytes
    /// or performance will suffer. If too large, e.g. over 100MB,
    /// other overflows become possible.
    type Key = Binary
    type Val = Binary

    /// A Critbit indicates a bit-level offset into a key. 
    ///
    /// To simplify some logic, we logically inject an extra 1 bit
    /// before each byte within the key. Hence bit 9 indicates if a
    /// key has two bytes, and bit 10 is the high bit of the second
    /// byte. 
    type Critbit = int

    let inline private keyElem (k:ByteString) (ix:int) : uint16 =
        if (ix >= k.Length) then 0us else (0x100us ||| uint16 k.[ix])

    let testCritbit (cb:Critbit) (k:ByteString) : bool =
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
    let findCritbit (cbMin:Critbit) (a:ByteString) (b:ByteString) : Critbit option =
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

    /// Tree size is simply a count of keys or values.
    ///
    /// The KVTree keeps a little metadata when compacting nodes
    /// such that it's easy to determine size of a tree without
    /// peeking into the database. This metadata is also necessary
    /// for efficient tree diffs.
    type TreeSize = uint64

    /// KVTree nodes are based on a modified critbit tree. These
    /// generally shouldn't be used directly (risk of creating an
    /// invalid tree) but are exposed in case new functions must
    /// be written via external modules.
    type Node =
        | Leaf of Val                // leaf value inline
        | INode of Critbit * Node * Key * Node  // critbit * left * keyRight * right
        | RNode of TreeSize * Rsc    // inner node reference

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

        // count number of values in a node
        let rec elemCt (node:Node) : TreeSize =
            match node with
            | INode (_, l, _, r) -> elemCt l + elemCt r
            | RNode (tsz, _) -> tsz
            | Leaf _ -> 1UL

        let inline sizeL (v:Val) = 1 + EncBin.size v
        let inline sizeR (tsz:TreeSize) = 1 + EncVarNat.size tsz + EncRsc.size

        // this is encoded size, not element count
        let rec size (node : Node) = 
            match node with 
            | Leaf v -> sizeL v
            | RNode (sz,r) -> sizeR sz
            | INode (cb,l,k,r) ->
                1 + EncVarNat.size (uint64 cb)
                  + size l
                  + EncBin.size k
                  + size r

        let rec write (o : System.IO.Stream) (node : Node) : unit =
            match node with
            | Leaf v -> EncByte.write o cLeaf; EncBin.write o v
            | RNode (sz,r) -> 
                EncByte.write o cRNode 
                EncVarNat.write o sz
                EncRsc.write o r
            | INode (cb,l,k,r) ->
                EncByte.write o cINode
                EncVarNat.write o (uint64 cb)
                write o l
                EncBin.write o k
                write o r

        let rec read (db:DB) (i:System.IO.Stream) : Node =
            let b0 = EncByte.read i
            if(b0 = cLeaf) then Leaf (EncBin.read db i)
            elif (b0 = cRNode) then
                let sz = EncVarNat.read i
                let rsc = EncRsc.read db i
                RNode (sz, rsc) 
            elif (b0 <> cINode) then failwith (sprintf "unrecognized Node %A" b0)
            else
                let cb = EncVarNat.read i
                if(cb > uint64 System.Int32.MaxValue) then failwith "critbit overflow"
                let l = read db i
                let k = EncBin.read db i
                let r = read db i
                INode (int cb,l,k,r)

        // during compaction, we always know exact size for stowage.
        let stow (db:DB) (byteCt:int) (node:Node) : Rsc =
            let b = Array.zeroCreate byteCt
            use o = new System.IO.MemoryStream(b)
            write o node
            assert(o.Position = int64 b.Length)
            Rsc.Stow db (Data.ByteString.unsafeCreateA b)

        /// Compaction threshold determines a serialization size above
        /// which we compact our tree nodes. A larger compaction threshold
        /// reduces sharing for small subtrees, but should be more efficient
        /// when working with very small keys and values.
        let compactThreshold : int = 10000

        // avoid redundant computation of encoded sizes!
        let rec compactSz (db:DB) (node:Node) : struct (Node * int) =
            match node with
            | INode (cb, l, kr, r) ->
                let struct (l', bytesL) = compactSz db l
                let struct (r', bytesR) = compactSz db r
                let node' = INode (cb, l', kr, r')
                let byteCt = 
                    1 + EncVarNat.size (uint64 cb) 
                      + bytesL + EncBin.size kr + bytesR
                if (compactThreshold >= byteCt) then struct(node', byteCt) else
                let tsz = elemCt node'
                let rsc = stow db byteCt node'
                struct(RNode(tsz,rsc), sizeR tsz)
            | RNode (tsz,rsc) -> assert(db = rsc.DB); struct(node, sizeR tsz)
            | Leaf v -> assert(db = v.DB); struct(node, sizeL v)

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

    /// Compute total tree size - the number of key-value elements.
    /// O(N) in general. O(1) for compacted subtrees. Together, this
    /// results in O(N) with number of updates since last compaction.
    let size (t:Tree) : TreeSize =
        match t with
        | Empty -> 0UL
        | Root(_,node) -> EncNode.elemCt node

    let rec private validateN (mcb:Critbit) (kl:Key) (node:Node) =
        match node with
        | Leaf v -> (kl.DB = v.DB)
        | RNode (tsz,rsc) ->
            let x = EncNode.expand rsc
            (kl.DB = rsc.DB)
                && (tsz = EncNode.elemCt x)
                && (validateN mcb kl x)
        | INode (cb, l, kr, r) ->
            (kl.DB = kr.DB) 
                && ((Some cb) = findCritbit mcb (kl.Bytes) (kr.Bytes))
                && (testCritbit cb (kr.Bytes))
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
        match t with
        | Empty -> None
        | Root(kl,node) ->
            let struct(node', szN) = EncNode.compactSz (kl.DB) node
            let b = Array.zeroCreate (EncBin.size kl + szN)
            use o = new System.IO.MemoryStream(b)
            EncBin.write o kl
            EncNode.write o node'
            assert(o.Position = int64 b.Length)
            let r = Rsc.Stow (kl.DB) (Data.ByteString.unsafeCreateA b)
            System.GC.KeepAlive node'
            System.GC.KeepAlive kl
            Some r

    /// Load tree partially into memory from a nullable resource.
    let load (rt : Rsc option) : Tree =
        match rt with
        | None -> Empty
        | Some r ->
            use i = EncBytes.toInputStream (loadRscDB r.DB r.ID)
            let kl = EncBin.read (r.DB) i
            let node = EncNode.read (r.DB) i
            System.GC.KeepAlive r
            Root (kl,node)

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
        | RNode (_, rsc) -> setLKV v (EncNode.expand rsc)
        | Leaf _ -> Leaf v

    // insert a new least-key value, at given critbit
    let rec private addLKV (ncb:Critbit) (oldLK:Key) (v:Val) (node:Node) : Node =
        match node with
        | INode (cb, l, kr, r) when (cb < ncb) ->
            assert(not (testCritbit cb (oldLK.Bytes)))
            INode (cb, addLKV ncb oldLK v l, kr, r) 
        | RNode (_, rsc) -> addLKV ncb oldLK v (EncNode.expand rsc)
        | _ -> 
            assert(testCritbit ncb (oldLK.Bytes)) 
            INode (ncb, Leaf v, oldLK, node) 

    // add or update key-value element somewhere to right of least-key
    //  uses a critbit relative to least-key for efficient insertion
    let rec private addRKV (mcb:Critbit) (k:Key) (v:Val) (node:Node) : Node =
        match node with
        | INode (cb, l, kr, r) ->
            if (cb < mcb) then // diff after node
                assert(not (testCritbit cb (k.Bytes))) 
                INode (cb, addRKV mcb k v l, kr, r) 
            elif (cb > mcb) then // diff before node
                assert(not (testCritbit mcb (kr.Bytes)))
                INode (mcb, node, k, Leaf v)
            else // diff aligns with kr
                assert(testCritbit cb (k.Bytes))
                match findCritbit (1+cb) (k.Bytes) (kr.Bytes) with
                | Some ncb ->
                    if testCritbit ncb (k.Bytes)
                        then INode (cb, l, kr, addRKV ncb k v r)
                        else INode (cb, l, k, addLKV ncb kr v r)
                | None -> INode (cb, l, kr, setLKV v r) // update at kr
        | RNode (_, rsc) -> addRKV mcb k v (EncNode.expand rsc)
        | Leaf _ -> INode (mcb, node, k, Leaf v)

    /// Add key-value to the tree, or update existing value.
    ///
    /// Note: All keys and values in the tree must be associated with
    /// the same DB object. 
    let add (nk : Key) (nv : Val) (t : Tree) : Tree =
        assert(nk.DB = nv.DB)
        match t with
        | Root (tk, tn) ->
            assert(nk.DB = tk.DB)
            match findCritbit 0 (nk.Bytes) (tk.Bytes) with
            | Some cb -> 
                if testCritbit cb (nk.Bytes)
                    then Root (tk, addRKV cb nk nv tn) // add to right of least-key
                    else Root (nk, addLKV cb tk nv tn) // new least-key
            | None -> Root (tk, setLKV nv tn) // update least-key
        | Empty -> Root (nk, Leaf nv)

    // remove least-key value for a node, return a tree.
    let rec private removeLKV (node:Node) : Tree =
        match node with
        | INode (cb, l, kr, r) ->
            match removeLKV l with
            | Empty -> Root(kr, r)
            | Root(kl', l') -> Root(kl', INode(cb, l', kr, r))
        | RNode (_, rsc) -> removeLKV (EncNode.expand rsc)
        | Leaf _  -> Empty // key removed

    // remove a key strictly greater than the least-key if present. 
    //  Uses critbit relative to least-key for efficient deletion.
    let rec private removeRKV (mcb:Critbit) (k:ByteString) (node:Node) : Node =
        match node with
        | INode (cb, l, kr, r) ->
            if (cb < mcb) then
                assert(not (testCritbit cb k))
                INode(cb, removeRKV mcb k l, kr, r)
            elif (cb > mcb) then node // key not present at expected depth
            else 
                assert(testCritbit cb k)
                match findCritbit (1+cb) k (kr.Bytes) with
                | Some ncb -> 
                    if not (testCritbit ncb k) then node else
                    INode (cb, l, kr, removeRKV ncb k r)
                | None -> // match kr, so remove it.
                    match removeLKV r with
                    | Empty -> l 
                    | Root(kr',r') -> INode (cb, l, kr', r')
        | RNode (_, rsc) -> removeRKV mcb k (EncNode.expand rsc)
        | Leaf _ -> node // key not present


    /// Remove key from tree if present. 
    let remove (k:ByteString) (t:Tree) : Tree =
        match t with
        | Root (kl, node) -> 
            match findCritbit 0 k (kl.Bytes) with
            | Some cb -> 
                if not (testCritbit cb k) then t else // less than kl
                Root (kl, removeRKV cb k node) // greater than kl
            | None -> removeLKV node // equal to kl, remove least key
        | Empty -> Empty

    // recursive search for a key
    let rec private tryFindN (k:ByteString) (kl:Key) (node:Node) : Val option =
        match node with
        | INode (cb, l, kr, r) ->
            if testCritbit cb k
                then tryFindN k kr r
                else tryFindN k kl l
        | RNode (_, rsc) ->
            if (k < kl.Bytes) then None else
            tryFindN k kl (EncNode.expand rsc)
        | Leaf v -> if (k = kl.Bytes) then Some v else None 

    /// Lookup value (if any) associated with a key. This may load
    /// tree nodes into memory, but doesn't hold onto them. So if
    /// you need to lookup a value many times, or plan to modify it
    /// afterwards, consider use of `touch` to pre-load the data.
    let tryFind (k:ByteString) (t:Tree) : Val option =
        match t with
        | Root (kl,node) -> tryFindN k kl node
        | Empty -> None

    /// Find value associated with a key 
    ///   or raise System.Collections.Generic.KeyNotFoundException
    let find (k:ByteString) (t:Tree) : Val =
        match tryFind k t with
        | Some v -> v
        | None -> raise (System.Collections.Generic.KeyNotFoundException())

    let rec private touchN (k:ByteString) (kl:Key) (node:Node) : Node =
        match node with
        | INode (cb, l, kr, r) ->
            if testCritbit cb k
                then INode(cb, l, kr, (touchN k kr r))
                else INode(cb, (touchN k kl l), kr, r)
        | RNode (_, rsc) -> 
            if (k < kl.Bytes) then node else
            touchN k kl (EncNode.expand rsc)
        | Leaf _ -> node

    /// Load a tree partially into memory based on lookup for a specific key.
    let touch (k:ByteString) (t:Tree) : Tree =
        match t with
        | Empty -> Empty
        | Root (kl,node) -> Root(kl, touchN k kl node)

    let rec private fullyExpandN (node : Node) : Node =
        match node with
        | INode (cb, l, k, r) -> INode (cb, fullyExpandN l, k, fullyExpandN r)
        | RNode (_, rsc) -> fullyExpandN (EncNode.expand rsc)
        | Leaf _ -> node

    /// Load entire tree into RAM. Essentially opposite to 'compact'.
    let fullyExpand (t : Tree) : Tree =
        match t with
        | Empty -> Empty
        | Root (k, n) -> Root (k, fullyExpandN n)

    // Enumeration of Tree Elements
    module private EnumNode =
        type Stack = List<(Key * Node)>
        type Elem = (Key * Val)
        type EnumState = (Elem * Stack)
        
        let rec enterL (s:Stack) (k:Key) (n:Node) : (Elem * Stack) =
            match n with
            | INode (_, l, kr, r) -> enterL ((kr,r)::s) k l
            | RNode (_,rsc) -> enterL s k (EncNode.expand rsc)
            | Leaf v -> ((k,v),s)
            
        let rec enterR (s : Stack) (k:Key) (n:Node) : (Elem * Stack) =
            match n with
            | INode (_, l, kr, r) -> enterR ((k,l)::s) kr r
            | RNode (_,rsc) -> enterR s k (EncNode.expand rsc)
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

    // union assuming in-order elements, preserving critbit 
    let private unionF (cb : int) (a:Tree) (b:Tree) : Tree = 
        match a with
        | Empty -> b
        | Root(ak,an) -> 
            match b with
            | Empty -> a 
            | Root(bk, bn) -> Root(ak, INode (cb, an, bk, bn))

    // select prefix up to critbit assuming successful match on least-key
    let rec private selectPrefixCB (mcb:Critbit) (node:Node) : Node =
        match node with
        | INode(cb, l, kr, r) -> if(cb >= mcb) then node else selectPrefixCB mcb l
        | RNode (_, rsc) -> selectPrefixCB mcb (EncNode.expand rsc)
        | Leaf _ -> node

    // find critbit tweaked to return None for any match with full prefix
    let inline private findPrefixCritbit (cb:Critbit) (p:ByteString) (k:ByteString) =
        findCritbit cb p (Data.ByteString.take p.Length k)

    // select prefix assuming failed match on least-key
    let rec private selectPrefixR (mcb:Critbit) (p:ByteString) (node:Node) : Tree =
        match node with
        | INode(cb, l, kr, r) ->
            if(mcb > cb) then selectPrefixR mcb p l
            elif(mcb < cb) then Empty
            else
                assert(testCritbit cb p)
                match findPrefixCritbit (1+cb) p (kr.Bytes) with
                | None -> Root(kr, selectPrefixCB (9 * p.Length) r)
                | Some ncb ->
                    if not (testCritbit ncb p) then Empty else
                    selectPrefixR ncb p r
        | RNode (_, rsc) -> selectPrefixR mcb p (EncNode.expand rsc)
        | Leaf _ -> Empty

    /// Filter a tree to just keys matching a specific prefix.
    let selectPrefix (p:ByteString) (t:Tree) : Tree =
        match t with
        | Empty -> Empty
        | Root(kl,l) ->
            match findPrefixCritbit 0 p (kl.Bytes) with
            | None -> Root(kl, selectPrefixCB (9 * p.Length) l)
            | Some ncb ->
                if not (testCritbit ncb p) then Empty else
                selectPrefixR ncb p l

    // partition node assuming k > least-key at mcb.
    let rec private partitionN (mcb:Critbit) (k:ByteString) (node:Node) : (Node * Tree) =
        match node with
        | INode(cb, l, kr, r) ->
            if(mcb > cb) then // split left node after cb
                let (ll, lr) = partitionN mcb k l
                let rr = unionF cb lr (Root(kr,r))
                (ll,rr)
            elif (mcb < cb) then // full node is in left
                (node, Empty)
            else // divide the node
                assert(testCritbit cb k)
                match findCritbit (1+cb) k (kr.Bytes) with
                | None -> (l, Root(kr,r)) //
                | Some ncb ->
                    if not (testCritbit ncb k) then (l, Root(kr,r)) else
                    let (rl,rr) = partitionN ncb k r
                    let ll = INode(cb, l, kr, rl)
                    (ll,rr)
        | RNode (_, rsc) -> partitionN mcb k (EncNode.expand rsc)
        | Leaf _ -> (node, Empty)
            

    /// Partition a tree such that all keys strictly less than the
    /// given key are in the left tree, and all remaining keys are
    /// in the right tree. This has a O(N) cost in key length.
    let partition (k:ByteString) (t:Tree) : (Tree * Tree) =
        match t with
        | Empty -> (Empty, Empty)
        | Root(kl, node) ->
            match findCritbit 0 k (kl.Bytes) with
            | None -> (Empty, t)
            | Some cb ->
                // if k is less than kl, entire tree in left
                if not (testCritbit cb k) then (Empty, t) else
                let (l, r) = partitionN cb k node
                (Root(kl,l), r)

    /// Simple difference of values relative to a tree key.
    type VDiff = 
        | InL of Val        // key only in left tree
        | InR of Val        // key only in right tree
        | InB of Val * Val  // key in both trees but values differ

    module private EnumNodeDiff =
        type Stack = (Key * Node) list
        type Elem = (Key * VDiff)
        type State = (Elem option * (Stack * Stack))

        // step to next value
        let rec stepV (s:Stack) (n:Node) : struct(Stack * Val) =
            match n with
            | INode (_, l, kr, r) -> stepV ((kr,r)::s) l
            | RNode (_, rsc) -> stepV s (EncNode.expand rsc)
            | Leaf v -> struct(s,v)

        // stepDiff is mutually recursive to avoid redundant key comparisons
        let rec stepDiff (l:Stack) (r:Stack) : State =
            match (l, r) with
            | ((kl,nl)::sl, (kr,nr)::sr) ->
                assert(kl.DB = kr.DB)
                let cmp = compare (kl.Bytes) (kr.Bytes)
                if (cmp < 0) then // key only in left
                    let struct(l',v) = stepV sl nl
                    (Some (kl, InL v), (l', r))
                elif (cmp > 0) then // key only in right
                    let struct(r',v) = stepV sr nr
                    (Some (kr, InR v), (l, r'))
                else stepDiffN kl nl sl nr sr // key in both
            | ((kl,nl)::sl, []) ->
                let struct(l',v) = stepV sl nl
                (Some (kl, InL v), (l', []))
            | ([], (kr,nr)::sr) ->
                let struct(r',v) = stepV sr nr
                (Some (kr, InR v), ([], r'))
            | ([],[]) -> (None, ([],[]))
        and stepDiffV (k:Key) (vl:Val) (sl:Stack) (vr:Val) (sr:Stack) : State =
            assert(vl.DB = vr.DB)
            if(vl.Bytes = vr.Bytes)
                then stepDiff sl sr // skip equal leaf values at same key
                else (Some (k, InB (vl, vr)), (sl, sr))
        and stepDiffN (k:Key) (nl:Node) (sl:Stack) (nr:Node) (sr:Stack) : State =
            match (nl, nr) with
            | (INode (_,nl',kr,r), _) -> 
                let sl' = ((kr,r)::sl)
                stepDiffN k nl' sl' nr sr
            | (_, INode (_,nr',kr,r)) -> 
                let sr' = ((kr,r)::sr)
                stepDiffN k nl sl nr' sr'
            | (Leaf vl, _) -> 
                let struct(sr',vr') = stepV sr nr
                stepDiffV k vl sl vr' sr'
            | (_, Leaf vr) ->
                let struct(sl',vl') = stepV sl nl
                stepDiffV k vl' sl' vr sr
            | (RNode (szl,rscl), RNode (szr,rscr)) ->
                if ((szl = szr) && (rscl.ID = rscr.ID)) then
                    stepDiff sl sr // skip equivalent subtrees
                elif (szl > szr) then
                    let nl' = EncNode.expand rscl
                    stepDiffN k nl' sl nr sr
                else
                    let nr' = EncNode.expand rscr
                    stepDiffN k nl sl nr' sr

        let iniStack (t:Tree) : Stack =
            match t with
            | Empty -> []
            | Root(kl,n) -> ((kl,n)::[])

        let iniState (a:Tree) (b:Tree) : State = 
            (None, (iniStack a, iniStack b))

        type Enumerator =
            val mutable private st : State
            new (st0:State) = { st = st0 }
            member e.Peek() : Elem =
                match fst e.st with
                | Some elem -> elem
                | None -> invalidOp "no element"
            interface System.Collections.Generic.IEnumerator<Elem> with
                member e.Current with get() = e.Peek()
            interface System.Collections.IEnumerator with
                member e.Current with get() = upcast e.Peek()
                member e.MoveNext() =
                    let (l,r) = snd e.st
                    e.st <- stepDiff l r
                    Option.isSome (fst e.st)
                member e.Reset() = raise (System.NotSupportedException())
            interface System.IDisposable with
                member e.Dispose() = ()
            
        type Enumerable =
            val private L : Tree
            val private R : Tree
            new(l:Tree,r:Tree) = { L = l; R = r }
            member e.GetEnum() = new Enumerator(iniState (e.L) (e.R))
            interface System.Collections.Generic.IEnumerable<Elem> with
                member e.GetEnumerator() = upcast e.GetEnum()
            interface System.Collections.IEnumerable with
                member e.GetEnumerator() = upcast e.GetEnum()


    /// Efficient difference between trees.
    /// 
    /// This returns a sequence of keys in ascending order with a VDiff for
    /// each key for which a difference between trees is observed. Attempts
    /// to avoid loading compacted subtrees unnecessarily, so we can perform
    /// an efficient structural diff on large compacted trees.
    let diff (a:Tree) (b:Tree) : seq<(Key * VDiff)> =
        upcast EnumNodeDiff.Enumerable(a,b)


type KVTree = KVTree.Tree
        







