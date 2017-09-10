namespace Stowage
open Data.ByteString

/// A key-value tree modeled above Stowage.
///
/// By modeling a key-value tree above Stowage, we essentially have first
/// class database values with a lot of nice properties: data persistence,
/// structure sharing, lightweight serialization, efficient diffs, etc..
/// The database may be larger than active memory. Keys and values may be
/// further structured.
///
/// The key-value tree is history-independent after compaction, meaning the
/// internal structure depends only on the data contained, not on the order
/// of update. This simplifies reasoning about structure sharing. 
///
/// If compaction is not used, this can serve as a simple critbit tree.
module KVTree =
    
    /// Keys in our KVTree should be short, simple bytestrings.
    /// Keep keys under a few kilobytes for performance.
    ///
    /// Note: Keys use ByteString, not Binary. Avoid using Keys
    /// for reference-structured binary data.
    type Key = ByteString
    
    /// A Critbit indicates a bit-level offset into a key.
    ///
    /// To simplify working with variable-sized keys, we treat each
    /// byte as 9 bits with the high bit indicating presence of the
    /// byte within the key. Hence, critbit 9 indicates whether our
    /// key has at least two bytes.
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

    /// KVTree nodes are based on a modified critbit tree.
    type Node<'V> =
        | Leaf of 'V                                    // leaf value inline
        | INode of Critbit * Node<'V> * Key * Node<'V>  // critbit * left * keyRight * right
        | RNode of TreeSize * LVRef<Node<'V>>            // remote node with tree size

    let rec private nodeElemCt (node:Node<_>) : TreeSize =
        match node with
        | INode (_, l, _, r) -> nodeElemCt l + nodeElemCt r
        | RNode (tsz, _) -> tsz
        | Leaf _ -> 1UL

    /// The Tree is generic in the value type. 
    ///
    /// A codec for compacting and serializing values must be provided
    /// upon compact or stowage operations. Modulo stowage, this can be
    /// used as a plain old persistent key-value data structure.
    type Tree<'V> =
        | Empty
        | Root of Key * Node<'V>

    module EncNode =
        // encoding is separated from compaction, and multiple nodes
        // may freely be inlined into one binary.

        //  L(val)   - Leaf
        //  N(inode) - INode
        //  R(rsc)   - RNode
        let cLeaf  : byte = byte 'L'
        let cINode : byte = byte 'N'
        let cRNode : byte = byte 'R'

        let inline sizeR (tsz:TreeSize) = 
            1 + EncVarNat.size tsz + EncLVRef.size


        // heuristic size threshold for compaction of a node.
        // in this case, I'm favoring relatively large nodes.
        let compactThreshold : int = 10000

        // it's convenient to start with the Codec here.
        let codec (cV:Codec<'V>) : Codec<Node<'V>> =
            { new Codec<Node<'V>> with
                member cN.Write node dst =
                    match node with
                    | Leaf v ->
                        EncByte.write cLeaf dst
                        Codec.write cV v dst
                    | RNode (sz, ref) ->
                        EncByte.write cRNode dst
                        EncVarNat.write sz dst
                        EncLVRef.write ref dst
                    | INode (cb, l, k, r) ->
                        EncByte.write cINode dst
                        EncVarNat.write (uint64 cb) dst
                        cN.Write l dst
                        EncBytes.write k dst
                        cN.Write r dst
                member cN.Read db src =
                    let b0 = EncByte.read src
                    if (b0 = cLeaf) then
                        Leaf (Codec.read cV db src)
                    elif (b0 = cRNode) then
                        let sz = EncVarNat.read src
                        let ref = EncLVRef.read cN db src
                        RNode (sz, ref)
                    elif (b0 <> cINode) then
                        raise ByteStream.ReadError
                    else
                        let cb = int (EncVarNat.read src)
                        let l = cN.Read db src
                        let k = EncBytes.read src
                        let r = cN.Read db src
                        INode (cb, l, k, r)
                member cN.Compact db node =
                    match node with
                    | Leaf v -> 
                        let struct(v',sz) = cV.Compact db v
                        struct(Leaf v',sz)
                    | RNode (tsz, ref) ->
                        struct(node, sizeR tsz) 
                    | INode (cb, l, kr, r) ->
                        let struct(l', szL) = cN.Compact db l
                        let struct(r', szR) = cN.Compact db r
                        let node' = INode (cb, l', kr, r')
                        let szN = 
                            1 + EncVarNat.size (uint64 cb)
                              + szL + EncBytes.size kr + szR
                        if (szN < compactThreshold) then struct(node',szN) else
                        let tsz = nodeElemCt node'
                        let ref = LVRef.stow cN db node'
                        struct(RNode(tsz,ref), sizeR tsz)
            }

    let private rootCodec (cV:Codec<'V>) : Codec<Key * Node<'V>> =
        Codec.pair (EncBytes.codec) (EncNode.codec cV)
    type TreeRef<'V> = LVRef<Key * Node<'V>> option

    /// Compute total tree size - the number of key-value elements.
    /// O(N) in general. O(1) for compacted subtrees. Together, this
    /// results in O(N) with number of updates since last compaction.
    let size (t:Tree<_>) : TreeSize =
        match t with
        | Empty -> 0UL
        | Root(_,node) -> nodeElemCt node

    let rec private validateN (mcb:Critbit) (kl:Key) (node:Node<_>) =
        match node with
        | Leaf _ -> true
        | RNode (tsz,ref) ->
            let x = LVRef.load ref
            (tsz = nodeElemCt x) && (validateN mcb kl x)
        | INode (cb, l, kr, r) ->
            ((Some cb) = findCritbit mcb kl kr)
                && (testCritbit cb kr)
                && (validateN (1+cb) kl l)
                && (validateN (1+cb) kr r)

    /// validate structural invariants of tree.
    let validate (t:Tree<_>) : bool =
        match t with
        | Empty -> true
        | Root(kl,n) -> validateN 0 kl n

    /// Compact a tree in memory.
    ///
    /// This will both clear any cached nodes, and possibly rewrite
    /// the tree to add more stowed nodes.
    let compact (c:Codec<'V>) (db:DB) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Empty -> Empty
        | Root (k,n) ->
            let n' = Codec.compact (EncNode.codec c) db n
            Root(k, n')

    /// stow a tree without performing a compaction pass.
    let stow' (c:Codec<'V>) (db:DB) (t:Tree<'V>) : TreeRef<'V> =
        match t with
        | Empty -> None
        | Root(kl,n) ->
            Some (LVRef.stow (rootCodec c) db (kl,n))
    
    /// Compact and stow a tree. (Normal case.)
    let inline stow (c:Codec<'V>) (db:DB) (t:Tree<'V>) : TreeRef<'V> = 
        stow' c db (compact c db t)

    /// Load tree partially into memory from a nullable resource.
    let load (rt : TreeRef<'V>) : Tree<'V> =
        match rt with
        | None -> Empty
        | Some ref -> Root (LVRef.load ref)

    let empty : Tree<_> = Empty
    let isEmpty (t : Tree<_>) : bool =
        match t with
        | Empty -> true
        | Root _ -> false
    let singleton (k : Key) (v : 'V) : Tree<'V> = Root (k, Leaf v)

    // update existing least-key value
    let rec private setLKV (v:'V) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb, l, k, r) -> INode (cb, setLKV v l, k, r)
        | RNode (_, ref) -> setLKV v (LVRef.load ref)
        | Leaf _ -> Leaf v

    // insert a new least-key value, at given critbit
    let rec private addLKV (ncb:Critbit) (oldLK:Key) (v:'V) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb, l, kr, r) when (cb < ncb) ->
            assert(not (testCritbit cb oldLK))
            INode (cb, addLKV ncb oldLK v l, kr, r) 
        | RNode (_, ref) -> addLKV ncb oldLK v (LVRef.load ref)
        | _ -> 
            assert(testCritbit ncb oldLK) 
            INode (ncb, Leaf v, oldLK, node) 

    // add or update key-value element somewhere to right of least-key
    //  uses a critbit relative to least-key for efficient insertion
    let rec private addRKV (mcb:Critbit) (k:Key) (v:'V) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb, l, kr, r) ->
            if (cb < mcb) then // diff after node
                assert(not (testCritbit cb k)) 
                INode (cb, addRKV mcb k v l, kr, r) 
            elif (cb > mcb) then // diff before node
                assert(not (testCritbit mcb kr))
                INode (mcb, node, k, Leaf v)
            else // diff aligns with kr
                assert(testCritbit cb k)
                match findCritbit (1+cb) k kr with
                | Some ncb ->
                    if testCritbit ncb k
                        then INode (cb, l, kr, addRKV ncb k v r)
                        else INode (cb, l, k, addLKV ncb kr v r)
                | None -> INode (cb, l, kr, setLKV v r) // update at kr
        | RNode (_, ref) -> addRKV mcb k v (LVRef.load ref)
        | Leaf _ -> INode (mcb, node, k, Leaf v)

    /// Add key-value to the tree, or update existing value.
    ///
    /// Note: All keys and values in the tree must be associated with
    /// the same DB object. 
    let add (k:Key) (v:'V) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Root (tk, tn) ->
            match findCritbit 0 k tk with
            | Some cb -> 
                if testCritbit cb k
                    then Root (tk, addRKV cb k v tn) // add to right of least-key
                    else Root (k, addLKV cb tk v tn) // new least-key
            | None -> Root (tk, setLKV v tn) // update least-key
        | Empty -> Root (k, Leaf v)

    // remove least-key value for a node, return a tree.
    let rec private removeLKV (node:Node<'V>) : Tree<'V> =
        match node with
        | INode (cb, l, kr, r) ->
            match removeLKV l with
            | Empty -> Root(kr, r)
            | Root(kl', l') -> Root(kl', INode(cb, l', kr, r))
        | RNode (_, ref) -> removeLKV (LVRef.load ref)
        | Leaf _  -> Empty // key removed

    // remove a key strictly greater than the least-key if present. 
    //  Uses critbit relative to least-key for efficient deletion.
    let rec private removeRKV (mcb:Critbit) (k:Key) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb, l, kr, r) ->
            if (cb < mcb) then
                assert(not (testCritbit cb k))
                INode(cb, removeRKV mcb k l, kr, r)
            elif (cb > mcb) then node // key not present at expected depth
            else 
                assert(testCritbit cb k)
                match findCritbit (1+cb) k kr with
                | Some ncb -> 
                    if not (testCritbit ncb k) then node else
                    INode (cb, l, kr, removeRKV ncb k r)
                | None -> // match kr, so remove it.
                    match removeLKV r with
                    | Empty -> l 
                    | Root(kr',r') -> INode (cb, l, kr', r')
        | RNode (_, ref) -> removeRKV mcb k (LVRef.load ref)
        | Leaf _ -> node // key not present


    /// Remove key from tree if present. 
    let remove (k:Key) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Root (kl, node) -> 
            match findCritbit 0 k kl with
            | Some cb -> 
                if not (testCritbit cb k) then t else // less than kl
                Root (kl, removeRKV cb k node) // greater than kl
            | None -> removeLKV node // equal to kl, remove least key
        | Empty -> Empty

    // TODO: consider performing lookup before remove, at least when we
    // reach an RNode. This could improve performance a bit when the
    // element is not present in the tree.

    // recursive search for a key
    let rec private tryFindN (k:Key) (kl:Key) (node:Node<'V>) : 'V option =
        match node with
        | INode (cb, l, kr, r) ->
            if testCritbit cb k
                then tryFindN k kr r
                else tryFindN k kl l
        | RNode (_, ref) ->
            if (k < kl) then None else
            tryFindN k kl (LVRef.load ref)
        | Leaf v -> if (k = kl) then Some v else None 

    /// Lookup value (if any) associated with a key.
    let tryFind (k:Key) (t:Tree<'V>) : 'V option =
        match t with
        | Root (kl,node) -> tryFindN k kl node
        | Empty -> None

    /// Find value associated with a key 
    ///   or raise System.Collections.Generic.KeyNotFoundException
    let find (k:Key) (t:Tree<'V>) : 'V =
        match tryFind k t with
        | Some v -> v
        | None -> raise (System.Collections.Generic.KeyNotFoundException())

    let rec private touchN (k:Key) (kl:Key) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb,l,kr,r) ->
            if testCritbit cb k
                then INode (cb,l,kr,touchN k kr r)
                else INode (cb,touchN k kl l, kr, r)
        | RNode (_,ref) -> 
            if(k < kl) then node else
            touchN k kl (LVRef.load ref)
        | Leaf _ -> node

    /// Load a tree along path of a key.
    let touch (k:Key) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Empty -> Empty
        | Root(kl,n) -> Root(kl, touchN k kl n)

    let rec private expandN (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb,l,kr,r) -> INode (cb,expandN l, kr, expandN r)
        | RNode (_,ref) -> LVRef.load ref
        | Leaf _ -> node

    /// Fully load tree structure into memory.
    let expand (t:Tree<'V>) : Tree<'V> =
        match t with
        | Empty -> Empty
        | Root(kl,n) -> Root(kl, expandN n)

    // Enumeration of Tree Elements
    module private EnumNode =
        type Stack<'V> = (Key * Node<'V>) list
        type Elem<'V> = (Key * 'V)
        type State<'V> = (Elem<'V> * Stack<'V>)
        
        let rec enterL (s:Stack<'V>) (k:Key) (n:Node<'V>) : State<'V> =
            match n with
            | INode (_, l, kr, r) -> enterL ((kr,r)::s) k l
            | RNode (_, ref) -> enterL s k (LVRef.load ref)
            | Leaf v -> ((k,v),s)
            
        let rec enterR (s:Stack<'V>) (k:Key) (n:Node<'V>) : State<'V> =
            match n with
            | INode (_, l, kr, r) -> enterR ((k,l)::s) kr r
            | RNode (_, ref) -> enterR s k (LVRef.load ref)
            | Leaf v -> ((k,v),s)

        type EnumeratorL<'V> = // enumerates left to right
            val mutable private st : State<'V>
            member e.Elem with get() = fst e.st
            member e.Stack with get() = snd e.st
            new (st0:State<'V>) = { st = st0 }
            interface System.Collections.Generic.IEnumerator<Elem<'V>> with
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

        type EnumerableL<'V> =
            val private k : Key
            val private n : Node<'V>
            new(k:Key, n:Node<'V>) = { k = k; n = n }
            member e.GetEnum() = new EnumeratorL<'V>(enterL List.empty e.k e.n)
            interface System.Collections.Generic.IEnumerable<Elem<'V>> with
                member e.GetEnumerator() = upcast e.GetEnum()
            interface System.Collections.IEnumerable with
                member e.GetEnumerator() = upcast e.GetEnum()

        type EnumeratorR<'V> = // enumerates right to left
            val mutable private st : State<'V>
            member e.Elem with get() = fst e.st
            member e.Stack with get() = snd e.st
            new (st0:State<'V>) = { st = st0 }
            interface System.Collections.Generic.IEnumerator<Elem<'V>> with
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

        type EnumerableR<'V> =
            val private k : Key
            val private n : Node<'V>
            new(k:Key, n:Node<'V>) = { k = k; n = n }
            member e.GetEnum() = new EnumeratorR<'V>(enterR List.empty e.k e.n)
            interface System.Collections.Generic.IEnumerable<Elem<'V>> with
                member e.GetEnumerator() = upcast e.GetEnum()
            interface System.Collections.IEnumerable with
                member e.GetEnumerator() = upcast e.GetEnum()

    /// sequence ordered from least key to greatest key
    let toSeq (t : Tree<'V>) : seq<(Key * 'V)> =
        match t with
        | Empty -> Seq.empty
        | Root(k,n) -> upcast EnumNode.EnumerableL<'V>(k,n)

    /// reverse-ordered sequence, greatest key to least key
    let toSeqR (t : Tree<'V>) : seq<(Key * 'V)> =
        match t with
        | Empty -> Seq.empty
        | Root(k,n) -> upcast EnumNode.EnumerableR<'V>(k,n)

    let inline fold (fn : 'St -> Key -> 'V -> 'St) (s0 : 'St) (t : Tree<'V>) : 'St =
        Seq.fold (fun s (k,v) -> fn s k v) s0 (toSeq t)

    let inline foldBack (fn : Key -> 'V -> 'St -> 'St) (t : Tree<'V>) (s0 : 'St) : 'St =
        Seq.fold (fun s (k,v) -> fn k v s) s0 (toSeqR t)

    // union assuming in-order elements, preserving critbit 
    let private unionF (cb : int) (a:Tree<'V>) (b:Tree<'V>) : Tree<'V> = 
        match a with
        | Empty -> b
        | Root(ak,an) -> 
            match b with
            | Empty -> a 
            | Root(bk, bn) -> Root(ak, INode (cb, an, bk, bn))

    // select prefix up to critbit assuming successful match on least-key
    let rec private selectPrefixCB (mcb:Critbit) (node:Node<'V>) : Node<'V> =
        match node with
        | INode(cb, l, kr, r) -> if(cb >= mcb) then node else selectPrefixCB mcb l
        | RNode (_, ref) -> selectPrefixCB mcb (LVRef.load ref)
        | Leaf _ -> node

    // find critbit tweaked to return None for any match with full prefix
    let inline private findPrefixCritbit (cb:Critbit) (p:ByteString) (k:Key) =
        findCritbit cb p (BS.take p.Length k)

    // select prefix assuming failed match on least-key
    let rec private selectPrefixR (mcb:Critbit) (p:ByteString) (node:Node<'V>) : Tree<'V> =
        match node with
        | INode(cb, l, kr, r) ->
            if(mcb > cb) then selectPrefixR mcb p l
            elif(mcb < cb) then Empty
            else
                assert(testCritbit cb p)
                match findPrefixCritbit (1+cb) p kr with
                | None -> Root(kr, selectPrefixCB (9 * p.Length) r)
                | Some ncb ->
                    if not (testCritbit ncb p) then Empty else
                    selectPrefixR ncb p r
        | RNode (_, ref) -> selectPrefixR mcb p (LVRef.load ref)
        | Leaf _ -> Empty

    /// Filter a tree to just keys matching a specific prefix.
    let selectPrefix (p:ByteString) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Empty -> Empty
        | Root(kl,l) ->
            match findPrefixCritbit 0 p kl with
            | None -> Root(kl, selectPrefixCB (9 * p.Length) l)
            | Some ncb ->
                if not (testCritbit ncb p) then Empty else
                selectPrefixR ncb p l

    // partition node assuming k > least-key at mcb.
    let rec private partitionN (mcb:Critbit) (k:Key) (node:Node<'V>) : struct(Node<'V> * Tree<'V>) =
        match node with
        | INode(cb, l, kr, r) ->
            if(mcb > cb) then // split left node after cb
                let struct(ll, lr) = partitionN mcb k l
                let rr = unionF cb lr (Root(kr,r))
                struct(ll,rr)
            elif (mcb < cb) then // full node is in left
                struct(node, Empty)
            else // divide the node
                assert(testCritbit cb k)
                match findCritbit (1+cb) k kr with
                | None -> struct(l, Root(kr,r)) //
                | Some ncb ->
                    if not (testCritbit ncb k) then struct(l, Root(kr,r)) else
                    let struct(rl,rr) = partitionN ncb k r
                    let ll = INode(cb, l, kr, rl)
                    struct(ll,rr)
        | RNode (_, ref) -> partitionN mcb k (LVRef.load ref)
        | Leaf _ -> struct(node, Empty)
            

    /// Partition a tree such that all keys strictly less than the
    /// given key are in the left tree, and all remaining keys are
    /// in the right tree. This has a O(N) cost in key length.
    let partition (k:Key) (t:Tree<'V>) : (Tree<'V> * Tree<'V>) =
        match t with
        | Empty -> (Empty, Empty)
        | Root(kl, node) ->
            match findCritbit 0 k kl with
            | None -> (Empty, t)
            | Some cb ->
                // if k is less than kl, entire tree in left
                if not (testCritbit cb k) then (Empty, t) else
                let struct(l, r) = partitionN cb k node
                (Root(kl,l), r)

    /// Value differences relative to a pair of trees
    type VDiff<'V> =
        | InL of 'V         // value in left
        | InR of 'V         // value in right
        | InB of 'V * 'V    // two values with equality failure

    module private EnumNodeDiff =
        type Stack<'V> = (Key * Node<'V>) list
        type Elem<'V> = (Key * VDiff<'V>)
        type State<'V> = (Elem<'V> option * (Stack<'V> * Stack<'V>))

        let iniStack (t:Tree<'V>) : Stack<'V> =
            match t with
            | Empty -> []
            | Root(kl,n) -> ((kl,n)::[])

        let iniState (a:Tree<'V>) (b:Tree<'V>) : State<'V> = 
            (None, (iniStack a, iniStack b))

        let rec stepV (s:Stack<'V>) (n:Node<'V>) : struct(Stack<'V> * 'V) =
            match n with
            | INode (_, l, kr, r) -> stepV ((kr,r)::s) l
            | RNode (_, ref) -> stepV s (LVRef.load ref)
            | Leaf v -> struct(s,v)

        // utility class, mostly for the equality constraint
        type Stepper<'V when 'V : equality>() =

            member x.StepDiff (l:Stack<'V>) (r:Stack<'V>) : State<'V> =
                match (l, r) with
                | ((kl,nl)::sl, (kr,nr)::sr) ->
                    let cmp = compare kl kr
                    if (cmp < 0) then // key only in left
                        let struct(l',v) = stepV sl nl
                        (Some (kl, InL v), (l', r))
                    elif (cmp > 0) then // key only in right
                        let struct(r',v) = stepV sr nr
                        (Some (kr, InR v), (l, r'))
                    else x.StepDiffN kl nl sl nr sr // key in both
                | ((kl,nl)::sl, []) ->
                    let struct(l',v) = stepV sl nl
                    (Some (kl, InL v), (l', []))
                | ([], (kr,nr)::sr) ->
                    let struct(r',v) = stepV sr nr
                    (Some (kr, InR v), ([], r'))
                | ([],[]) -> (None, ([],[]))

            member x.StepDiffV (k:Key) (vl:'V) (sl:Stack<'V>) (vr:'V) (sr:Stack<'V>) : State<'V> =
                if (vl = vr)
                    then x.StepDiff sl sr
                    else (Some (k, InB (vl,vr)), (sl,sr))

            member x.StepDiffN (k:Key) (nl:Node<'V>) (sl:Stack<'V>) (nr:Node<'V>) (sr:Stack<'V>) : State<'V> =
                match (nl, nr) with
                | (Leaf vl, _) -> 
                    let struct(sr',vr') = stepV sr nr
                    x.StepDiffV k vl sl vr' sr'
                | (_, Leaf vr) ->
                    let struct(sl',vl') = stepV sl nl
                    x.StepDiffV k vl' sl' vr sr
                | (INode (_,nl',kr,r), _) -> 
                    let sl' = ((kr,r)::sl)
                    x.StepDiffN k nl' sl' nr sr
                | (_, INode (_,nr',kr,r)) -> 
                    let sr' = ((kr,r)::sr)
                    x.StepDiffN k nl sl nr' sr'
                | (RNode (szl,refl), RNode (szr,refr)) ->
                    if ((szl = szr) && (refl.ID = refr.ID)) then
                        x.StepDiff sl sr // skip equivalent subtrees
                    elif (szl > szr) then
                        let nl' = LVRef.load refl
                        x.StepDiffN k nl' sl nr sr
                    else
                        let nr' = LVRef.load refr
                        x.StepDiffN k nl sl nr' sr

        type Enumerator<'V when 'V : equality>(s:State<'V>) =
            inherit Stepper<'V>()
            member val private ST = s with get, set
            member e.Peek() : Elem<'V> =
                match fst e.ST with
                | Some elem -> elem
                | None -> invalidOp "no element"
            interface System.Collections.Generic.IEnumerator<Elem<'V>> with
                member e.Current with get() = e.Peek()
            interface System.Collections.IEnumerator with
                member e.Current with get() = upcast e.Peek()
                member e.MoveNext() =
                    let (l,r) = snd e.ST
                    e.ST <- e.StepDiff l r
                    Option.isSome (fst e.ST)
                member e.Reset() = raise (System.NotSupportedException())
            interface System.IDisposable with
                member e.Dispose() = ()
            
        type Enumerable<'V when 'V : equality> =
            val private L : Tree<'V>
            val private R : Tree<'V>
            new(l:Tree<'V>,r:Tree<'V>) = { L = l; R = r }
            member e.GetEnum() = new Enumerator<'V>(iniState (e.L) (e.R))
            interface System.Collections.Generic.IEnumerable<Elem<'V>> with
                member e.GetEnumerator() = upcast e.GetEnum()
            interface System.Collections.IEnumerable with
                member e.GetEnumerator() = upcast e.GetEnum()


    /// Efficient difference between trees.
    /// 
    /// This returns a sequence of keys in ascending order with a VDiff for
    /// each key for which a difference between trees is observed. Attempts
    /// to avoid loading compacted subtrees unnecessarily, so we can perform
    /// an efficient structural diff on large compacted trees.
    let diff<'V when 'V : equality> (a:Tree<'V>) (b:Tree<'V>) : seq<(Key * VDiff<'V>)> =
        upcast EnumNodeDiff.Enumerable<'V>(a,b)

    /// Deep structural equality for Trees. (Tests for diff.)
    let deepEq<'V when 'V : equality> (a:Tree<'V>) (b:Tree<'V>) : bool = 
        Seq.isEmpty (diff a b)


type KVTree<'V> = KVTree.Tree<'V>
        







