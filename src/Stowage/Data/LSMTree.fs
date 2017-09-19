namespace Stowage
open Data.ByteString

/// A Log Structured Merge (LSM) Tree above Stowage
///
/// The LSM-tree is essentially a key-value tree with buffered update.
/// Recent updates are aggregated in memory and applied together. This
/// is highly suitable for write-heavy processes or on-disk storage.
/// Stowage is a form of on-disk storage and benefits from buffering.
///
/// An LSM-tree has some disadvantages. Due to buffered removals, the
/// tree size or isEmpty computations are non-trivial. The logic is more
/// involved. And history-dependent structure can hinder sharing with
/// trees that received the same data in a different order (although the
/// sharing with the tree's own past is pretty good).
module LSMTree =

    type Key = ByteString
    type Critbit = int
    let inline findCritbit cbMin a b = BTree.findCritbit cbMin a b
    let inline testCritbit cb k = BTree.testCritbit cb k

    // Buffer of recent updates to a remote node.
    type Updates<'V> = BTree<'V option>

    type Node<'V> =
        | Leaf of 'V
        | INode of Critbit * Node<'V> * Key * Node<'V>       // Inner Tree Node
        | RNode of Critbit * Updates<'V> * LVRef<Node<'V>>   // Remote Node with Update Buffer

    // A Tree is either obviously empty or has a sample key and node.
    //
    // Modulo buffering, this sample key is also the least-key for the
    // node. But once an RNode is referenced, it refers instead to the
    // sample key associated with the remote node. Regardless, it is a
    // valid key for monotonic critbit computations.
    type Tree<'V> =
        | Empty
        | Root of Key * Node<'V>

    let empty : Tree<_> = Empty
    let inline singleton (k:Key) (v:'V) : Tree<'V> = Root(k, Leaf v)

    // we've matched the key recorded at the node parent, but we might still
    // have an update or removal via buffer.
    let rec private tryFindLKV (kl:Key) (node:Node<'V>) : 'V option =
        match node with
        | INode (_,l,_,_) -> tryFindLKV kl l
        | Leaf v -> Some v
        | RNode (_,buff,ref) -> 
            match BTree.tryFind kl buff with
            | None -> tryFindLKV kl (LVRef.load ref) 
            | Some vOpt -> vOpt

    let rec private tryFindN (mb:Critbit) (k:Key) (kl:Key) (node:Node<'V>) : 'V option =
        match node with
        | INode (cb, l, kr, r) ->
            if testCritbit cb k 
                then tryFindN mb k kr r
                else tryFindN mb k kl l
        | Leaf v ->
            let keysMatch = Option.isNone (findCritbit mb k kl)
            if keysMatch then Some v else None
        | RNode (cb, upd, ref) ->
            match BTree.tryFind k upd with
            | Some vOpt -> vOpt
            | None ->
                match findCritbit mb k kl with
                | None -> tryFindLKV kl (LVRef.load ref)
                | Some mb' ->
                    let stop = (mb' < cb) || (testCritbit mb' kl)
                    if stop then None else
                    tryFindN mb' k kl (LVRef.load ref)

    /// Find some value associated with a key, or return none.
    ///
    /// This will access values in recent update buffers before checking
    /// remote stowage references. Will cache lookups that do touch remote
    /// nodes, albeit only briefly (via LVRef). 
    let tryFind (k:Key) (t:Tree<'V>) : 'V option =
        match t with
        | Root (kl,n) -> tryFindN 0 k kl n
        | Empty -> None

    /// Find value associated with key 
    ///   or raise `System.Collections.Generic.KeyNotFoundException()`.
    let find (k:Key) (t:Tree<'V>) : 'V =
        match tryFind k t with
        | Some v -> v
        | None -> raise (System.Collections.Generic.KeyNotFoundException())
    
    /// Test whether key is present in tree.
    let inline containsKey k t = Option.isSome (tryFind k t)

    // modify least-key value
    let rec private setLKV (kl:Key) (v:'V) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb, l, kr, r) -> INode(cb, setLKV kl v l, kr, r)
        | Leaf _ -> Leaf v
        | RNode (cb, upd, ref) -> RNode(cb, BTree.add kl (Some v) upd, ref)

    // introduce a new least-key value at mb.
    // this must return old least-key if adding to RNode buffer.
    let rec private addLKV (mb:Critbit) (k:Key) (v:'V) (kl:Key) (node:Node<'V>) : struct(Key * Node<'V>) =
        match node with
        | INode (cb, l, kr, r) when (mb >= cb) ->
            let struct(kl',l') = addLKV mb k v kl l
            struct(kl', INode(cb, l', kr, r))
        | RNode (cb, upd, ref) when (mb >= cb) ->
            let upd' = BTree.add k (Some v) upd
            struct(kl, RNode(cb, upd', ref))
        | _ -> struct(k, INode(mb, Leaf v, kl, node))

    // assume k > kl at mb
    let rec private addRKV (mb:Critbit) (k:Key) (v:'V) (kl:Key) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb, l, kr, r) when (mb >= cb) ->
            if (mb > cb) then
                let l' = addRKV mb k v kl l
                INode (cb, l', kr, r)
            else // mb = cb
                match findCritbit (1+cb) k kr with
                | Some mb' ->
                    if testCritbit mb' k then // k > kr
                        INode (cb, l, kr, addRKV mb' k v kr r)
                    else
                        let struct(kr', r') = addLKV mb' k v kr r
                        INode (cb, l, kr', r')
                | None -> INode (cb, l, kr, setLKV kr v r)
        | RNode (cb, upd, ref) when (mb >= cb) -> 
            let upd' = BTree.add k (Some v) upd
            RNode (cb, upd', ref)
        | _ -> INode (mb, node, k, Leaf v)

    /// Add a key-value association to a tree. 
    ///
    /// Persistent and buffered: this returns a new tree with the update
    /// applied, and does not rewrite remote nodes. A future compaction
    /// will heuristically flush buffers into remote nodes.
    let add (k:Key) (v:'V) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Root (kl, n) -> 
            match findCritbit 0 k kl with
            | Some mb' -> 
                if testCritbit mb' k then // k > kl
                    Root(kl, addRKV mb' k v kl n)
                else // k < kl, but might add to RNode buffer
                    let struct(kl',n') = addLKV mb' k v kl n
                    Root(kl',n')
            | None -> Root (kl, setLKV kl v n)
        | Empty -> singleton k v

    // remove a key from a node.
    let rec private removeN (k:Key) (kl:Key) (node:Node<'V>) : Tree<'V> =
        match node with
        | INode (cb, l, kr, r) ->
            if testCritbit cb k 
               then match removeN k kr r with
                    | Root(kr',r') -> Root(kl, INode(cb, l, kr', r'))
                    | Empty -> Root(kl,l)
               else match removeN k kl l with
                    | Root(kl',l') -> Root(kl', INode(cb, l', kr, r))
                    | Empty -> Root(kr,r)
        | Leaf _ -> if (k = kl) then Empty else Root(kl,node)
        | RNode (cb, upd, ref) ->
            match findCritbit 0 k kl with
            | Some mb when (mb < cb) -> Root(kl,node) // key not in node
            // note: cannot short-circuit on `testCritbit mb kl` since 
            // kl is not guaranteed to be least-key from node at ref. 
            | _ -> 
                // add pending removal to buffer
                let upd' = BTree.add k None upd
                Root(kl, RNode(cb, upd', ref))

    /// Remove key-value association from tree.
    ///
    /// Like add, this is persistent and buffered. This returns a new 
    /// tree with the update applied, and does not rewrite remote nodes.
    /// A future compaction can heuristically flush buffers into remote
    /// nodes. 
    let remove (k:Key) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Root (kl, node) -> removeN k kl node
        | Empty -> Empty

    /// Remove a key, filtered by containsKey.
    ///
    /// Because buffered removals can 'add' keys to a buffer even when 
    /// the key is not present, it's sometimes more efficient to ensure
    /// the key is present before removal.
    let inline checkedRemove k t = if containsKey k t then remove k t else t

    let private applyUpd t k vOpt =
        match vOpt with
        | None -> remove k t
        | Some v -> add k v t

    let inline private loadR' (upd:Updates<'V>) (kl:Key) (ref:LVRef<Node<'V>>) : Tree<'V> =
        BTree.fold applyUpd (Root(kl, LVRef.load' ref)) upd

    // Enumeration of Tree Elements
    module private EnumTree =
        type Stack<'V> = Tree<'V> list
        type Elem<'V> = (Key * 'V) 
        type State<'V> = (Elem<'V> option * Stack<'V>)
        type StepFn<'V> = Stack<'V> -> State<'V>
        
        let rec stepL (s:Stack<'V>) : State<'V> =
            match s with
            | (t::s') ->
                match t with
                | Root(kl,node) -> stepLN s' kl node
                | Empty -> stepL s'
            | [] -> (None, [])
        and stepLN s kl node =
            match node with
            | INode(_,l,kr,r) -> stepLN (Root(kr,r)::s) kl l
            | Leaf v -> (Some(kl,v), s)
            | RNode(_,upd,ref) -> stepL ((loadR' upd kl ref)::s)

        let rec stepR (s:Stack<'V>) : State<'V> =
            match s with
            | (t::s') ->
                match t with
                | Root(kl,node) -> stepRN s' kl node
                | Empty -> stepR s'
            | [] -> (None, [])
        and stepRN s kl node =
            match node with 
            | INode(_,l,kr,r) -> stepRN (Root(kl,l)::s) kr r
            | Leaf v -> (Some(kl,v),s)
            | RNode(_,upd,ref) -> stepR ((loadR' upd kl ref)::s)

        type Enumerator<'V> = // enumerates left to right
            val step : StepFn<'V>
            val mutable private state : State<'V>
            new (s,t) = { step = s; state = (None, t::[]) }
            member e.Elem 
                with get() = 
                    match fst e.state with
                    | Some v -> v
                    | None -> invalidOp "no current element"
            member e.Stack with get() = snd e.state
            interface System.Collections.Generic.IEnumerator<Elem<'V>> with
                member e.Current with get() = e.Elem
            interface System.Collections.IEnumerator with
                member e.Current with get() = upcast e.Elem
                member e.MoveNext() =
                    e.state <- e.step e.Stack
                    Option.isSome (fst e.state)
                member e.Reset() = raise (System.NotSupportedException())
            interface System.IDisposable with
                member e.Dispose() = ()

        type Enumerable<'V> =
            val private s : StepFn<'V>
            val private t : Tree<'V>
            new(s,t) = { s = s; t = t }
            member e.GetEnum() = new Enumerator<'V>(e.s, e.t)
            interface System.Collections.Generic.IEnumerable<Elem<'V>> with
                member e.GetEnumerator() = upcast e.GetEnum()
            interface System.Collections.IEnumerable with
                member e.GetEnumerator() = upcast e.GetEnum()

    let toSeq (t:Tree<'V>) : seq<(Key * 'V)> =
        upcast EnumTree.Enumerable(EnumTree.stepL, t)

    let toSeqR (t:Tree<'V>) : seq<(Key * 'V)> =
        upcast EnumTree.Enumerable(EnumTree.stepR, t)

    /// Check whether the tree is empty. O(N).
    let isEmpty (t:Tree<_>) : bool = Seq.isEmpty (toSeq t)

    /// Compute size of tree. O(N). 
    let size (t:Tree<_>) : int = Seq.length (toSeq t)


    let inline fold (fn : 'St -> Key -> 'V -> 'St) (s0 : 'St) (t : Tree<'V>) : 'St =
        Seq.fold (fun s (k,v) -> fn s k v) s0 (toSeq t)
    let inline foldBack (fn : Key -> 'V -> 'St -> 'St) (t : Tree<'V>) (s0 : 'St) : 'St =
        Seq.fold (fun s (k,v) -> fn k v s) s0 (toSeqR t)
    let inline iter (fn: Key -> 'V -> unit) (t:Tree<'V>) : unit =
        Seq.iter (fun (k,v) -> fn k v) (toSeq t)
    let inline tryPick (fn:Key -> 'V -> 'U option) (t:Tree<'V>) : 'U option =
        Seq.tryPick (fun (k,v) -> fn k v) (toSeq t)
    let inline tryFindKey fn t = tryPick (fun k v -> if fn k v then Some k else None) t
    let inline exists fn t = tryFindKey fn t |> Option.isSome
    let inline forall fn t = not (exists (fun k v -> not (fn k v)) t)

    let inline toArray (t:Tree<'V>) : (Key * 'V) array = Array.ofSeq (toSeq t)
    let inline toList (t:Tree<'V>) : (Key * 'V) list = List.ofSeq (toSeq t)

    let ofSeq (s:seq<Key * 'V>) : Tree<'V> =
        Seq.fold (fun t (k,v) -> add k v t) empty s
    let ofList (lst:(Key * 'V) list) : Tree<'V> =
        List.fold (fun t (k,v) -> add k v t) empty lst
    let inline ofArray (a: (Key * 'V) array) : Tree<'V> =
        Array.fold (fun t (k,v) -> add k v t) empty a

    // find critbit tweaked to return None for any match with full prefix
    let inline private findPrefixCritbit (cb:Critbit) (p:ByteString) (k:Key) =
        findCritbit cb p (BS.take p.Length k)    


    /// Filter a tree to just keys matching a specific prefix.
    let rec selectPrefix (p:ByteString) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Root(kl,node) -> selectPrefixN p kl node
        | Empty -> Empty
    and private selectPrefixN p kl node =
        let pb = (9 * p.Length)
        match node with
        | INode(cb,l,kr,r) when (cb < pb) ->
            if testCritbit cb p
                then selectPrefixN p kr r
                else selectPrefixN p kl l
        | RNode(cb,upd,ref) when (cb < pb) ->
            // filter to avoid loading RNodes unnecessarily
            match findPrefixCritbit 0 p kl with
            | Some mb when (mb < cb) -> Empty
            | _ -> selectPrefix p (loadR' upd kl ref)
        | _ -> if (p = (BS.take p.Length kl)) then Root(kl,node) else Empty

    let inline private joinCB cb tl tr =
        match tl with
        | Empty -> tr
        | Root(kl,nl) ->
            match tr with
            | Empty -> tl
            | Root(kr,nr) -> Root(kl, INode(cb,nl,kr,nr))


    // partition aligned with kl
    let rec private partitionT mb k t =
        match t with
        | Root(kl,node) ->
            match findCritbit mb k kl with
            | Some mb' -> partitionN mb' k kl node
            | None -> partitionM kl node
        | Empty -> struct(Empty,Empty)

    // partition when k=kl (cannot assume kl is true least-key at RNode)
    and private partitionM kl node =
        match node with
        | INode(cb,l,kr,r) -> 
            let struct(ll,lr) = partitionM kl l
            let rr = joinCB cb lr (Root(kr,r))
            struct(ll,rr)
        | Leaf _ -> struct(Empty,Root(kl,node))
        | RNode(_,upd,ref) ->
            partitionT (9 * kl.Length) kl (loadR' upd kl ref)

    // partition when k <> kl at mb
    and private partitionN mb k kl node =
        match node with
        | INode(cb,l,kr,r) when (mb >= cb) ->
            if((mb = cb) && (testCritbit cb k)) then
                let struct(rl,rr) = partitionT (1+cb) k (Root(kr,r))
                let ll = joinCB cb (Root(kl,l)) rl
                struct(ll,rr)
            else 
                let struct(ll,lr) = partitionN mb k kl l
                let rr = joinCB cb lr (Root(kr,r))
                struct(ll,rr)
        | RNode(cb,upd,ref) when (mb >= cb) ->
            partitionT mb k (loadR' upd kl ref)
        | _ -> // leaf or cb>mb; full subtree at one side
            let t = Root(kl,node)
            if (testCritbit mb k) 
                then struct(t,Empty) 
                else struct(Empty,t)

    /// Partition a tree such that all keys strictly less than the
    /// given key are in the left tree, and all remaining keys are
    /// in the right tree.
    let partitionK (k:Key) (t:Tree<'V>) : (Tree<'V> * Tree<'V>) =
        let struct(ll,rr) = partitionT 0 k t
        (ll,rr) 


    // TODO: structural diff!
    //  It is at least feasible to diff full LSM trees and compare
    //  RNodes for equality where needed.

    // Tree Codec
    module EncTree =
        let cLeaf  : byte = byte 'L'    // L(value)
        let cINode : byte = byte 'N'    // N(cb)(node)(key)(node)
        let cRNode : byte = byte 'R'    // R(cb)(updates)(ref)

        let cEmpty : byte = 128uy       // 0 from EncVarNat
        let cRoot  : byte = 129uy       // 1 from EncVarNat

        let inline updCodec cV = EncBTree.codec (EncOpt.codec cV)

        // Nodes can be read and written, but compaction is naive - it 
        // compacts only the values. Full compaction must be at the tree
        // layer (to properly update the sample keys). 
        //
        // RNode buffers are recorded as an array of (Key * Val opt) pairs.
        let nodeCodec (cV : Codec<'V>) =
            let cU = updCodec cV
            { new Codec<Node<'V>> with
                member c.Write node dst =
                    match node with
                    | Leaf v ->
                        EncByte.write cLeaf dst
                        cV.Write v dst
                    | INode(cb,l,kr,r) ->
                        EncByte.write cINode dst
                        EncVarNat.write (uint64 cb) dst
                        c.Write l dst
                        EncBytes.write kr dst
                        c.Write r dst
                    | RNode(cb,upd,ref) ->
                        EncByte.write cRNode dst
                        EncVarNat.write (uint64 cb) dst
                        cU.Write upd dst
                        EncLVRef.write ref dst
                member c.Read db src =
                    let b0 = EncByte.read src
                    if(cLeaf = b0) then
                        Leaf (cV.Read db src)
                    else if(cINode = b0) then
                        let cb = int (EncVarNat.read src)
                        let l = c.Read db src
                        let kr = EncBytes.read src
                        let r = c.Read db src
                        INode(cb,l,kr,r)
                    else if(cRNode <> b0) then
                        raise ByteStream.ReadError
                    else
                        let cb = int (EncVarNat.read src)
                        let upd = cU.Read db src
                        let ref = EncLVRef.read c db src
                        RNode(cb,upd,ref)
                member c.Compact db node =
                    // compact values and estimate size only
                    match node with
                    | Leaf v ->
                        let struct(v',szV) = cV.Compact db v
                        struct(Leaf v', 1 + szV)
                    | INode(cb,l,kr,r) ->
                        let struct(l',szL) = c.Compact db l
                        let struct(r',szR) = c.Compact db r
                        let szN = 1 + EncVarNat.size (uint64 cb)
                                    + szL + EncBytes.size kr + szR
                        struct(INode(cb,l',kr,r'),szN)
                    | RNode(cb,upd,ref) ->
                        let struct(upd',szU) = cU.Compact db upd
                        let sz = 1 + EncVarNat.size (uint64 cb) 
                                   + szU + EncLVRef.size
                        struct(RNode(cb,upd',ref),sz)
            }

        /// Construct a tree codec given a value codec.
        /// In this case, we have two thresholds:
        ///   - cThresh: approx size for INode compaction
        ///   - fThresh: approx size for RNode buffer flush 
        /// Sizes here are in bytes.
        let treeCodec (cThresh:int) (fThresh:int) (cV:Codec<'V>) =
            assert((cThresh >= 0) && (fThresh >= 0))
            let cU = updCodec cV  // for compaction
            let cN = nodeCodec cV // for read, write, stowage
            { new Codec<Tree<'V>> with
                member c.Write t dst =
                    match t with
                    | Empty -> EncByte.write cEmpty dst
                    | Root(kl,node) ->
                        EncByte.write cRoot dst
                        EncBytes.write kl dst
                        cN.Write node dst
                member c.Read db src =
                    let b0 = EncByte.read src
                    if (cEmpty = b0) then Empty else
                    if (cRoot <> b0) then raise ByteStream.ReadError else
                    let kl = EncBytes.read src
                    let node = cN.Read db src
                    Root(kl,node)
                member c.Compact db t =
                    match t with
                    | Root(kl,node) ->
                        match node with
                        | Leaf v ->
                            let struct(v',szV) = cV.Compact db v
                            let szT = 2 + EncBytes.size kl + szV
                            struct(Root(kl,Leaf v'),szT)
                        | INode(cb,l,kr,r) ->
                            let struct(tl',szL) = c.Compact db (Root(kl,l))
                            let struct(tr',szR) = c.Compact db (Root(kr,r))
                            let szCB = EncVarNat.size (uint64 cb)
                            match tl' with
                            | Root(kl',l') ->
                                match tr' with
                                | Root(kr',r') ->
                                    let szT = szL + szR + szCB
                                    let node' = INode(cb, l', kr', r')
                                    if ((szT - kl'.Length) < cThresh) then 
                                        struct(Root(kl',node'),szT) 
                                    else // perform stowage
                                        let ref = LVRef.stow cN db node'
                                        let rn = RNode(cb,BTree.Empty,ref)
                                        c.Compact db (Root(kl',rn))
                                | Empty -> struct(tl',szL)
                            | Empty -> struct(tr',szR)
                        | RNode(cb,BTree.Empty,ref) ->
                            // cannot compact or flush
                            let szR = 3 + EncBytes.size kl
                                        + EncVarNat.size (uint64 cb)
                                        + EncLVRef.size
                            struct(t,szR) 
                        | RNode(cb,upd,ref) ->
                            // potentially flush updates
                            let struct(upd',szUpd) = cU.Compact db upd
                            if ((szUpd + EncLVRef.size) < fThresh) then
                                let t' = Root(kl,RNode(cb,upd',ref))
                                let szT = 2 + EncBytes.size kl
                                            + EncVarNat.size (uint64 cb)
                                            + szUpd
                                            + EncLVRef.size
                                struct(t',szT)
                            else // flush updates, compact anew
                                let t' = loadR' upd' kl ref
                                c.Compact db t'
                    | Empty -> struct(Empty,1)
            }

        /// default heuristic thresholds
        let compactThresh : int = 30000
        let flushThresh : int = 7000
        let codec (cV:Codec<'V>) = treeCodec compactThresh flushThresh cV


    /// Codec for full LSM tree.
    let treeCodec (cV:Codec<'V>) = EncTree.codec cV

    /// Compact a tree in memory.
    ///
    /// Note: Even after compaction, a tree can be relatively large
    /// in memory or when serialized. If you want to treat trees as
    /// small values, use `stow` and the tree reference type instead!
    let inline compact (cV:Codec<'V>) (db:DB) (t:Tree<'V>) =
        Codec.compact (treeCodec cV) db t

    /// A bounded-size reference for a potentially large tree.
    ///
    /// This is convenient when you have large collections of trees, and
    /// you wish to control the size of elements when serialized. This is
    /// simply an alias for CVRef together with a few utility functions.
    type Ref<'V> = CVRef<Tree<'V>>

    /// A few hundred bytes is small enough for collections while large
    /// enough to keep many smaller JSON-like data structures inline or
    /// in memory.
    let refThresh : int = 400

    /// Codec for tree references (CVRef with refThresh)
    let refCodec (cV:Codec<'V>) = CVRef.codec refThresh (treeCodec cV)

    /// Wraps tree without compaction as if it were a stowed ref. The
    /// tree is not actually compacted or serialized. Useful when the
    /// tree is owned by another collection and may be compacted later. 
    let inline stow' (t:Tree<'V>) : Ref<'V> = CVRef.local t

    /// Compact tree into a bounded-size reference.
    ///
    /// There is a short window where GC may intercede and prevent
    /// stowage unless the value is immediately serialized.
    let stow (cV:Codec<'V>) (db:DB) (t:Tree<'V>) : Ref<'V> =
        Codec.compact (refCodec cV) db (stow' t)

    /// non-caching load
    let inline load' (ref:Ref<'V>) : Tree<'V> = CVRef.load' ref

    /// caching load.
    let inline load  (ref:Ref<'V>) : Tree<'V> = CVRef.load ref


type LSMTree<'V> = LSMTree.Tree<'V>
type LSMTreeRef<'V> = LSMTree.Ref<'V>


