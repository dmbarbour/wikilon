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

    // Buffer of recent adds and removes, represented in-memory, plus
    // the prior 'least key' for the node. 
    type Updates<'V> = (BTree<'V option> * Key) option

    type Node<'V> =
        | Leaf of 'V
        | INode of Critbit * Node<'V> * Key * Node<'V>              // Inner Tree Node
        | RNode of Critbit * Updates<'V> * LVRef<Node<'V>>          // Remote Node with Update Buffer

    // A Tree is either empty or has a key and a node.
    //
    // Note: the least-key might not actually be a member of the tree 
    // due to how the LSM-tree buffers removals. Regardless, it is a
    // valid sample key for computing critbits!
    //
    // Relatedly, it is possible to have an empty tree that is represented
    // by a remote node with a ton of buffered deletions! So the 'isEmpty'
    // function is not trivial.
    type Tree<'V> =
        | Empty
        | Root of Key * Node<'V>

    let empty : Tree<_> = Empty
    let inline singleton (k:Key) (v:'V) : Tree<'V> = Root(k, Leaf v)

    // we've matched the least-key recorded at the parent, but it might
    // be a pending removal, so we still need to return an optional value.
    let rec private tryGetLKV (kl:Key) (node:Node<'V>) : 'V option =
        match node with
        | INode (_,l,_,_) -> tryGetLKV kl l
        | Leaf v -> Some v
        | RNode (_,None,ref) -> tryGetLKV kl (LVRef.load ref)
        | RNode (_,Some(buff,kl0),ref) ->
            // either in buffer or should be least-key in ref
            match BTree.tryFind kl buff with
            | Some vOpt -> vOpt // recently added or removed
            | None -> assert(kl = kl0); tryGetLKV kl (LVRef.load ref)

    let rec private tryFindN (mb:Critbit) (k:Key) (kl:Key) (node:Node<'V>) : 'V option =
        match node with
        | INode (cb, l, kr, r) ->
            if testCritbit cb k 
                then tryFindN mb k kr r
                else tryFindN mb k kl l
        | Leaf v ->
            let keysMatch = Option.isNone (findCritbit mb k kl)
            if keysMatch then Some v else None
        | RNode (cb, None, ref) ->
            match findCritbit mb k kl with
            | None -> tryGetLKV kl (LVRef.load ref)
            | Some mb' ->
                // stop if diff in prefix or if smaller than least-key
                let stop = (mb' < cb) || (testCritbit mb' kl)
                if stop then None else
                tryFindN mb' k kl (LVRef.load ref)
        | RNode (cb, Some(buff,kl0), ref) ->
            match BTree.tryFind k buff with
            | Some vOpt -> vOpt
            | None -> tryFindN mb k kl0 (RNode(cb, None, ref))

    /// Find some value associated with a key, or return none.
    ///
    /// This will access values in recent update buffers before 
    /// checking remote stowage references. The lookup cost is
    /// hence pretty good for a recent working set.
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
    
    /// Check whether key is present in tree. 
    let inline containsKey k t = Option.isSome (tryFind k t)
        // cannot be short-circuited because the 'least key' in the
        // parent node is not guaranteed to be present in the tree.

    let private updAdd (k:Key) (v:'V) (kl:Key) (upd:Updates<'V>) : Updates<'V> =
        match upd with
        | None -> Some(BTree.singleton k (Some v), kl)
        | Some (buff, kl0) -> Some (BTree.add k (Some v) buff, kl0)

    let rec private setLKV (kl:Key) (v:'V) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb, l, kr, r) -> 
            let l' = setLKV kl v l
            INode (cb, l', kr, r)
        | Leaf _ -> Leaf v
        | RNode (cb, upd, ref) -> 
            let upd' = updAdd kl v kl upd
            RNode (cb, upd', ref)

    let rec private addLKV (mb:Critbit) (k:Key) (v:'V) (kl:Key) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb, l, kr, r) when (mb >= cb) ->
            let l' = addLKV mb k v kl l
            INode (cb, l', kr, r)
        | RNode (cb, upd, ref) when (mb >= cb) ->
            let upd' = updAdd k v kl upd
            RNode (cb, upd', ref)
        | _ -> INode(mb, Leaf v, kl, node)

    // assume k > kl at mb
    let rec private addRKV (mb:Critbit) (k:Key) (v:'V) (kl:Key) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb, l, kr, r) when (mb >= cb) ->
            if (mb > cb) then
                let l' = addRKV mb k v kl l
                INode (cb, l', kr, r)
            else
                match findCritbit (1+cb) k kr with
                | Some mb' ->
                    if testCritbit mb' k 
                        then INode (cb, l, kr, addRKV mb' k v kr r)
                        else INode (cb, l, k,  addLKV mb' k v kr r)
                | None -> INode (cb, l, kr, setLKV kr v r)
        | RNode (cb, upd, ref) when (mb >= cb) -> 
            let upd' = updAdd k v kl upd
            RNode (cb, upd', ref)
        | _ -> INode (mb, node, k, Leaf v)


    /// Add a key-value association to a tree. 
    ///
    /// Returns a new tree with the update applied. As an LSM-tree 
    /// variant, add is buffered: we don't touch remote nodes. This
    /// buffer is NOT automatically flushed. It must be flushed by
    /// explicit compaction.
    let add (k:Key) (v:'V) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Root (kl, n) -> 
            match findCritbit 0 k kl with
            | Some mb -> 
                if testCritbit mb k
                    then Root (kl, addRKV mb k v kl n)
                    else Root (k,  addLKV mb k v kl n)
            | None -> Root (kl, setLKV kl v n)
        | Empty -> singleton k v

    // min key assuming equality up to critbit
    let private minKeyCB mb a b =
        match findCritbit mb a b with
        | Some mb' when (testCritbit mb' b) -> a
        | _ -> b

    // remove from update buffer, optimizing the case where it is
    // locally obvious that the removed key is not in the remote node.
    let inline private updRem (k:Key) (kl:Key) (cb:Critbit) (upd:Updates<'V>) : (Key * Updates<'V>) =
        let (buff,kl0) =
            match upd with
            | None -> (BTree.empty, kl)
            | Some (buff,kl0) -> (buff,kl0)
        match findCritbit 0 k kl0 with
        | Some mb when (mb < cb) -> (kl,upd) // current node not in k's path
        | Some mb when (testCritbit mb kl0) -> // k is not in remote ref
            let buff' = BTree.remove k buff // remove recent add, if any
            match buff' with
            | BTree.Empty -> (kl0, None) // update buffer cleared
            | BTree.Root(klB, _) -> ((minKeyCB cb klB kl0), Some(buff',kl0))
        | _ -> 
            let buff' = BTree.add k None buff
            (kl, Some(buff',kl0))

    // remove a key from a node.
    let rec private removeN (k:Key) (kl:Key) (node:Node<'V>) : Tree<'V> =
        match node with
        | INode (cb, l, kr, r) ->
            if testCritbit cb k then
                match removeN k kr r with
                | Root(kr',r') -> Root(kl, INode(cb, l, kr', r'))
                | Empty -> Root(kl,l)
            else 
                match removeN k kl l with
                | Root(kl',l') -> Root(kl', INode(cb, l', kr, r))
                | Empty -> Root(kr,r)
        | Leaf _ -> if (k = kl) then Empty else Root(kl,node)
        | RNode (cb, upd, ref) ->
            // delay removal via buffer
            let (kl',upd') = updRem k kl cb upd
            Root(kl',RNode(cb,upd',ref))

    /// Remove key from tree, returning modified tree.
    ///
    /// Like add, this is a buffered operation and does not touch 
    /// any remote nodes. Key removal from a remote node will be
    /// delayed until future compaction when the buffer is oversized. 
    ///
    /// If there is a high probability that the key is not present,
    /// and if keys are large enough for the extra buffer overhead to
    /// be a concern, favor use of checkedRemove.
    ///
    /// Note: this is a 'blind' removal, although in the worst case
    /// it adds the key to a buffer. If keys are large and the key
    /// is probably not in the buffer, favor use of checkedRemove. 
    let remove (k:Key) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Root (kl, node) -> removeN k kl node
        | Empty -> Empty

    /// Remove a key, filtered by containsKey.
    let inline checkedRemove k t = if containsKey k t then remove k t else t


    let private applyUpd t k vOpt =
        match vOpt with
        | None -> remove k t
        | Some v -> add k v t

    let private applyUpdates (upd:Updates<'V>) (kl:Key) (node:Node<'V>) : Tree<'V> =
        match upd with
        | None -> Root(kl,node) 
        | Some(buff,kl0) -> BTree.fold applyUpd (Root(kl0,node)) buff

    let inline private loadR' (upd:Updates<'V>) (kl:Key) (ref:LVRef<Node<'V>>) : Tree<'V> =
        applyUpdates upd kl (LVRef.load' ref)

    let inline private loadR (upd:Updates<'V>) (kl:Key) (ref:LVRef<Node<'V>>) : Tree<'V> =
        applyUpdates upd kl (LVRef.load ref)

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
            | RNode(_,upd,ref) -> stepL ((loadR upd kl ref)::s)

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
            | RNode(_,upd,ref) -> stepR ((loadR upd kl ref)::s)

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



(*

    // encoding the update buffer
    module EncBuff =
                                     // in context:
        let cNoUpdates = byte '='    // R={hash}
        let cSomeUpdates = byte '+'  // R+(updates)(oldKey){hash}

        let codec (cV:Codec<'V>) = 
            let cKV = EncPair.codec (EncBytes.codec) (EncOption.codec cV)
            { new Codec<Node<'V>> with
                member __.Write buff dst =
                    match buff with
                    | None -> EncByte.write cNoUpdates dst
                    | Some (t,oldLK) ->
                        assert(not (CBTree.isEmpty t))
                        EncByte.write cSomeUpdates dst
                        EncArray.write cKV (CBTree.toArray t) dst
                        EncBytes.write oldLK dst
                member __.Read db src =
                    let b0 = EncByte.read src
                    if (cNoUpdates = b0) then None 
                    elif (cSomeUpdates <> b0) then raise ByteStream.ReadError
                    else 
                        let t = CBTree.fromArray (EncArray.read cKV db src)
                        let oldLK = EncBytes.read src
                        Some (t, oldLK)
                member __.Compact db buff =
                    match buff with
                    | None -> struct(buff,1)
                    | Some (t,oldLK) ->
                        let struct(a,szB) = EncArray.compact' cKV db (CBTree.toArray t)
                        let t' = CBTree.fromArray a
                        struct(Some(t',oldLK), szB + EncBytes.size oldLK)
            }

    // Nodes are encoded almost directly, but require a special context
    // of the prior least-key when propagating updates 
    module EncNode =
        let cLeaf  : byte = byte 'L'
        let cINode : byte = byte 'N'
        let cRNode : byte = byte 'R'

        // heuristic size threshold for compaction of a node.
        // in this case, I'm favoring relatively large nodes.
        let compactThreshold : int = 14000

        let inline buffToArray buff = Array.ofSeq (CBTree.toSeq buff)
        let inline arrayToBuff arr = 
            Array.fold (fun buff (k,v) -> CBTree.add k v buff) (CBTree.empty) arr
        let kvCodec cV = EncPair.codec (EncBytes.codec) (EncOption.codec cV)

        let codec (cV:Codec<'V>) =
            let cBuff = EncBuff.codec cV
            { new Codec<Node<'V>> with
                member cN.Write node dst =
                    match node with
                    | Leaf v ->
                        EncByte.write cLeaf dst
                        cV.Write v dst
                    | RNode (buff, ref) ->
                        EncByte.write cRNode dst
                        cBuff.Write buff dst
                        EncLVRef.write ref dst
                    | INode (cb, l, kr, r) ->
                        EncByte.write cINode dst
                        EncVarNat.write (uint64 cb) dst
                        cN.Write l dst
                        EncBytes.write kr dst
                        cN.Write r dst
                member cN.Read db src =
                    let b0 = EncByte.read src
                    if(cLeaf = b0) then
                        Leaf (cV.Read db src)
                    elif(cRNode = b0) then
                        let buff = cBuff.Read db src
                        let ref = EncLVRef.read cN db src
                        RNode (buff,ref)
                    elif(cINode <> b0) then
                        raise ByteStream.ReadError
                    else
                        let cb = int (EncVarNat.read src)
                        let l = cN.Read db src
                        let kr = EncBytes.read src
                        let r = cN.Read db src
                        INode (cb, l, kr, r)
                member cN.Compact db node =
                    match node with
                    | Leaf v ->
                        let struct(v',szV) = cV.Compact db v
                        struct(Leaf v', 1 + szV)
                    | RNode (buff, ref) ->
                        let struct(buff', szBuff) = cBuff.Compact db buff
                        if(szBuff < compactThreshold) 
                            then struct(RNode(buff',ref), 1 + szBuff + EncLVRef.size)
                            else cN.Compact db (applyUpdates buff (LVRef.load' ref))
                    | INode (cb, l, kr, r) ->
                        let struct(l',szL) = cN.Compact db l
                        let struct(r',szR) = cN.Compact db r
                        let node' = INode (cb, l', kr, r')
                        let szN = 1 + EncVarNat.size (uint64 cb)
                                    + szL + EncBytes.size kr + szR
                        if(szN < compactThreshold) then struct(node',szN) else
                        let ref = LVRef.stow cN db node'
                        struct(RNode(None,ref), 2+EncLVRef.size) // R={hash}
            }
                 

*)
