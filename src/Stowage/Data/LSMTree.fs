namespace Stowage
open Data.ByteString

/// A Log Structured Merge (LSM) Tree modeled above Stowage
///
/// An LSM-tree is essentially a key-value tree with a built-in buffer
/// model. It is optimized for write-heavy processes. Recent updates
/// are written near to the root of the tree. Some keys may be marked
/// for deletion.
///
/// LSM-trees are especially suitable for on-disk storage because the 
/// buffering reduces frequency and increases batch-size for on-disk
/// updates, and potentially shares old structure for longer. Although
/// designed for filesystems, this also works very nicely with Stowage.
///
/// There are costs to this design in the form of larger lookup times
/// and greater overhead to store older versions for a key.
module LSMTree =
    type Key = ByteString
    type Critbit = CBTree.Critbit
    let inline testCritbit cb k = CBTree.testCritbit cb k
    let inline findCritbit cbMin a b = CBTree.findCritbit cbMin a b
    type TreeSize = uint64

    /// An update buffer is currently encoded as a critbit tree, with
    /// value None for deletion and Some for insertion. However, this
    /// CBTree is fully in memory, and will be serialized as an array.
    type Updates<'V> = CBTree<'V option>

    /// I track whether we have dropped the least-key for an RNode.
    /// Updates on an RNode should all be to the right of this least
    /// key, such that we only update keys represented within the
    /// node. A least key update is handled via new internal node.
    type DropKL = bool
    let inline private addUpd k vOpt u = CBTree.add k vOpt u

    type Node<'V> =
        | Leaf of 'V
        | INode of Critbit * Node<'V> * Key * Node<'V> 
        | RNode of Critbit * Updates<'V> * DropKL * LVRef<Node<'V>>

    type Tree<'V> =
        | Empty
        | Root of Key * Node<'V>

    let empty : Tree<_> = Empty
    let inline singleton (k:Key) (v:'V) : Tree<'V> = Root(k, Leaf v)

    // without a Key in our root, our Add function may need to 

    let add (k:Key) (v:'V) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Empty -> singleton k v
        | Root n -> Root (addN k v n)

    // update an existing least-key value
    let rec private setLKV (kl:Key) (v:'V) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb, l, kr, r) -> 
            let l' = setLKV kl v l
            INode (cb, l', kr, r)
        | RNode (upd, ref) ->
            let upd' = 
                match upd with
                | None -> Some (CBTree.singleton kl (Some v), kl)
                | Some (t,oldLK) -> Some (CBTree.add kl (Some v) t, oldLK)
            RNode (upd',ref)
        | Leaf _ -> Leaf v
    
    let rec private addLKV (ncb:Critbit) (kl0:Key) (k:Key) (v:'V) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb, l, kr, r) when (cb < ncb) ->
            assert(not (testCritbit cb kl0))
            let l' = addLKV ncb kl0 k v l
            INode (cb, l', kr, r) 
        | RNode (upd, ref) -> 
            let upd' =
                match upd with
                | None -> Some (CBTree.singleton k (Some v), kl0)
                | Some(t,oldLK) -> Some (CBTree.add k (Some v) t, oldLK)
            RNode (upd', ref) 
        | _ ->
            assert(testCritbit ncb kl0)
            INode (ncb, Leaf v, kl, node)

    // add or update key-value element somewhere to right of least-key
    //  uses a critbit relative to least-key for efficient insertion
    let rec private addRKV (mcb:Critbit) (kl:Key) (k:Key) (v:'V) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb, l, kr, r) ->
            if (cb < mcb) then // diff after node
                assert(not (testCritbit cb k)) 
                INode (cb, addRKV mcb kl k v l, kr, r) 
            elif (cb > mcb) then // diff before node
                assert(not (testCritbit mcb kr))
                INode (mcb, node, k, Leaf v)
            else // diff aligns with kr
                assert(testCritbit cb k)
                match findCritbit (1+cb) k kr with
                | Some ncb ->
                    if testCritbit ncb k
                        then INode (cb, l, kr, addRKV ncb kr k v r)
                        else INode (cb, l, k, addLKV ncb kr v r)
                | None -> INode (cb, l, kr, setLKV kr v r) // update at kr
        | RNode (upd, ref) -> 
            let upd' = 
                match upd with
                | None -> Some (CBTree.singleton k (Some v), kl)
                | Some (t,oldLK) -> Some (CBTree.add k (Some v) t, oldLK)
            RNode (upd', ref)
        | Leaf _ -> INode (mcb, node, k, Leaf v)

    let add (k:Key) (v:'V) (t:Tree<'V>) =
        match t with
        | Root(tk,n) ->
            match findCritbit 0 k tk with
            | Some cb ->
                if testCritbit cb k
                    then Root(tk, addRKV cb tk k v n)
                    else Root(k, addLKV cb tk k v n)
            | None -> Root (tk, setLKV tk v n)
        | Empty -> singleton k v

    // remove least-key value for a node, return a tree.
    let rec private removeLKV (kl:Key) (node:Node<'V>) : Tree<'V> =
        match node with
        | INode (cb, l, kr, r) ->
            match removeLKV kl l with
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
        let compactThreshold : int = 30000

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
                 
