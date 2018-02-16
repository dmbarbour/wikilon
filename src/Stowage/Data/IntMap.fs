namespace Stowage
open Data.ByteString

/// The IntMap is a map specialized for integral keys. This is useful
/// for modeling persistent arrays, sparse arrays, hash maps. A radix
/// tree is used under the hood, hence tree height is never greater 
/// than integer width (64 bits) and may be less.
///
/// As a Stowage data structure, the main feature here is integration
/// with Stowage databases. This IntMap supports stowage at any inner
/// node of the tree - i.e. at any bit where a decision is made. But
/// a CVRef size threshold ensures we don't write small nodes; if the
/// values are small, we could have hundreds per write to Stowage.
///
/// Use of stowage is not automatic. Explicit compaction is required.
/// Hence, updates are 'buffered' in memory between compactions. If
/// compaction is not used, this IntMap is a memory-only structure.
module IntMap =

    type Key = uint64   
    type Critbit = byte

    module Critbit =

        // return high bit for a uint16 (domain 0..63).
        let highBitIndex (x0:uint64) : byte =
            let mutable x = x0
            let mutable r = 0uy
            if(0UL <> (0xFFFFFFFF00000000UL &&& x)) then r <- (32uy + r); x <- (x >>> 32)
            if(0UL <> (0x00000000FFFF0000UL &&& x)) then r <- (16uy + r); x <- (x >>> 16)
            if(0UL <> (0x000000000000FF00UL &&& x)) then r <- ( 8uy + r); x <- (x >>>  8)
            if(0UL <> (0x00000000000000F0UL &&& x)) then r <- ( 4uy + r); x <- (x >>>  4)
            if(0UL <> (0x000000000000000CUL &&& x)) then r <- ( 2uy + r); x <- (x >>>  2)
            if(0UL <> (0x0000000000000002UL &&& x)) then (1uy + r) else r

    /// A node may be directly used as a non-empty tree.
    ///
    /// A node is either a Leaf or an Inner node with two children.
    /// The "key" in each case is actually a key segment - a suffix
    /// for the leaf, a right-shifted slice for inner nodes.
    type Node<'V> =
        | Leaf of Key * 'V
        | Inner of Key * Critbit * CVRef<struct(Node<'V> * Node<'V>)>


    module Node =

        let inline local l r = CVRef.local (struct(l,r))
        let inline load np = CVRef.load np
        let inline load' np = CVRef.load' np

        // prefix and suffix exclude the critbit
        let inline prefix (k:Key) (b:Critbit) : Key = ((k >>> int b) >>> 1)
        let inline testCritbit (k:Key) (b:Critbit) : bool =
            (0UL <> (k &&& (1UL <<< int b)))
        let inline suffixMask (b : Critbit) : Key = ((1UL <<< int b) - 1UL)
        let inline suffix (k:Key) (b:Critbit) : Key = (k &&& suffixMask b)

        // merge a key-prefix with key in node (via bitwise OR) 
        let mergeKP kp node =
            match node with
            | Leaf (k,v) -> Leaf ((kp ||| k), v)
            | Inner (p,b,np) -> 
                let p' = (prefix kp b) ||| p
                Inner (p', b, np)

        let inline keyPrefixL p b = ((p <<< 1) <<< int b)  
        let inline keyPrefixR p b = (keyPrefixL p b) ||| (1UL <<< int b)
        let inline mergeL p b l = mergeKP (keyPrefixL p b) l
        let inline mergeR p b r = mergeKP (keyPrefixR p b) r 


        let rec tryFind kf node =
            match node with
            | Inner (p, b, np) ->
                if (p <> prefix kf b) then None else
                let inR = testCritbit kf b
                let kf' = suffix kf b
                let struct(l,r) = load np
                if inR then tryFind kf' r else tryFind kf' l
            | Leaf (k,v) -> if (kf = k) then Some v else None

        let inline containsKey kf node = 
            Option.isSome (tryFind kf node)

        // test whether a key is remote
        let rec isKeyRemote kf node =
            match node with
            | Inner (p, b, np) ->
                if (p <> prefix kf b) then false else
                match np with
                | Local (struct(l,r),_) ->
                    let inR = testCritbit kf b
                    let kf' = suffix kf b
                    if inR then isKeyRemote kf' r 
                           else isKeyRemote kf' l
                | Remote _ -> true
            | Leaf _ -> false

        // pull key into local memory
        let rec touch kf node =
            match node with
            | Inner (p, b, np) ->
                if (p <> prefix kf b) then node else
                let inR = testCritbit kf b
                let kf' = suffix kf b
                let struct(l,r) = load' np
                if inR 
                    then Inner (p, b, local l (touch kf' r))
                    else Inner (p, b, local (touch kf' l) r)
            | Leaf _ -> node

        // add or update a key-value pair
        let rec add k v node =
            match node with
            | Inner (p, b, np) ->
                let kp = prefix k b
                if (p = kp) then // add with shared prefix
                    let inR = testCritbit k b
                    let k' = suffix k b
                    let struct(l,r) = load' np
                    let np' = if inR then local l (add k' v r)
                                     else local (add k' v l) r
                    Inner (p, b, np')
                else // insert just above current node
                    let kl = keyPrefixL p b
                    let b' = Critbit.highBitIndex (kl ^^^ k)
                    assert(b' > b)
                    let oldNode = Inner(prefix (suffix kl b') b, b, np)
                    let newLeaf = Leaf(suffix k b', v)
                    let inR = testCritbit k b'
                    let np' = if inR then local oldNode newLeaf
                                     else local newLeaf oldNode
                    Inner (prefix k b', b', np')
            | Leaf (kl,vl) ->
                if (kl = k) then Leaf(k,v) else
                let b = Critbit.highBitIndex (kl ^^^ k)
                let oldLeaf = Leaf (suffix kl b, vl)
                let newLeaf = Leaf (suffix k  b, v)
                let inR = testCritbit k b
                let np = if inR then local oldLeaf newLeaf 
                                else local newLeaf oldLeaf
                Inner(prefix k b, b, np)

        let rec remove (k:Key) (node:Node<'V>) : Node<'V> option =
            match node with
            | Inner (p, b, np) ->
                if (p <> prefix k b) then Some node else
                let inR = testCritbit k b
                let struct(l,r) = load np
                if inR then 
                    match remove (suffix k b) r with
                    | Some r' -> Some (Inner (p, b, local l r'))
                    | None -> Some (mergeL p b l)
                else
                    match remove (suffix k b) l with
                    | Some l' -> Some (Inner (p, b, local l' r))
                    | None -> Some (mergeR p b r)
            | Leaf (kl,_) -> if (kl = k) then None else Some node


        let rec splitAtKey k node =
            match node with
            | Inner (p, b, np) ->
                let kp = prefix k b
                if (p > kp) then struct(None, Some node) else // node keys > k
                if (p < kp) then struct(Some node, None) else // node keys < k
                // need to split children
                let struct(l,r) = load' np
                let inR = testCritbit k b
                if inR then
                    let struct(rl,rr) = splitAtKey (suffix k b) r
                    let l' = 
                        match rl with
                        | Some n -> Inner(p,b,local l n)
                        | None -> mergeL p b l
                    let rr' = 
                        match rr with
                        | Some n -> Some (mergeR p b n)
                        | None -> None
                    struct(Some l', rr')
                else
                    let struct(ll,lr) = splitAtKey (suffix k b) l
                    let ll' = 
                        match ll with
                        | Some n -> Some (mergeL p b n)
                        | None -> None
                    let r' = 
                        match lr with
                        | Some n -> Inner(p,b,local n r)
                        | None -> mergeR p b r
                    struct(ll', Some r')
            | Leaf (kl,_) ->
                if (kl >= k) 
                    then struct(None, Some node) 
                    else struct(Some node, None)

        let inline keyPrefixes kp p b =
            let kl = kp ||| (keyPrefixL p b)
            let kr = (kl ||| (1UL <<< int b))
            struct(kl,kr)

        // map function for nodes with potential compaction
        //  here 'c' should be a compaction function or `id`. It's simply
        //  applied for every new node. And 'fn' is applied to every value.
        let rec map (c:Node<'B> -> Node<'B>) (fn:Key -> 'A -> 'B) (kp:Key) (node:Node<'A>) : Node<'B> =
            match node with
            | Leaf (k,v) -> 
                let v' = fn (kp ||| k) v
                c (Leaf (k,v'))
            | Inner (p, b, np) ->
                let struct(kl,kr) = keyPrefixes kp p b
                let struct(l,r) = load' np
                let l' = map c fn kl l
                let r' = map c fn kr r
                c (Inner (p, b, local l' r'))

        // filter and map with potential compaction.
        let rec filterMap c fn kp node =
            match node with
            | Leaf (k,v) ->
                match fn (kp ||| k) v with
                | Some v' -> Some (c (Leaf (k, v')))
                | None -> None
            | Inner (p, b, np) ->
                let struct(kl,kr) = keyPrefixes kp p b
                let struct(l,r) = load' np
                let lFM = filterMap c fn kl l
                let rFM = filterMap c fn kr r
                match (lFM,rFM) with
                | (Some l', Some r') ->
                    let node' = c (Inner (p, b, local l' r'))
                    Some node'
                | (Some l', None) -> Some (mergeL p b l')
                | (None, Some r') -> Some (mergeR p b r')
                | (None, None) -> None

        // TODO: functions to merge trees would be appreciated, e.g.
        // for an efficient union or subtraction. 

        let rec iter fn kp node =
            match node with
            | Leaf (k,v) -> fn (kp ||| k) v
            | Inner (p, b, np) ->
                let struct(kl,kr) = keyPrefixes kp p b
                let struct(l,r) = load' np
                iter fn kl l
                iter fn kr r

        let rec fold fn s kp node =
            match node with
            | Leaf (k,v) -> fn s (kp ||| k) v
            | Inner (p, b, np) ->
                let struct(kl,kr) = keyPrefixes kp p b
                let struct(l,r) = load' np
                fold fn (fold fn s kl l) kr r

        let rec foldBack fn kp node s =
            match node with
            | Leaf (k,v) -> fn (kp ||| k) v s
            | Inner (p, b, np) ->
                let struct(kl,kr) = keyPrefixes kp p b
                let struct(l,r) = load' np
                foldBack fn kl l (foldBack fn kr r s)

        let rec tryPick fn kp node =
            match node with
            | Leaf (k,v) -> fn (kp ||| k) v
            | Inner (p, b, np) ->
                let struct(kl,kr) = keyPrefixes kp p b
                let struct(l,r) = load' np
                let pickL = tryPick fn kl l
                if Option.isSome pickL then pickL else
                tryPick fn kr r

        let rec toSeq kp node =
            seq {
                match node with
                | Leaf (k,v) -> yield ((kp ||| k), v)
                | Inner (p, b, np) ->
                    let struct(kl,kr) = keyPrefixes kp p b
                    let struct(l,r) = load' np
                    yield! toSeq kl l
                    yield! toSeq kr r
            }

        let rec toSeqR kp node = 
            seq {
                match node with
                | Leaf (k,v) -> yield ((kp ||| k), v)
                | Inner (p, b, np) ->
                    let struct(kl,kr) = keyPrefixes kp p b
                    let struct(l,r) = load' np
                    yield! toSeqR kr r
                    yield! toSeqR kl l
            }


        let seqInL kp n = toSeq kp n |> Seq.map (fun (k,v) -> (k, InL v))
        let seqInR kp n = toSeq kp n |> Seq.map (fun (k,v) -> (k, InR v))

        let inline private refEq npa npb =
            if System.Object.ReferenceEquals(npa,npb) then true else
            match (npa,npb) with
            | (Remote ra, Remote rb) -> (ra = rb)
            | _ -> false

        let keyRange kp node =
            match node with
            | Leaf (ks,_) -> 
                let k = kp ||| ks
                struct(k,k)
            | Inner (p,b,_) ->
                let kmin = kp ||| (keyPrefixL p b)
                let cb = 1UL <<< int b
                let kmax = kmin ||| cb ||| (cb - 1UL)
                struct(kmin,kmax)

        let private nodeInRight kp node cb =
            match node with
            | Leaf (kf,_) -> testCritbit (kp ||| kf) cb
            | Inner (p,b,_) -> 
                assert(b < cb) // cannot place nodes out of critbit order
                let kmin = kp ||| (keyPrefixL p b)
                testCritbit kmin cb

        // reference-equality based diff, including equality for
        // stowage resources, intended to minimize observation of nodes.
        let rec diffRef kpa na kpb nb =
            // filter the most obviously equivalent nodes
            let eq = (kpa = kpb) && System.Object.ReferenceEquals(na,nb)
            if eq then Seq.empty else
            seq {
                // detect non-overlapping keys to simplify code
                let struct(ka_min,ka_max) = keyRange kpa na
                let struct(kb_min,kb_max) = keyRange kpb nb
                if (ka_max < kb_min) then
                    yield! seqInL kpa na 
                    yield! seqInR kpb nb
                else if(kb_max < ka_min) then
                    yield! seqInR kpb nb
                    yield! seqInL kpa na
                // otherwise handle overlapping keys
                else 
                    match na with
                    | Leaf (_,va) ->
                        match nb with
                        | Leaf (ksb,vb) ->
                            yield ((kpb ||| ksb), InB (va,vb))
                        | Inner (pb,bb,npb) -> 
                            yield! diffRefSplitB kpa na kpb pb bb npb
                    | Inner (pa,ba,npa) ->
                        match nb with 
                        | Leaf _ -> 
                            yield! diffRefSplitA kpa pa ba npa kpb nb 
                        | Inner (pb,bb,npb) ->
                            // split node with larger critbit
                            if (ba < bb) then
                                yield! diffRefSplitB kpa na kpb pb bb npb
                            else if(bb < ba) then
                                yield! diffRefSplitA kpa pa ba npa kpb nb 
                            else // overlap keys with same critbit.
                                // The two paths have identical keys. 
                                if refEq npa npb then () else
                                let struct(kpl,kpr) = keyPrefixes kpa pa ba
                                let struct(nal,nar) = load' npa
                                let struct(nbl,nbr) = load' npb
                                yield! diffRef kpl nal kpl nbl
                                yield! diffRef kpr nar kpr nbr
            }

        and private diffRefSplitB kpa na kpb pb bb npb =
            let struct(kbl,kbr) = keyPrefixes kpb pb bb
            let struct(nbl,nbr) = load' npb
            if nodeInRight kpa na bb
                then Seq.append (seqInR kbl nbl) (diffRef kpa na kbr nbr)
                else Seq.append (diffRef kpa na kbl nbl) (seqInR kbr nbr)

        and private diffRefSplitA kpa pa ba npa kpb nb =
            let struct(kal,kar) = keyPrefixes kpa pa ba
            let struct(nal,nar) = load' npa
            if nodeInRight kpb nb ba
                then Seq.append (seqInL kal nal) (diffRef kar nar kpb nb)
                else Seq.append (diffRef kal nal kpb nb) (seqInL kar nar)

        // monotonic key suffixes
        let private validKeySuffix b node =
            let limit = (1UL <<< int b) 
            match node with
            | Leaf (k,_) -> (limit > k)
            | Inner (pn,bn,_) -> 
                (b > bn) && ((prefix limit bn) > pn)

        let rec validate node =
            match node with
            | Leaf _ -> true
            | Inner (p, b, np) ->
                let struct(l,r) = load' np
                (63uy >= (1uy + b + Critbit.highBitIndex p))
                    && (validKeySuffix b l) && (validate l)
                    && (validKeySuffix b r) && (validate r)

    /// An IntMap tree is empty or has a node with keys and data.
    type Tree<'V> = Node<'V> option

    /// Deeply validate invariant structure of the tree.
    let validate t =
        match t with
        | Some n -> Node.validate n 
        | None -> true

    let empty : Tree<_> = None
    let isEmpty (t:Tree<_>) = Option.isNone t
    let singleton (k:Key) (v:'V) : Tree<'V> = Some(Leaf(k,v))

    /// Basic lookup on tree.
    let tryFind (k:Key) (t:Tree<'V>) : 'V option =
        match t with
        | Some n -> Node.tryFind k n
        | None -> None

    /// Check for whether a tree contains a specific key.
    let inline containsKey k t = Option.isSome (tryFind k t)

    /// Find or raise `System.Collection.Generic.KeyNotFoundException`
    let find (k:Key) (t:Tree<'V>) : 'V =
        match tryFind k t with
        | Some v -> v
        | None -> raise (System.Collections.Generic.KeyNotFoundException())

    /// Return tree with data at the given key loaded, or enough data
    /// to verify presence of the key without Stowage loads.
    let touch (k:Key) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Some n -> Some (Node.touch k n)
        | None -> None

    /// Add or update a key-value pair. Returns a new tree with the
    /// modification applied. 
    let add (k:Key) (v:'V) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Some n -> Some (Node.add k v n)
        | None -> singleton k v

    /// Remove a key and its associated value from the tree. Returns
    /// a new tree with the modification applied. Note: this will 
    /// `touch` the tree along the removed key. Use checkedRemove to
    /// preserve Stowage references when key isn't present in tree.
    let remove (k:Key) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Some n -> Node.remove k n
        | None -> None

    /// Add or remove key.
    let adjust (k:Key) (vOpt: 'V option) (t:Tree<'V>) : Tree<'V> =
        match vOpt with
        | Some v -> add k v t
        | None -> remove k t

    /// Remove that doesn't `touch` tree when key is not present. This
    /// does require an extra lookup step, but it can save redundant
    /// compaction efforts. 
    let inline checkedRemove k t = 
        if containsKey k t then remove k t else t

    /// Partition a tree at a specified key. Keys strictly less than
    /// the specified key will be in the left tree, while keys greater
    /// or equal will be placed in the right tree.
    let splitAtKey (k:Key) (t:Tree<'V>) : struct(Tree<'V> * Tree<'V>) =
        match t with
        | Some n -> Node.splitAtKey k n
        | None -> struct(None,None)

    /// Map a function to every value in a tree. (see compactingMap.)
    let map fn t =
        match t with
        | Some n -> Some (Node.map id fn 0UL n)
        | None -> None 

    /// Filter and Map a function over a tree. (see compactingFilterMap.)
    let filterMap (fn:Key -> 'V -> 'U option) (t:Tree<'V>) : Tree<'U> =
        match t with
        | Some n -> Node.filterMap id fn 0UL n
        | None -> None

    /// Filter a tree with a given function. (See compactingFilter.)
    let inline filter (pred:Key -> 'V -> bool) (t:Tree<'V>) : Tree<'V> =
        filterMap (fun k v -> if pred k v then Some v else None) t

    /// Iterate over every element in the map (in key order).
    let iter fn t =
        match t with
        | Some n -> Node.iter fn 0UL n
        | None -> ()

    /// Fold over every element in the map (in key order).
    let fold fn s t =
        match t with
        | Some n -> Node.fold fn s 0UL n
        | None -> s

    /// Fold over every element in the map (reverse key order).
    let foldBack fn t s =
        match t with
        | Some n -> Node.foldBack fn 0UL n s 
        | None -> s

    /// Short-circuiting search for an element.
    let tryPick (fn : Key -> 'V -> 'U option) (t:Tree<'V>) : 'U option =
        match t with
        | Some n -> Node.tryPick fn 0UL n
        | None -> None

    let inline tryFindKey pred t = tryPick (fun k v -> if pred k v then Some k else None) t
    let inline exists pred t = tryFindKey pred t |> Option.isSome
    let inline forall pred t = not (exists (fun k v -> not (pred k v)) t)
    
    /// Sequence of key-value pairs (ordered by key)
    let toSeq (t:Tree<'V>) : seq<(Key * 'V)> =
        match t with
        | Some n -> Node.toSeq 0UL n
        | None -> Seq.empty

    /// Sequence of key-value pairs (reverse-ordered by key)
    let toSeqR (t:Tree<'V>) : seq<(Key * 'V)> =
        match t with
        | Some n -> Node.toSeqR 0UL n
        | None -> Seq.empty

    // frequent conversions

    let inline toArray (t:Tree<'V>) : (Key * 'V) array = Array.ofSeq (toSeq t)
    let inline toList (t:Tree<'V>) : (Key * 'V) list = List.ofSeq (toSeq t)
    let ofSeq (s:seq<Key * 'V>) : Tree<'V> =
        Seq.fold (fun t (k,v) -> add k v t) empty s
    let ofList (lst:(Key * 'V) list) : Tree<'V> =
        List.fold (fun t (k,v) -> add k v t) empty lst
    let ofArray (a: (Key * 'V) array) : Tree<'V> =
        Array.fold (fun t (k,v) -> add k v t) empty a

    /// Conservative difference of two trees based on reference equality
    /// at inner tree nodes, including secure hash comparisons and memory
    /// reference equality. Values are not compared! This offers benefits
    /// for comparing persistent data structures with a few differences.
    ///
    /// Note: The precision for this comparison is relatively low. You will
    /// need additional filters on the resulting sequence.
    let diffRef (a:Tree<'V>) (b:Tree<'V>) : seq<Key * VDiff<'V>> =
        match a with
        | Some na ->
            match b with
            | Some nb -> Node.diffRef 0UL na 0UL nb
            | None    -> Node.seqInL 0UL na 
        | None ->
            match b with
            | Some nb -> Node.seqInR 0UL nb
            | None    -> Seq.empty

    /// Diff with given equality function.
    let inline diffEq eq a b =
        let trueDiff ((ix,vdif)) =
            match vdif with
            | InB (l,r) -> not (eq l r)
            | _ -> true
        diffRef a b |> Seq.filter trueDiff

    /// Diff with value equality comparisons.
    let diff a b = diffEq (=) a b

    // TODO: effective support for union-merge. Ideally something better
    // than just folding over a diff.
   
    module EncNode =
        let cLeaf = byte 'L'
        let cInner = byte 'N'

        let write cV cNP node dst =
            match node with
            | Leaf (k,v) ->
                EncByte.write cLeaf dst
                EncVarNat.write k dst
                Codec.write cV v dst
            | Inner (p,b,np) ->
                EncByte.write cInner dst
                EncVarNat.write p dst
                EncByte.write b dst
                EncCVRef.write cNP np dst

        let read cV cNP db src =
            let b0 = EncByte.read src
            if (cLeaf = b0) then
                let k = EncVarNat.read src
                let v = Codec.read cV db src
                Leaf (k,v)
            else if (cInner <> b0) then
                raise (ByteStream.ReadError)
            else 
                let p = EncVarNat.read src
                let b = EncByte.read src
                let np = EncCVRef.read cNP db src
                Inner (p,b,np)

        let compact thresh cV cNP db node =
            match node with
            | Leaf (k,v) ->
                let struct(v',szV) = Codec.compactSz cV db v
                struct(Leaf(k,v'), 1 + EncVarNat.size k + szV)
            | Inner (p,b,np) ->
                let szPrefix = 2 + EncVarNat.size p // cInner, b, p
                let threshNP = thresh - szPrefix
                let struct(np',szNP) = EncCVRef.compact threshNP cNP db np
                struct(Inner(p,b,np'), szPrefix + szNP)

        // codec for pair of nodes is primary recursion point,
        // given we only compact at split points (pair of nodes).
        let codecNP (thresh:int) (cV:Codec<'V>) = 
            { new Codec<struct(Node<'V> * Node<'V>)> with
                member cNP.Write (struct(l,r)) dst = 
                    write cV cNP l dst
                    write cV cNP r dst
                member cNP.Read db src =
                    let l = read cV cNP db src
                    let r = read cV cNP db src
                    struct(l,r)
                member cNP.Compact db (struct(l,r)) =
                    let struct(l',szL) = compact thresh cV cNP db l
                    let struct(r',szR) = compact thresh cV cNP db r
                    struct(struct(l',r'), szL + szR)
            }

        let codec' (thresh:SizeEst) (cV:Codec<'V>) = 
            let cNP = codecNP thresh cV
            { new Codec<Node<'V>> with
                member __.Write node dst = write cV cNP node dst
                member __.Read db src = read cV cNP db src
                member __.Compact db node = compact thresh cV cNP db node
            }

        // Using a large threshold for compaction of nodes.
        let defaultThreshold : SizeEst = 30000

        let inline codec (cV:Codec<'V>) = codec' defaultThreshold cV

    /// Codec for IntMap tree with specified compaction threshold.
    let inline codec' (thresh:SizeEst) (cV:Codec<'V>) = 
        EncOpt.codec (EncNode.codec' thresh cV)

    /// Codec for IntMap tree with default compaction threshold.
    let inline codec (cV:Codec<'V>) : Codec<Tree<'V>> =
        codec' (EncNode.defaultThreshold) cV 

    /// Map and filter while simultaneously compacting. Useful for large trees.
    let compactingFilterMap (cTree:Codec<Tree<'U>>) (db:Stowage) (fn:Key -> 'V -> 'U option) (t:Tree<'V>) : Tree<'U> =
        match t with
        | None -> None
        | Some node -> 
            let cc n = Option.get (Codec.compact cTree db (Some n))
            Node.filterMap cc fn 0UL node

    /// Map a function, simultaneously compacting. Useful for large trees.
    let compactingMap (cTree:Codec<Tree<'U>>) (db:Stowage) (fn:Key -> 'V -> 'U) (t:Tree<'V>) : Tree<'U> =
        match t with
        | None -> None
        | Some node -> 
            let cc n = Option.get (Codec.compact cTree db (Some n))
            Some (Node.map cc fn 0UL node)

    /// Filter while compacting. Useful for large trees.
    let inline compactingFilter cTree db pred t =
        let fn k v = if pred k v then Some v else None
        compactingFilterMap cTree db fn t

    /// Test whether a key's location is "remote" in the sense that
    /// operations like add, remove, tryFind, or containsKey would
    /// need to load a Stowage reference. Doesn't touch Stowage.
    let isKeyRemote (k:Key) (t:Tree<_>) =
        match t with
        | Some n -> Node.isKeyRemote k n
        | None -> false

type IntMap<'V> = IntMap.Tree<'V>

