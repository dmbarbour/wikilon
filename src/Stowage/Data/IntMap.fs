namespace Stowage
open Data.ByteString

/// The IntMap is a map specialized for integral keys. This is useful
/// for modeling persistent arrays, sparse arrays, hash maps. A radix
/// tree is used under the hood, hence tree height is never greater 
/// than integer width (64 bits) and may be less.
///
/// As a Stowage data structure, the main feature here is integration
/// with Stowage databases. The IntMap supports stowage at any inner
/// node of the tree, guided by serialization size thresholds. Remote
/// nodes use LVRef to cache the load and parse for multiple lookups.
/// For a sufficiently large IntMap, it's possible that only several
/// volumes (subtrees) are fully loaded into memory. And updates to a
/// durable IntMap can be efficient, sharing many on-disk references. 
///
/// Use of stowage is not automatic. Explicit compaction is required.
/// Hence, updates are 'buffered' in memory between compactions. If
/// compaction is not used, this IntMap is a memory-only structure.
module IntMap =

    type Key = uint64   
    type Critbit = byte
    type Size = uint64

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
        | Inner of Size * Key * Critbit * NodePair<'V>
    and NodePair<'V> = // either local or remote pair
        | Local of Node<'V> * Node<'V> * SizeEst
        | Remote of LVRef<Node<'V> * Node<'V>>


    module Node =

        let local l r = Local (l,r,System.Int32.MaxValue)

        let inline size node = 
            match node with
            | Leaf _ -> 1UL
            | Inner (ct,_,_,_) -> ct

        let inline load np = 
            match np with
            | Local (l,r,_) -> (l,r)
            | Remote ref -> LVRef.load ref

        let inline load' np = 
            match np with
            | Local (l,r,_) -> (l,r)
            | Remote ref -> LVRef.load' ref


        // merge a key-prefix with key in node (via bitwise OR) 
        let mergeKP kp node =
            match node with
            | Leaf (k,v) -> Leaf ((kp ||| k), v)
            | Inner (ct,p,b,np) -> Inner (ct,(kp ||| p),b,np)


        let inline keyPrefixL p b = ((p <<< 1) <<< int b)  
        let inline keyPrefixR p b = (keyPrefixL p b) ||| (1UL <<< int b)
        let inline mergeL p b l = mergeKP (keyPrefixL p b) l
        let inline mergeR p b r = mergeKP (keyPrefixR p b) r 

        // split a node, assuming amt is both greater than one
        // and less than the node's size. This divides the node
        // into two non-empty nodes.
        let rec splitSize amt node =
            match node with
            | Inner (ct, p, b, np) when (ct > amt) ->
                let (l,r) = load' np
                let ctL = size l
                if (amt > ctL) then
                    let struct(rl,rr) = splitSize (amt - ctL) r
                    struct(Inner(ct,p,b,local l rl), mergeR p b rr)
                else if(amt = ctL) then // exact split
                    struct(mergeL p b l, mergeR p b r)
                else
                    let struct(ll,lr) = splitSize amt l
                    struct(mergeL p b ll, Inner(ct,p,b,local lr r))
            | _ -> failwith "invalid split!"


        // prefix and suffix exclude the critbit
        let inline prefix (k:Key) (b:Critbit) : Key = ((k >>> int b) >>> 1)
        let inline testCritbit (k:Key) (b:Critbit) : bool =
            (0UL <> (1UL &&& (k >>> int b)))
        let inline suffixMask (b : Critbit) : Key = ((1UL <<< int b) - 1UL)
        let inline suffix (k:Key) (b:Critbit) : Key = (k &&& suffixMask b)

        let rec tryFind kf node =
            match node with
            | Inner (_, p, b, np) ->
                if (p <> prefix kf b) then None else
                let inR = testCritbit kf b
                let kf' = suffix kf b
                let (l,r) = load np
                if inR then tryFind kf' r else tryFind kf' l
            | Leaf (k,v) -> if (kf = k) then Some v else None

        let inline containsKey kf node = 
            Option.isSome (tryFind kf node)

        // test whether a key is remote
        let rec isKeyRemote kf node =
            match node with
            | Inner (_, p, b, np) ->
                if (p <> prefix kf b) then false else
                match np with
                | Local (l,r,_) -> 
                    let inR = testCritbit kf b
                    let kf' = suffix kf b
                    if inR then isKeyRemote kf' r 
                           else isKeyRemote kf' l
                | Remote _ -> true
            | Leaf _ -> false

        // pull key into local memory
        let rec touch kf node =
            match node with
            | Inner (ct, p, b, np) ->
                if (p <> prefix kf b) then node else
                let inR = testCritbit kf b
                let kf' = suffix kf b
                let (l,r) = load' np
                if inR 
                    then Inner (ct, p, b, local l (touch kf' r))
                    else Inner (ct, p, b, local (touch kf' l) r)
            | Leaf _ -> node

        // localize all nodes
        let rec expand node =
            match node with
            | Inner (ct, p, b, np) ->
                let (l,r) = load' np
                Inner(ct, p, b, local (expand l) (expand r))
            | Leaf _ -> node

        // add or update a key-value pair
        let rec add k v node =
            match node with
            | Inner (ct, p, b, np) ->
                let kp = prefix k b
                if (p = kp) then // add with shared prefix
                    let inR = testCritbit k b
                    let k' = suffix k b
                    let (l,r) = load' np
                    let (l',r') = if inR then (l, add k' v r) else (add k' v l, r)
                    let ct' = size l' + size r'
                    Inner (ct', p, b, local l' r')
                else // insert just above current node
                    let b' = 1uy + b + Critbit.highBitIndex (p ^^^ kp)
                    assert(63uy >= b')
                    let inR = testCritbit k b'
                    let node' = Inner(ct, suffix p b', b, np)
                    let leaf = Leaf(suffix k b', v)
                    let np' = if inR then local node' leaf else local leaf node'
                    Inner (1UL + ct, prefix k b', b', np')
            | Leaf (kl,vl) ->
                if (kl = k) then Leaf(k,v) else
                let b' = Critbit.highBitIndex (kl ^^^ k)
                let node' = Leaf (suffix kl b', vl)
                let leaf  = Leaf (suffix k b', v)
                let inR = testCritbit k b'
                let np' = if inR then local node' leaf else local leaf node'
                Inner(2UL, prefix k b', b', np')

        let rec remove (k:Key) (node:Node<'V>) : Node<'V> option =
            match node with
            | Inner (ct, p, b, np) ->
                if (p <> prefix k b) then Some node else
                let inR = testCritbit k b
                let k' = suffix k b
                let (l,r) = load np
                if inR then 
                    match remove k' r with
                    | Some r' -> Some (Inner (ct, p, b, local l r'))
                    | None -> Some (mergeL p b l)
                else
                    match remove k' l with
                    | Some l' -> Some (Inner (ct, p, b, local l' r))
                    | None -> Some (mergeR p b r)
            | Leaf (kl,_) -> if (kl = k) then None else Some node


        let rec splitAtKey k node =
            match node with
            | Inner (ct, p, b, np) ->
                let kp = prefix k b
                if (kp < p) then (Some node, None) else
                if (kp > p) then (None, Some node) else
                let k' = suffix k b
                let inR = testCritbit k b
                let (l,r) = load' np
                if inR then
                    let (rl,rr) = splitAtKey k' r
                    let l' = 
                        match rl with
                        | Some n -> Inner(ct,p,b,local l n)
                        | None -> mergeL p b l
                    (Some l', rr)
                else
                    let (ll,lr) = splitAtKey k' l
                    let r' = 
                        match lr with
                        | Some n -> Inner(ct,p,b,local n r)
                        | None -> mergeR p b r
                    (ll, Some r')
            | Leaf (kl,_) ->
                if (kl >= k) 
                    then (None, Some node) 
                    else (Some node, None)

        let inline keyPrefixes kp p b =
            let kl = keyPrefixL (kp ||| p) b
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
            | Inner (ct, p, b, np) ->
                let struct(kl,kr) = keyPrefixes kp p b
                let (l,r) = load' np
                let l' = map c fn kl l
                let r' = map c fn kr r
                c (Inner (ct, p, b, local l' r'))

        // filter and map with potential compaction.
        let rec filterMap c fn kp node =
            match node with
            | Leaf (k,v) ->
                match fn (kp ||| k) v with
                | Some v' -> Some (c (Leaf (k, v')))
                | None -> None
            | Inner (ct, p, b, np) ->
                let struct(kl,kr) = keyPrefixes kp p b
                let (l,r) = load' np
                let lFM = filterMap c fn kl l
                let rFM = filterMap c fn kr r
                match (lFM,rFM) with
                | (Some l', Some r') ->
                    let ct' = size l' + size r'
                    let node' = c (Inner (ct', p, b, local l' r'))
                    Some node'
                | (Some l', None) -> Some (mergeL p b l')
                | (None, Some r') -> Some (mergeR p b r')
                | (None, None) -> None

        // TODO: functions to merge trees would be appreciated, e.g.
        // for an efficient union or subtraction. 

        let rec iter fn kp node =
            match node with
            | Leaf (k,v) -> fn (kp ||| k) v
            | Inner (_, p, b, np) ->
                let struct(kl,kr) = keyPrefixes kp p b
                let (l,r) = load' np
                iter fn kl l
                iter fn kr r

        let rec fold fn s kp node =
            match node with
            | Leaf (k,v) -> fn s (kp ||| k) v
            | Inner (_, p, b, np) ->
                let struct(kl,kr) = keyPrefixes kp p b
                let (l,r) = load' np
                fold fn (fold fn s kl l) kr r

        let rec foldBack fn kp node s =
            match node with
            | Leaf (k,v) -> fn (kp ||| k) v s
            | Inner (_, p, b, np) ->
                let struct(kl,kr) = keyPrefixes kp p b
                let (l,r) = load' np
                foldBack fn kl l (foldBack fn kr r s)

        let rec tryPick fn kp node =
            match node with
            | Leaf (k,v) -> fn (kp ||| k) v
            | Inner (_, p, b, np) ->
                let struct(kl,kr) = keyPrefixes kp p b
                let (l,r) = load' np
                let pickL = tryPick fn kl l
                if Option.isSome pickL then pickL else
                tryPick fn kr r

        let rec toSeq kp node =
            seq {
                match node with
                | Leaf (k,v) -> yield ((kp ||| k), v)
                | Inner (_, p, b, np) ->
                    let struct(kl,kr) = keyPrefixes kp p b
                    let (l,r) = load' np
                    yield! toSeq kl l
                    yield! toSeq kr r
            }

        let rec toSeqR kp node = 
            seq {
                match node with
                | Leaf (k,v) -> yield ((kp ||| k), v)
                | Inner (_, p, b, np) ->
                    let struct(kl,kr) = keyPrefixes kp p b
                    let (l,r) = load' np
                    yield! toSeqR kr r
                    yield! toSeqR kl l
            }

        // monotonic key suffixes
        let private validKeySuffix b node =
            let limit = (1UL <<< int b) 
            match node with
            | Leaf (k,_) -> (limit > k)
            | Inner (_,pn,bn,_) -> ((prefix limit bn) > pn)

        let rec validate node =
            match node with
            | Leaf _ -> true
            | Inner (ct, p, b, np) ->
                let (l,r) = load' np
                (63uy >= (1uy + b + Critbit.highBitIndex p))
                    && (ct = (size l + size r))
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

    /// Return size of tree (number of elements). O(1).
    let size (t:Tree<_>) : Size =
        match t with
        | Some n -> Node.size n
        | None -> 0UL

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

    /// Return tree with all keys in memory. Touch everything.
    let expand (t:Tree<'V>) : Tree<'V> =
        match t with
        | Some n -> Some (Node.expand n)
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
        
    /// Partition a tree based on element count. This will take the
    /// requested count of keys (from least key to the Nth least key)
    /// into the first tree, then all remaining keys into the second. 
    /// If the tree is smaller than the request, everything will be
    /// in the first tree.
    let splitSize (amt:Size) (t:Tree<'V>) : (Tree<'V> * Tree<'V>) =
        if (0UL = amt) then (None,t) else
        match t with
        | Some n when (amt < Node.size n) -> 
            let struct(l,r) = Node.splitSize amt n
            (Some l, Some r)
        | _ -> (t,None)

    /// Partition a tree at a specified key. Keys strictly less than
    /// the specified key will be in the left tree, while keys greater
    /// or equal will be placed in the right tree.
    let splitAtKey (k:Key) (t:Tree<'V>) : (Tree<'V> * Tree<'V>) =
        match t with
        | Some n -> Node.splitAtKey k n
        | None -> (None,None)

    /// Select a range of keys.
    let slice (kMin:Key option) (kMax:Key option) (t0:Tree<'V>) : Tree<'V> =
        let inline sliceLB kOpt t = 
            match kOpt with
            | Some k when (0UL <> k) -> snd (splitAtKey k t)
            | _ -> t
        let inline sliceUB kOpt t = 
            match kOpt with
            | Some k when (System.UInt64.MaxValue <> k) -> 
                fst (splitAtKey (k + 1UL) t)
            | _ -> t
        t0 |> sliceLB kMin |> sliceUB kMax 

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


    module EnumDiff =
        type Stack<'V> = Node<'V> list // key fully merged in node
        type Elem<'V> = (Key * VDiff<'V>)
        type State<'V> = (Elem<'V> option * (Stack<'V> * Stack<'V>))

        let iniStack t = Option.toList t
        let iniState (a:Tree<'V>) (b:Tree<'V>) : State<'V> = 
            (None, (iniStack a, iniStack b))

        // load + keys merged
        let split p b np = 
            let (l,r) = Node.load' np
            let l' = Node.mergeL p b l
            let r' = Node.mergeR p b r
            struct(l',r')

        // step to next leaf value (if any)
        let rec stepV n s =
            match n with
            | Leaf (k,v) -> struct(k,v,s)
            | Inner (_,p,b,np) ->
                let struct(l,r) = split p b np
                stepV l (r::s)

        let refEQ npa npb = // shallow reference equivalence
            if (System.Object.ReferenceEquals(npa,npb)) then true else
            match (npa,npb) with
            | (Remote ra, Remote rb) -> (ra = rb) // stowage equivalence
            | _ -> false

        let rec stepDiff sa sb =
            match sa with
            | [] -> 
                match sb with
                | (nb::moreb) ->
                    let struct(kb,vb,sb') = stepV nb moreb
                    (Some(kb, InR vb), ([], sb'))
                | [] -> (None, ([],[]))
            | (Leaf(ka,va)::sa') -> 
                match sb with
                | (nb::moreb) ->
                    let struct(kb,vb,sb') = stepV nb moreb
                    stepDiffK ka va sa' kb vb sb'
                | [] -> (Some(ka, InL va), (sa', []))
            | (Inner(cta,pa,ba,npa)::sa') ->
                match sb with
                | (Inner(ctb,pb,bb,npb)::sb') when (bb >= ba) ->
                    let obviousEq = (ba = bb) && (pa = pb)
                                 && (cta = ctb) && (refEQ npa npb)
                    if obviousEq then stepDiff sa' sb' else
                    let struct(lb,rb) = split pb bb npb
                    stepDiff sa (lb::rb::sb')
                | _ ->
                    let struct(la,ra) = split pa ba npa
                    stepDiff (la::ra::sa') sb
        and stepDiffK ka va sa kb vb sb =
            if (ka < kb) then
                (Some (ka, InL va), (sa, Leaf(kb,vb)::sb))
            else if (kb < ka) then
                (Some (kb, InR vb), (Leaf(ka,va)::sa, sb))
            else if (va <> vb) then
                (Some (ka, InB(va,vb)), (sa, sb))
            else stepDiff sa sb

        type Enumerator<'V when 'V : equality>(s:State<'V>) =
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
                    e.ST <- stepDiff l r
                    Option.isSome (fst e.ST)
                member e.Reset() = raise (System.NotSupportedException())
            interface System.IDisposable with
                member e.Dispose() = ()
            
        type Enumerable<'V when 'V : equality> =
            val private L : Tree<'V>
            val private R : Tree<'V>
            new(l,r) = { L = l; R = r }
            member e.GetEnum() = new Enumerator<'V>(iniState (e.L) (e.R))
            interface System.Collections.Generic.IEnumerable<Elem<'V>> with
                member e.GetEnumerator() = upcast e.GetEnum()
            interface System.Collections.IEnumerable with
                member e.GetEnumerator() = upcast e.GetEnum()

    /// Structural Difference of IntMap Trees
    ///
    /// This function leverages reference equality for both Stowage secure
    /// hashes and .Net object references to filter subtrees at splits if
    /// feasible. This assumes equal binaries correspond to equal values.
    let diff<'V when 'V : equality> (a:Tree<'V>) (b:Tree<'V>) : seq<Key * VDiff<'V>> =
        upcast (EnumDiff.Enumerable(a,b))

    /// Deep structural equality for Trees. (Tests for diff.)
    let deepEq<'V when 'V : equality> (a:Tree<'V>) (b:Tree<'V>) : bool = 
        Seq.isEmpty (diff a b)

    // TODO: effective support for union-merge. Ideally something better
    // than just folding over a diff.
   
    module EncNode =
        let cLeaf = byte 'L'
        let cInner = byte 'N'
        let cLocal = byte 'P'
        let cRemote = byte 'R'

        let szRemote = 1 + EncLVRef.size

        // The encoder is a bit awkward to write since I want to
        // avoid constructing codec objects after initial setup.
        //
        // Also, element count is serialized only at Remote nodes,
        // and recomputed upon read. 

        let writeNodePair cNP ct np dst =
            match np with
            | Local (l,r,_) ->
                EncByte.write cLocal dst
                Codec.write cNP (l,r) dst
            | Remote ref ->
                EncByte.write cRemote dst
                EncVarNat.write ct dst
                EncLVRef.write ref dst

        let write cV cNP node dst =
            match node with
            | Leaf (k,v) ->
                EncByte.write cLeaf dst
                EncVarNat.write k dst
                Codec.write cV v dst
            | Inner (ct,p,b,np) ->
                EncByte.write cInner dst
                EncVarNat.write p dst
                EncByte.write b dst
                writeNodePair cNP ct np dst

        let readNodePair cNP db src =
            let b0 = EncByte.read src
            if (cLocal = b0) then
                let s0 = ByteStream.bytesRem src
                let (l,r) = Codec.read cNP db src
                let sf = ByteStream.bytesRem src 
                let ct = Node.size l + Node.size r
                struct(Local (l,r,(s0 - sf)), ct)
            else if (cRemote <> b0) then
                raise (ByteStream.ReadError)
            else
                let ct = EncVarNat.read src
                let ref = EncLVRef.read cNP db src
                struct(Remote ref, ct)

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
                let struct(np,ct) = readNodePair cNP db src
                Inner (ct,p,b,np)

        let compactNodePair thresh cV cNP db np =
            match np with
            | Local(l,r,szEst) ->
                if (szEst < thresh) then struct(np, 1 + szEst) else
                let struct((l',r'),szLR) = Codec.compactSz cNP db (l,r)
                if (szLR < thresh) then struct(Local(l',r',szLR), 1 + szLR) else
                let ref = LVRef.stow cNP db (l',r')
                struct(Remote ref, szRemote)
            | Remote _ -> struct(np, szRemote)

        let compact thresh cV cNP db node =
            match node with
            | Leaf (k,v) ->
                let struct(v',szV) = Codec.compactSz cV db v
                struct(Leaf(k,v'), 1 + EncVarNat.size k + szV)
            | Inner (ct,p,b,np) ->
                let struct(np',szNP) = compactNodePair thresh cV cNP db np
                let szCPB = EncVarNat.size ct + EncVarNat.size p + 1
                struct(Inner(ct,p,b,np'), 1 + szCPB + szNP)

        // codec for pair of nodes is primary recursion point,
        // given we only compact at split points (pair of nodes).
        let codecNP (thresh:int) (cV:Codec<'V>) = 
            { new Codec<Node<'V> * Node<'V>> with
                member cNP.Write ((l,r)) dst = 
                    write cV cNP l dst
                    write cV cNP r dst
                member cNP.Read db src =
                    let l = read cV cNP db src
                    let r = read cV cNP db src
                    (l,r)
                member cNP.Compact db ((l,r)) =
                    let struct(l',szL) = compact thresh cV cNP db l
                    let struct(r',szR) = compact thresh cV cNP db r
                    struct((l',r'), szL + szR)
            }

        let codec (thresh:int) (cV:Codec<'V>) = 
            let cNP = codecNP thresh cV
            { new Codec<Node<'V>> with
                member __.Write node dst = write cV cNP node dst
                member __.Read db src = read cV cNP db src
                member __.Compact db node = compact thresh cV cNP db node
            }

        // Using a large threshold for compaction of nodes.
        let defaultThreshold = 14000

    /// Codec for node with default threshold.
    let nodeCodec cV = EncNode.codec (EncNode.defaultThreshold) cV

    /// Codec for the full IntMap tree, given a value codec.
    ///
    /// Note: Trees are large, having high compaction thresholds. if
    /// you want small tree roots (e.g. if IntMap is value in another
    /// collection), use the Ref type instead.
    let treeCodec (cV:Codec<'V>) : Codec<Tree<'V>> =
        EncOpt.codec (nodeCodec cV)

    /// Compact a tree in memory.
    ///
    /// This serializes large subtrees into the Stowage database, thus
    /// removing them from runtime memory. The data will be loaded as
    /// needed for further lookups and manipulations of the tree. There
    /// is some latency added for GC to intercede if it turns out this
    /// value was intermediate.
    ///
    /// Note: If you plan to compact frequently, you should ensure your
    /// value type is either cheap to compact or caches compactions. An
    /// easy way to ensure this is to wrap CVRef around the value type.
    let compact (cV:Codec<'V>) (db:Stowage) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Some n -> Some (Codec.compact (nodeCodec cV) db n)
        | None -> None

    let private compactor (cV:Codec<'V>) (db:Stowage) : Node<'V> -> Node<'V> =
        let cN = nodeCodec cV
        let inline force node =
            match node with
            | Inner(_,_,_,Remote ref) -> LVRef.force ref
            | _ -> ()
        let compactAndForce node =
            match node with
            | Inner _ -> 
                let node' = Codec.compact cN db node
                force node'
                node'
            | Leaf _ -> node
        compactAndForce

    /// Map a function, simultaneously compacting.
    ///
    /// This is intended for use with larger-than-memory maps, and 
    /// will systematically compact inner nodes.
    let compactingMap (cU:Codec<'U>) (db:Stowage) (fn :Key -> 'V -> 'U) (t:Tree<'V>) : Tree<'U> =
        match t with
        | Some node -> Some (Node.map (compactor cU db) fn 0UL node)
        | None -> None

    /// Map and filter while simultaneously compacting. Useful for large trees.
    let compactingFilterMap cU db fn t =
        match t with
        | Some node -> Node.filterMap (compactor cU db) fn 0UL node
        | None -> None

    /// Filter while compacting. Useful for large trees.
    let inline compactingFilter cU db pred t =
        compactingFilterMap cU db (fun k v -> if pred k v then Some v else None) t

    /// Test whether a key's location is "remote" in the sense that
    /// operations like add, remove, tryFind, or containsKey would
    /// need to load a Stowage reference. Doesn't touch Stowage.
    let isKeyRemote (k:Key) (t:Tree<_>) =
        match t with
        | Some n -> Node.isKeyRemote k n
        | None -> false

    /// A bounded-size reference for large maps.
    ///
    /// This is convenient when you have large collections of trees, or
    /// will reference a map from many places and wish to avoid redundant
    /// serialization. This is simply an alias for CVRef together with a 
    /// few utility functions.
    type Ref<'V> = CVRef<Tree<'V>>

    /// A few hundred bytes is small enough for collections while large
    /// enough to keep smaller collections inline or in memory, and to
    /// help pay for the stowage reference overheads.
    let refThresh : int = 600

    /// Codec for tree references (CVRef with refThresh)
    let refCodec (cV:Codec<'V>) = CVRef.codec refThresh (treeCodec cV)

    /// Wrap any tree as if it were a stowed ref. This is convenient
    /// when the IntMap is a value in a larger collection and may be
    /// later compacted via refCodec. 
    let inline stow' (t:Tree<'V>) : Ref<'V> = CVRef.local t

    /// Rewrite a tree into a bounded-size reference.
    ///
    /// There is some latency between the request and serialization
    /// to the database, and GC may intercede.
    let stow (cV:Codec<'V>) (db:Stowage) (t:Tree<'V>) : Ref<'V> =
        Codec.compact (refCodec cV) db (stow' t)

    /// Non-caching load. Will use cache opportunistically, but 
    /// does not keep values in memory.
    let inline load' (ref:Ref<'V>) : Tree<'V> = CVRef.load' ref

    /// Caching load. This will tend to keep the value in memory
    /// for a little while so further loads are more efficient.
    let inline load  (ref:Ref<'V>) : Tree<'V> = CVRef.load ref


type IntMap<'V> = IntMap.Tree<'V>
type IntMapRef<'V> = IntMap.Ref<'V>

