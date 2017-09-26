namespace Stowage
open Data.ByteString

/// The IntMap is a trie-like structure for integer updates. All
/// operations are O(W) for the width of the contained integers,
/// and average to less than O(lg(N)) in most cases. This IntMap
/// uses 64-bit integers (uint64). 
///
/// A map indexed by integers is essentially a sparse array. This
/// could be used as a basis for hashmaps and other structures.
///
/// As a Stowage data structure, volumes of the tree may be remote
/// at any given moment. Explicit compaction or stowage is needed
/// to move data into the Stowage database.
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
            if(0UL <> (0x000000000000FF00UL &&& x)) then r <- (8uy + r); x <- (x >>> 8)
            if(0UL <> (0x00000000000000F0UL &&& x)) then r <- (4uy + r); x <- (x >>> 4)
            if(0UL <> (0x000000000000000CUL &&& x)) then r <- (2uy + r); x <- (x >>> 2)
            if(0UL <> (0x0000000000000002UL &&& x)) then (1uy + r) else r

    // internal node structure
    type Node<'V> =
        | Leaf of Key * 'V
        | Inner of Size * Key * Critbit * NodePair<'V>
    and NodePair<'V> = // either local or remote pair
        | Local of Node<'V> * Node<'V> * SizeEst
        | Remote of LVRef<Node<'V> * Node<'V>>

    type Tree<'V> = Node<'V> option

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


        // add given key-prefix to key in node (via bitwise OR) 
        let mergeKP kp node =
            match node with
            | Leaf (k,v) -> Leaf ((kp ||| k), v)
            | Inner (ct,p,b,np) -> Inner (ct,(kp|||p),b,np)

        let inline mergeL p b l = 
            let pb = p <<< int b
            let kp = pb <<< 1
            mergeKP kp l
        let inline mergeR p b r = 
            let pb = p <<< int b
            let kp = (pb <<< 1) ||| 1UL
            mergeKP kp r

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
        let inline suffixMask (b : Critbit) : Key =
            (System.UInt64.MaxValue >>> 1) >>> int (63uy - b)
        let inline suffix (k:Key) (b:Critbit) : Key = (k &&& suffixMask b)
        let inline testCritbit (k:Key) (b:Critbit) : bool =
            (0UL <> (1UL &&& (k >>> int b)))

        let rec tryFind kf node =
            match node with
            | Inner (_, p, b, np) ->
                if (p <> prefix kf b) then None else
                let inR = testCritbit kf b
                let kf' = suffix kf b
                let (l,r) = load np
                if inR then tryFind kf' r else tryFind kf' l
            | Leaf (k,v) -> if (kf = k) then Some v else None

        let inline containsKey kf node = Option.isSome (tryFind kf node)

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
                if (p = kp) then
                    let (l,r) = load' np
                    let k' = suffix k b
                    let (l',r') = 
                        if testCritbit k b
                            then (l, add k' v r)
                            else (add k' v l, r)
                    let ct' = size l' + size r'
                    Inner (ct', p, b, local l' r')
                else // insert just above current node
                    let b' = 1uy + b + Critbit.highBitIndex (p ^^^ kp)
                    let node' = Inner(ct, suffix p b', b, np)
                    let leaf = Leaf(suffix k b', v)
                    let inR = testCritbit k b'
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
    /// to test for presence of the key without Stowage loads.
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
    /// touch the tree along the removed key. Use checkedRemove if
    /// you want to preserve Stowage references when the key isn't
    /// present.
    let remove (k:Key) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Some n -> Node.remove k n
        | None -> None

    /// Remove that doesn't touch tree when key is not present.
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
        let lb k t = 
            match k with
            | Some ix when (ix <> 0UL) -> snd (splitAtKey ix t)
            | _ -> t
        let ub k t =
            match k with
            | Some ix when (ix <> System.UInt64.MaxValue) -> fst (splitAtKey (1UL + ix) t)
            | _ -> t
        lb kMin (ub kMax t0)


    // TODO:
    //  enumeration, iteration, fold, map, tryPick, forall, exists, etc.
    //  efficient structural diffs
    //  conversion to/from lists, arrays, sequences
    //  compact, compacting map, Ref type, stow, load, 
   
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

        let compactNP thresh cV cNP db np =
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
                let struct(np',szNP) = compactNP thresh cV cNP db np
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


type IntMap<'V> = IntMap.Tree<'V>

