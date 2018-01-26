namespace Stowage
open Data.ByteString

/// A Log Structured Merge (LSM) Tree above Stowage, based on Tries.
///
/// An LSM tree is an associative structure with built-in buffering for
/// updates. This buffering reduces average write costs, especially if 
/// most updates are applied to a relatively small working set. LSM trees
/// are history dependent, but still support a large degree of structure
/// sharing based on shared histories or common leaf volumes.
module LSMTrie =

    /// Keys in our Trie should be short, simple bytestrings. Even
    /// with prefix sharing, try to keep keys under a few kilobytes.
    type Key = ByteString

    /// Each tree node has a key fragment, a value at that key, a sparse
    /// array of children indexed 0..255, and buffered updates. If values
    /// are potentially large, try CVRef<'V> type for indirection to limit
    /// node sizes.
    ///
    /// Updates to remote tree nodes are buffered locally until compaction.
    /// A compaction operation will heuristically flush update buffers.
    type Tree<'V> =
        { prefix    : ByteString
          value     : 'V option         // value, if any.
          children  : IntMap<Tree<'V>>  // tree child array (uses stowage)
          updates   : IntMap<Trie<'V option>> // updates buffered in memory
        }
        // note: compaction for updates should be performed with large
        // thresholds, which allows us to cache size estimates and avoid
        // repeated recompaction for multiple values.

    let empty : Tree<_> =
        { prefix = BS.empty
          value = None
          children = IntMap.empty
          updates = IntMap.empty
        }

    let singleton (k:Key) (v:'V) : Tree<'V> =
        { prefix = k
          value = Some v
          children = IntMap.empty
          updates = IntMap.empty
        }

    /// Test whether LSMTrie is empty. (Assuming valid structure.)
    let inline isEmpty (t:Tree<_>) : bool =
        Option.isNone (t.value) && IntMap.isEmpty (t.children)

    let rec private validChild k (t:Tree<_>) : bool =
        if (k > 255UL) then false else
        let validUpdate ix u = IntMap.isKeyRemote ix (t.children)
                            && not (Trie.isEmpty u) 
                            && Trie.validate u
        let validUpdates = IntMap.forall validUpdate (t.updates)
        if not validUpdates then false else
        match t.children with
        | None -> Option.isSome (t.value) // child must be non-empty
        | Some(IntMap.Leaf(k,c)) ->
            // singleton child requires value at node
            Option.isSome (t.value) && validChild k c
        | cs -> IntMap.forall validChild cs

    /// Validate tree structural invariants.
    let validate (t:Tree<_>) : bool =
        (empty = t) || (validChild 0UL t)

    // compute size of shared prefix for two strings.
    let private bytesShared (a:ByteString) (b:ByteString) : int =
        let limit = min (a.Length) (b.Length)
        let rec loop ix =
            if ((ix = limit) || (a.[ix] <> b.[ix])) then ix else
            loop (ix + 1)
        loop 0

    let rec tryFind (k:Key) (t:Tree<'V>) : 'V option =
        let n = bytesShared k (t.prefix)
        if (n <> t.prefix.Length) then None else
        if (n = k.Length) then (t.value) else
        let ix = uint64 (k.[n])
        let k' = BS.drop (n+1) k
        // Search update buffer before child nodes.
        let keyUpdate =
            match IntMap.tryFind ix (t.updates) with
            | Some u -> Trie.tryFind k' u
            | None -> None
        match keyUpdate with
        | Some vUpd -> vUpd // is `None` if removed.
        | None -> // search children recursively.
            match IntMap.tryFind ix (t.children) with
            | Some c -> tryFind k' c
            | None -> None

    /// Test whether key is available in the tree.
    let inline containsKey k t = Option.isSome (tryFind k t)    

    /// Find or raise `System.Collection.Generic.KeyNotFoundException`
    let find (k:Key) (t:Tree<'V>) : 'V =
        match tryFind k t with
        | Some v -> v
        | None -> raise (System.Collections.Generic.KeyNotFoundException())

    
    let private joinBytes (a:ByteString) (b:byte) (c:ByteString) : ByteString =
        ByteStream.write (fun dst ->
            let len = a.Length + 1 + c.Length
            ByteStream.reserve len dst
            ByteStream.writeBytes a dst
            ByteStream.writeByte b dst
            ByteStream.writeBytes c dst)

    // make a node after partial filtering of value or children.
    // assumes remote update-set is valid relative to child-set.
    let private mkNode p v cs us =
        if (Option.isSome v) || not (IntMap.isEmpty us) then 
            { prefix = p; value = v; children = cs; updates = us }
        else 
            match cs with
            | None -> empty
            | Some (IntMap.Leaf(b,c)) ->
                { c with prefix = joinBytes p (byte b) (c.prefix) }
            | _ -> { prefix = p; value = None; children = cs; updates = IntMap.empty }

 
    let inline private setChildAt ix c' cs =
        if isEmpty c'
            then IntMap.remove ix cs
            else IntMap.add ix c' cs

    /// Return copy of tree minus a specified key.
    ///
    /// If the key is associated with a Stowage resource, we'll instead
    /// record the key in a local update buffer for pending deletion.
    let rec remove (k:Key) (t:Tree<'V>) : Tree<'V> =
        let n = bytesShared k (t.prefix)
        if (n <> t.prefix.Length) then t // nothing to remove
        else if (n = k.Length) then 
            // remove value at current tree node
            mkNode (t.prefix) None (t.children) (t.updates)
        else
            let ix = uint64 (k.[n])
            let k' = BS.drop (n + 1) k
            if IntMap.isKeyRemote ix (t.children) then
                // remote child, so buffer update at this node.
                let us = t.updates
                let upd = defaultArg (IntMap.tryFind ix us) (Trie.empty)
                let us' = IntMap.add ix (Trie.add k' None upd) us
                { t with updates = us' }
            else 
                // local child node, so modify t.children
                match IntMap.tryFind ix (t.children) with
                | None -> t
                | Some c ->
                    let c' = remove k' c
                    let cs' = setChildAt ix c' (t.children)
                    mkNode (t.prefix) (t.value) cs' (t.updates)

    /// Remove key from tree only if it exists. This avoids rewriting
    /// the tree or adding to the update buffer in the cases where the
    /// key is not present. However, it also costs a lookup.
    let inline checkedRemove (k:Key) (t:Tree<'V>) : Tree<'V> =
        if containsKey k t then remove k t else t

    // add to a non-empty tree
    let rec private add' (k:Key) (v:'V) (t:Tree<'V>) : Tree<'V> =
        let n = bytesShared k (t.prefix)
        if (n = k.Length) then
            if (n = t.prefix.Length) then
                // exact match, modify value at this node
                { t with value = Some v }
            else 
                // prefix contains key; inject value linearly
                let c = { t with prefix = BS.drop (n+1) (t.prefix) }
                let ix = uint64 (t.prefix.[n])
                { prefix = BS.take n (t.prefix)
                  value = Some v
                  children = IntMap.singleton ix c
                  updates = IntMap.empty
                }
        else if (n = t.prefix.Length) then
            // key is deeper; add or buffer the write
            let ix = uint64 (k.[n])
            let k' = BS.drop (n+1) k
            if IntMap.isKeyRemote ix (t.children) then
                // child is remote, so buffer the write
                let us = t.updates
                let upd = defaultArg (IntMap.tryFind ix us) (Trie.empty)
                let us' = IntMap.add ix (Trie.add k' (Some v) upd) us
                { t with updates = us' }
            else
                // child node is local, so write directly
                let c' =
                    match IntMap.tryFind ix (t.children) with
                    | None -> singleton k' v
                    | Some c -> add' k' v c
                let cs' = IntMap.add ix c' (t.children)
                { t with children = cs' }
        else
            // byte mismatch; new split of trie nodes.
            let ixK = uint64 k.[n]
            let cK = singleton (BS.drop (n+1) k) v
            let ixP = uint64 (t.prefix.[n])
            let cP = { t with prefix = BS.drop (n+1) (t.prefix) }
            assert (ixK <> ixP)
            let cs' = IntMap.empty
                    |> IntMap.add ixK cK
                    |> IntMap.add ixP cP
            { prefix = BS.take n (t.prefix)
              value = None
              children = cs' 
              updates = IntMap.empty
            }

    /// Return copy of tree with key-value pair added or updated.
    let add (k:Key) (v:'V) (t:Tree<'V>) : Tree<'V> =
        if isEmpty t then singleton k v else add' k v t
    
    let private addUpd k vOpt c =
        match vOpt with
        | None -> remove k c
        | Some v -> add k v c 

    let inline private flushUpd u t = Trie.foldBack addUpd u t
    let private addUpdates ix upd cs =
        let c = defaultArg (IntMap.tryFind ix cs) empty
        setChildAt ix (flushUpd upd c) cs
    // fully merge update set into child set
    let inline private flush us cs = IntMap.foldBack addUpdates us cs

    let rec private toSeq' (k:Key) (t:Tree<'V>) : seq<Key * 'V> =
        seq {
            match t.value with
            | Some v -> yield (k,v)
            | None -> ()
            for ((ix,c)) in IntMap.toSeq (flush (t.updates) (t.children)) do
                let kc = joinBytes k (byte ix) (c.prefix)
                yield! toSeq' kc c
        }

    /// Iteration through a Trie, with lexicographic ordering.
    let toSeq (t:Tree<'V>) : seq<Key * 'V> = toSeq' (t.prefix) t

    let rec private toSeqR' (k:Key) (t:Tree<'V>) : seq<Key * 'V> =
        seq { 
            for ((ix,c)) in IntMap.toSeqR (flush (t.updates) (t.children)) do
                let kc = joinBytes k (byte ix) (c.prefix)
                yield! toSeqR' kc c
            match t.value with
            | Some v -> yield (k,v)
            | None -> ()
        }

    /// Reverse lexicographic ordered iteration through a Trie.
    let toSeqR (t:Tree<'V>) : seq<Key * 'V> = toSeqR' (t.prefix) t

    // common conversions
    let inline toArray (t:Tree<'V>) : (Key * 'V) array = Array.ofSeq (toSeq t)
    let inline toList (t:Tree<'V>) : (Key * 'V) list = List.ofSeq (toSeq t)
    let ofSeq (s:seq<Key * 'V>) : Tree<'V> =
        Seq.fold (fun t (k,v) -> add k v t) empty s
    let ofList (lst:(Key * 'V) list) : Tree<'V> =
        List.fold (fun t (k,v) -> add k v t) empty lst
    let ofArray (a: (Key * 'V) array) : Tree<'V> =
        Array.fold (fun t (k,v) -> add k v t) empty a

    // common iterations
    //  currently using toSeq. I could try to optimize a little, later.
    let inline fold (fn : 'S -> Key -> 'V -> 'S) (s0 : 'S) (t : Tree<'V>) : 'S =
        Seq.fold (fun s (k,v) -> fn s k v) s0 (toSeq t)
    let inline foldBack (fn : Key -> 'V -> 'S -> 'S) (t : Tree<'V>) (s0 : 'S) : 'S =
        Seq.fold (fun s (k,v) -> fn k v s) s0 (toSeqR t)
    let inline iter (fn : Key -> 'V -> unit) (t : Tree<'V>) : unit =
        Seq.iter (fun (k,v) -> fn k v) (toSeq t)
    let inline tryPick (fn : Key -> 'V -> 'U option) (t : Tree<'V>) : 'U option =
        Seq.tryPick (fun (k,v) -> fn k v) (toSeq t)
    let inline tryFindKey fn t = tryPick (fun k v -> if fn k v then Some k else None) t
    let inline exists fn t = tryFindKey fn t |> Option.isSome
    let inline forall fn t = not (exists (fun k v -> not (fn k v)) t)

    // filter-map with compacting operation.
    // This will naturally flush all updates, too.
    let rec private filterMapCC (cc : Tree<'B> -> Tree<'B>) (fn : Key -> 'A -> 'B option) (k : Key) (a : Tree<'A>) : Tree<'B> =
        let bv =
            match a.value with
            | Some v -> fn k v
            | None -> None
        let fmc (ix:uint64) (c:Tree<'A>) : Tree<'B> option =
            let k' = joinBytes k (byte ix) (c.prefix)
            let c' = filterMapCC cc fn k' c
            if isEmpty c' then None else Some c'
        let bcs = IntMap.filterMap fmc (flush (a.updates) (a.children))
        mkNode (a.prefix) (bv) (bcs) (IntMap.empty)

    /// Apply a filtering map to every key-value pair.
    /// Unsuitable for huge trees. Consider compactingFilterMap. 
    let filterMap (fn:Key -> 'A -> 'B option) (t:Tree<'A>) : Tree<'B> =
        filterMapCC id fn (t.prefix) t

    /// Apply a function to every key-value pair. 
    /// Unsuitable for huge trees. Consider compactingMap. 
    let inline map (fn:Key -> 'A -> 'B) (t:Tree<'A>) : Tree<'B> =
        filterMap (fun k v -> Some (fn k v)) t

    /// Filter a tree given a predicate. 
    /// Unsuitable for huge trees. Consider compactingFilter. 
    let inline filter (pred:Key -> 'V -> bool) (t:Tree<'V>) : Tree<'V> =
        filterMap (fun k v -> if pred k v then Some v else None) t

    /// Add a prefix to all keys in a tree.
    let addPrefix (p:ByteString) (t:Tree<'V>) : Tree<'V> =
        if isEmpty t then empty else
        {t with prefix = (BS.append p (t.prefix)) }

    /// Drop key prefix and all keys that don't match, i.e. such
    /// that remaining keys are suffixes from given prefix.
    let rec remPrefix (p:ByteString) (t:Tree<'V>) : Tree<'V> =
        let n = bytesShared p (t.prefix)
        if (n = p.Length) then
            // filter prefix matched
            { t with prefix = (BS.drop n (t.prefix)) }
        else if(n = (t.prefix.Length)) then
            // node prefix matched
            let ix = uint64 (p.[n])
            let p' = BS.drop (n+1) p
            let upd = // updates with prefix removed
                match IntMap.tryFind ix (t.updates) with
                | None -> Trie.empty
                | Some upd -> Trie.remPrefix p' upd
            let c' = // children with prefix removed
                match IntMap.tryFind ix (t.children) with
                | None -> empty
                | Some c -> remPrefix p' c
            Trie.foldBack addUpd upd c'
        else empty // incomplete match

    /// Filter a tree to just keys matching given prefix.
    let inline filterPrefix (p:ByteString) (t:Tree<'V>) : Tree<'V> =
        t |> remPrefix p |> addPrefix p

    let private updTrie (us:IntMap<Trie<'V option>>) : Trie<'V option> =
        { prefix = BS.empty; value = None; children = us }

    /// Partition a tree at a specified key. Keys strictly less than
    /// the specified key will be in the left tree, while keys greater
    /// or equal will be placed in the right tree.
    let rec splitAtKey (k:Key) (t:Tree<'V>) : (Tree<'V> * Tree<'V>) =
        let n = bytesShared k (t.prefix)
        if (n = k.Length) then (empty,t)
        else if (n = t.prefix.Length) then
            // first split children
            let ix = uint64 (k.[n])
            let struct(csl,csr) = 
                let (csl0,csr0) = IntMap.splitAtKey ix (t.children)
                match IntMap.tryFind ix csr0 with
                | Some c ->
                    let (tl,tr) = splitAtKey (BS.drop (n+1) k) c
                    struct(setChildAt ix tl csl0, setChildAt ix tr csr0)
                | None -> struct(csl0,csr0)
            // redistribute pending updates
            let splitUpd kU vOpt (struct(tl,tr)) =
                if (kU < k) then struct(addUpd kU vOpt tl, tr) 
                            else struct(tl, addUpd kU vOpt tr)
            let tl0 = mkNode (t.prefix) (t.value) csl (IntMap.empty)
            let tr0 = mkNode (t.prefix)   None    csr (IntMap.empty)
            let upd : Trie<'V option> =
                { prefix = t.prefix; value = None; children = t.updates }
            let struct(tl,tr) = Trie.foldBack splitUpd upd (struct(tl0,tr0))
            (tl,tr)
        else if(k.[n] < t.prefix.[n]) then (empty,t)
        else (t,empty)

    /// Test whether a key access requires dereferencing stowage.
    /// Keys in the local update buffer are considered local.
    let rec isKeyRemote (k:Key) (t:Tree<'V>) : bool =
        let n = bytesShared k (t.prefix)
        if ((n = k.Length) || (n <> t.prefix.Length)) then false else
        let ix = uint64 (k.[n])
        let k' = BS.drop (n+1) k
        let keyInUpdates = 
            match IntMap.tryFind ix (t.updates) with
            | Some u -> Trie.containsKey k' u
            | None -> false
        if keyInUpdates then false else
        if IntMap.isKeyRemote ix (t.children) then true else
        match IntMap.tryFind ix (t.children) with
        | Some c -> isKeyRemote k' c
        | None -> false

    /// Rewrite a tree such that isKeyRemote returns false. For LSM trees,
    /// this copies a remote value (if any) into a local update buffer.
    let touch (k:Key) (t:Tree<'V>) : Tree<'V> =
        match tryFind k t with
        | Some v -> add k v t
        | None -> remove k t 

    // divide a node at a given index in the prefix. This will
    // result in an equivalent tree, albeit an invalid one with
    // one linear child. Used temporarily to simplify diffs.
    let private splitPrefixAt n t =
        assert (n < BS.length t.prefix)
        let c = { t with prefix = (BS.drop (n+1) (t.prefix)) }
        let ix = uint64 (t.prefix.[n])
        { prefix = BS.take (n) (t.prefix)
          value = None
          children = IntMap.singleton ix c
          updates = IntMap.empty
        }


    let inline private seqInL t = toSeq t |> Seq.map (fun (k,v) -> (k, InL v))
    let inline private seqInR t = toSeq t |> Seq.map (fun (k,v) -> (k, InR v))

    let inline private updatesAt ix (t:Tree<'V>) : Trie<'V option> =
        match IntMap.tryFind ix (t.updates) with
        | Some t -> t
        | None -> Trie.empty

    let inline private childrenAt ix (t:Tree<'V>) : Tree<'V> =
        match IntMap.tryFind ix (t.children) with
        | Some c -> c
        | None -> empty

    let inline private fullChildrenAt ix (t:Tree<'V>) : Tree<'V> =
        flushUpd (updatesAt ix t) (childrenAt ix t)

    let private eqref a b = System.Object.ReferenceEquals(a,b)
    let private eqOpt eq a b =
        match a with
        | Some va ->
            match b with
            | Some vb -> eq va vb
            | None -> false
        | None -> Option.isNone b
    let inline private eqUpd eqVal ua ub = 
        Trie.diffEq (eqOpt eqVal) ua ub |> Seq.isEmpty
    let inline private trueDiff eq vd =
        match vd with
        | InB (l,r) -> not (eq l r)
        | _ -> true

    // Differences in updates are filtered aggressively, if feasible.
    let rec diffEq' (eq : 'V -> 'V -> bool) (p:ByteString) (a:Tree<'V>) (b:Tree<'V>) : seq<Key * VDiff<'V>> =
        seq {
            let n = bytesShared (a.prefix) (b.prefix)
            if (n < (BS.length a.prefix)) then
                if (n < (BS.length b.prefix)) then
                    // keys in tree fully diverge at offset `n`
                    let a' = addPrefix p a
                    let b' = addPrefix p b
                    if (a.prefix.[n] < b.prefix.[n]) 
                        then yield! Seq.append (seqInL a') (seqInR b')
                        else yield! Seq.append (seqInR b') (seqInL a')
                else yield! diffEq' eq p (splitPrefixAt n a) b // realign a
            else if (n < (BS.length b.prefix)) then
                yield! diffEq' eq p a (splitPrefixAt n b) // realign b
            else // tree keys match at current node
                let k = BS.append p (a.prefix) 
                // potentially yield value at this node.
                match a.value with
                | None ->
                    match b.value with
                    | None -> ()
                    | Some vb -> yield (k, InR vb)
                | Some va ->
                    match b.value with 
                    | None -> yield (k, InL va)
                    | Some vb ->
                        // final memory reference comparison on Option type
                        // final value comparison if required
                        let skip = (eqref (a.value) (b.value)) || (eq va vb)
                        if skip then () else
                        yield (k, InB (va,vb))

                // prescan for potential differences in child nodes.
                let diffAt : bool[] = Array.create 256 false
                for ((ix,vd)) in IntMap.diffRef (a.children) (b.children) do
                    diffAt.[int ix] <- (* diffAt.[int ix] || *) trueDiff eqref vd
                for ((ix,vd)) in IntMap.diffRef (a.updates) (b.updates) do
                    // don't compare updates if children are different
                    diffAt.[int ix] <- diffAt.[int ix] || trueDiff (eqUpd eq) vd

                // yield differences
                for ix = 0UL to 255UL do
                    if not (diffAt.[int ix]) then () else
                    let p' = BS.snoc k (byte ix)
                    let ca = fullChildrenAt ix a
                    let cb = fullChildrenAt ix b
                    yield! diffEq' eq p' ca cb
                   
        } // end seq

    /// Difference of two LSM tries using both reference equality of
    /// nodes and the given equality function for values.
    ///
    /// Due to how LSM trees are structured, values may be compared
    /// more than once when processing pending update buffers.
    let diffEq eq a b = 
        if eqref a b then Seq.empty else
        if isEmpty a then seqInR b else
        if isEmpty b then seqInL a else
        diffEq' eq (BS.empty) a b

    /// Difference of LSM trees based only on reference equality.
    let diffRef a b = diffEq (fun _ _ -> true) a b
    
    /// Difference of LSM trees based on standard equality of values.
    let diff a b = diffEq (=) a b 
                        
    module Enc =
        // Encoding is concatenation of key, value, updates, and children.
        // Compaction will flush updates based on the given `buffer` argument.
        // And nodes are compacted based on `thresh`. In general, we'll want
        // buffer to be somewhat smaller than threshold.
        type TreeCodec<'V> =
            val value    : Codec<'V>                  // value encoder
            val children : Codec<IntMap<Tree<'V>>>    // for recursion
            val updates  : Codec<IntMap<Trie<'V option>>>  // for updates
            val buffer   : SizeEst                    // update buffer threshold
            interface Codec<Tree<'V>> with
                member c.Write t dst =
                    EncBytes.write (t.prefix) dst
                    EncOpt.write (c.value) (t.value) dst
                    Codec.write (c.children) (t.children) dst
                    Codec.write (c.updates) (t.updates) dst
                member c.Read db src =
                    let p = EncBytes.read src
                    let v = EncOpt.read (c.value) db src
                    let cs = Codec.read (c.children) db src
                    let us = Codec.read (c.updates) db src
                    { prefix = p; value = v; children = cs; updates = us }
                member c.Compact db t =
                    let szP = EncBytes.size (t.prefix)
                    let struct(v',szV) = EncOpt.compact (c.value) db (t.value)
                    let struct(us',szUpd) = Codec.compactSz (c.updates) db (t.updates)
                    if (szUpd > c.buffer) then
                        // flush the update buffer during compaction
                        let struct(cs',szCS) = Codec.compactSz (c.children) db (flush us' (t.children))
                        let t' = { prefix = t.prefix; value = v'; children = cs'; updates = IntMap.empty }
                        struct(t',szP + szV + szCS + 1)
                    else 
                        // reuse children then write update buffer
                        let struct(cs',szCS) = Codec.compactSz (c.children) db (t.children)
                        let t' = { prefix = t.prefix; value = v'; children = cs'; updates = us' }
                        struct(t',szP + szV + szCS + szUpd)
            new(cv,thresh,buffer) as tc =
                let cc = IntMap.codec' thresh (tc :> Codec<Tree<'V>>)
                let cu = Trie.Enc.TreeCodec(EncOpt.codec cv, System.Int32.MaxValue).children
                { value = cv; children = cc; updates = cu; buffer = buffer }

    /// Codec with specified heuristic compaction thresholds.
    /// 
    /// We have two thresholds: a buffer size for flushing updates, and a
    /// stowage threshold for remote storage.
    let inline codec' (thresh:SizeEst) (buffer:SizeEst) (cV:Codec<'V>) =
        Enc.TreeCodec<'V>(cV,thresh,buffer) :> Codec<Tree<'V>>

    /// Codec with default compaction thresholds.
    let inline codec cV = 
        let thresh = IntMap.EncNode.defaultThreshold
        codec' thresh thresh cV

    /// Map and filter while compacting each trie node. 
    ///
    /// This is useful for huge trees, where we shouldn't keep the entire
    /// tree in memory. While mapping, this will keep a node and all of
    /// its immediate children in memory, but will compact each node as it
    /// forms, moving oversized data from memory into Stowage. 
    let compactingFilterMap (cTree:Codec<Tree<'B>>) (db:Stowage) (fn:Key -> 'A -> 'B option) (t:Tree<'A>) : Tree<'B> =
        filterMapCC (Codec.compact cTree db) fn (t.prefix) t

    /// Map while compacting for large trees. See compactingFilterMap.
    let inline compactingMap (cTree:Codec<Tree<'B>>) (db:Stowage) (fn:Key -> 'A -> 'B) (t:Tree<'A>) : Tree<'B> =
        compactingFilterMap cTree db (fun k v -> Some (fn k v)) t

    /// Filter while compacting for large trees. See compactingFilterMap.
    let inline compactingFilter (cTree:Codec<Tree<'V>>) (db:Stowage) (pred:Key -> 'V -> bool) (t:Tree<'V>) : Tree<'V> =
        let fn k v = if pred k v then Some v else None
        compactingFilterMap cTree db fn t


type LSMTrie<'V> = LSMTrie.Tree<'V>


