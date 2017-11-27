namespace Stowage
open Data.ByteString

/// Trie (Radix tree) with bytestring keys, above Stowage. 
///
/// This is an implementation of a prefix-sharing radix tree with 
/// ability to shift large subtrees into a Stowage database via a
/// compaction operation. Effectively, this allows representation
/// of large key-value databases as first-class values, buffering
/// updates in memory until compaction.
module Trie =

    /// Keys in our Trie should be short, simple bytestrings. Even
    /// with prefix sharing, try to keep keys under a few kilobytes.
    type Key = ByteString

    /// Each Trie has a prefix key fragment, a possible value at that
    /// key, and a sparse array of children indexed 0..255. 
    ///
    /// Most of the complex logic is via the IntMap for a sparse array
    /// of children. This enables fine-grained decisions for compaction
    /// at arbitrary bits in the keys, while enabling multiple smaller
    /// nodes to share blocks of storage. Values are represented inline.
    /// If values can be very large use explicit indirection via CVRef.
    type Tree<'V> = 
        { prefix    : ByteString 
          value     : 'V option
          children  : IntMap<Tree<'V>>  // our sparse array
        }  

    /// Empty Tree Value
    let empty : Tree<_> = 
        { prefix = BS.empty
          value = None 
          children = IntMap.empty 
        }

    /// Singleton tree value.
    let singleton (k:ByteString) (v:'V) : Tree<'V> =
        { prefix = k
          value = Some v
          children = IntMap.empty
        }    

    /// Test whether the Trie is empty. Assumes valid structure.
    let isEmpty (t:Tree<_>) : bool =
        Option.isNone (t.value) && IntMap.isEmpty (t.children)

    let rec private validChild k (t:Tree<_>) : bool =
        if ((k > 255UL) || (isEmpty t)) then false else
        match t.children with
        | Some(IntMap.Leaf(k,c)) ->
            // singleton child requires value at node
            Option.isSome (t.value) && validChild k c
        | cs -> IntMap.forall validChild cs

    /// Validate tree structure invariants.
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
        match IntMap.tryFind ix (t.children) with
        | Some t' -> tryFind (BS.drop (n+1) k) t'
        | None -> None 

    /// Check for whether a tree contains a specific key.
    let inline containsKey k t = 
        Option.isSome (tryFind k t)

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
    // Will recombine unnecessary nodes. Assumes valid children.
    let private mkNode p v cs =
        if Option.isSome v then 
            { prefix = p; value = v; children = cs }
        else 
            match cs with
            | None -> empty
            | Some (IntMap.Leaf(b,c)) ->
                { c with prefix = joinBytes p (byte b) (c.prefix) }
            | _ -> { prefix = p; value = None; children = cs }


    let inline private setChildAt ix c' cs =
        if isEmpty c'
            then IntMap.remove ix cs
            else IntMap.add ix c' cs

    /// Return copy of tree minus a specified key. 
    let rec remove (k:Key) (t:Tree<'V>) : Tree<'V> =
        let n = bytesShared k (t.prefix)
        if (n <> t.prefix.Length) then t 
        else if (n = k.Length) then 
            mkNode (t.prefix) None (t.children) 
        else
            let ix = uint64 (k.[n])
            match IntMap.tryFind ix (t.children) with
            | None -> t
            | Some c ->
                let c' = remove (BS.drop (n+1) k) c
                let cs' = setChildAt ix c' (t.children)
                mkNode (t.prefix) (t.value) cs'

    /// Remove key from tree only if it exists. This avoids rewriting
    /// the tree in the cases where the key is unlikely to be present.            
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
                }
        else if (n = t.prefix.Length) then
            // key is deeper; add value to child tree
            let ix = uint64 (k.[n])
            let k' = BS.drop (n+1) k
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
            }

    /// Return copy of key with key-value pair added or updated.
    let add (k:Key) (v:'V) (t:Tree<'V>) : Tree<'V> =
        if isEmpty t then singleton k v else add' k v t

    let rec private toSeq' (k:Key) (t:Tree<'V>) : seq<Key * 'V> =
        seq {
            match t.value with
            | Some v -> yield (k,v)
            | None -> ()
            for ((ix,c)) in IntMap.toSeq (t.children) do
                let kc = joinBytes k (byte ix) (c.prefix)
                yield! toSeq' kc c
        }

    /// Iteration through a Trie, with lexicographic ordering.
    let toSeq (t:Tree<'V>) : seq<Key * 'V> = toSeq' (t.prefix) t

    let rec private toSeqR' (k:Key) (t:Tree<'V>) : seq<Key * 'V> =
        seq { 
            for ((ix,c)) in IntMap.toSeqR (t.children) do
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
    let rec private filterMapCC (cc : Tree<'B> -> Tree<'B>) (fn : Key -> 'A -> 'B option) (k : Key) (a : Tree<'A>) : Tree<'B> =
        let bv =
            match a.value with
            | Some v -> fn k v
            | None -> None
        let fmc (ix:uint64) (c:Tree<'A>) : Tree<'B> option =
            let k' = joinBytes k (byte ix) (c.prefix)
            let c' = filterMapCC cc fn k' c
            if isEmpty c' then None else Some c'
        let bcs = IntMap.filterMap fmc (a.children)
        mkNode (a.prefix) (bv) (bcs)

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

    /// Drop key prefix and all keys that don't match.
    let rec remPrefix (p:ByteString) (t:Tree<'V>) : Tree<'V> =
        let n = bytesShared p (t.prefix)
        if (n = p.Length) then
            // filter prefix matched
            { t with prefix = (BS.drop n (t.prefix)) }
        else if(n = (t.prefix.Length)) then
            // node prefix matched
            let ix = uint64 (p.[n])
            match IntMap.tryFind ix (t.children) with
            | Some c -> remPrefix (BS.drop (n+1) p) c
            | None -> empty
        else empty // incomplete match

    /// Filter a tree to just keys matching given prefix.
    let inline filterPrefix (p:ByteString) (t:Tree<'V>) : Tree<'V> =
        t |> remPrefix p |> addPrefix p

    /// Partition a tree at a specified key. Keys strictly less than
    /// the specified key will be in the left tree, while keys greater
    /// or equal will be placed in the right tree. 
    let rec splitAtKey (k:Key) (t:Tree<'V>) : (Tree<'V> * Tree<'V>) =
        let n = bytesShared k (t.prefix)
        if (n = k.Length) then (empty,t)
        else if (n = t.prefix.Length) then 
            let ix = uint64 (k.[n])
            let struct(csl,csr) = 
                let (csl0,csr0) = IntMap.splitAtKey ix (t.children)
                match IntMap.tryFind ix csr0 with
                | Some c ->
                    let (tl,tr) = splitAtKey (BS.drop (n+1) k) c
                    struct(setChildAt ix tl csl0, setChildAt ix tr csr0)
                | None -> struct(csl0,csr0)
            (mkNode (t.prefix) (t.value) csl, mkNode (t.prefix) None csr)
        else if(k.[n] < t.prefix.[n]) then (empty,t)
        else (t,empty)

    /// Test whether a key access requires dereferencing stowage.
    let rec isKeyRemote (k:Key) (t:Tree<'V>) : bool =
        let n = bytesShared k (t.prefix)
        if ((n = k.Length) || (n <> t.prefix.Length)) then false else
        let ix = uint64 (k.[n])
        if IntMap.isKeyRemote ix (t.children) then true else
        match IntMap.tryFind ix (t.children) with
        | Some c -> isKeyRemote (BS.drop (n+1) k) c
        | None -> false

    /// Minimally rewrite a tree such that isKeyRemote returns false.
    let rec touch (k:Key) (t:Tree<'V>) : Tree<'V> =
        let n = bytesShared k (t.prefix)
        if ((n = k.Length) || (n <> t.prefix.Length)) then t else
        let ix = uint64 (k.[n])
        match IntMap.tryFind ix (t.children) with
        | Some c ->
            let c' = touch (BS.drop (n+1) k) c
            let cs' = IntMap.add ix c' (t.children)
            { t with children = cs' }
        | None -> t
        
    // TODO:
    // - efficient structural diffs (low priority)
    // - efficient tree merges 

    module Enc =
        // encoding is trivial concatenation of key, value, and children.

        // to avoid dynamic construction of the IntMap codec I'm using
        // a recursive object constructor. Awkward, but safe in this case.
        type TreeCodec<'V> =
            val value    : Codec<'V>                  // value encoder
            val children : Codec<IntMap<Tree<'V>>>    // for recursion
            interface Codec<Tree<'V>> with
                member c.Write t dst =
                    EncBytes.write (t.prefix) dst
                    EncOpt.write (c.value) (t.value) dst
                    Codec.write (c.children) (t.children) dst
                member c.Read db src =
                    let p = EncBytes.read src
                    let v = EncOpt.read (c.value) db src
                    let cs = Codec.read (c.children) db src
                    { prefix = p; value = v; children = cs }
                member c.Compact db t =
                    let szP = EncBytes.size (t.prefix)
                    let struct(v',szV) = EncOpt.compact (c.value) db (t.value)
                    let struct(cs',szCS) = Codec.compactSz (c.children) db (t.children)
                    let t' = { prefix = (t.prefix); value = v'; children = cs' }
                    struct(t', szP + szV + szCS)
            new(cv,thresh) as tc =
                let cc = IntMap.codec' thresh (tc :> Codec<Tree<'V>>)
                { value = cv; children = cc }

    /// Codec with specified heuristic compaction threshold.
    let inline codec' (thresh:SizeEst) (cV:Codec<'V>) =
        Enc.TreeCodec<'V>(cV,thresh) :> Codec<Tree<'V>>

    /// Codec with default compaction threshold.
    let inline codec cV = codec' (IntMap.EncNode.defaultThreshold) cV 

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


type Trie<'V> = Trie.Tree<'V>






