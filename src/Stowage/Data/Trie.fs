namespace Stowage
open Data.ByteString

/// Trie with bytestring keys, above Stowage. 
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

    let inline private matchPrefix prefix k = 
        (prefix = (BS.take (prefix.Length) k))

    let rec tryFind (k:Key) (t:Tree<'V>) : 'V option =
        if not (matchPrefix (t.prefix) k) then None else
        let exactMatch = (k.Length = t.prefix.Length)
        if exactMatch then (t.value) else
        let k' = BS.drop (t.prefix.Length + 1) k
        let ix = uint64 (k.[t.prefix.Length])
        match IntMap.tryFind ix (t.children) with
        | Some t' -> tryFind (BS.unsafeTail k') t'
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

    // make a pure branching node (no value).
    // Recognizes simplification cases if we don't branch.
    let private mkBrNode p cs =
        match cs with
        | None -> empty
        | Some (IntMap.Leaf(b,c)) ->
            { c with prefix = joinBytes p (byte b) (c.prefix) }
        | _ -> { prefix = p; value = None; children = cs }

    /// Return copy of tree minus a specified key. 
    let rec remove (k:Key) (t:Tree<'V>) : Tree<'V> =
        if not (matchPrefix (t.prefix) k) then t else
        let exactMatch = (k.Length = t.prefix.Length)
        if exactMatch then
            // matched the key, but we might still branch.
            mkBrNode (t.prefix) (t.children)
        else 
            let ix = uint64 (k.[t.prefix.Length])
            let cs = t.children
            match IntMap.tryFind ix cs with
            | None -> t
            | Some c ->
                let k' = BS.drop (t.prefix.Length + 1) k
                let c' = remove k' c
                let cs' = 
                    if isEmpty c' 
                       then IntMap.remove ix cs
                       else IntMap.add ix c' cs
                if Option.isNone (t.value)
                    then mkBrNode (t.prefix) cs'
                    else { t with children = cs' }

    /// Remove key from tree only if it exists. This avoids rewriting
    /// the tree in the cases where the key is unlikely to be present.            
    let inline checkedRemove (k:Key) (t:Tree<'V>) : Tree<'V> =
        if containsKey k t then remove k t else t

    // compute size of shared prefix for two strings.
    let private bytesShared (a:ByteString) (b:ByteString) : int =
        let limit = min (a.Length) (b.Length)
        let rec loop ix =
            if ((ix = limit) || (a.[ix] <> b.[ix])) then ix else
            loop (ix + 1)
        loop 0

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
            let bK = uint64 k.[n]
            let cK = singleton (BS.drop (n+1) k) v
            let bP = uint64 (t.prefix.[n])
            let cP = { t with prefix = BS.drop (n+1) (t.prefix) }
            assert (bK <> bP)
            let cs' = IntMap.empty
                    |> IntMap.add (uint64 bK) cK
                    |> IntMap.add (uint64 bP) cP
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

    // TODO: efficient structural diff
    //  I might need to peek into IntMap structure

    module internal E =
        // encoding is `key value children`, trivially.

        // to avoid dynamic construction of the IntMap codec I'm using
        // a recursive object constructor. Awkward, but safe in this case.
        type TreeCodec<'V> =
            val value    : Codec<'V>                  // value encoder
            val children : Codec<IntMap<Tree<'V>>>   // for recusion
            new(cv) as tc =
                let cc = IntMap.treeCodec (tc :> Codec<Tree<'V>>)
                { value = cv; children = cc }
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

           
    let codec (cV:Codec<'V>) =
        E.TreeCodec<'V>(cV) :> Codec<Tree<'V>>


    // utilities todo:
    //  filter tree by prefix, remove/add prefix
    //  split tree at key


type Trie<'V> = Trie.Tree<'V>






