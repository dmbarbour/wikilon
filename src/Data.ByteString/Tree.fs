namespace Data.ByteString

/// This is a lookup simple tree specialized for ByteString keys. 
/// It is based on the Critbit tree.
///
/// This is intended as an alternative to Map for ByteString keys.
/// Maps aren't optimal for large keys with shared prefixes since
/// they must compare those prefixes redundantly. A critbit tree
/// avoids this, and only looks at the necessary points in a key
/// to distinguish them plus a final full-key comparison.
module CritbitTree =

    /// Keys in the Tree are short, simple ByteStrings.
    ///
    /// Most operations on a Tree are O(Key length). So keys should
    /// be reasonably short for performance. But a few kilobytes is
    /// probably okay.
    type Key = ByteString
    let maxKeyLen = (System.Int32.MaxValue / 9) // about 200MB
    let inline isValidKey (k:Key) = (maxKeyLen >= k.Length)

    /// A Critbit indicates a bit-level offset into a key, a point
    /// of difference between two keys. However, we use 9-bit bytes
    /// in this case, with the high bit of every byte indicating 
    /// whether the key is at least that large. This simplifies work
    /// with variable-sized keys.
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
        let mutable x = x0
        let mutable r = 0
        if(0us <> (0xFF00us &&& x)) then r <- (8 + r); x <- (x >>> 8)
        if(0us <> (0x00F0us &&& x)) then r <- (4 + r); x <- (x >>> 4)
        if(0us <> (0x000Cus &&& x)) then r <- (2 + r); x <- (x >>> 2)
        if(0us <> (0x0002us &&& x)) then (1 + r) else r

    /// Find first critbit between keys greater or equal to specified critbit.
    /// May return None when there are no differences beyond the specified bit.
    let findCritbit (cbMin:Critbit) (a:ByteString) (b:ByteString) : Critbit option =
        let len = max a.Length b.Length
        assert ((cbMin >= 0) && (maxKeyLen >= len))
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
    
    /// A node is either a Leaf or an Inner node with a critbit where
    /// all keys with the critbit 0 are on the left and all others are
    /// on the right. Critbit must increase monotonically, i.e. so we
    /// always look at the first bit that doesn't match.
    ///
    /// The 'least key' for a given subtree is always held in the parent.
    /// Hence, the Inner node carries the least-key for the right child,
    /// and the tree root carries the least-key for the leftmost value.
    /// This design simplifies insertion.
    type Node<'V> =
        | Leaf of 'V
        | Inner of Critbit * Node<'V> * Key * Node<'V>

    /// The Tree structure.
    ///
    /// The critbit tree is history-independent, so naive comparison 
    /// implementations work well enough.
    type Tree<'V> =
        | Empty
        | Root of Key * Node<'V>

    let rec private tryFindN (k:Key) (kl:Key) (node:Node<'V>) : 'V option =
        match node with
        | Inner(cb,l,kr,r) ->
            if testCritbit cb k 
                then tryFindN k kr r
                else tryFindN k kl l
        | Leaf v -> if (kl = k) then Some v else None

    /// try to find value associated with a key.
    let tryFind (k:Key) (t:Tree<'V>) : 'V option =
        match t with
        | Root(kl,n) -> tryFindN k kl n
        | Empty -> None

    let inline containsKey k t = Option.isSome (tryFind k t)

    /// Find or raise `System.Collection.Generic.KeyNotFoundException`
    let find (k:Key) (t:Tree<'V>) : 'V =
        match tryFind k t with
        | Some v -> v
        | None -> raise (System.Collections.Generic.KeyNotFoundException())


    let rec private validateN (mb:Critbit) (kl:Key) (node:Node<_>) : bool =
        match node with
        | Leaf _ -> true
        | Inner (cb, l, kr, r) ->
            ((Some cb) = findCritbit mb kl kr)
                && (not (testCritbit cb kl))
                && (testCritbit cb kr)
                && (validateN (1+cb) kl l)
                && (validateN (1+cb) kr r)

    /// validate structural invariants of tree.
    let validate (t:Tree<_>) : bool =
        match t with
        | Empty -> true
        | Root(kl,n) -> validateN 0 kl n

    let empty : Tree<_> = Empty
    let isEmpty (t:Tree<_>) : bool =
        match t with 
        | Empty -> true
        | _ -> false

    let inline singleton (k:Key) (v:'V) : Tree<'V> = Root (k, Leaf v)

    let rec private sizeN node = 
        match node with
        | Leaf _ -> 1
        | Inner (_,l,_,r) -> sizeN l + sizeN r

    /// Size computation is O(N). 
    ///   If you need efficient size, consider wrapping the tree with
    ///   some metadata.
    let size (t:Tree<_>) : int =
        match t with
        | Empty -> 0
        | Root (_,n) -> sizeN n

    let rec private setLKV (v:'V) (node:Node<'V>) : Node<'V> =
        match node with
        | Inner (cb, l, kr, r) -> Inner (cb, setLKV v l, kr, r)
        | Leaf _ -> Leaf v
    let rec private addLKV (kl0:Key) (ncb:Critbit) (v:'V) (node:Node<'V>) : Node<'V> =
        match node with
        | Inner (cb, l, kr, r) when (cb < ncb) ->
            Inner(cb, addLKV kl0 ncb v l, kr, r)
        | _ -> Inner(ncb, Leaf v, kl0, node)
    let rec private addRKV (mb:Critbit) (k:Key) (v:'V) (node:Node<'V>) : Node<'V> =
        match node with
        | Inner (cb, l, kr, r) ->
            if (cb < mb) then Inner (cb, addRKV mb k v l, kr, r)
            elif (cb > mb) then Inner (mb, node, k, Leaf v)
            else
                match findCritbit (1+cb) k kr with
                | None -> Inner (cb, l, kr, setLKV v r)
                | Some mb' ->
                    if testCritbit mb' k
                        then Inner (cb, l, kr, addRKV mb' k v r)
                        else Inner (cb, l, k, addLKV kr mb' v r)
        | Leaf _ -> Inner (mb, node, k, Leaf v)

    /// Add or modify key-value association. Returns a new tree.
    let add (k:Key) (v:'V) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Root (kl, n) ->
            match findCritbit 0 k kl with
            | None -> Root (kl, setLKV v n)
            | Some mb ->
                if testCritbit mb k
                    then Root (kl, addRKV mb k v n)
                    else Root (k, addLKV kl mb v n)
        | Empty -> singleton k v

    let rec private remN (kl:Key) (k:Key) (node:Node<'V>) : Tree<'V> =
        match node with
        | Inner (cb, l, kr, r) ->
            if testCritbit cb k then
                match remN kr k r with
                | Root(kr',r') -> Root(kl, Inner(cb, l, kr', r'))
                | Empty -> Root(kl,l)
            else
                match remN kl k l with
                | Root(kl',l') -> Root(kl', Inner(cb, l', kr, r))
                | Empty -> Root(kr,r)
        | Leaf _ -> if (kl = k) then Empty else Root(kl,node)

    /// Remove a key if it is present in the tree.
    let remove (k:Key) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Root(kl,n) -> remN kl k n
        | Empty -> Empty

    let rec private mapN fn kl node =
        match node with
        | Inner (cb,l,kr,r) -> 
            let l' = mapN fn kl l
            let r' = mapN fn kr r
            Inner(cb, l', kr, r')
        | Leaf v -> Leaf (fn kl v)

    /// Map a function to every key-value pair.
    /// (Performs no key comparisons.)
    let map (fn:Key -> 'T -> 'U) (t:Tree<'T>) : Tree<'U> =
        match t with
        | Root(kl,n) -> Root(kl, mapN fn kl n)
        | Empty -> Empty


    let rec selectPrefixN (p:ByteString) (kl:Key) (node:Node<'V>) : Tree<'V> =
        match node with
        | Inner(cb, l, kr, r) when (cb < (9 * p.Length)) ->
            if testCritbit cb p 
                then selectPrefixN p kr r
                else selectPrefixN p kl l
        | _ -> if (p = (BS.take p.Length kl)) then Root(kl,node) else Empty
            
    /// Obtain subset of tree where all keys match a specified prefix.
    let selectPrefix (p:ByteString) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Root(kl,n) -> selectPrefixN p kl n
        | Empty -> Empty

    let rec dropPrefixN (p:ByteString) (kl:Key) (node:Node<'V>) : Tree<'V> =
        match node with
        | Inner(cb, l, kr, r) when (cb < (9 * p.Length)) ->
            if testCritbit cb p then
                match dropPrefixN p kr r with
                | Root(kr',r') -> Root(kl, Inner(cb, l, kr', r'))
                | Empty -> Root(kl,l)
            else
                match dropPrefixN p kl l with
                | Root(kl',l') -> Root(kl', Inner(cb, l', kr, r))
                | Empty -> Root(kr,r)
        | _ -> if (p = (BS.take p.Length kl)) then Empty else Root(kl,node)

    /// Remove all keys matching the given prefix. 
    let dropPrefix (p:ByteString) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Root(kl,n) -> dropPrefixN p kl n
        | Empty -> Empty

    // pKR assumes k > least-key starting at mb
    let rec private pKR (mb:Critbit) (k:Key) (node:Node<'V>) : struct(Node<'V> * Tree<'V>) =
        match node with
        | Inner(cb, l, kr, r) when (mb >= cb) ->
            if(mb > cb) then
                let struct(ll,lr) = pKR mb k l
                let rr = 
                    match lr with
                    | Root(kl',l') -> Root(kl', Inner(cb, l', kr, r))
                    | Empty -> Root(kr,r)
                struct(ll,rr)
            else
                assert(testCritbit cb k)
                match findCritbit (1+cb) k kr with
                | Some mb' when testCritbit mb' k ->
                    let struct(rl,rr) = pKR mb' k r
                    let ll = Inner(cb, l, kr, rl)
                    struct(ll,rr)
                | _ -> struct(l, Root(kr,r))
        | _ -> struct(node, Empty) 

    /// Partition a tree on a key. All keys less than the target are in
    /// the left, and all other keys are in the right.
    let splitAtKey (k:Key) (t:Tree<'V>) : struct(Tree<'V> * Tree<'V>) =
        match t with
        | Root(kl,n) ->
            match findCritbit 0 k kl with
            | Some cb when testCritbit cb k ->
                let struct(l,r) = pKR cb k n
                struct(Root(kl,l),r)
            | _ -> struct(Empty, t)
        | Empty -> struct(Empty, Empty)

    let ofArray (arr:(Key * 'V) array) : Tree<'V> =
        Array.fold (fun t (k,v) -> add k v t) empty arr
    let ofList (lst:(Key * 'V) list) : Tree<'V> =
        List.fold (fun t (k,v) -> add k v t) empty lst
    let ofSeq (s:seq<Key * 'V>) : Tree<'V> =
        Seq.fold (fun t (k,v) -> add k v t) empty s

    let rec private toSeqN kl node = seq {
        match node with
        | Leaf v -> yield (kl,v)
        | Inner (_,l,kr,r) ->
            yield! toSeqN kl l
            yield! toSeqN kr r
        }

    /// Convert to lexicographically ordered sequence.
    let toSeq (t:Tree<'V>) : seq<Key * 'V> =
        match t with
        | Empty -> Seq.empty
        | Root(kl,n) -> toSeqN kl n

    let rec private toSeqNR kl node = seq {
        match node with
        | Leaf v -> yield (kl,v)
        | Inner (_,l,kr,r) ->
            yield! toSeqNR kr r
            yield! toSeqNR kl l
        }

    /// Reverse-ordered sequence (greatest key to least, foldBack order)
    let toSeqR (t:Tree<'V>) : seq<Key * 'V> =
        match t with
        | Empty -> Seq.empty
        | Root(kl,n) -> toSeqNR kl n

    let toArray (t:Tree<'V>) : (Key * 'V) array = Array.ofSeq (toSeq t)
    let toList (t:Tree<'V>) : (Key * 'V) list = List.ofSeq (toSeq t)

    // I could use Seq for folds and iteration, but I'd prefer to
    // keep these operations on the normal stack for performance.

    let fold (fn:'S -> Key -> 'V -> 'S) (s0:'S) (t:Tree<'V>) : 'S =
        let rec loop st kl node =
            match node with
            | Leaf v -> fn st kl v
            | Inner (_,l,kr,r) -> loop (loop st kl l) kr r
        match t with
        | Root(k,n) -> loop s0 k n
        | Empty -> s0

    let foldBack (fn:Key -> 'V -> 'S -> 'S) (t:Tree<'V>) (s0:'S) : 'S =
        let rec loop kl node st =
            match node with
            | Leaf v -> fn kl v st
            | Inner(_,l,kr,r) -> loop kl l (loop kr r st)
        match t with
        | Root(k,n) -> loop k n s0
        | Empty -> s0

    let iter (fn:Key -> 'V -> unit) (t:Tree<'V>) : unit =
        let rec loop kl node =
            match node with
            | Leaf v -> fn kl v
            | Inner(_,l,kr,r) -> loop kl l; loop kr r
        match t with
        | Root(k,n) -> loop k n
        | Empty -> ()

    let tryPick (fn:Key -> 'V -> 'U option) (t:Tree<'V>) : 'U option =
        let rec loop kl node =
            match node with
            | Leaf v -> fn kl v
            | Inner(_,l,kr,r) ->
                let pickL = loop kl l
                if Option.isSome pickL then pickL else
                loop kr r
        match t with
        | Root(k,n) -> loop k n
        | Empty -> None

    let inline tryFindKey fn t = tryPick (fun k v -> if fn k v then Some k else None) t
    let inline exists fn t = tryFindKey fn t |> Option.isSome
    let inline forall fn t = not (exists (fun k v -> not (fn k v)) t)

    let filter (fn:Key -> 'V -> bool) (t:Tree<'V>) : Tree<'V> =
        toSeq t |> Seq.filter (fun (k,v) -> fn k v) |> ofSeq

    // TODO: view disassembly and improve performance!

type CritbitTree<'V> = CritbitTree.Tree<'V>

