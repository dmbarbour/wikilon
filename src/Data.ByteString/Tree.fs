namespace Data.ByteString

/// This is a lookup simple tree specialized for ByteString keys. 
/// It is based on the Critbit tree.
///
/// This is intended as an alternative to Map for ByteString. The
/// disadvantage of Map is that it must compare from the start of
/// a key in each comparison, resulting in redundant effort when
/// large prefixes are shared between keys. A critbit tree avoids
/// extra comparisons.
module Tree =

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
    let partitionK (k:Key) (t:Tree<'V>) : (Tree<'V> * Tree<'V>) =
        match t with
        | Root(kl,n) ->
            match findCritbit 0 k kl with
            | Some cb when testCritbit cb k ->
                let struct(l,r) = pKR cb k n
                (Root(kl,l),r)
            | _ -> (Empty, t)
        | Empty -> (Empty, Empty)

    module private EnumNode =
        type Stack<'V> = (Key * Node<'V>) list
        type Elem<'V> = (Key * 'V)
        type State<'V> = (Elem<'V> * Stack<'V>)
        type StepFn<'V> = Stack<'V> -> Key -> Node<'V> -> State<'V>
        
        let rec stepL s kl n =
            match n with
            | Inner (_, l, kr, r) -> stepL ((kr,r)::s) kl l
            | Leaf v -> ((kl,v),s)

        let rec stepR s kl n =
            match n with
            | Inner (_, l, kr, r) -> stepR ((kl,l)::s) kr r
            | Leaf v -> ((kl,v),s)

        type Enumerator<'V> = // enumerates left to right
            val step : StepFn<'V>
            val mutable private state : State<'V>
            member e.Elem with get() = fst e.state
            member e.Stack with get() = snd e.state
            new (step,k,n) = 
                // need MoveNext for first element.
                let state0 = (Unchecked.defaultof<_>, (k,n)::[])
                { step = step; state = state0 }
            interface System.Collections.Generic.IEnumerator<Elem<'V>> with
                member e.Current with get() = e.Elem
            interface System.Collections.IEnumerator with
                member e.Current with get() = upcast e.Elem
                member e.MoveNext() =
                    match e.Stack with
                    | ((kr,r)::sr) -> e.state <- e.step sr kr r; true
                    | _ -> false
                member e.Reset() = raise (System.NotSupportedException())
            interface System.IDisposable with
                member e.Dispose() = ()

        type Enumerable<'V> =
            val private s : StepFn<'V>
            val private k : Key
            val private n : Node<'V>
            new(s:StepFn<'V>, k:Key, n:Node<'V>) = { s = s; k = k; n = n }
            member e.GetEnum() = new Enumerator<'V>(e.s, e.k, e.n)
            interface System.Collections.Generic.IEnumerable<Elem<'V>> with
                member e.GetEnumerator() = upcast e.GetEnum()
            interface System.Collections.IEnumerable with
                member e.GetEnumerator() = upcast e.GetEnum()


    let ofArray (arr:(Key * 'V) array) : Tree<'V> =
        Array.fold (fun t (k,v) -> add k v t) empty arr
    let ofList (lst:(Key * 'V) list) : Tree<'V> =
        List.fold (fun t (k,v) -> add k v t) empty lst
    let ofSeq (s:seq<Key * 'V>) : Tree<'V> =
        Seq.fold (fun t (k,v) -> add k v t) empty s

    /// Convert to reverse-ordered sequence. 
    let toSeq (t:Tree<'V>) : seq<Key * 'V> =
        match t with
        | Empty -> Seq.empty
        | Root(kl,n) -> upcast EnumNode.Enumerable<'V>(EnumNode.stepL,kl,n)

    /// Reverse-ordered sequence (greatest key to least, foldBack order)
    let toSeqR (t:Tree<'V>) : seq<Key * 'V> =
        match t with
        | Empty -> Seq.empty
        | Root(kl,n) -> upcast EnumNode.Enumerable<'V>(EnumNode.stepR,kl,n)

    let toArray (t:Tree<'V>) : (Key * 'V) array = Array.ofSeq (toSeq t)
    let toList (t:Tree<'V>) : (Key * 'V) list = List.ofSeq (toSeq t)
    
    let fold (fn:'S -> Key -> 'V -> 'S) (s0:'S) (t:Tree<'V>) : 'S =
        Seq.fold (fun s (k,v) -> fn s k v) s0 (toSeq t)
    let foldBack (fn:Key -> 'V -> 'S -> 'S) (t:Tree<'V>) (s0:'S) : 'S =
        Seq.fold (fun s (k,v) -> fn k v s) s0 (toSeqR t)
    let iter (fn: Key -> 'V -> unit) (t:Tree<'V>) : unit =
        Seq.iter (fun (k,v) -> fn k v) (toSeq t)

    let filter (fn:Key -> 'V -> bool) (t:Tree<'V>) : Tree<'V> =
        toSeq t |> Seq.filter (fun (k,v) -> fn k v) |> ofSeq
    

type Tree<'V> = Tree.Tree<'V>

