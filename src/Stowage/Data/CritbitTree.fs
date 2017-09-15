namespace Stowage
open Data.ByteString

/// A critbit tree modeled above Stowage.
///
/// A critbit tree is a simple associative key-value structure with many
/// trie-like properties. This includes a linear lookup cost in key size
/// and a history-independent structure.
///
/// By modeling a key-value tree above Stowage, we essentially have first
/// class database values with a lot of nice properties: data persistence,
/// structure sharing, lightweight serialization, efficient diffs, etc..
/// The database may be larger than active memory. Keys and values may be
/// further structured.
///
/// For integration with Stowage, we use an explicit 'compact' step that
/// shoves larger nodes into the stowage database. Thus we do have history
/// dependent structure between compactions.
///
/// If compaction is not used, this can serve as a simple critbit tree.
module CBTree =
    
    /// Keys in our CBTree should be short, simple bytestrings.
    /// Keep keys under a few kilobytes for performance.
    type Key = ByteString
    
    /// A Critbit indicates a bit-level offset into a key.
    type Critbit = BTree.Critbit
    let inline testCritbit cb k = BTree.testCritbit cb k
    let inline findCritbit cbMin a b = BTree.findCritbit cbMin a b

    /// Tree size is simply a count of keys or values.
    ///
    /// The CBTree keeps a little metadata when compacting nodes
    /// such that it's easy to determine size of a tree without
    /// peeking into the database. This metadata is also necessary
    /// for efficient tree diffs.
    type TreeSize = uint64

    /// CBTree nodes are based on a modified critbit tree. The least
    /// key for any node is kept in the parent (to simplify inserts).
    /// Remote nodes
    type Node<'V> =
        | Leaf of 'V                            // leaf value inline
        | INode of Critbit * Node<'V> * Key * Node<'V>  // inner node
        | RNode of Critbit * TreeSize * LVRef<Node<'V>> // ref to INode

    let rec private nodeElemCt (node:Node<_>) : TreeSize =
        match node with
        | Leaf _ -> 1UL
        | INode (_,l,_,r) -> nodeElemCt l + nodeElemCt r
        | RNode (_,tsz,_) -> tsz

    /// The Tree is generic in the value type. 
    ///
    /// A codec for compacting and serializing values must be provided
    /// upon compact or stowage operations. If you don't need Stowage,
    /// consider use of Data.ByteString.BTree to avoid representing the
    /// states you don't need.
    type Tree<'V> =
        | Empty
        | Root of Key * Node<'V>

    /// Compute total tree size - the number of key-value elements.
    /// O(N) in general. O(1) for compacted subtrees. Together, this
    /// results in O(N) with number of updates since last compaction.
    let size (t:Tree<_>) : TreeSize =
        match t with
        | Empty -> 0UL
        | Root(_,node) -> nodeElemCt node

    let rec private validateN (mb:Critbit) (kl:Key) (node:Node<_>) =
        match node with
        | Leaf _ -> true
        | RNode (cb, tsz, ref) ->
            let x = LVRef.load ref
            (tsz = nodeElemCt x)
                && (tsz > 1UL) // no leaf refs!
                && (cb >= mb) 
                && (validateN cb kl x)
        | INode (cb, l, kr, r) ->
            ((Some cb) = findCritbit mb kl kr)
                && (testCritbit cb kr)
                && (validateN (1+cb) kl l)
                && (validateN (1+cb) kr r)

    /// validate structural invariants of tree.
    let validate (t:Tree<_>) : bool =
        match t with
        | Empty -> true
        | Root(kl,n) -> validateN 0 kl n

    let empty : Tree<_> = Empty
    let isEmpty (t : Tree<_>) : bool =
        match t with
        | Empty -> true
        | Root _ -> false
    let inline singleton (k : Key) (v : 'V) : Tree<'V> = Root (k, Leaf v)

    // obtain value associated with least-key
    let rec private getLKV (node:Node<'V>) : 'V =
        match node with
        | INode (_, l, _, _) -> getLKV l
        | RNode (_, _, ref) -> getLKV (LVRef.load ref)
        | Leaf v -> v

    // here 'mb' is the first potential unmatched critbit
    let rec private tryFindN (mb:Critbit) (k:Key) (kl:Key) (node:Node<'V>) : 'V option =
        match node with
        | INode (cb, l, kr, r) ->
            if testCritbit cb k
               then tryFindN mb k kr r
               else tryFindN mb k kl l
        | RNode (cb, _, ref) ->
            match findCritbit mb k kl with
            | None -> Some (getLKV (LVRef.load ref))
            | Some mb' ->
                // stop if diff in prefix or if smaller than least-key
                let stop = (mb' < cb) || (testCritbit mb' kl)
                if stop then None else
                tryFindN mb' k kl (LVRef.load ref)
        | Leaf v ->
            let keysMatch = Option.isNone (findCritbit mb k kl)
            if keysMatch then Some v else None

    /// Lookup value (if any) associated with a key.
    let tryFind (k:Key) (t:Tree<'V>) : 'V option =
        match t with
        | Root (kl,node) -> tryFindN 0 k kl node
        | Empty -> None

    // it is only when comparing partial keys at Leaf or RNode
    let rec private containsKeyN (mb:Critbit) (k:Key) (kl:Key) (node:Node<_>) : bool =
        match node with
        | INode (cb, l, kr, r) ->
            if testCritbit cb k
               then containsKeyN mb k kr r
               else containsKeyN mb k kl l
        | RNode (cb, _, ref) ->
            match findCritbit mb k kl with
            | None -> true // least-key matches, no lookup needed
            | Some mb' ->
                let stop = (mb' < cb) || (testCritbit mb' kl)
                if stop then false else
                containsKeyN mb' k kl (LVRef.load ref)
        | Leaf _ -> Option.isNone (findCritbit mb k kl)

    /// Test whether a value is contained within a tree.
    /// This aims to avoid opening RNodes where feasible.
    let containsKey (k:Key) (t:Tree<_>) : bool = 
        match t with
        | Root (kl,n) -> containsKeyN 0 k kl n
        | Empty -> false


    /// Find value associated with a key 
    ///   or raise `System.Collections.Generic.KeyNotFoundException`
    let find (k:Key) (t:Tree<'V>) : 'V =
        match tryFind k t with
        | Some v -> v
        | None -> raise (System.Collections.Generic.KeyNotFoundException())

    // update existing least-key value
    let rec private setLKV (v:'V) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb, l, k, r) -> INode (cb, setLKV v l, k, r)
        | RNode (_, _, ref) -> setLKV v (LVRef.load' ref)
        | Leaf _ -> Leaf v

    // add to left of least-key (new least-key)
    let rec private addLKV (mb:Critbit) (oldLK:Key) (v:'V) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb, l, kr, r) when (mb >= cb) ->
            assert(mb > cb) // otherwise not a least-key
            INode (cb, addLKV mb oldLK v l, kr, r) 
        | RNode (cb, _, ref) when (mb >= cb) -> 
            assert(mb > cb) // otherwise not a least-key
            addLKV mb oldLK v (LVRef.load' ref)
        | _ -> 
            assert(testCritbit mb oldLK) // old key larger at mb?
            INode (mb, Leaf v, oldLK, node) 

    // add to right of least-key. 
    let rec private addRKV (mb:Critbit) (k:Key) (v:'V) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb, l, kr, r) when (mb >= cb) ->
            if (mb > cb) then 
                INode (cb, addRKV mb k v l, kr, r)
            else
                match findCritbit (1+cb) k kr with
                | Some mb' ->
                    if testCritbit mb' k 
                        then INode(cb, l, kr, addRKV mb' k v r)
                        else INode(cb, l, k, addLKV mb' kr v r)
                | None -> INode(cb, l, kr, setLKV v r)
        | RNode (cb, _, ref) when (mb >= cb) ->
            addRKV mb k v (LVRef.load' ref)
        | _ -> INode (mb, node, k, Leaf v)

    /// Create a tree that's almost the same but has the given
    /// value associated with the specified key.
    let add (k:Key) (v:'V) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Root (tk, tn) ->
            match findCritbit 0 k tk with
            | Some mb -> 
                if testCritbit mb k
                    then Root (tk, addRKV mb k v tn) // add to right of least-key
                    else Root (k, addLKV mb tk v tn) // new least-key
            | None -> Root (tk, setLKV v tn) // update least-key
        | Empty -> singleton k v

    // remove least-key value for a node, return a tree.
    let rec private removeLKV (node:Node<'V>) : Tree<'V> =
        match node with
        | INode (cb, l, kr, r) ->
            match removeLKV l with
            | Root(kl', l') -> Root(kl', INode(cb, l', kr, r))
            | Empty -> Root(kr, r)
        | RNode (_, _, ref) -> removeLKV (LVRef.load' ref)
        | Leaf _  -> Empty // key removed

    // remove a key from a node.
    let rec private removeN (mb:Critbit) (k:Key) (kl:Key) (node:Node<'V>) : Tree<'V> =
        match node with
        | INode (cb, l, kr, r) ->
            if testCritbit cb k
               then match removeN mb k kr r with
                    | Root(kr',r') -> Root(kl, INode(cb, l, kr', r'))
                    | Empty -> Root(kl,l)
               else match removeN mb k kl l with
                    | Root(kl',l') -> Root(kl', INode(cb, l', kr, r))
                    | Empty -> Root(kr,r)
        | RNode (cb, _, ref) ->
            match findCritbit mb k kl with
            | None -> removeLKV (LVRef.load' ref)
            | Some mb' ->
                let stop = (mb' < cb) || (testCritbit mb' kl)
                if stop then Root(kl,node) else
                removeN mb' k kl (LVRef.load' ref)
        | Leaf _ ->
            let keysMatch = Option.isNone (findCritbit mb k kl)
            if keysMatch then Empty else Root(kl,node)

    /// Remove key from tree. 
    ///
    /// Note: this essentially touches the key (cf. 'touch' below).
    /// If the key is probably not present, use checkedRemove instead.
    let remove (k:Key) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Root (kl, node) -> removeN 0 k kl node
        | Empty -> Empty

    /// Remove key, filtered by containsKey.
    let inline checkedRemove k t = if containsKey k t then remove k t else t


    let rec private touchLKV (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb,l,kr,r) -> INode (cb,touchLKV l,kr,r)
        | RNode (_, _, ref) -> touchLKV (LVRef.load' ref)
        | Leaf _ -> node

    let rec private touchN (mb:Critbit) (k:Key) (kl:Key) (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb,l,kr,r) ->
            if testCritbit cb k
                then INode (cb,l,kr,touchN mb k kr r)
                else INode (cb,touchN mb k kl l, kr, r)
        | RNode (cb,_,ref) -> 
            match findCritbit mb k kl with
            | Some mb' -> 
                let stop = (mb' < cb) || (testCritbit mb' kl)
                if stop then node else
                touchN mb' k kl (LVRef.load' ref)
            | None -> touchLKV (LVRef.load' ref)
        | Leaf _ -> node

    /// Load tree in path of specified key.
    ///
    /// This doesn't use the LVRef cache. It expands nodes within the
    /// tree, which lasts until the tree is compacted or GC'd. It may
    /// be useful to touch keys that you plan to manipulate frequently,
    /// but it might be wiser to simply model an explicit working set.
    let touch (k:Key) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Empty -> Empty
        | Root(kl,n) -> Root(kl, touchN 0 k kl n)

    let rec private expandN (node:Node<'V>) : Node<'V> =
        match node with
        | INode (cb,l,kr,r) -> INode (cb,expandN l, kr, expandN r)
        | RNode (_,_,ref) -> expandN (LVRef.load' ref)
        | Leaf _ -> node

    /// Fully load tree structure into memory. (Touch everything.)
    let expand (t:Tree<'V>) : Tree<'V> =
        match t with
        | Empty -> Empty
        | Root(kl,n) -> Root(kl, expandN n)

    // Enumeration of Tree Elements
    module private EnumNode =
        type Stack<'V> = (Key * Node<'V>) list
        type Elem<'V> = (Key * 'V)
        type State<'V> = (Elem<'V> * Stack<'V>)
        type StepFn<'V> = Stack<'V> -> Key -> Node<'V> -> State<'V>
        
        let rec stepL (s:Stack<'V>) (k:Key) (n:Node<'V>) : State<'V> =
            match n with
            | INode (_, l, kr, r) -> stepL ((kr,r)::s) k l
            | RNode (_, _, ref) -> stepL s k (LVRef.load ref)
            | Leaf v -> ((k,v),s)
            
        let rec stepR (s:Stack<'V>) (k:Key) (n:Node<'V>) : State<'V> =
            match n with
            | INode (_, l, kr, r) -> stepR ((k,l)::s) kr r
            | RNode (_, _, ref) -> stepR s k (LVRef.load ref)
            | Leaf v -> ((k,v),s)

        type Enumerator<'V> = // enumerates left to right
            val step : StepFn<'V>
            val mutable private state : State<'V>
            new (step,k,n) = 
                let state0 = (Unchecked.defaultof<_>,(k,n)::[])
                { step = step; state = state0 }
            member e.Elem with get() = fst e.state
            member e.Stack with get() = snd e.state
            interface System.Collections.Generic.IEnumerator<Elem<'V>> with
                member e.Current with get() = e.Elem
            interface System.Collections.IEnumerator with
                member e.Current with get() = upcast e.Elem
                member e.MoveNext() =
                    match e.Stack with
                    | ((k,n)::s) -> e.state <- e.step s k n; true
                    | _ -> false
                member e.Reset() = raise (System.NotSupportedException())
            interface System.IDisposable with
                member e.Dispose() = ()

        type Enumerable<'V> =
            val private s : StepFn<'V>
            val private k : Key
            val private n : Node<'V>
            new(s,k,n) = { s = s; k = k; n = n }
            member e.GetEnum() = new Enumerator<'V>(e.s, e.k, e.n)
            interface System.Collections.Generic.IEnumerable<Elem<'V>> with
                member e.GetEnumerator() = upcast e.GetEnum()
            interface System.Collections.IEnumerable with
                member e.GetEnumerator() = upcast e.GetEnum()

    /// sequence ordered from least key to greatest key
    let toSeq (t : Tree<'V>) : seq<(Key * 'V)> =
        match t with
        | Empty -> Seq.empty
        | Root(k,n) -> upcast EnumNode.Enumerable<'V>(EnumNode.stepL,k,n)

    /// reverse-ordered sequence, greatest key to least key
    let toSeqR (t : Tree<'V>) : seq<(Key * 'V)> =
        match t with
        | Empty -> Seq.empty
        | Root(k,n) -> upcast EnumNode.Enumerable<'V>(EnumNode.stepR,k,n)


    let inline fold (fn : 'St -> Key -> 'V -> 'St) (s0 : 'St) (t : Tree<'V>) : 'St =
        Seq.fold (fun s (k,v) -> fn s k v) s0 (toSeq t)
    let inline foldBack (fn : Key -> 'V -> 'St -> 'St) (t : Tree<'V>) (s0 : 'St) : 'St =
        Seq.fold (fun s (k,v) -> fn k v s) s0 (toSeqR t)
    let inline iter (fn: Key -> 'V -> unit) (t:Tree<'V>) : unit =
        Seq.iter (fun (k,v) -> fn k v) (toSeq t)
    let inline tryPick (fn:Key -> 'V -> 'U option) (t:Tree<'V>) : 'U option =
        Seq.tryPick (fun (k,v) -> fn k v) (toSeq t)
    let inline tryFindKey fn t = tryPick (fun k v -> if fn k v then Some k else None) t
    let inline exists fn t = tryFindKey fn t |> Option.isSome
    let inline forall fn t = not (exists (fun k v -> not (fn k v)) t)

    let inline toArray (t:Tree<'V>) : (Key * 'V) array = Array.ofSeq (toSeq t)
    let inline toList (t:Tree<'V>) : (Key * 'V) list = List.ofSeq (toSeq t)

    let ofSeq (s:seq<Key * 'V>) : Tree<'V> =
        Seq.fold (fun t (k,v) -> add k v t) empty s
    let ofList (lst:(Key * 'V) list) : Tree<'V> =
        List.fold (fun t (k,v) -> add k v t) empty lst
    let inline ofArray (a: (Key * 'V) array) : Tree<'V> =
        Array.fold (fun t (k,v) -> add k v t) empty a


    // select prefix when we have already matched the least-key. 
    let rec selectPrefixL (pb:Critbit) (node:Node<'V>) : Node<'V> =
        match node with
        | INode(cb, l, _, _) when (cb < pb) -> selectPrefixL pb l
        | RNode(cb, _, ref) when (cb < pb) -> selectPrefixL pb (LVRef.load' ref)
        | _ -> node

    // find critbit tweaked to return None for any match with full prefix
    let inline private findPrefixCritbit (cb:Critbit) (p:ByteString) (k:Key) =
        findCritbit cb p (BS.take p.Length k)    

    // here 'mb' is count of bits matched against kl
    let rec selectPrefixN (mb:Critbit) (p:ByteString) (kl:Key) (node:Node<'V>) : Tree<'V> =
        let pb = 9 * p.Length
        match node with
        | INode(cb, l, kr, r) when (cb < pb) ->
            if testCritbit cb p
                then selectPrefixN mb p kr r
                else selectPrefixN mb p kl l
        | RNode(cb, _, ref) when (cb < pb) ->
            match findPrefixCritbit mb p kl with
            | None -> Root(kl, selectPrefixL pb (LVRef.load' ref))
            | Some mb' -> // test if prefix differs before opening RNode
                let stop = (mb' < cb) || (testCritbit mb' kl)
                if stop then Empty else 
                selectPrefixN mb' p kl (LVRef.load' ref)
        | _ ->
            let prefixMatch = Option.isNone (findPrefixCritbit mb p kl)
            if prefixMatch then Root(kl,node) else Empty

    /// Filter a tree to just keys matching a specific prefix.
    let selectPrefix (p:ByteString) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Root(kl,l) -> selectPrefixN 0 p kl l
        | Empty -> Empty

    // partition node assuming k > least-key at mb.
    let rec private partitionN (mb:Critbit) (k:Key) (node:Node<'V>) : struct(Node<'V> * Tree<'V>) =
        match node with
        | INode(cb, l, kr, r) when (mb >= cb) ->
            if(mb > cb) then // split left node after cb
                let struct(ll, lr) = partitionN mb k l
                let rr = 
                    match lr with
                    | Empty -> Root(kr,r)
                    | Root(kl',l') -> Root(kl', INode(cb, l', kr, r))
                struct(ll,rr)
            else // divide right node
                assert(testCritbit cb k)
                match findCritbit (1+cb) k kr with
                | Some mb' when testCritbit mb' k ->
                    let struct(rl,rr) = partitionN mb' k r
                    let ll = INode(cb, l, kr, rl)
                    struct(ll,rr)
                | _ -> struct(l, Root(kr,r)) 
        | RNode (cb, _, ref) when (mb >= cb) -> 
            partitionN mb k (LVRef.load' ref)
        | _ -> struct(node, Empty) // all keys are less than k

    /// Partition a tree such that all keys strictly less than the
    /// given key are in the left tree, and all remaining keys are
    /// in the right tree. This has a O(N) cost in key length.
    let partition (k:Key) (t:Tree<'V>) : (Tree<'V> * Tree<'V>) =
        match t with
        | Empty -> (Empty, Empty)
        | Root(kl, node) ->
            match findCritbit 0 k kl with
            | Some mb when testCritbit mb k -> // kl < k
                let struct(l, r) = partitionN mb k node
                (Root(kl,l), r)
            | _ -> (Empty, t) // kl >= k

    /// Value differences relative to a pair of trees
    type VDiff<'V> =
        | InL of 'V         // value in left
        | InR of 'V         // value in right
        | InB of 'V * 'V    // two values with equality failure

    module private EnumNodeDiff =
        type Stack<'V> = (Key * Node<'V>) list
        type Elem<'V> = (Key * VDiff<'V>)
        type State<'V> = (Elem<'V> option * (Stack<'V> * Stack<'V>))

        let iniStack (t:Tree<'V>) : Stack<'V> =
            match t with
            | Empty -> []
            | Root(kl,n) -> ((kl,n)::[])

        let iniState (a:Tree<'V>) (b:Tree<'V>) : State<'V> = 
            (None, (iniStack a, iniStack b))

        let rec stepV (s:Stack<'V>) (n:Node<'V>) : struct(Stack<'V> * 'V) =
            match n with
            | INode (_, l, kr, r) -> stepV ((kr,r)::s) l
            | RNode (_, _, ref) -> stepV s (LVRef.load ref)
            | Leaf v -> struct(s,v)

        // utility class, mostly for the equality constraint
        type Stepper<'V when 'V : equality>() =

            member x.StepDiff (l:Stack<'V>) (r:Stack<'V>) : State<'V> =
                match (l, r) with
                | ((kl,nl)::sl, (kr,nr)::sr) ->
                    let cmp = compare kl kr
                    if (cmp < 0) then // key only in left
                        let struct(l',v) = stepV sl nl
                        (Some (kl, InL v), (l', r))
                    elif (cmp > 0) then // key only in right
                        let struct(r',v) = stepV sr nr
                        (Some (kr, InR v), (l, r'))
                    else x.StepDiffN kl nl sl nr sr // key in both
                | ((kl,nl)::sl, []) ->
                    let struct(l',v) = stepV sl nl
                    (Some (kl, InL v), (l', []))
                | ([], (kr,nr)::sr) ->
                    let struct(r',v) = stepV sr nr
                    (Some (kr, InR v), ([], r'))
                | ([],[]) -> (None, ([],[]))

            member x.StepDiffV (k:Key) (vl:'V) (sl:Stack<'V>) (vr:'V) (sr:Stack<'V>) : State<'V> =
                if (vl = vr)
                    then x.StepDiff sl sr
                    else (Some (k, InB (vl,vr)), (sl,sr))

            member x.StepDiffN (k:Key) (nl:Node<'V>) (sl:Stack<'V>) (nr:Node<'V>) (sr:Stack<'V>) : State<'V> =
                match (nl, nr) with
                | (Leaf vl, _) -> 
                    let struct(sr',vr') = stepV sr nr
                    x.StepDiffV k vl sl vr' sr'
                | (_, Leaf vr) ->
                    let struct(sl',vl') = stepV sl nl
                    x.StepDiffV k vl' sl' vr sr
                | (INode (_,nl',kr,r), _) -> 
                    let sl' = ((kr,r)::sl)
                    x.StepDiffN k nl' sl' nr sr
                | (_, INode (_,nr',kr,r)) -> 
                    let sr' = ((kr,r)::sr)
                    x.StepDiffN k nl sl nr' sr'
                | (RNode (cbl,szl,refl), RNode (cbr,szr,refr)) ->
                    // While comparing critbit and size isn't strictly needed,
                    // it's preferable to short-circuit BEFORE comparing ID.
                    // Use of ID with LVRef forces stowage of the tree data.
                    let eqNodes = (cbl = cbr) && (szl = szr) && (refl.ID = refr.ID)
                    if eqNodes then // skip equivalent subtrees
                        x.StepDiff sl sr 
                    elif (cbl < cbr) then // open node closest to root
                        let nl' = LVRef.load refl
                        x.StepDiffN k nl' sl nr sr
                    else
                        let nr' = LVRef.load refr
                        x.StepDiffN k nl sl nr' sr

        type Enumerator<'V when 'V : equality>(s:State<'V>) =
            inherit Stepper<'V>()
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
                    e.ST <- e.StepDiff l r
                    Option.isSome (fst e.ST)
                member e.Reset() = raise (System.NotSupportedException())
            interface System.IDisposable with
                member e.Dispose() = ()
            
        type Enumerable<'V when 'V : equality> =
            val private L : Tree<'V>
            val private R : Tree<'V>
            new(l:Tree<'V>,r:Tree<'V>) = { L = l; R = r }
            member e.GetEnum() = new Enumerator<'V>(iniState (e.L) (e.R))
            interface System.Collections.Generic.IEnumerable<Elem<'V>> with
                member e.GetEnumerator() = upcast e.GetEnum()
            interface System.Collections.IEnumerable with
                member e.GetEnumerator() = upcast e.GetEnum()


    /// Efficient difference between trees.
    /// 
    /// This returns a sequence of keys in ascending order with a VDiff for
    /// each key for which a difference between trees is observed. Attempts
    /// to avoid loading compacted subtrees unnecessarily, so we can perform
    /// an efficient structural diff on large compacted trees.
    let diff<'V when 'V : equality> (a:Tree<'V>) (b:Tree<'V>) : seq<(Key * VDiff<'V>)> =
        upcast EnumNodeDiff.Enumerable<'V>(a,b)

    /// Deep structural equality for Trees. (Tests for diff.)
    let deepEq<'V when 'V : equality> (a:Tree<'V>) (b:Tree<'V>) : bool = 
        Seq.isEmpty (diff a b)


    module EncNode =
        // encoding is separated from compaction, and multiple nodes
        // may freely be inlined into one binary.

        //  L(val)   - Leaf
        //  N(inode) - INode
        //  R(rnode) - RNode
        let cLeaf  : byte = byte 'L'
        let cINode : byte = byte 'N'
        let cRNode : byte = byte 'R'

        // Heuristic size threshold for compaction of a node.
        let compactThreshold : int = 14000

        // it's convenient to start with the Codec here.
        let codec (cV:Codec<'V>) : Codec<Node<'V>> =
            { new Codec<Node<'V>> with
                member cN.Write node dst =
                    match node with
                    | Leaf v ->
                        EncByte.write cLeaf dst
                        Codec.write cV v dst
                    | RNode (cb, tsz, ref) ->
                        EncByte.write cRNode dst
                        EncVarNat.write (uint64 cb) dst
                        EncVarNat.write tsz dst
                        EncLVRef.write ref dst
                    | INode (cb, l, kr, r) ->
                        EncByte.write cINode dst
                        EncVarNat.write (uint64 cb) dst
                        cN.Write l dst
                        EncBytes.write kr dst
                        cN.Write r dst
                member cN.Read db src =
                    let b0 = EncByte.read src
                    if (b0 = cLeaf) then
                        let v = cV.Read db src
                        Leaf v
                    elif (b0 = cRNode) then
                        let cb = int (EncVarNat.read src)
                        let tsz = EncVarNat.read src
                        let ref = EncLVRef.read cN db src
                        RNode (cb, tsz, ref)
                    elif (b0 <> cINode) then
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
                        let struct(v',sz) = cV.Compact db v
                        struct(Leaf v',1 + sz)
                    | RNode (cb, tsz, ref) ->
                        let szR = 1 + EncVarNat.size (uint64 cb) 
                                    + EncVarNat.size tsz 
                                    + EncLVRef.size
                        struct(node, szR) 
                    | INode (cb, l, kr, r) ->
                        let struct(l', szL) = cN.Compact db l
                        let struct(r', szR) = cN.Compact db r
                        let node' = INode (cb, l', kr, r')
                        let szN = 
                            1 + EncVarNat.size (uint64 cb)
                              + szL + EncBytes.size kr + szR
                        if (szN < compactThreshold) then 
                            struct(node',szN) 
                        else
                            let tsz = nodeElemCt node'
                            let ref = LVRef.stow cN db node'
                            cN.Compact db (RNode(cb,tsz,ref))
            }

        // TODO: consider parallelizing INode compaction.


    /// Codec for the full critbit tree, given a value codec.
    /// The root of the tre is encoded inline.
    let treeCodec (cV:Codec<'V>) : Codec<Tree<'V>> = 
        let cK = EncBytes.codec
        let cN = EncNode.codec cV
        let cRep = EncOpt.codec (EncPair.codec cK cN)
        let get rOpt = 
            match rOpt with
            | None -> Empty
            | Some r -> Root r
        let set t = 
            match t with
            | Empty -> None
            | Root (k,n) -> Some (k,n)
        Codec.lens cRep get set

    /// Compact a tree in memory.
    let compact (cV:Codec<'V>) (db:DB) (t:Tree<'V>) : Tree<'V> =
        match t with
        | Root (k,n) -> Root(k, Codec.compact (EncNode.codec cV) db n)
        | Empty -> Empty

    /// A small reference object for a potentially large tree.
    ///
    /// This is convenient if you want strong guarantees about the
    /// memory consumption modulo caching, or about node sizes when
    /// working with larger collections of tree values.
    type TreeRef<'V> = LVRef<(Key * Node<'V>)> option

    /// Reduce tree to ref, with asynchronous stowage.
    let stow' (cV:Codec<'V>) (db:DB) (t:Tree<'V>) : TreeRef<'V> =
        match t with
        | Empty -> None
        | Root(k,n) -> 
            let cKN = EncPair.codec (EncBytes.codec) (EncNode.codec cV)
            let ref = LVRef.stow cKN db (k,n)
            Some ref

    /// Compacts before stowing asynchronously.
    let inline stow (cV:Codec<'V>) (db:DB) (t:Tree<'V>) : TreeRef<'V> =
        stow' cV db (compact cV db t)

    /// Load TreeRef (caching)
    let load (ref:TreeRef<'V>) : Tree<'V> =
        match ref with
        | None -> Empty
        | Some r -> Root (LVRef.load r)

    /// Load TreeRef (non-caching)
    let load' (ref:TreeRef<'V>) : Tree<'V> =
        match ref with
        | None -> Empty
        | Some r -> Root (LVRef.load' r)

    /// Codec for reading or writing TreeRefs.
    let refCodec (cV:Codec<'V>) : Codec<TreeRef<'V>> =
        let cK = EncBytes.codec
        let cN = EncNode.codec cV
        let cKN = EncPair.codec cK cN
        EncOpt.codec (EncLVRef.codec cKN)


type CBTree<'V> = CBTree.Tree<'V>
        






