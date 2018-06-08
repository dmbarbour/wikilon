namespace Awelon
open Data.ByteString
open Stowage
// open System.Collections.Immutable

// Awelon language specifies a standard dictionary representation, based
// on a log-structured merge-tree with radix-tree indexing (an LSM-trie).
// This design is intended for legible import/export, distribution, and
// hierarchical embedding.
//
//      /prefix1 secureHash1
//      /prefix2 secureHash2
//      :symbol1 definition1
//      :symbol2 definition2
//      ~symbol3
// 
// A dictionary node is encoded in line-oriented ASCII text. Each line
// either defines (:) or deletes (~) a symbol, or directs to another 
// node based on a matching prefix (/). Referenced nodes are identified
// by their secure hash, and the matched prefix is stripped from all 
// symbols and prefixes within the referenced node. The empty symbol or
// prefix is valid and useful.
//
// This dictionary representation is streamable, representing real-time
// updates in an append-only log. Only the final match for the symbol or
// prefix is used. In a stream, empty prefix `/ secureHash` can be used
// for checkpoints or resets because empty prefix matches all symbols.
// But outside of streams, we'll normalize - erase irrelevant lines then 
// sort what remains.
//
// The dictionary could be used outside of Awelon, but you would need to
// escape symbols containing SP or definitions containing LF. Performance
// is worse than Stowage.LSMTrie (currently).
//
// Hierarchical dictionaries simply use `dictname/word` symbols. We
// can use `/dictname/ secureHash` to logically include one dictionary
// into another.
module Dict2 =
    let cLF = 10uy
    let cSP = 32uy
    let cDef = byte ':'
    let cDel = byte '~'
    let cDir = byte '/' 

    /// Symbols should be ASCII with no control characters, usually
    /// a word or extended with `/` for hierarchical dictionaries. 
    /// But minimally, excluding SP and LF is sufficient. Symbols 
    /// should not be too large, e.g. no more than a few hundred 
    /// bytes. For inner nodes, the common prefix is stripped so the
    /// symbol may be a suffix of a word.
    type Symbol = ByteString
    let isValidSymbolChar c = (cLF <> c) && (cSP <> c)
    let isValidSymbol s = BS.forall isValidSymbolChar s

    /// A directory is represented by a symbol prefix.
    type Prefix = Symbol
    
    /// A definition should consist of valid Awelon code, but we do
    /// not parse it at this layer. Minimally, it must not contain
    /// LF, which is used to separate definitions.
    ///
    /// Because definitions may contain secure hash references, we
    /// also provide a dependency slot to interact with Stowage GC.
    ///
    /// Overly large definitions can degrade performance, so it is 
    /// recommended to externalize large definitions before adding
    /// them to the dictionary, e.g. using `$secureHash` in Awelon.
    [<Struct; CustomEquality; CustomComparison>]
    type Def = 
        val Data : ByteString
        val private Deps : System.Object
        new(def,deps) = { Data = def; Deps = deps }
        new(def) = new Def(def,null)
        member x.KeepAlive() : unit = System.GC.KeepAlive (x.Deps)
        override x.GetHashCode() = x.Data.GetHashCode()
        override x.Equals(yobj) =
            match yobj with
            | :? Def as y -> (ByteString.Eq (x.Data) (y.Data))
            | _ -> false
        interface System.IComparable with
            member x.CompareTo (yobj : System.Object) =
                match yobj with
                | :? Def as y -> compare (x.Data) (y.Data)
                | _ -> invalidArg "yobj" "cannot compare values of different types"

 
    let isValidDefChar c = (cLF <> c)
    let isValidDefStr s = BS.forall isValidDefChar s
    let inline isValidDef (def:Def) = isValidDefStr (def.Data)

    /// Construct definition with automatic management of the
    /// contained secure hashes (if any).
    let autoDef (db:Stowage) (s:ByteString) : Def =
        let inline mkDep h = VRef.wrap (EncBytesRaw.codec) db h
        let inline addDep l h = ((mkDep h)::l)
        let deps = List.toArray (RscHash.foldHashDeps addDep [] s)
        Def(s, deps :> System.Object)


    /// Rewrite local definition to `$secureHash` remote redirect.
    let externDef (db:Stowage) (def:Def) : Def =
        let ref = VRef.stow (EncBytesRaw.codec) db (def.Data)
        def.KeepAlive()
        Def(BS.cons (byte '$') (ref.ID), (ref :> System.Object))

    // A definition update: None for delete, Some for define.
    type DefUpd = Def option 

    /// A dictionary node in-memory is represented as a trie with
    /// injected references to remote nodes (for /prefix entries).
    /// Size estimates are remembered for incremental compaction.
    type Dict = 
        { pd : Dir option     // optional empty prefix entry
          vu : DefUpd option  // optional empty symbol entry
          cs : Children       // nodes with larger prefixes
            // ct,sz are computed incrementally upon compaction
          ct : uint32         // visible entry count (or max value)
          sz : uint32         // serialization size (or max value)
        }
    and Children = Map<byte,struct(Prefix * Dict)>
    and Dir = LVRef<Dict> option  // secureHash (Some) or blank (None)
    let ctmax = System.UInt32.MaxValue
    let szmax = System.UInt32.MaxValue

    let inline private mkDict pd vu cs = 
        { pd = pd; vu = vu; cs = cs; ct = ctmax; sz = szmax }

    // This operation is inefficient. I've a proposal at fsharp-suggestions 
    // (#673) to support an efficient accessor for singleton maps to fix the
    // issue. 
    let private tryMapSingleton (m:Map<'K,'V>) : ('K * 'V) option =
        // this is ugly and inefficient
        let e = (Map.toSeq m).GetEnumerator()
        if not (e.MoveNext()) then None else
        let r = e.Current
        if e.MoveNext() then None else
        Some r

    let private joinBytes (a:ByteString) (b:byte) (c:ByteString) : ByteString =
        ByteStream.write (fun dst ->
            let len = a.Length + 1 + c.Length
            ByteStream.reserve len dst
            ByteStream.writeBytes a dst
            ByteStream.writeByte b dst
            ByteStream.writeBytes c dst)

    let inline private isPrefix p s = (p = (BS.take (BS.length p) s))

    // compute size of shared prefix for two strings.
    let private bytesShared (a:ByteString) (b:ByteString) : int =
        let limit = min (a.Length) (b.Length)
        let rec loop ix =
            if ((ix = limit) || (a.[ix] <> b.[ix])) then ix else
            loop (ix + 1)
        loop 0

    // Smart add-child function, after the child might have had some
    // entries removed. May remove empty child or merge prefixes if
    // it's an unnecessary trie node. Moderately expensive, because
    // we don't have an easy way to access a singleton map.
    let private updChild (ix:byte) (p:Prefix) (c:Dict) (cs:Children) : Children =
        if Option.isSome (c.pd) || Option.isSome (c.vu) then
            Map.add ix (struct(p,c)) cs // add non-empty child entry
        else if Map.isEmpty (c.cs) then
            Map.remove ix cs // empty child node is removed
        else 
            // check for unnecessary node split point
            match tryMapSingleton (c.cs) with
            | None -> Map.add ix (struct(p,c)) cs // child is split point
            | Some (ix',struct(p',c')) -> // merge radix tree prefixes
                // no recursion; assume c' valid by prior construction
                Map.add ix (struct((joinBytes p ix' p'), c')) cs

    /// Empty dictionary. This should only exist at the tree root.
    let empty : Dict = 
        { pd = None; vu = None; cs = Map.empty; ct = 0u; sz = 0u }

    /// Test for obviously empty dictionary - no entries. This does
    /// not recognize whether a dictionary is empty due to pending
    /// deletions. For that, favor isEmpty'.
    let isEmpty (d:Dict) : bool =
        (Map.isEmpty (d.cs) && Option.isNone (d.vu) && Option.isNone (d.pd))

    /// Create a dictionary from an initial directory (without loading)
    let fromProto (dir:Dir) : Dict =
        match dir with
        | None -> empty
        | Some _ -> { pd = Some dir; vu = None; cs = Map.empty; ct = 1u; sz = 67u }

    /// Prepend a common prefix to every symbol in a dictionary. 
    /// O(1) via trie structure. 
    let prependPrefix (p:Prefix) (d:Dict) : Dict =
        if BS.isEmpty p then d else
        let cs' = updChild (BS.unsafeHead p) (BS.unsafeTail p) d (Map.empty)
        mkDict None None cs'

    /// Load a directory from Stowage as a Dict.
    ///
    /// This may temporarily cache the Dict in memory.
    let load (dir:Dir) : Dict =
        match dir with
        | None -> empty
        | Some ref -> LVRef.load ref

    /// Load without caching.
    let load' (dir:Dir) : Dict =
        match dir with
        | None -> empty
        | Some ref -> LVRef.load' ref

    /// Find the most recent update for a symbol. Returns None if
    /// there are no relevant entries.
    let rec tryFindUpd (k:Symbol) (d:Dict) : DefUpd option =
        if BS.isEmpty k then findWithFallback k (d.pd) (d.vu) else
        match Map.tryFind (BS.unsafeHead k) (d.cs) with
        | Some (struct(p,c)) when isPrefix p (BS.unsafeTail k) ->
            let k' = BS.drop (1 + BS.length p) k
            findWithFallback k (d.pd) (tryFindUpd k' c)
        | _ -> None
    and private findWithFallback k pd v =
        if Option.isSome v then v else
        match pd with
        | None -> None
        | Some dir -> // found matching directory
            match tryFindUpd k (load dir) with
            | None -> Some None // logically deleted
            | result -> result

    /// Find the definition of a given symbol.
    let tryFind (k:Symbol) (d:Dict) : Def option =
        match tryFindUpd k d with
        | Some result -> result // explicit update or deletion
        | None -> None // no entry implies no definition

    // returns directory for longest matching prefix and bytes matched
    let rec private matchDir' (k:Symbol) (d:Dict) : struct(int * Dir option) =
        if BS.isEmpty k then struct(0, d.pd) else
        match Map.tryFind (BS.unsafeHead k) (d.cs) with
        | Some (struct(p,c)) when isPrefix p (BS.unsafeTail k) ->
            let len = 1 + BS.length p
            let struct(len',pd) = matchDir' (BS.drop len k) c
            if Option.isNone pd then struct(0, d.pd) else
            struct((len + len'),pd) // longer prefix matched!
        | _ -> struct(0, d.pd)

    /// Find directory with longest matching prefix for a symbol.
    let matchDirectory (k:Symbol) (dict:Dict) : (struct(Prefix * Dir)) option =
        let struct(plen,pd) = matchDir' k dict 
        match pd with
        | Some dir -> Some (struct((BS.take plen k),dir))
        | None -> None

    // Rewrite a Dict node via function at given symbol, potential erasure.
    let rec private rewriteAtKey (k:Symbol) (rw:Dict -> Dict) (d:Dict) : Dict =
        if BS.isEmpty k then rw d else
        let ix = BS.unsafeHead k
        let krem = BS.unsafeTail k
        match Map.tryFind ix (d.cs) with
        | None -> 
            let cs' = updChild ix krem (rw empty) (d.cs)
            mkDict (d.pd) (d.vu) cs'
        | Some (struct(p,c)) ->
            let n = bytesShared p krem
            if (n = BS.length p) then
                // child c is within path of rewrite
                let c' = rewriteAtKey (BS.drop n krem) rw c
                let cs' = updChild ix p c' (d.cs)
                mkDict (d.pd) (d.vu) cs'
            else // create a split prefix node at given offset
                assert(n < (BS.length p))
                let pShared = BS.take n p
                let ixSplit = p.[n]
                let pRem = BS.drop (n+1) p
                let csSplit = Map.add ixSplit (struct(pRem,c)) (Map.empty)
                let cSplit = mkDict None None csSplit
                let cSplit' = rewriteAtKey (BS.drop n krem) rw cSplit
                let cs' = updChild ix pShared cSplit' (d.cs)
                mkDict (d.pd) (d.vu) cs'

    // check for potential remote entries 
    let private hasRE k d =
        let struct(_,pd) = matchDir' k d
        match pd with
        | Some (Some _) -> true
        | _ -> false

    /// Update definition for a symbol. May try to remove `~symbol`
    /// entries that are obviously unnecessary, but does not load
    /// remote Stowage nodes.
    let updSym (k:Symbol) (du:DefUpd) (dict:Dict) : Dict =
        let bFullDel = Option.isNone du && not (hasRE k dict)
        let vu = if bFullDel then None else Some du
        let rw d = mkDict (d.pd) vu (d.cs)
        rewriteAtKey k rw dict 

    let inline add sym def d = updSym sym (Some def) d
    let inline remove sym d = updSym sym None d

    /// Update directory node at a given prefix. This will overwrite
    /// all existing entries with the same prefix.
    let updPrefix (p:Prefix) (dir:Dir) (dict:Dict) : Dict =
        if BS.isEmpty p then fromProto dir else
        let bFullDel = Option.isNone dir 
                    && not (hasRE (BS.dropLast 1 p) dict)
        let pdu = if bFullDel then None else Some dir
        let rw _ = mkDict pdu None (Map.empty)
        rewriteAtKey p rw dict

    /// Remove all symbols with given prefix from the dictionary.
    let inline dropPrefix (p:Prefix) (d:Dict) : Dict = updPrefix p None d



    // TODO: select prefix, drop prefix


    // Updates:
    //  For deletion of a prefix or symbol, we can drop entries if
    //  they would have been obviously absent. (Testing whether we
    //  have an entry before deletion should be explicit.)

    
    /// 
    /// In this case, we might try to avoid
    //let rec updSym (k:Symbol) (du:DefUpd) (d:Dict) : Dict =





