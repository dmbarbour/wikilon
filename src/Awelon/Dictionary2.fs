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
        let addDep l h = ((mkDep h)::l)
        let lDeps = RscHash.foldHashDeps addDep (List.empty) s
        Def(s, lDeps :> System.Object)

    /// Rewrite local definition to `$secureHash` remote redirect.
    let externDef (db:Stowage) (def:Def) : Def =
        let ref = VRef.stow (EncBytesRaw.codec) db (def.Data)
        def.KeepAlive()
        Def(BS.cons (byte '$') (ref.ID), (ref :> System.Object))

    // A definition update: None for delete, Some for define.
    type DefUpd = Def option 

    /// A dictionary node in-memory is represented as a trie with
    /// injected references to remote nodes (for /prefix entries).
    type Dict = 
        { pd : Dir option     // optional empty prefix entry
          vu : DefUpd option  // optional empty symbol entry
          cs : Children       // nodes with larger prefixes
        }
    and Children = Map<byte,struct(Prefix * Dict)>
    and Dir = LVRef<Dict> option  // secureHash (Some) or blank (None)

        // TODO: consider keeping a size-estimate per Dict node to
        // help with compaction algorithms. Whether this is worthwhile
        // will depend on the compaction algorithm.

    /// The DictEnt type corresponds to a single line in a dictionary,
    /// a single update to a symbol or prefix. 
    type DictEnt =
        | Direct of Prefix * Dir
        | Define of Symbol * DefUpd

    let private joinBytes (a:ByteString) (b:byte) (c:ByteString) : ByteString =
        let mem = Array.zeroCreate (a.Length + 1 + c.Length)
        do Array.blit a.UnsafeArray a.Offset mem 0 a.Length
        do mem.[a.Length] <- b
        do Array.blit c.UnsafeArray c.Offset mem (1 + a.Length) c.Length
        BS.unsafeCreateA mem

    let inline private isPrefix p s = (p = (BS.take (BS.length p) s))

    // compute size of shared prefix for two strings.
    let private bytesShared (a:ByteString) (b:ByteString) : int =
        let limit = min (a.Length) (b.Length)
        let rec loop ix =
            if ((ix = limit) || (a.[ix] <> b.[ix])) then ix else
            loop (ix + 1)
        loop 0

    // adds contextual prefix to compute entries in dictionary
    let rec private toSeqEntP (p:Prefix) (d:Dict) : seq<DictEnt> =
        let spd = 
            match d.pd with
            | None -> Seq.empty
            | Some dir -> Seq.singleton (Direct(p,dir))
        let svu =
            match d.vu with
            | None -> Seq.empty
            | Some du -> Seq.singleton (Define(p,du))
        let sc (ix,struct(p',c)) = toSeqEntP (joinBytes p ix p') c
        let scs = Seq.concat (Seq.map sc (Map.toSeq (d.cs)))
        Seq.append spd (Seq.append svu scs)
    
    /// Translate a dictionary to a sequence of local entries. This
    /// has a moderate overhead to reconstruct symbols and prefixes.
    let toSeqEnt (d:Dict) : seq<DictEnt> = toSeqEntP (BS.empty) d

    let inline mkDict pd vu cs = { pd = pd; vu = vu; cs = cs }

    // TODO: consider developing a mutable DictBuilder variant for efficient
    // parsing. This could improve read performance by a moderate degree. 
    // OTOH, the benefits might not be worth the complexity.

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

    /// Prepend a common prefix to every symbol in a dictionary. 
    /// O(1) via trie structure. 
    let prependPrefix (p:Prefix) (d:Dict) : Dict =
        if BS.isEmpty p then d else
        let cs' = updChild (BS.unsafeHead p) (BS.unsafeTail p) d (Map.empty)
        mkDict None None cs'

    // prepend prefix specialized for known child nodes
    let inline private prependChildPrefix (p:Prefix) (c:Dict) : Dict =
        if BS.isEmpty p then c else
        let cs' = Map.add (BS.unsafeHead p) (struct((BS.unsafeTail p), c)) (Map.empty)
        mkDict None None cs'

    /// Empty dictionary. This should only exist at the tree root.
    let empty : Dict = mkDict None None (Map.empty)

    /// Test for obviously empty dictionary - no entries. This does
    /// not recognize whether a dictionary is empty due to pending
    /// deletions. For that, favor isEmpty'.
    let isEmpty (d:Dict) : bool =
        (Map.isEmpty (d.cs) && Option.isNone (d.vu) && Option.isNone (d.pd))

    /// Create a dictionary from an initial directory (without loading)
    let fromProto (dir:Dir) : Dict =
        match dir with
        | None -> empty
        | Some _ -> mkDict (Some dir) None (Map.empty)

    // Split empty prefix entry from given dictionary.
    let private splitProto (d:Dict) : struct(Dir * Dict) =
        match (d.pd) with
        | Some dir -> struct(dir, { d with pd = None })
        | None -> struct(None, d)

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

    // returns longest matching prefix entry and prefix bytes matched
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
    /// For example, 'prefix' may return /pref or /pre, but would
    /// favor returning the entry at /pref. 
    let matchDirectory (k:Symbol) (dict:Dict) : (struct(Prefix * Dir)) option =
        let struct(plen,pd) = matchDir' k dict 
        match pd with
        | Some dir -> Some (struct((BS.take plen k),dir))
        | None -> None

    // search for symbol's definition update in local memory
    let rec private tryFindLocal (k:Symbol) (d:Dict) : DefUpd option =
        if BS.isEmpty k then (d.vu) else
        match Map.tryFind (BS.unsafeHead k) (d.cs) with
        | Some (struct(p,c)) when isPrefix p (BS.unsafeTail k) ->
            tryFindLocal (BS.drop (1 + BS.length p) k) c
        | _ -> None

    /// Find the definition of a given symbol, if any.
    let rec tryFind (k:Symbol) (d:Dict) : Def option =
        match tryFindLocal k d with
        | Some du -> du // definition or deletion found locally
        | None -> // look in directory of longest matching prefix 
            let struct(plen,pdir) = matchDir' k d
            match pdir with
            | Some (Some ref) -> tryFind (BS.drop plen k) (LVRef.load ref)
            | _ -> None // symbol is undefined

    /// Test whether dictionary contains a specified symbol.
    let inline contains (k:Symbol) (d:Dict) : bool = 
        Option.isSome (tryFind k d)

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
            let pc = prependChildPrefix (BS.drop n p) c
            let pc' = rewriteAtKey (BS.drop n krem) rw pc
            let cs' = updChild ix (BS.take n p) pc' (d.cs)
            mkDict (d.pd) (d.vu) cs'

    // check for potential remote entries 
    let private hasRemote k d =
        let struct(_,pd) = matchDir' k d
        match pd with
        | Some (Some _) -> true
        | _ -> false

    /// Update definition for a symbol. May try to remove `~symbol`
    /// entries that are obviously unnecessary, but does not load
    /// remote Stowage nodes.
    let updSym (k:Symbol) (du:DefUpd) (dict:Dict) : Dict =
        let bFullDel = Option.isNone du && not (hasRemote k dict)
        let vu = if bFullDel then None else Some du
        let rw d = mkDict (d.pd) vu (d.cs)
        rewriteAtKey k rw dict 

    let inline add sym def d = updSym sym (Some def) d
    let inline remove sym d = updSym sym None d

    /// Update directory node at a given prefix. This will overwrite
    /// all existing entries with the same prefix. Will avoid adding
    /// a locally obviously unnecessary empty `/prefix` entry.
    let updPrefix (p:Prefix) (dir:Dir) (dict:Dict) : Dict =
        if BS.isEmpty p then fromProto dir else
        let bFullDel = Option.isNone dir
                    && not (hasRemote (BS.dropLast 1 p) dict)
        let pdu = if bFullDel then None else Some dir
        let rw _ = mkDict pdu None (Map.empty)
        rewriteAtKey p rw dict

    /// Remove all symbols with given prefix from the dictionary.
    let inline dropPrefix (p:Prefix) (d:Dict) : Dict = updPrefix p None d

    /// Modify dictionary by logically appending an entry.
    /// Useful for streaming construction of dictionaries.
    let applyEnt (dict:Dict) (upd:DictEnt) : Dict =
        match upd with
        | Direct (p,dir) -> updPrefix p dir dict
        | Define (sym,def) -> updSym sym def dict

    /// Apply a stream or sequence of entries to a dictionary.
    let inline applySeqEnt (d:Dict) (s:seq<DictEnt>) : Dict =
        Seq.fold applyEnt d s

    /// Compute dictionary from sequence of entries.
    let inline fromSeqEnt s = applySeqEnt empty s

    // here `hr` tracks the `has remote entries` context from the parent.
    // When false, we can often eliminate deletion entries from `b`.
    let rec private flushUpdates' (hr:bool) (a:Dict) (b:Dict) : Dict =
        match b.pd with 
        | Some dir -> 
            if hr || Option.isSome dir then b else
            mkDict None (b.vu) (b.cs) 
        | None ->
            let hr' =
                match a.pd with
                | None -> hr
                | Some dir -> Option.isSome dir
            let vu' = 
                match b.vu with
                | None -> a.vu
                | Some du -> if hr' || Option.isSome du then b.vu else None
            let fcu = if hr' then flushChildUpdT else flushChildUpdF
            let cs' = Map.fold fcu (a.cs) (b.cs)
            mkDict (a.pd) vu' cs'
        // specializations to resist runtime closure allocation
    and private flushChildUpdT acs ix pc = flushChildUpd true acs ix pc
    and private flushChildUpdF acs ix pc = flushChildUpd false acs ix pc
    and private flushChildUpd hr acs ix (struct(bp,bc)) =
        match Map.tryFind ix acs with
        | None -> // may need to erase deletion entries from bc
            let bc' = if hr then bc else flushUpdates' false empty bc
            updChild ix bp bc' acs
        | Some (struct(ap,ac)) -> // align prefixes then merge
            let n = bytesShared ap bp
            let apc = prependChildPrefix (BS.drop n ap) ac
            let bpc = prependChildPrefix (BS.drop n bp) bc
            let ac' = flushUpdates' hr apc bpc
            updChild ix (BS.take n ap) ac' acs
        
    /// specialized equivalent to `applySeqEnt a (toSeqEnt b)`.
    /// This is an entry level append, not a union of definitions.
    let flushUpdates (a:Dict) (b:Dict) : Dict = flushUpdates' false a b

    /// Merge with prototype chain. Transitively eliminates the empty
    /// prefix entry `/ secureHash` while preserving definitions. In
    /// normal use cases, our prototype chain should be very short.
    let rec mergeProto (d0:Dict) : Dict =
        let struct(proto,dLocal) = splitProto d0
        match proto with
        | None -> dLocal
        | Some ref -> mergeProto (flushUpdates (LVRef.load' ref) dLocal)

    // Extract local dictionary entries at specified prefix.
    let rec private extractPrefixLocal (p:Prefix) (d:Dict) : Dict =
        if BS.isEmpty p then d else
        let ix = BS.unsafeHead p
        match Map.tryFind (BS.unsafeHead p) (d.cs) with
        | Some (struct(p',c)) when isPrefix p' (BS.unsafeTail p) ->
            extractPrefixLocal (BS.drop (1 + BS.length p') p) c
        | _ -> empty

    /// Extract dictionary at specified prefix, erasing the prefix.
    let rec extractPrefix (p:Prefix) (d:Dict) : Dict =
        let dLocal = extractPrefixLocal p d
        let struct(l,pd) = matchDir' p d
        match pd with
        | Some (Some ref) when (l < BS.length p) ->
            let dRemote = extractPrefix (BS.drop l p) (LVRef.load' ref)
            flushUpdates dRemote dLocal
        | _ -> dLocal

    /// Select symbols matching a given prefix from dictionary.
    let inline selectPrefix (p:Prefix) (d:Dict) : Dict = 
        prependPrefix p (extractPrefix p d)

    // sequence definitions in context of prefix
    let rec private toSeqP (p:Prefix) (d0:Dict) : seq<Symbol * Def> =
        let d = mergeProto d0 
        let sv =
            match (d.vu) with
            | Some (Some def) -> Seq.singleton (p,def)
            | _ -> Seq.empty
        let seqChild (ix,struct(p',c)) = toSeqP (joinBytes p ix p') c
        let scs = Seq.concat (Seq.map seqChild (Map.toSeq (d.cs)))
        Seq.append sv scs
        
    /// Compute a sequence of defined symbols. This will perform
    /// erasures and updates incrementally, as needed.
    let toSeq (d:Dict) : seq<Symbol * Def> = toSeqP (BS.empty) d

    /// Partition the dictionary on a symbol, such that all symbols
    /// smaller are to the left and symbols equal or greater are to
    /// the right. Use with toSeq for indexed browsing.
    let rec splitAtKey (k:Symbol) (d0:Dict) : struct(Dict * Dict) =
        if BS.isEmpty k then struct(empty,d0) else
        let d = mergeProto d0 // merge prefixes in path of key
        let ix = BS.unsafeHead k
        let lcs = Map.filter (fun k _ -> (k < ix)) (d.cs)
        let rcs = Map.filter (fun k _ -> (ix < k)) (d.cs)
        let struct(lcs',rcs') = // split and include ix
            match Map.tryFind ix (d.cs) with
            | None -> struct(lcs,rcs)
            | Some (struct(p,c)) ->
                let krem = BS.unsafeTail k
                let n = bytesShared p krem
                if (n = BS.length p) then
                    let struct(lc,rc) = splitAtKey (BS.drop n krem) c
                    struct(updChild ix p lc lcs, updChild ix p rc rcs)
                else if ((n < BS.length krem) && (p.[n] < krem.[n])) 
                    then struct(Map.add ix (struct(p,c)) lcs, rcs)
                    else struct(lcs, Map.add ix (struct(p,c)) rcs)
        struct(mkDict None (d.vu) lcs', mkDict None None rcs')


    // translate options to VDiff
    let private diffOpt optA optB =
        match optA, optB with
        | Some a, Some b ->
            if (a = b) then None else
            Some (InB (a,b))
        | Some a, None -> Some (InL a)
        | None, Some b -> Some (InR b)
        | None, None -> None

    // trivial sequence 0uy .. 255uy
    let private seqBytes : seq<byte> = 
        Seq.ofArray [|0uy..255uy|]
    
    // helper for diff
    let inline private getDefFB p d d0 =
        match d.vu with
        | Some du -> du
        | None -> tryFind p d0

    /// Compute an efficient difference of two dictionaries.
    ///
    /// The efficiency goal is to avoid iterating nodes when we have
    /// obvious reference equality. Of course, we might still need to
    /// read nodes beyond those we iterate to look up old definitions.
    /// This implementation assumes short `/ secureHash` chains, and 
    /// does not check for a latest common ancestor. 
    let diff (a0:Dict) (b0:Dict) : seq<Symbol * VDiff<Def>> =
        let rec diffP p a b =
            if System.Object.ReferenceEquals(a,b) then Seq.empty else
            if (a.pd = b.pd) then diffV p a b else
            diffV p (mergeProto a) (mergeProto b)
        and diffV p a b =
            // avoid lookup when no local difference
            if (a.vu = b.vu) then diffCS p a b else
            let adu = getDefFB p a a0
            let bdu = getDefFB p b b0
            let sv = 
                match diffOpt adu bdu with
                | None -> Seq.empty
                | Some vdiff -> Seq.singleton (p,vdiff)
            Seq.append sv (diffCS p a b)
        and diffCS p a b = // diff all possible indexes 
            Seq.concat (Seq.map (diffIX p (a.cs) (b.cs)) seqBytes)
        and diffIX p acs bcs ix = // diff specific index
            match Map.tryFind ix acs, Map.tryFind ix bcs with
            | None,None -> Seq.empty // no differences
            | None,Some(struct(bp,bc)) -> diffP (joinBytes p ix bp) empty bc
            | Some(struct(ap,ac)),None -> diffP (joinBytes p ix ap) ac empty
            | Some(struct(ap,ac)),Some(struct(bp,bc)) ->
                let n = bytesShared ap bp // prefix alignment
                let ac' = prependChildPrefix (BS.drop n ap) ac
                let bc' = prependChildPrefix (BS.drop n bp) bc
                diffP (joinBytes p ix (BS.take n ap)) ac' bc' 
        diffP (BS.empty) a0 b0


    // Functions for Writing a Dictionary

    /// Return two sizes for dictionary: (line count * byte count)
    /// based on what would be written to a dictionary node. Each
    /// line corresponds to one entry, and lines are as compact as
    /// feasible (avoiding unnecessary whitespace).
    let rec size (d:Dict) : struct(SizeEst * SizeEst) =
        let struct(lnp,szp) =
            // We should avoid forcing the lazy reference ID.
            match d.pd with
            | None -> struct(0UL,0UL)       // no line
            | Some None -> struct(1UL,2UL)  //  `/` LF
            | Some (Some _) -> struct(1UL,uint64 (3 + RscHash.size))
        let struct(lnv,szv) =
            match d.vu with
            | None -> struct(0UL,0UL)
            | Some None -> struct(1UL,2UL)  // `~` LF
            | Some (Some def) -> 
                let len = def.Data.Length
                if (0 = len) 
                    then struct(1UL,2UL)    // `:` LF
                    else struct(1UL,3UL + uint64 len) // `:` SP def LF
        let struct(lncs,szcs) = Map.fold sizeChild (struct(0UL,0UL)) (d.cs)
        struct((lnp+lnv+lncs),(szp+szv+szcs))
    and private sizeChild (struct(ln,sz)) ix (struct(p,c)) =
        let struct(lnc,szc) = size c
        // add child's prefix to every line in the child
        let szp = lnc * (1UL + uint64 (BS.length p)) 
        struct((ln+lnc),(sz+szc+szp))

    let private entBytes (ent:DictEnt) : struct(byte * ByteString * ByteString) =
        match ent with
        | Direct (p, dirOpt) ->
            match dirOpt with
            | Some ref -> struct(cDir, p, ref.ID)
            | None -> struct(cDir, p, BS.empty)
        | Define (s, defOpt) ->
            match defOpt with
            | Some def -> struct(cDef, s, def.Data)
            | None -> struct(cDel, s, BS.empty)

    // write entry, including LF
    let private writeEnt (dst:ByteDst) (e:DictEnt) : unit =
        let struct(c,s,d) = entBytes e
        ByteStream.writeByte c dst
        ByteStream.writeBytes s dst
        if (not (BS.isEmpty d)) then
            ByteStream.writeByte cSP dst
            ByteStream.writeBytes d dst
        ByteStream.writeByte cLF dst

    /// Write a dictionary node as a ByteString.
    let write (d:Dict) : ByteString = 
        let struct(_,sz) = size d
        let szMax = uint64 (System.Int32.MaxValue)
        if (sz > szMax) then raise (System.OutOfMemoryException()) else
        ByteStream.write (fun dst -> 
            ByteStream.reserve (int sz) dst 
            Seq.iter (writeEnt dst) (toSeqEnt d))


    let private isSP c = (c = cSP)
    let inline private trimSP s = 
        s |> BS.dropWhile isSP |> BS.dropWhileEnd isSP
    
    /// Parse a single line from a dictionary. May raise ByteStream.ReadError.
    let parseDictEnt mkDef mkDir ln =
        if BS.isEmpty ln then raise ByteStream.ReadError else
        let c0 = BS.unsafeHead ln
        let struct(sym,spdef) = BS.span ((<>) cSP) (BS.unsafeTail ln)
        let def = BS.drop 1 spdef // drop symbol-def separator
        if (c0 = cDef) then Define(sym, Some (mkDef def)) else
        let h = trimSP def // ignore whitespace for Del and Dir lines.
        if ((c0 = cDel) && (BS.isEmpty h)) then Define (sym, None) else
        if ((c0 = cDir) && (BS.isEmpty h)) then Direct (sym, None) else
        if ((c0 = cDir) && (RscHash.isValidHash h)) then Direct (sym, Some (LVRef.wrap (mkDir h))) else
        raise ByteStream.ReadError

    let private parseLineStep onLine s =
        if BS.isEmpty s then None else
        let struct(ln,more) = BS.span ((<>) cLF) s
        Some (onLine ln, BS.drop 1 more)

    /// Parse entries in a dictionary string. May raise ByteStream.ReadError.
    let parseDictEnts mkDef mkDir (s:ByteString) : seq<DictEnt> =
        Seq.unfold (parseLineStep (parseDictEnt mkDef mkDir)) s

    let inline parseDict mkDef mkDir s : Dict =
        fromSeqEnt (parseDictEnts mkDef mkDir s)

    

    // Compaction howto?
    // I can translate the prior compaction algorithm easily enough.
    // 
    (*
    /// Configurable codec for Dictionary nodes.
    ///
    /// Configuration options:
    ///
    /// - nodeMin: minimal size for node allocation
    /// - updBuff: per node data buffering size
    ///
    /// 
    /// After compaction, we can be certain that our node will be
    /// smaller than the allocation node size. But we'll flush only
    /// if 
    /// Nodes larger than nodeMin are rewritten to `/ secureHash`.
    /// Smaller nodes will be inlined. The updBuff determines the
    /// recursive use of compaction: if nodes are larger than the
    /// buffer, we create or rewrite child nodes to control size.
    /// This allows recent updates to aggregate near the root of
    /// the tree.
    ///
    /// For worst case nodes (or if updBuff is 0) we reduce to
    /// Trie based compaction behaviors, which also isn't bad.
    let node_codec_config (nodeMin:SizeEst) (updBuff:SizeEst) =
        { new Codec<Dict> with
            member __.Write d dst = 
                writeEnts (toSeqEnt d) dst
            member c.Read db src = 
                parseDict (autoDef db) (VRef.wrap c db) (ByteStream.readRem src)
            member c.Compact db d =
                let struct(prior,upd) = splitProto d
                if isEmpty upd then struct(d, sizeDict d) else
                let d0 = concatDictEnts (load prior) upd
                (struct(d0, sizeDict d0))
                    |> nodeFlush updBuff c db 
                    |> nodeStow nodeMin c db 
        }


    *)

