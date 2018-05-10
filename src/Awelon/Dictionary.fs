namespace Awelon
open System.Collections.Generic
open Stowage
open Data.ByteString

// Awelon language has a simple standard dictionary definition, based
// on a log-structured merge-tree with radix-tree indexing (LSM-trie).
// The design is intended for legible import/export with distributed,
// prefix-oriented sharing and synchronization.
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
// by their secure hash, but empty string may drop a prefix. A matched
// prefix is stripped from the referenced node. Only the last update for
// a symbol or matching prefix is used. Empty prefix is valid and useful
// for prototype inheritance, overflow buffers, or checkpoints. 
//
// This representation relies on Awelon symbols and definitions not
// using arbitrary bytes. In particular, we assume SP is not used in
// symbols, and LF is not used in definitions or symbols.
//
// Hierarchical dictionaries simply use `dictname/word` symbols. We
// can use `/dictname/ secureHash` to logically include one dictionary
// into another.
//
// For many problems, we'll also want 
module Dict =

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

    /// A dictionary node, indexed and normalized for efficient 
    /// lookups. Assumes addend-only for updates to dictionary.
    ///  
    /// Definitions are encoded using an option type for deletion:
    ///
    ///  Some def -> :symbol definition  
    ///  None     -> ~symbol
    ///
    /// Directories are encoded using an option type for empty node:
    ///
    ///  Some dir -> /prefix secureHash
    ///  None     -> /prefix
    ///
    /// This directory structure is encoded using VRef to simplify
    /// GC management, but we require the standard dictionary codec.
    [<Struct>]
    type Dict = 
        { dirs : CritbitTree<Dir>
          defs : CritbitTree<Def option>
        }
    and Dir = VRef<Dict> option

    /// The DictEnt type corresponds to a single line in a dictionary,
    /// a single update to a symbol or prefix. Delete is represented
    /// here by defining a symbol to the `None` value.
    type DictEnt =
        | Direct of Prefix * Dir
        | Define of Symbol * (Def option)

    /// Empty dictionary, corresponding to empty bytestring.
    let empty : Dict = 
        { dirs = CritbitTree.empty
          defs = CritbitTree.empty
        }

    /// Test for obviously empty dictionary - no entries.
    /// Note: see also `isEmpty'` to test for no definitions.
    let isEmpty (dict:Dict) : bool =
        CritbitTree.isEmpty (dict.defs)
            && CritbitTree.isEmpty (dict.dirs)

    /// The empty prefix entry `/ secureHashRef` essentially models a
    /// prototype inheritance, a fallback for all definitions. We can
    /// construct a dictionary from a directory reference, without 
    /// loading it.
    let fromProto (dir:Dir) : Dict = 
        match dir with
        | None -> empty
        | _ -> 
            { dirs = CritbitTree.singleton (BS.empty) dir
              defs = CritbitTree.empty
            }

    // obtain leftmost value of a CritbitTree node
    let rec private leftValN node =
        match node with
        | CritbitTree.Inner(_,l,_,_) -> leftValN l
        | CritbitTree.Leaf v -> v

    let inline private isPrefix p s = (p = (BS.take (BS.length p) s))

    // Any prefix is certainly in the search path because bits in the
    // prefix of a symbol will match up to prefix length. But not all
    // keys in the search path are valid prefixes, since we don't look
    // at every bit. So just test from most matched critbits to fewest.
    let rec private matchDirN sym kl node =
        match node with
        | CritbitTree.Inner(cb,l,kr,r) ->
            if CritbitTree.testCritbit cb sym then
                // test for larger match in right. 
                let inR = matchDirN sym kr r
                if Option.isSome inR then inR else
                // otherwise, kl might be longest match.
                if isPrefix kl sym 
                    then Some (struct(kl,leftValN l))
                    else None
            else matchDirN sym kl l // test kl in later recursion
        | CritbitTree.Leaf v ->
            if isPrefix kl sym
                then Some (struct(kl,v))
                else None

    let private matchDirT sym dirs =
        match dirs with
        | CritbitTree.Empty -> None
        | CritbitTree.Root(kl,node) -> matchDirN sym kl node

    /// Find directory with longest matching prefix for a symbol.
    let matchDirectory (sym:Symbol) (dict:Dict) : (struct(Prefix * Dir)) option =
        matchDirT sym (dict.dirs)

    // Test whether the directory containing `sym` is empty. 
    // (If so, we can drop entries that would erase `sym`.)
    let private isEmptyDirT sym dirs =
        match matchDirT sym dirs with
        | Some(struct(_,Some _)) -> false
        | _ -> true

    /// Update the definition for a given symbol.
    let updSym (sym:Symbol) (defOpt:Def option) (dict:Dict) : Dict =
        let erase = (Option.isNone defOpt) && (isEmptyDirT sym (dict.dirs))
        if erase
            then { dict with defs = CritbitTree.remove sym (dict.defs) }
            else { dict with defs = CritbitTree.add sym defOpt (dict.defs) }

    /// Update directory index at a given prefix.
    let updPrefix (p:Prefix) (dir:Dir) (dict:Dict) : Dict =
        // mask existing entries with the given prefix
        let defs' = CritbitTree.dropPrefix p (dict.defs)
        let dirs  = CritbitTree.dropPrefix p (dict.dirs)
        let noent = (Option.isNone dir) && (isEmptyDirT p dirs)
        let dirs' = if noent then dirs else CritbitTree.add p dir dirs
        { dirs = dirs'; defs = defs' }

    /// Modify dictionary by logically appending an entry.
    /// Useful for streaming construction of dictionaries.
    let applyEnt (dict:Dict) (upd:DictEnt) : Dict =
        match upd with
        | Direct (p,dir) -> updPrefix p dir dict
        | Define (sym,def) -> updSym sym def dict

    // obtain leftmost value AND remainder of CritbitTree node
    let rec private splitLeftValN (node:CritbitTree.Node<'V>) : struct('V * CritbitTree<'V>) =
        match node with
        | CritbitTree.Inner(cb,l,kr,r) ->
            let struct(v,rem) = splitLeftValN l
            let rem' = 
                match rem with
                | CritbitTree.Root(kl',l') ->
                    CritbitTree.Root(kl', CritbitTree.Inner(cb, l', kr, r))
                | CritbitTree.Empty -> CritbitTree.Root(kr,r)
            struct(v,rem')
        | CritbitTree.Leaf v -> struct(v,CritbitTree.Empty)

    let inline private stepDefEntL dict s defN =
        let struct(def,defs') = splitLeftValN defN
        let dict' = { dict with defs = defs' }
        struct(Define(s,def),dict')

    // Separate an entry from remainder of dictionary.
    let private stepEntL (dict:Dict) : (struct(DictEnt * Dict)) option =
        match dict.dirs with
        | CritbitTree.Root(p,dirsN) ->
            match dict.defs with
            | CritbitTree.Root(s,defN) when (s < p) ->
                Some(stepDefEntL dict s defN)
            | _ -> 
                let struct(dir,dirs') = splitLeftValN dirsN
                let dict' = { dict with dirs = dirs' }
                Some(struct(Direct (p,dir), dict'))
        | CritbitTree.Empty ->
            match dict.defs with
            | CritbitTree.Root(s,defN) -> 
                Some(stepDefEntL dict s defN)
            | CritbitTree.Empty -> 
                None

    /// Expand to sequence of entries, sorted such that prefix /p
    /// would appear just before :p, using the bytestring order.
    let rec toSeqEnt (dict:Dict) : seq<DictEnt> = seq {
        match stepEntL dict with
        | Some(struct(e,dict')) -> 
            yield e
            yield! toSeqEnt dict'
        | None -> ()
        }

    /// Apply a stream or sequence of entries to a dictionary.
    let inline applySeqEnt (d:Dict) (s:seq<DictEnt>) : Dict =
        Seq.fold applyEnt d s

    /// Compute dictionary from sequence of entries.
    let inline fromSeqEnt s = applySeqEnt empty s

    let inline private trimSP s =
        let isSP c = (c = cSP)
        s |> BS.dropWhile isSP |> BS.dropWhileEnd isSP

    /// Parse a single line from a dictionary. May raise ByteStream.ReadError.
    let parseDictEnt (mkDef:ByteString -> Def) (mkDir:RscHash -> VRef<Dict>) (ln:ByteString) : DictEnt =
        if BS.isEmpty ln then raise ByteStream.ReadError else
        let c0 = BS.unsafeHead ln
        let struct(sym,spdef) = BS.span ((<>) cSP) (BS.unsafeTail ln)
        let def = BS.drop 1 spdef // drop symbol-def separator
        if (c0 = cDef) then Define(sym, Some (mkDef def)) else
        let h = trimSP def // ignore whitespace for Del and Def lines.
        if ((c0 = cDel) && (BS.isEmpty h)) then Define (sym, None) else
        if ((c0 = cDir) && (BS.isEmpty h)) then Direct (sym, None) else
        if ((c0 = cDir) && (RscHash.isValidHash h)) then Direct (sym, Some (mkDir h)) else
        raise ByteStream.ReadError

    /// Parse entries in a dictionary string. May raise ByteStream.ReadError.
    let rec parseDictEnts mkDef mkDir (s:ByteString) : seq<DictEnt> =
        if BS.isEmpty s then Seq.empty else
        seq {
            let struct(line,lfmore) = BS.span ((<>) cLF) s
            yield parseDictEnt mkDef mkDir line
            yield! parseDictEnts mkDef mkDir (BS.drop 1 lfmore)
        }

    let inline parseDict mkDef mkDir s : Dict =
        fromSeqEnt (parseDictEnts mkDef mkDir s)

    let private entBytes (du:DictEnt) : struct(byte * ByteString * ByteString) =
        match du with
        | Direct (p, dirOpt) ->
            match dirOpt with
            | Some ref -> struct(cDir, p, ref.ID)
            | None -> struct(cDir, p, BS.empty)
        | Define (s, defOpt) ->
            match defOpt with
            | Some def -> struct(cDef, s, def.Data)
            | None -> struct(cDel, s, BS.empty)

    /// Write an entry. Assumes valid context (start of line).
    /// NOTE: Does not include LF to terminate/separate entry!
    let writeEnt (du:DictEnt) (dst:ByteDst) : unit =
        let struct(c,s,def) = entBytes du
        ByteStream.writeByte c dst
        ByteStream.writeBytes s dst
        if (not (BS.isEmpty def)) then
            ByteStream.writeByte cSP dst
            ByteStream.writeBytes def dst

    /// Size (in bytes) to write a dictionary entry. Exact.
    let sizeEnt (du:DictEnt) : SizeEst =
        let struct(_,s,def) = entBytes du
        let szSP = if BS.isEmpty def then 0UL else 1UL
        1UL + (uint64 (BS.length s)) + szSP + (uint64 (BS.length def)) 

    /// Write each entry. Adds an LF after each entry.
    let writeEnts (ents:seq<DictEnt>) (dst:ByteDst) : unit =
        let wfn du = writeEnt du dst; ByteStream.writeByte cLF dst
        Seq.iter wfn ents

    /// Precompute exact size to write entries.
    let sizeEnts (ents:seq<DictEnt>) : SizeEst =
        let accum n du = n + sizeEnt du + 1UL
        Seq.fold accum 0UL ents
    
    /// Serialize/Render a dictionary to a ByteString.
    let writeDict (dict:Dict) : ByteString =
        ByteStream.write (writeEnts (toSeqEnt dict))

    /// Size in bytes for serializing dictionary node. Exact.
    let sizeDict (dict:Dict) : SizeEst = 
        sizeEnts (toSeqEnt dict)

    // Shared dictionary parse cache. This permits structure sharing
    // in memory, albeit at the cost of extra load overheads.
    let private dictLoadCache = 
        let cm = Stowage.Cache.defaultManager
        // using constant time equality to resist time leaks of refs
        let eq = { new IEqualityComparer<ByteString> with
                    member __.GetHashCode s = ByteString.Hash32 s |> int
                    member __.Equals(x,y) = ByteString.CTEq x y
                 }
        new MCache<ByteString,Dict>(cm,eq)

    /// Load and cache a dictionary node. Uses Stowage.Cache. 
    let loadRef (ref:VRef<Dict>) : Dict =
        match MCache.tryFind (ref.ID) (dictLoadCache) with
        | Some d -> d
        | None ->
            let mkDef s = Def(s, (ref :> System.Object))
            let mkDir h = VRef.wrap (ref.Codec) (ref.DB) h
            let s = ref.DB.Load(ref.ID)
            let sz = 120UL + uint64 (BS.length s)
            let d = parseDict mkDef mkDir s
            System.GC.KeepAlive(ref)
            MCache.tryAdd (ref.ID) d sz (dictLoadCache)

    /// Load a directory as a dictionary.
    let load (dir:Dir) : Dict =
        match dir with
        | Some ref -> loadRef ref
        | None -> empty

    /// Find a definition for a symbol, if it exists. This will
    /// recursively load remote nodes as needed, caching them 
    /// for subsequent lookups.
    let rec tryFind (k:Symbol) (d:Dict) : Def option =
        match CritbitTree.tryFind k (d.defs) with
        | Some defOpt -> defOpt
        | None ->
            match matchDirT k (d.dirs) with
            | None -> None
            | Some (struct(p,dir)) ->
                let k' = BS.drop (BS.length p) k
                tryFind k' (load dir)


    // TODO: support review of historical definitions recorded in
    // the dictionary?

    let private prependPrefixT (p:Prefix) (t:CritbitTree<'A>) : CritbitTree<'A> =
        // CritbitTree doesn't support shortcuts here: no sharing of
        // prefixes. So we'll just need to add a prefix to every key.
        let addP k v t = CritbitTree.add (BS.append p k) v t
        CritbitTree.foldBack addP t (CritbitTree.empty)

    /// Prepend a prefix to every symbol in a dictionary.
    let prependPrefix (p:Prefix) (d:Dict) : Dict =
        if BS.isEmpty p then d else
        { dirs = prependPrefixT p (d.dirs)
          defs = prependPrefixT p (d.defs)
        }

    /// Remove all symbols with given prefix from the dictionary.
    let inline dropPrefix (p:Prefix) (d:Dict) : Dict = 
        updPrefix p None d

    // Concatenate dictionary entries optimized for cases where
    // dictionary `a` is either empty or probably larger than b.
    let inline private concatDictEnts' (a:Dict) (b:Dict) : Dict =
        if isEmpty a then b else 
        applySeqEnt a (toSeqEnt b)

    // Concatenate dictionaries at entry level, optimized for larger b.
    // That is, we'll iterate entries in `a` instead of `b`. This isn't
    // safe for the general use case due to how we erase some prefixes.
    // But it's useful when `a` is a directory extracted from `b`.
    let private concatDictEnts (a:Dict) (b:Dict) : Dict =
        if isEmpty b then a else
        let inline maskPrefix p = Option.isSome (matchDirT p (b.dirs))
        let inline maskSymbol s = CritbitTree.containsKey s (b.defs) || maskPrefix s
        let accumDir p dir t = if maskPrefix p then t else CritbitTree.add p dir t
        let accumDef s def t = if maskSymbol s then t else CritbitTree.add s def t
        let dirs' = CritbitTree.foldBack accumDir (a.dirs) (b.dirs)
        let defs' = CritbitTree.foldBack accumDef (a.defs) (b.defs)
        { dirs = dirs'; defs = defs' }

    let private extractPrefixT (p:Prefix) (t:CritbitTree<'A>) : CritbitTree<'A> =
        let remP k v t = CritbitTree.add (BS.drop (BS.length p) k) v t
        CritbitTree.foldBack remP (CritbitTree.selectPrefix p t) (CritbitTree.empty)

    /// Extract the dictionary at a given prefix, erasing said prefix.
    let rec extractPrefix (prefix:Prefix) (d:Dict) : Dict =
        if BS.isEmpty prefix then d else
        // the obvious items
        let dP = { dirs = extractPrefixT prefix (d.dirs)
                   defs = extractPrefixT prefix (d.defs)
                 }
        // search for items under a wider prefix such as `/pre` or `/`
        match matchDirT prefix (d.dirs) with
        | Some (struct(pre,Some ref)) when ((BS.length pre) < (BS.length prefix)) ->
            let fix = BS.drop (BS.length pre) prefix
            let ss = extractPrefix fix (loadRef ref)
            concatDictEnts ss dP
        | _ -> dP
        
    /// Select symbols matching a given prefix from dictionary.
    let inline selectPrefix (p:Prefix) (d:Dict) : Dict = 
        prependPrefix p (extractPrefix p d)

    // Split `/ secureHash` prototype from Dict remainder.
    let private splitProto (d:Dict) : struct(Dir * Dict) =
        match (d.dirs) with
        | CritbitTree.Root(e,n) when (BS.isEmpty e) ->
            let struct(dir,dirs') = splitLeftValN n
            struct(dir, { d with dirs = dirs' })
        | _ -> struct(None,d)

    /// Merge with prototype chain. Transitively eliminates
    /// the `/ secureHash` entry, preserving definitions.
    let rec mergeProto (d0:Dict) : Dict =
        let struct(p,d) = splitProto d0
        if Option.isNone p then d else
        mergeProto (concatDictEnts (load p) d)

    // Note: For now, I'm assuming the prototype chain is relatively
    // short, so we just merge it as needed. This assumption holds 
    // for the default codec compaction method. Consequently, I don't
    // try to find common ancestors or similar.

    /// Iterate through all defined symbols in order.
    let rec toSeq (d:Dict) : seq<(Symbol * Def)> = seq {
        match stepEntL d with
        | Some(struct(e,d')) ->
            match e with
            | Define(sym, Some def) -> 
                yield (sym,def)
                yield! toSeq d'
            | Direct(p, Some ref) ->
                let dp = prependPrefix p (loadRef ref)
                yield! toSeq (concatDictEnts dp d')
            | _ -> 
                yield! toSeq d'
        | None -> ()
        }

    // iteratively partition dictionary, accumulating directories
    // to left of the given key in 'l' and opening directories if
    // they (naively) might include elements from both partitions.
    let rec private splitAtKey' k l (d:Dict) =
        match (d.dirs) with
        | CritbitTree.Root(p, dirN) when (p < k) ->
            // prefix is either to left of key or includes key
            let struct(dir,dirs') = splitLeftValN dirN
            let dRem = { d with dirs = dirs' }
            if isPrefix p k then // prefix includes key
                // load directory into dictionary, retry split
                let dp = prependPrefix p (load dir)
                let d' = concatDictEnts dp dRem
                splitAtKey' k l d'
            else // record prefix to left of tree
                let l' = CritbitTree.add p dir l
                splitAtKey' k l' dRem
        | _ -> // remaining prefixes are >= to the key
            let struct(defsL,defsR) = CritbitTree.splitAtKey k (d.defs)
            let dl = { dirs = l; defs = defsL }
            let dr = { dirs = (d.dirs); defs = defsR }
            struct(dl,dr)

    /// Partition the dictionary on a symbol, such that all symbols
    /// smaller are to the left and symbols equal or greater are to
    /// the right. Use with toSeq for browsing. 
    let splitAtKey (k:Symbol) (d:Dict) : struct(Dict * Dict) =
        splitAtKey' k (CritbitTree.empty) d

    // efficiently select least prefix or symbol in dictionary
    type private IsDir = bool
    let private leastSym (d:Dict) : (struct(Symbol * IsDir)) option =
        match (d.dirs) with
        | CritbitTree.Root(p,_) ->
            match (d.defs) with
            | CritbitTree.Root(s,_) when (s < p) -> Some(struct(s,false))
            | _ -> Some(struct(p,true))
        | CritbitTree.Empty ->
            match (d.defs) with
            | CritbitTree.Root(s,_) -> Some(struct(s,false))
            | CritbitTree.Empty -> None

    // select least prefix or symbol from two leastSym results
    let private minSym onA onB =
        match onA with
        | None -> onB
        | Some(struct(sa,ba)) ->
            match onB with
            | None -> onA
            | Some(struct(sb,_)) ->
                let cmp = compare sa sb
                if (cmp < 0) then onA else
                if (cmp > 0) then onB else
                if ba then onA else onB // prefixes before symbols

    // translate options to VDiff
    let private diffOpt optA optB =
        match optA, optB with
        | Some a, Some b ->
            if (a = b) then None else
            Some (InB (a,b))
        | Some a, None -> Some (InL a)
        | None, Some b -> Some (InR b)
        | None, None -> None

    /// Compute an efficient difference of two dictionaries.
    ///
    /// The efficiency goal is to avoid loading of nodes that
    /// share the same secure hash. But upon override we will
    /// need to load some nodes regardless.
    let diff (a0:Dict) (b0:Dict) : seq<Symbol * VDiff<Def>> =
        // holding a0, b0 for origin lookups involving overrides.
        //
        // The implementation at the moment has redundant case analysis
        // computations to simplify the code. This is mitigated by these
        // two step functions, which specialize access to least symbols.
        let inline stepDef (origin:Dict) (sym:Symbol) (local:Dict) : struct(Def option * Dict) =
            match (local.defs) with
            | CritbitTree.Root(s,defN) when (s = sym) -> 
                let struct(def,defs') = splitLeftValN defN
                struct(def, { local with defs = defs' })
            | _ -> struct(tryFind sym origin, local)

        let inline stepDir (origin:Dict) (prefix:Prefix) (local:Dict) : struct(Dir * Dict) =
            match (local.dirs) with
            | CritbitTree.Root(p,dirN) when (p = prefix) ->    
                let struct(dir,dirs') = splitLeftValN dirN
                struct(dir, { local with dirs = dirs' })
            | _ -> 
                let struct(dir,upd) = splitProto (extractPrefix prefix origin)
                struct(dir, concatDictEnts (prependPrefix prefix upd) local)

        let rec diffLoop a b = seq {
            // a,b serve as iterators and accumulators
            match minSym (leastSym a) (leastSym b) with
            | None -> ()
            | Some(struct(s,isDir)) -> 
                if not isDir then
                    let struct(defA, a') = stepDef a0 s a
                    let struct(defB, b') = stepDef b0 s b
                    match diffOpt defA defB with
                    | Some vdiff -> yield (s, vdiff)
                    | None -> ()
                    yield! diffLoop a' b'
                else
                    let struct(dirA, a') = stepDir a0 s a
                    let struct(dirB, b') = stepDir b0 s b
                    // short-circuit if prefixes refer to same node
                    if (dirA = dirB) then yield! diffLoop a' b' else
                    // otherwise, merge the two prefixes and continue
                    let inline ld dir = prependPrefix s (mergeProto (load dir))
                    let inline m dir d0 = concatDictEnts (ld dir) d0
                    yield! diffLoop (m dirA a') (m dirB b')
            }
        diffLoop a0 b0


    // If the node is large enough, flush updates to child nodes.
    // This is achieved by extracting and compacting each prefix
    // independently. The codec provides the strategy for how to
    // merge or chain updates. We do assume that compaction will
    // control maximum size at the prefix (e.g. after compaction,
    // a node shouldn't require more than 2kB to serialize).
    let private nodeFlush (thresh:SizeEst) (c:Codec<Dict>) (db:Stowage) (struct(d0,sz0)) =
        if (sz0 < thresh) then struct(d0,sz0) else
        let step (ix:byte) (d:Dict) : Dict = 
            let p = BS.singleton ix
            let dp = Codec.compact c db (extractPrefix p d)
            concatDictEnts' (dropPrefix p d) (prependPrefix p dp)
        let rec loop ix d =
            let d' = step ix d
            if (ix = (System.Byte.MaxValue)) then d' else
            loop (ix + 1uy) d'
        let df = loop (System.Byte.MinValue) (mergeProto d0)
        struct(df, sizeDict df)

    // push oversized nodes to stowage via `/ secureHash`
    let private nodeStow (thresh:SizeEst) (c:Codec<Dict>) (db:Stowage) (struct(d0,sz0)) =
        if (sz0 < thresh) then struct(d0,sz0) else
        let d = fromProto (Some (VRef.stow c db d0))
        struct(d, sizeDict d)


    /// LSM-trie based codec for Dictionary nodes.
    /// 
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
                let d0 = concatDictEnts' (load prior) upd
                (struct(d0, sizeDict d0))
                    |> nodeFlush updBuff c db 
                    |> nodeStow nodeMin c db 
        }

    // Note: compaction by node_codec doesn't leverage prototype 
    // chains via `/ secureHash`. Such chains could extend node 
    // lifespan and structure sharing, albeit at expense to cost
    // of lookups. But I lack simple, efficient heuristics for
    // deciding when to use chaining.

    // Limit reference overheads to about 4% (~1.6kB)
    let private defaultNodeMin = 25UL * uint64 (RscHash.size)

    // Flush nodes if they're larger than 30 small nodes (~48kB).
    let private defaultUpdBuff = 30UL * defaultNodeMin

    /// Node codec with default LSM-trie compaction thresholds. 
    let node_codec = node_codec_config defaultNodeMin defaultUpdBuff

    /// Node codec with Trie compactions: no update buffering.
    let node_codec' = node_codec_config defaultNodeMin 0UL

    /// The root codec is for the "root" Dict value.
    ///
    /// Critically, this prefixes the Dict value with size information
    /// so we can use the Dict within other Stowage data structures. 
    /// For contrast, node_codec will always consume the remainder of
    /// the byte stream when reading or writing.
    let inline root_codec_config (nc:Codec<Dict>) = EncSized.codec nc

    /// Root Dict value codec with normal LSM-trie buffering.
    let root_codec = root_codec_config node_codec

    /// Root Dict value codec with Trie-style compactions.
    let root_codec' = root_codec_config node_codec'

    // TODO: potential extensions
    //
    // - dictionary unions and intersections
    // - dictionary browsing heuristics (prefix selection?)
    // - reverse lookup indexing on dictionaries
    // - three-way diffs?
    // - cached evaluations and dictionaries

type Dict = Dict.Dict


