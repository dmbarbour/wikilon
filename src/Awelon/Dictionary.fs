namespace Awelon
open System.Collections.Generic
open Stowage
open Data.ByteString

// Awelon language has a simple standard dictionary definition, based
// on a log-structured merge-tree with radix-tree indexing. This is 
// intended to support import/export and distributed computing.
//
//      /prefix1 secureHash1
//      /prefix2 secureHash2
//      :symbol1 definition1
//      :symbol2 definition2
//      ~symbol3
// 
// A dictionary node is encoded in line-oriented ASCII text. Each line
// either defines (:) or deletes (~) a symbol, or directs to another 
// node based on a matching prefix (/). Definitions should be Awelon 
// code. Referenced nodes are identified by their secure hash or empty
// string to delete a prefix. The matched prefix is stripped from inner
// nodes. The empty prefix is valid, useful for prototype inheritance
// or checkpointing a stream. 
//
// This representation relies on Awelon symbols and definitions not
// using arbitrary bytes. In particular, we assume SP is not used in
// symbols, and LF is not used in definitions or symbols. Legibility
// is a goal, for easy debugging if not direct editing.
//
// Only the last update for a symbol or matching prefix is used. So we
// can normalize by erasing masked updates and sorting what remains.
// This module manages dictionary nodes in their normal form. Prefixes
// are listed first, but other symbols appear in lexicographic order.
//
// Hierarchical dictionaries simply use `dictname/word` symbols, and we
// can use `/dictname/ secureHash` to update a hierarchical dictionary.
//
// The main weakness of this representation is inefficient iteration
// and selection of lexicographic ranges.
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
    /// also provide a hidden slot to hold Stowage dependencies.
    ///
    /// Large definitions can incur repetitive overheads, so it is
    /// recommended to move large definitions into Stowage, using
    /// the `$secureHash` redirect. Binary resources should almost
    /// always be defined via `%secureHash` redirects.
    [<Struct>]
    type Def = 
        val Data : ByteString
        val private Deps : System.Object
        new(def,deps) = { Data = def; Deps = deps }
        new(def) = { Data = def; Deps = null }
        member x.KeepAlive() : unit = System.GC.KeepAlive (x.Deps)
        override x.GetHashCode() = x.Data.GetHashCode()
        override x.Equals(yobj) =
            match yobj with
            | :? Def as y -> (ByteString.Eq (x.Data) (y.Data))
            | _ -> false
 
    let isValidDefChar c = (cLF <> c)
    let isValidDefStr s = BS.forall isValidDefChar s
    let inline isValidDef (d:Def) = isValidDefStr (d.Data)

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
    /// construct a dictionary given the initial directory reference.
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

    /// Update the definition for a given symbol.
    let updSym (sym:Symbol) (defOpt:Def option) (dict:Dict) : Dict =
        let erase = Option.isNone defOpt
                 && Option.isNone (matchDirT sym (dict.dirs))
        if erase
            then { dict with defs = CritbitTree.remove sym (dict.defs) }
            else { dict with defs = CritbitTree.add sym defOpt (dict.defs) }
    
    let updPrefix (p:Prefix) (dir:Dir) (dict:Dict) : Dict =
        // mask existing entries with the given prefix
        let defs' = CritbitTree.dropPrefix p (dict.defs)
        let dirs  = CritbitTree.dropPrefix p (dict.dirs)
        // add new entry if necessary
        let noent = (Option.isNone dir) && (Option.isNone (matchDirT p dirs))
        let dirs' = if noent then dirs else CritbitTree.add p dir dirs
        { dirs = dirs'; defs = defs' }


    /// Modify dictionary by logically appending an entry.
    /// Useful for streaming construction of dictionaries.
    let applyEnt (dict:Dict) (upd:DictEnt) : Dict =
        match upd with
        | Direct (p,dir) -> updPrefix p dir dict
        | Define (sym,def) -> updSym sym def dict

    /// Expand to sequence of entries, directories before definitions.
    let toSeqEnt (dict:Dict) : seq<DictEnt> =
        let seqDirs = CritbitTree.toSeq (dict.dirs) |> Seq.map Direct
        let seqDefs = CritbitTree.toSeq (dict.defs) |> Seq.map Define
        Seq.append seqDirs seqDefs

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
        struct(defEnt (s,def),dict')

    // separate first lexicographic entry from remainder of dictionary,
    // favoring prefixes before symbols when they match. a utility for
    // sequencing of dictionaries
    let private stepEntL (dict:Dict) : (struct(DictEnt * Dict)) option =
        match dict.dirs with
        | CritbitTree.Root(p,dirsN) ->
            match dict.defs with
            | CritbitTree.Root(s,defN) when (s < p) ->
                Some(stepDefEntL dict s defN)
            | _ -> 
                let struct(dir,dirs') = splitLeftValN dirN
                let dict' = { dict with dirs = dirs' }
                Some(struct(Direct (p,dir), dict'))
        | CritbitTree.Empty ->
            match dict.defs with
            | CritbitTree.Root(s,defN) -> 
                Some(stepDefEntL dict s defN)
            | CritbitTree.Empty -> 
                None

    /// Expand to sequence of entries, sorted lexicographically.
    /// (Prefix /p should appear just before symbol :p or ~p.)
    let rec toSeqEnt' (dict:Dict) : seq<DictEnt> = seq {
        match stepEntL dict with
        | Some(struct(e,dict')) -> 
            yield e
            yield! toSeqEnt' dict'
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
        ByteStream.writeByte c
        ByteStream.writeBytes s
        if (not (BS.isEmpty def)) then
            ByteStream.writeByte cSP
            ByteStream.writeBytes def

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

    /// Load a dictionary using cache;.
    let loadRef (ref:VRef<Dict>) : Dict =
        match MCache.tryFind (ref.ID) (dictLoadCache) with
        | Some d -> d
        | None ->
            let mkDef s = Def(s, (ref :> System.Object))
            let mkDir h = VRef.wrap (ref.Codec) (ref.DB) h
            let s = ref.DB.Load(ref.ID)
            let sz = 120UL + uint64 (BS.length s)
            let d = parseDict mkDef mkDir s
            MCache.tryAdd (ref.ID) d sz (dictLoadCache)

    /// Load a dictionary node given a directory reference.
    ///
    /// Uses default Stowage.Cache. Loading a directory many times
    /// in a short period should return the same value in memory.
    let load (dir:Dir) : Dict =
        match dir with
        | None -> empty
        | Some ref -> loadRef ref

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

    let private addPrefixT (p:Prefix) (t:CritbitTree<'A>) : CritbitTree<'A> =
        // CritbitTree doesn't support shortcuts here: no sharing of
        // prefixes. So we'll just need to add a prefix to every key.
        let addP k v t = CritbitTree.add (BS.append p k) v t
        CritbitTree.foldBack addP t (CritbitTree.empty)

    /// Add a prefix to every symbol in a dictionary.
    let addPrefix (p:Prefix) (d:Dict) : Dict =
        if BS.isEmpty p then d else
        { dirs = addPrefixT p (d.dirs)
          defs = addPrefixT p (d.defs)
        }

    /// Remove all symbols with given prefix from the dictionary.
    let inline dropPrefix (p:Prefix) (d:Dict) : Dict = 
        updPrefix p None d

    // Concatenate dictionaries at entry level, optimized for larger b.
    let private concatDictEnts (a:Dict) (b:Dict) : Dict =
        let inline blockPrefix s = Option.isSome (matchDirT s (b.dirs))
        let inline blockSymbol s = CritbitTree.containsKey s (b.defs) || blockPrefix s
        let accumDir p dir t = if blockPrefix p then t else CritbitTree.add p dir t
        let accumDef s def t = if blockSymbol s then t else CritbitTree.add s def t
        let dirs' = CritbitTree.foldBack accumDir (a.dirs) (b.dirs)
        let defs' = CritbitTree.foldBack accumDef (a.defs) (b.defs)
        { dirs = dirs'; defs = defs' }

    let private removePrefixT (p:Prefix) (t:CritbitTree<'A>) : CritbitTree<'A> =
        let remP k v t = CritbitTree.add (BS.drop (BS.length p) k) v t
        CritbitTree.foldBack remP (CritbitTree.selectPrefix p t) (CritbitTree.empty)

    /// Filter for symbols matching a prefix then erase that prefix.
    let rec removePrefix (prefix:Prefix) (d:Dict) : Dict =
        if BS.isEmpty prefix then d else
        // the obvious items
        let dP = { dirs = removePrefixT prefix (d.dirs)
                   defs = removePrefixT prefix (d.defs)
                 }
        // search for items under a wider prefix such as `/pre` or `/`
        match matchDirT prefix (d.dirs) with
        | Some (struct(pre,Some ref)) when ((BS.length pre) < (BS.length prefix)) ->
            let fix = BS.drop (BS.length pre) prefix
            let ss = removePrefix fix (loadRef ref)
            concatDictEnts ss dP
        | _ -> dP
        
    /// Select symbols matching a given prefix from dictionary.
    let inline selectPrefix (p:Prefix) (d:Dict) : Dict = 
        addPrefix p (removePrefix p d)

    /// Eliminate empty prefix entry by transitively merging it.
    let rec mergeProto (dict:Dict) : Dict =
        match stepEntL d with
        | Some(struct(Direct(p,dir),d')) wh
        match (dict.dirs) with
        | CritbitTree.Root (e,dirsN) when (BS.isEmpty e) ->
            let struct(proto,dirs') = splitLeftValN dirsN
            let d0 = { dict with dirs = dirs' }
            let dProto = load proto
            flattenPrototype (concatDictEnts dProto d0)
        | _ -> dict


    /// Iterate through all defined symbols in lexicographic order.
    let rec toSeq (dict:Dict) : seq<(Symbol * Def)> = seq {
        match stepEntL dict with
        | Some(struct(e,dRem)) ->
            match e with
            | Define(sym,def) -> yield sym def
            | _ -> ()
            let dict' = 
                match e with
                | Direct(p,dir) -> 
                    let dDir = addPrefix p (load dir)
                    concatDictEnts dDir dRem
                | _ -> dRem
            yield! toSeq dict'
        | None -> ()
        }

    /// Test whether a dictionary has no definitions.
    ///
    /// This is more expensive than testing for 'no entries' because
    /// it must apply deletion entries. 
    let isEmpty' d = Seq.isEmpty (toSeqR d)

    // iteratively partition dictionary, accumulating directories
    // to left of the given key in 'l' and opening directories if
    // they (naively) might include elements from both partitions.
    let rec private splitAtKey' k l (dict : Dict) =
        match (d.dirs) with
        | CritbitTree.Root(dirP, dirN) when (dirP < k) ->
            // prefix is either to left of key or includes key
            let struct(dirV,dirs') = splitLeftValN dirN
            if isPrefix dirP k then // prefix includes key
                // load directory into dictionary, retry split
                let dDir = addPrefix dirP (load dirV)
                let dRem = { d with dirs = dirs' } 
                let d' = concatDictEnts dDir dRem
                splitAtKey' k l d'
            else // record prefix to left of tree
                let l' = CritbitTree.add dirP dirV l
                let d' = { d with dirs = dirs' }
                splitAtKey' k l' d'
        | _ -> // remaining prefixes are >= to the key
            let struct(defsL,defsR) = CritbitTree.splitAtKey k (d.defs)
            let dl = { dirs = l; defs = defsL }
            let dr = { dirs = (d.dirs); defs = defsR }
            struct(dl,dr)

    /// Partition the dictionary on a symbol, such that all symbols
    /// lexicographically smaller are to the left and symbols equal
    /// or greater are to the right. Use with toSeq for browsing.
    let splitAtKey (k:Symbol) (dict:Dict) : struct(Dict * Dict) =
        splitAtKey' k (CritbitTree.empty) dict


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
        let stepDef (origin:Dict) (sym:Symbol) (local:Dict) : struct(Def option * Dict) =
            match (local.defs) with
            | CritbitTree.Root(s,defN) where (s = sym) -> 
                let struct(def,defs') = splitLeastValN defN
                struct(def, { local with defs = defs' })
            | _ -> struct(tryFind sym origin, local)

        let stepDir (origin:Dict) (prefix:Prefix) (local:Dict) : struct(Dir * Dict) =
            match (local.dirs) with
            | CritbitTree.Root(p,dirN) where (p = prefix) ->    
                let struct(dir,dirs') = splitLeastValN dirN
                struct(dir, { local with dirs = dirs' })
            | _ -> 
                // access a /prefix subdirectory from origin
                match matchDirectory prefix origin with
                | Some (struct(pre,pdir)) when (BS.length pre < BS.length prefix) ->
                    let fix = BS.drop (BS.length pre) prefix
                    let dDir = removePrefix fix (load pdir)
                    let local' = concatDictEnts (addPrefix prefix dDir) local
                    match local'.dirs with // might have found /prefix
                    | CritbitTree.Root(p,dirN) where (p = prefix) ->
                        let struct(dir,dirs') = splitLeastValN dirN
                        struct(dir, {local' with dirs = dirs' })
                    | _ -> struct(None, local') // compare defs and subdirs
                | _ -> struct(None, local)

        let rec diffLoop a b = 
            // a,b serve as iterators and accumulators
            match minSym (leastSym a) (leastSym b) with
            | None -> Seq.empty
            | Some(struct(s,isDir)) -> seq {
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
                    // Note: for long chains with empty prefix, we should
                    // optimize by searching for common ancestor of dirA
                    // and dirB. But for now, I assume short chains.
                    if (dirA = dirB) then 
                        yield! diffLoop a' b' // optimize, avoid loads
                    else  // merge both directories
                        let inline ld dir = addPrefix s (mergeProto (load dir))
                        let inline m dir d0 = concatDictEnts (ld dir) d0
                        yield! diffLoop (m dirA a') (m dirB b')
                }
        diffLoop a0 b0

    // Compacting Large Dictionary Nodes
    //
    // To work with larger-than-memory dictionaries, and to benefit
    // from structure-sharing, we must compact dictionary nodes by 
    // flushing entries into stowage (binaries identified by secure
    // hash) then erasing redundant entries as needed. The prefix
    // indices are based on this. 
    //
    // For performance, nodes shouldn't be too small or too large,
    // long prefixes are favorable over short prefixes, and chains
    // of the empty prefix should be avoided. 
    //
    // I assume definitions aren't too large due to refactoring 
    // or use of `$secureHash` stowage references. 
    //
    // To pick prefixes for compaction, we can incrementally merge
    // symbols with a common prefix until a threshold is met, then
    // simply emit the candidate. In some cases, we might have some
    // overlapping prefixes, e.g. if `/pre` is large enough and `/p`
    // is also large enough after subtracting `/pre`.
    //
    // 
    // 
    // Anyhow, we can try to compute prefix 'weights' for a set of
    // definitions, and find common prefixes by merging candidates 
    // whose weight isn't at a node's size threshold. We might try
    // for some overlapping prefixes, too, e.g. if `/pre` is enough
    // for a prefix, then perhaps `/p` is also large enough even 
    // after we separate the `/pre`.
    // whose weight is smaller than a target threshold. If we want
    // some overlapping prefixes (e.g. both /p and /pre) then we
    // might also need to 
    //
    module PrefixWeights = 
        type PW = CritbitTree<SizeEst>

        let getWeight p pw = 
            match CritbitTree.tryFind p pw with
            | Some w -> w
            | None -> 0UL

        let inline private addWeight p sz pw =
            CritbitTree.add p (sz + getWeight p pw) pw

        let dictWeights (d:Dict) : PW =
            let accum pw du =
                let struct(_,s,def) = entBytes du
                let szSP = if BS.isEmpty def then 0UL else 1UL
                let sz = 2UL + szSP + uint64 (BS.length s) + uint64 (BS.length def)
                addWeight s sz pw
            toSeqEnt d |> Seq.fold accum (CritbitTree.empty)

        let totalWeight pw =
            CritbitTree.foldBack (fun _ sz tot -> (sz + tot)) pw 0UL

        let rec mergeWeightsN thresh kl node =
            match node with
            | Leaf _ -> struct(kl,node) // can't merge a single leaf
            | CritbitTree.Inner(cb,l,kr,r) // 


        /// 
        let mergeWeights (thresh:SizeEst) (pw:PW) : PW =
        


    let rec private mergePrefixesN thresh kl node =
        match node with
        | CritbitTree.Inner(cb,l,kr,r) ->
            let struct(kl',l') = mergePrefixesN thresh kl l
            let struct(kr',r') = mergePrefixesR thresh kr r
            // 
            if (kl' = kr') then
            match (l',r') with
            | (CritbitTree.Leaf szL, CritbitTree.Leaf szR) when ((szL < thresh) && (szR < thresh)) ->
                let szMerged = szL + szR
                let kMerged = BS.take (cb / 9) kl'
        | _ -> struct(kl,node)

    /// Merge prefixes that up to the size threshold. 
    let rec mergePrefixes thresh (pw:PrefixWeights) : PrefixWeights =
        match pw with
        | CritbitTree.Root(kl,node) -> 
            match node with
            | CritbitTree.Inner(cb,l,kr,r) ->
                let l' = mergePrefixes thresh kl l (CritbitTree.Root(kl,l))
                let r' = mergePrefixes thresh (CritbitTree.Root(kr,r))
                

mergePrefixesN thresh node
        | CritbitTree.Empty -> CritbitTree.Empty
    

    //
    // Dictionaries support fanout on individual bytes. For Awelon,
    // words use lower case alphas, digits, and some punctuation.
    // So the fanout is around 40. If we limit nodes to be at least 
    // 1kB, then a dictionary could reach 40kB without any useful
    // partitioning.
    // 
    // 
 

 
    // So the fanout is around 40 in the worst case. If a "small" 
    // node is up to 1kB, then a node after compaction could be 
    // around 40kB. This is a wide variance - a cost of keeping the
    // representation simple and legible (bit-level indices would
    // be much more difficult to represent!).

 a consequence of
    // keeping dictionary representations simple and legible.
    //
    // In any case, we'll want to ensure  
    //
    // LF and SP - up to 254, but for Awelon closer to 40 (lower
    // case alphas, digits, a little punctuation). Hence, if the
    // threshold for a separate node is 1kB, then we can expect
    // a worst case dictionary is around 40kB after compaction.
    //
    //   
 for our
    // Awelon dictionaries (e.g. words include [a-z0-9/-]*). Hence,
    // if our threshold for a 'small' node is 1kB, then a compacted
    // dictionary shouldn't be larger than 40kB in practice, and 
    // at most 254kB. 

Worst case is a dictionary with maximum fanout
    // (254) where every prefix is just shy of our upper threshold
    // for a small node (for example 1k), hence about 254k. But for
    // Awelon dictionaries, we can assume worst case fanout is at most 40 - 
    // lower case alphas (26), numerics (10), and some punctuation
    // (such as `-` or `/`). 

This
    // weakness of the Dictionary type, but was permitted to
    // keep the dictionary simple and legible. 

. E.g. if our threshold is 1k 

 so in the
    // worst case we can have a "small node" worth of data per byte
    // level prefix, insufficient to trigger compaction. In this 
    // case, we might leverage 
    //  entry 
    // per byte. 
    // 
    // 
    




    let compactRoot (thresh : SizeEst) (stowDir : Dict -> VRef<Dict>) (d:Dict) : Dict =
        let d' = compactNode thresh stowDir d
        let sz = 

    /// we can also leverage an empty prefix.
    ///
    /// Usually, indexing on prefix is preferable. But this only 
    /// holds if the dictionary node at a prefix is large enough.
    /// For very small prefixes, it simply isn't worth much. 
    ///
    /// For lookup performance, indexing on a prefix is preferable,
    /// but only when this allows us to 
    /// latter is preferable. But we should be wary of indexing on
    /// a prefix if we don't have enough data 

Ideally, most of these should
    /// be accessed by prefix index, to avoid 
    ///
    /// The dictionary structure is not too rigid, allowing ad-hoc
    /// prefixes and even overlapping prefixes. So we need heuristics
    /// to decide which prefixes to separate. OTOH, minimally our 
    /// prefix will be one byte, hence we cannot   

 a strategy involved in selecting which prefixes to 
    /// index, as this affects performance. It's preferable if 
    /// dictionary nodes are not too small. But there are some limits 
    /// on what we can do for indexing. 

 into separate tree nodes. This allows
    /// subsequent updates  
 The dictionary
    /// representation can support many compaction strategies, which
    /// influence performance.
    ///
    /// To get started swiftly, we'll implement a simple strategy
    /// that won't necessary perform well, but should be on par with
    /// 
    /// 
    /// 
    ///
    /// 
    /// 
    ///  
    /// The strategy
    /// will influence performance, but if the main goal is structure
    /// sharing and huge dictionaries then a naive strategy will work.
    ///

    // TODO: simplistic compaction of dictionaries.
    //
    // The dictionary representation has a lot of heuristic freedom.
    // We may prefer larger prefixes or smaller ones, for example. 
    // But for the moment, it's more important to just support a 
    // simple compaction heuristic. Improving its quality can wait.
    //
    // Each prefix has a cost of `/p secureHash` or about 68 bytes,
    // more for a larger prefix. Since prefixes are byte oriented 
    // and exclude only LF and SP, we can have up to 254 prefixes 
    // to cover every byte. 254 * 68 = 17kB of references.
    // 
    //
    // My proposal at the moment is to essentially follow the LSM Trie
    // model, perhaps with a slightly different heuristic for how many
    // unique prefixes we'll permit. 
    //
    // One point to consider is that the "working set" for definitions
    // shouldn't have too many pending definitions after we decide to
    // flush the updates. So, an all-or-nothing approach for pushing
    // data into nodes might work best.
    //
    // 
    // For compaction of dictionaries, it's preferable if we can use
    // relatively large prefixes. Also, we don't want a thrashing 
    // working set, so it's preferable to 
    //
    // The dictionary structure used here has a relatively large
    // degree-of-freedom 
    

        


    
    // Compute a definition with GC-managed Stowage dependencies.
    let private autoDef (db:Stowage) (s:ByteString) : Def =
        let inline mkDep h = VRef.wrap (EncBytesRaw.codec) db h
        let inline addDep l h = ((mkDep h)::l)
        let deps = List.toArray (RscHash.foldHashDeps addDep [] s)
        Def(s, deps :> System.Object)

    /// Simplified codec for VRef<Dict>. This does not perform any
    /// automatic compaction for large dictionaries.
    let codec =
        { new Codec<Dict> with
            member __.Write d dst = writeEnts (toSeqEnt d) dst
            member cD.Read db src = parseDict (autoDef db) (VRef.wrap cD db) (ByteStream.readRem src)
            member cD.Compact db d = struct(d, sizeDictBytes d)
        }
        
       

    // TODO:
    // - intersections?
    //
    // efficient intersection would be convenient for search, i.e. if
    // we maintain labeled sets for reverse lookup and we want to find
    // elements that have multiple labels. It should be feasible, using
    // a similar concept to 'diff'.
    //
        

