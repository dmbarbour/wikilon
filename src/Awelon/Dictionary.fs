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
    let inline isValidDef (d:Def) = isValidDefStr (defData d)

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
    /// construct a dictionary given the initial directory reference,
    /// without loading the directory.
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
    /// Prefix `/p` will appear just before symbol `:p` or `~p`
    /// when both entries are present.
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
    // the dictionary

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
    // That is, we'll iterate entries in `a` instead of `b`. 
    let private concatDictEnts (a:Dict) (b:Dict) : Dict =
        let inline maskPrefix p = Option.isSome (matchDirT p (b.dirs))
        let inline maskSymbol s = CritbitTree.containsKey s (b.defs) || maskPrefix s
        let accumDir p dir t = if maskPrefix p then t else CritbitTree.add p dir t
        let accumDef s def t = if maskSymbol s then t else CritbitTree.add s def t
        let dirs' = CritbitTree.foldBack accumDir (a.dirs) (b.dirs)
        let defs' = CritbitTree.foldBack accumDef (a.defs) (b.defs)
        { dirs = dirs'; defs = defs' }

    let private removePrefixT (p:Prefix) (t:CritbitTree<'A>) : CritbitTree<'A> =
        let remP k v t = CritbitTree.add (BS.drop (BS.length p) k) v t
        CritbitTree.foldBack remP (CritbitTree.selectPrefix p t) (CritbitTree.empty)

    // Filter for symbols matching a prefix then erase that prefix.
    let rec private removePrefix (prefix:Prefix) (d:Dict) : Dict =
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

    /// Access the `/ secureHash` directory for a dictionary.
    /// Will return the empty directory (None) if no prototype.
    let dictProto (d:Dict) : Dir =
        match (d.dirs) with
        | CritbitTree.Root(e,n) when (BS.isEmpty e) -> leftValN n
        | _ -> None

    /// Access the `/ secureHash` prototype and the remainder
    /// of the dictionary.
    let dictProto' (d:Dict) : struct(Dir * Dict) =
        match (d.dirs) with
        | CritbitTree.Root(e,n) when (BS.isEmpty e) ->
            let struct(dir,dirs') = splitLeftValN n
            struct(dir, { dict with dirs = dirs' })
        | _ -> struct(None,d)

    /// Merge with prototype chain. Transitively eliminates
    /// the `/ secureHash` entry, preserving definitions.
    let rec mergeProto (d0:Dict) : Dict =
        let struct(p,d) = dictProto' d0
        if Option.isNone p then d else
        mergeProto (concatDictEnts (load p) d)

    /// Merge with the prototype head, if any. Non-transitive.
    let mergeProtoHd (d0:Dict) : Dict =
        let struct(p,d) = dictProto' d0
        if Option.isNone p then d else
        concatDictEnts (load p) d

    // return true if prototype chain is at least length k
    let rec private protoChainLenGE k d =
        if (k < 1) then true else
        match dictProto d with
        | Some ref -> 
            if (k = 1) then true else
            protoChainLenGE (k - 1) (loadRef ref)
        | None -> false

    /// Limit prototype chain to a given length or smaller. Performs
    /// mergeProto if the chain is longer than the given size value.
    let limitProtoChain k d =
        if protoChainLenGE k d 
            then mergeProto d
            else d

    /// Find first common ancestral prototype for two directories.
    ///
    /// That is, scan backwards through the directories for a shared
    /// `/ secureHash` entry, if one exists. Otherwise returns None.
    let findCommonProtoDir (a0:Dir) (b0:Dir) : Dir =
        // loop1: step through remaining chain until match is found
        let rec loop1 ma b =
            match b with
            | None -> None
            | Some bref ->
                if CritbitTree.containsKey (bref.ID) ma then b else
                loop1 ma (dictProto (loadRef bref))
        // loop2: step through two chains until match is found.
        let rec loop2 ma aref mb b =
            match b with
            | None ->
                loop1 mb (dictProto (loadRef aref))
            | Some bref ->
                if CritbitTree.containsKey (bref.ID) ma then b else
                let mb' = CritbitTree.add (bref.ID) () mb
                loop2 mb' bref ma (dictProto (loadRef aref))
        match a0,b0 with
        | Some aref, Some bref ->
            if (aref = bref) then a0 else
            let ma = CritbitTree.singleton (aref.ID) ()
            let mb = CritbitTree.singleton (bref.ID) ()
            loop2 ma aref mb (dictProto (loadRef bref))
        | _ -> None


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

    // LSM-trie inspired compactions.
    //
    // - When sufficient data is buffered, we flush.
    // - On flush, we push updates to every child node.
    // - Child nodes that would be too small are inlined.
    //
    // Inlining allows multiple entries with shared prefixes,
    // such as `/p1` and `/p2` logically existing under `/p`.
    let rec private lsm_compact (stowD:Dict->VRef<Dict>) (szL:SizeEst) (szU:SizeEst) (d0:Dict) : struct(Dict * SizeEst) =
        let sz0 = sizeDict d0
        if (sz0 < szU) then struct(d0,sz0) else
        let step (ix:int) (d:Dict) : Dict = 
            let p = BS.singleton (byte ix)
            let dp = removePrefix p d
            // push updates if there are any updates to push
            if CritbitTree.isEmpty (dp.defs) then d else
            let struct(dpc,szdpc) = lsm_compact mkDir szL szU (mergeProto dp)
            if (szdpc < szL) 
                then concatDictEnts (dropPrefix p d) (addPrefix p dpc)
                else updPrefix p (Some (stowD dpc)) d
        let rec loop ix d = 
            if (ix >= 256) then d else
            loop (ix+1) (step ix d)
        let df = loop 0 d0
        struct(df, sizeDict df)

    // wrap definition with GC-managed Stowage dependencies.
    let private autoDef (db:Stowage) (s:ByteString) : Def =
        let inline mkDep h = VRef.wrap (EncBytesRaw.codec) db h
        let inline addDep l h = ((mkDep h)::l)
        let deps = List.toArray (RscHash.foldHashDeps addDep [] s)
        Def(s, deps :> System.Object)

    let private codecConf szU =
        let szL = 900UL // size for efficient stowage refs
        { new Codec<Dict> with
            member __.Write d dst = writeEnts (toSeqEnt d) dst
            member cD.Read db src = parseDict (autoDef db) (VRef.wrap cD db) (ByteStream.readRem src)
            member cD.Compact db d = lsm_compact (VRef.stow cD db) szL szU (mergeProto d)
        }

    /// Default codec for Dict, using LSM-trie inspired compaction.
    /// That is, we treat each node as a buffer for recent writes.
    /// Suitable for write-heavy workloads, but it isn't easy to 
    /// determine total tree size due to holding some defunct data.
    let codec = codecConf 60_000UL

    /// Codec for Dict with trie inspired compaction. Updates will
    /// be aggressively pushed to leaf nodes, such that we have at
    /// most one entry per symbol. This is history-independent, so
    /// it's also efficient for diffs and comparisons. But writes
    /// are more expensive, and reads are often deeper.
    let codec' = codecConf 1UL

    // Todo: full compactions?
    //   this could be achieved via rebuilding with codec', using
    //   relatively large write batches (a few megabytes each).

    // TODO:
    // - intersections?
    //
    // efficient intersection would be convenient for search
        

