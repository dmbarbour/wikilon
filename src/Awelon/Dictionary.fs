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
    /// a single update to a symbol or prefix.
    type DictEnt =
        | Direct of Prefix * Dir
        | Define of Symbol * Def
        | Delete of Symbol

    /// Empty dictionary, corresponding to empty bytestring.
    let empty : Dict = 
        { dirs = CritbitTree.empty
          defs = CritbitTree.empty
        }

    /// Test for trivially empty dictionary. (Does not test whether
    /// directories are non-empty.)
    let isEmpty (dict:Dict) : bool =
        CritbitTree.isEmpty (dict.defs)
            && CritbitTree.isEmpty (dict.dirs)

    /// Trivially inherit from a dictionary, via empty prefix. 
    /// Unlike `load`, this simply wraps a dictionary node.
    let fromPrototype (dir:Dir) : Dict = 
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
    let defSym (sym:Symbol) (def:Def) (dict:Dict) : Dict =
        { dict with defs = CritbitTree.add sym (Some def) (dict.defs) }

    /// Delete the given symbol, removing its definition.
    let delSym (sym:Symbol) (dict:Dict) : Dict =
        // erase entry if the symbol is obviously undefined,
        // otherwise add a `~sym` entry for pending deletion.
        if Option.isNone (matchDirT sym (dict.dirs))
            then { dict with defs = CritbitTree.remove sym (dict.defs) }
            else { dict with defs = CritbitTree.add sym None (dict.defs) }
    
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
    let applyUpd (dict:Dict) (upd:DictEnt) : Dict =
        match upd with
        | Direct (p,dir) -> updPrefix p dir dict
        | Define (sym,def) -> defSym sym def dict
        | Delete sym -> delSym sym dict

    let private defEnt (sym,defOpt) =
        match defOpt with
        | Some def -> Define(sym,def)
        | None -> Delete sym

    /// Expand a dictionary to a sequence of entries.
    let toSeqEnt (dict:Dict) : seq<DictEnt> =
        let dirs = CritbitTree.toSeq (dict.dirs) |> Seq.map Direct
        let defs = CritbitTree.toSeq (dict.defs) |> Seq.map defEnt
        Seq.append dirs defs

    let fromSeqEnt (s:seq<DictEnt>) : Dict =
        Seq.fold applyUpd empty s

    let inline private trimSP s =
        let isSP c = (c = cSP)
        s |> BS.dropWhile isSP |> BS.dropWhileEnd isSP

    /// Parse a single line from a dictionary. May raise ByteStream.ReadError.
    let parseDictEnt (mkDef:ByteString -> Def) (mkDir:RscHash -> VRef<Dict>) (ln:ByteString) : DictEnt =
        if BS.isEmpty ln then raise ByteStream.ReadError else
        let c0 = BS.unsafeHead ln
        let struct(sym,spdef) = BS.span ((<>) cSP) (BS.unsafeTail ln)
        let def = BS.drop 1 spdef // drop symbol-def separator
        if (c0 = cDef) then Define(sym, mkDef def) else
        let h = trimSP def // ignore whitespace for Del and Def lines.
        if ((c0 = cDel) && (BS.isEmpty h)) then Delete sym else
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

    /// Write an entry. Assumes valid context (start of line).
    /// NOTE: Does not include LF to terminate/separate entry!
    let writeEnt (du:DictEnt) (dst:ByteDst) : unit =
        match du with
        | Direct (p, dir) -> 
            ByteStream.writeByte cDir dst
            ByteStream.writeBytes p dst
            match dir with
            | None -> ()
            | Some ref ->
                ByteStream.writeByte cSP dst
                ByteStream.writeBytes (ref.ID) dst
        | Define(s,def) ->
            ByteStream.writeByte cDef dst
            ByteStream.writeBytes s dst
            if not (BS.isEmpty (def.Data)) then
                ByteStream.writeByte cSP dst
                ByteStream.writeBytes (def.Data) dst
        | Delete s ->
            ByteStream.writeByte cDel dst
            ByteStream.writeBytes s dst

    /// Write each entry. Adds an LF after each entry.
    let writeEnts (ents:seq<DictEnt>) (dst:ByteDst) : unit =
        let wfn du = writeEnt du dst; ByteStream.writeByte cLF dst
        Seq.iter wfn ents
    
    /// Serialize/Render a dictionary to a ByteString.
    /// This will write a normalized
    let writeDict (dict:Dict) : ByteString =
        ByteStream.write (writeEnts (toSeqEnt dict))

    let inline private sizeEntHdBody du =
        match du with
        | Direct(p,Some _) -> struct(BS.length p, RscHash.size)
        | Direct(p,None) -> struct(BS.length p, 0)
        | Define(s,def) -> struct(BS.length s, BS.length def.Data)
        | Delete(s) -> struct(BS.length s, 0)

    /// Size (in bytes) to write a dictionary entry.
    let sizeEntBytes (du:DictEnt) : SizeEst =
        let struct(szHd,szBody) = sizeEntHdBody du 
        let szSP = if (szBody > 0) then 1UL else 0UL
        1UL + (uint64 szHd) + szSP + (uint64 szBody)

    /// Size (in bytes) to write an entire dictionary.
    let sizeDictBytes (dict:Dict) : SizeEst =
        let accum n du = n + sizeEntBytes du
        Seq.fold accum 0UL (toSeqEnt dict)

    // Shared dictionary parse cache. This permits structure sharing
    // in memory, albeit at the cost of extra lookup overheads.
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

    // Concatenate dictionaries at entry level, O(L * lg(L+R)).
    // It's necessary to optimize for smaller first argument
    // for efficient sequencing and partitioning.
    let private concatDictEnts (a:Dict) (b:Dict) : Dict =
        let inline blockPrefix s = Option.isSome (matchDirT s (b.dirs))
        let inline blockSymbol s = CritbitTree.containsKey s (b.defs) || blockPrefix s
        let accumDir p dir t = if blockPrefix p then t else CritbitTree.add p dir t
        let accumDef s def t = if blockSymbol s then t else CritbitTree.add s def t
        let dirs' = CritbitTree.foldBack accumDir (a.dirs) (b.dirs)
        let defs' = CritbitTree.foldBack accumDef (a.defs) (b.defs)
        { dirs = dirs'; defs = defs' }

    /// Select symbols matching a given prefix from dictionary.
    /// This filters out symbols that lack the matching prefix.
    /// Useful for browsing a dictionary based on prefixes. More
    /// efficient than splitTreeAt.
    let rec selectPrefix (prefix:Prefix) (d:Dict) : Dict =
        // preserve obvious prefix matches in dictionary
        let dP = { dirs = CritbitTree.selectPrefix prefix (d.dirs)
                   defs = CritbitTree.selectPrefix prefix (d.defs)
                 }
        // recursively filter from a wider directory, if necessary 
        match matchDirT prefix (d.dirs) with
        | Some (struct(pre,Some ref)) when ((BS.length pre) < (BS.length prefix)) ->
            let fix = BS.drop (BS.length pre) prefix
            let ss = selectPrefix fix (loadRef ref)
            concatDictEnts (addPrefix pre ss) dP
        | _ -> dP // no subdirectory to include

    // extract and remove leftmost element of a non-empty critbit tree.
    let rec private splitLeastValN (node:CritbitTree.Node<'V>) : struct('V * CritbitTree<'V>) =
        match node with
        | CritbitTree.Inner(cb,l,kr,r) ->
            let struct(v,rem) = splitLeastValN l
            let rem' = 
                match rem with
                | CritbitTree.Root(kl',l') ->
                    CritbitTree.Root(kl', CritbitTree.Inner(cb, l', kr, r))
                | CritbitTree.Empty -> CritbitTree.Root(kr,r)
            struct(v,rem')
        | CritbitTree.Leaf v -> struct(v,CritbitTree.Empty)

    let rec private seqDefsN (kl:Symbol) (node:CritbitTree.Node<Def option>) : seq<(Symbol * Def)> = 
        seq {
            match node with
            | CritbitTree.Inner(_,l,kr,r) ->
                yield! seqDefsN kl l
                yield! seqDefsN kr r
            | CritbitTree.Leaf defOpt ->
                match defOpt with
                | Some def -> yield (kl,def)
                | None -> ()
        }
    let private seqDefs (t:CritbitTree<Def option>) : seq<Symbol * Def> =
        match t with
        | CritbitTree.Root(kl,l) -> seqDefsN kl l
        | CritbitTree.Empty -> Seq.empty

    /// Iterate through all defined symbols in lexicographic order.
    /// Only the most recent definition for each symbol is rendered.
    let rec toSeq (d:Dict) : seq<(Symbol * Def)> = seq {
        // yield least symbol if before least directory.
        // Otherwise, open the least directory and retry.
        match (d.dirs) with
        | CritbitTree.Root(dirK,dirN) ->
            match (d.defs) with
            | CritbitTree.Root(defK,defN) when (defK < dirK) ->
                let struct(defOpt, defs') = splitLeastValN defN
                let dRem = { d with defs = defs' }
                match defOpt with
                | Some def -> yield (defK,def)
                | None -> () // don't report deleted symbols
                yield! toSeq dRem
            | _ ->
                // load directory into dictionary, then retry
                let struct(dirV,dirs') = splitLeastValN dirN
                let dDir = addPrefix dirK (load dirV)
                let dRem = { d with dirs = dirs' }
                yield! toSeq (concatDictEnts dDir dRem)
        | CritbitTree.Empty -> yield! seqDefs (d.defs)
        }

    // iteratively partition dictionary, accumulating directories
    // to left of the given key in 'l' and opening directories if
    // they (naively) might include elements from both partitions.
    let rec private splitAtKey' k l (d : Dict) =
        match (d.dirs) with
        | CritbitTree.Root(dirP, dirN) when (dirP < k) ->
            // prefix is either to left of key or includes key
            let struct(dirV,dirs') = splitLeastValN dirN
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
    /// or greater are to the right. Useful for browsing dictionary.
    let splitAtKey (k:Symbol) (dict:Dict) : struct(Dict * Dict) =
        splitAtKey' k (CritbitTree.empty) dict

    // TODO: simplistic compaction of dictionaries.
    //
    // The dictionary representation has a lot of heuristic freedom.
    // We may prefer larger prefixes or smaller ones, for example. 
    // But for the moment, it's more important to just support a 
    // simple compaction heuristic.
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
        
       

        

