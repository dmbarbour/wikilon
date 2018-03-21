namespace Awelon
open System.Collections.Generic
open Stowage
open Data.ByteString

// Awelon language has a simple standard language definition, based
// on a log-structured merge-tree with radix-tree indexing. Example:
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
// nodes. The empty prefix is valid, useful for prototype inheritance.  
//
// This representation relies on Awelon symbols and definitions not
// requiring all bytes, such that we can use LF to separate updates
// and SP to separate the symbol without sizes or escapes. Legibility
// is a goal, although this representation is unsuitable for direct
// human editing. 
//
// Only the last update for a symbol or matching prefix is used. So we
// can normalize by erasing masked updates and sorting what remains.
// This module manages dictionary nodes in their normal form. Prefixes
// are listed first, but other symbols appear in lexicographic order.
//
// Hierarchical dictionaries simply use `dictname/word` symbols, and we
// can use `/dictname/ secureHash` to update a hierarchical dictionary.
//
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
    /// recommended to move large definitions into Stowage (using
    /// the `$secureHash` wrapper). Binary resources should almost
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

    /// A dictionary node indexed for efficient lookups.
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
    /// direct access and compaction.
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

    let inline private isPrefix p s = (p = (BS.take (BS.length p) s))

    // obtain leftmost value of a CritbitTree node
    let rec private leftValN node =
        match node with
        | CritbitTree.Inner(_,l,_,_) -> leftValN l
        | CritbitTree.Leaf v -> v

    // Any prefix is certainly in the search path because in a prefix
    // of a symbol will match the symbol up to prefix length. But not
    // all keys in the search path are valid prefixes. So test each 
    // from longest match to shortest.
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
        if Option.isNone (matchDirectory sym dict)
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
    let applyUpd (dict:Dict) (upd:DictEnt) : Dict =
        match upd with
        | Direct (p,dir) -> updPrefix p dir dict
        | Define (sym,def) -> defSym sym def dict
        | Delete sym -> delSym sym dict

    let fromSeq (s:seq<DictEnt>) : Dict =
        Seq.fold applyUpd empty s

    /// Expand a dictionary to a sequence of entries.
    let toSeq (dict:Dict) : seq<DictEnt> =
        let defEnt (sym,defOpt) =
            match defOpt with
            | Some def -> Define(sym,def)
            | None -> Delete sym
        let dirs = CritbitTree.toSeq (dict.dirs) |> Seq.map Direct
        let defs = CritbitTree.toSeq (dict.defs) |> Seq.map defEnt
        Seq.append dirs defs

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

    /// Parse all entries in a dictionary. May raise ByteStream.ReadError.
    let rec parseDictEnts mkDef mkDir s : seq<DictEnt> =
        if BS.isEmpty s then Seq.empty else
        seq {
            let struct(line,lfmore) = BS.span ((<>) cLF) s
            yield parseDictEnt mkDef mkDir line
            yield! parseDictEnts mkDef mkDir (BS.drop 1 lfmore)
        }

    let inline parseDict mkDef mkDir s : Dict =
        fromSeq (parseDictEnts mkDef mkDir s)

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
        ByteStream.write (writeEnts (toSeq dict))

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
        Seq.fold accum 0UL (toSeq dict)

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

    /// Load a dictionary node given a directory reference.
    ///
    /// Uses default Stowage.Cache. Loading a directory many times
    /// in a short period should return the same value in memory.
    let load (dir:Dir) : Dict =
        match dir with
        | None -> empty
        | Some ref ->
            match MCache.tryFind (ref.ID) (dictLoadCache) with
            | Some d -> d
            | None ->
                let mkDef s = Def(s, (ref :> System.Object))
                let mkDir h = VRef.wrap (ref.Codec) (ref.DB) h
                let s = ref.DB.Load(ref.ID)
                let sz = 120UL + uint64 (BS.length s)
                let d = parseDict mkDef mkDir s
                MCache.tryAdd (ref.ID) d sz (dictLoadCache)

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
            member __.Write d dst = writeEnts (toSeq d) dst
            member cD.Read db src = parseDict (autoDef db) (VRef.wrap cD db) (ByteStream.readRem src)
            member cD.Compact db d = struct(d, sizeDictBytes d)
        }
        
       

        

