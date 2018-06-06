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
    /// extra inheritances (corresponding to /prefix entries). We
    /// also keep size estimates for incremental compaction.
    type Dict = 
        { p : Dir option      // optional empty prefix entry
          v : DefUpd option   // optional empty symbol entry
          c : Map<byte,struct(Prefix*Dict)>  // non-empty child nodes
            // z,n are generally computed upon compaction
          n : uint32          // visible entry count (or max value)
          z : uint32          // serialization size (or max value)
        }
    and Dir = LVRef<Dict> option    // blank or secureHash

    let nmax = System.UInt32.MaxValue
    let zmax = System.UInt32.MaxValue

    /// Empty dictionary. This should only exist at the tree root.
    let empty : Dict =
        { p = None
          v = None
          c = Map.empty
          n = 0u
          z = 0u
        }

    /// Test for obviously empty dictionary - no entries. This does
    /// not recognize whether a dictionary is empty due to pending
    /// deletions. For that, favor isEmpty'.
    let isEmpty (d:Dict) : bool =
        (Map.isEmpty (d.c) && Option.isNone (d.v) && Option.isNone (d.p))

    /// Create a dictionary from an initial directory (without loading)
    let fromProto (dir:Dir) : Dict =
        match dir with
        | None -> empty
        | Some _ -> { empty with p = Some dir }

    let inline private isPrefix p s = (p = (BS.take (BS.length p) s))

    /// Load a directory from Stowage as a Dict.
    let load (dir:Dir) : Dict =
        match dir with
        | None -> empty
        | Some ref -> LVRef.load ref

    /// Find the most recent update for a symbol
    let rec tryFindUpd (k:Symbol) (d:Dict) : DefUpd option =
        if BS.isEmpty k then (d.v) else
        match Map.tryFind (BS.unsafeHead k) (d.c) with
        | Some (struct(p,c)) when isPrefix p (BS.unsafeTail k) ->
            let k' = BS.drop (1 + BS.length p) k
            match tryFindUpd k' c with
            | None ->
                match (d.p) with
                | None -> None 
                | Some dir ->
                    // if we don't find our key in the prototype directory,
                    // we can consider it to have been deleted. That is, we
                    // don't want to search directories on shorter prefixes.
                    match tryFindUpd k (load dir) with
                    | None -> Some None // logically delete entry
                    | result -> result
            | result -> result
        | _ -> None 


    /// Update the definition for a given symbol.
    /// 
    /// In this case, we might try to avoid
    //let rec updSym (k:Symbol) (du:DefUpd) (d:Dict) : Dict =



        
    // compute size of shared prefix for two strings.
    let private bytesShared (a:ByteString) (b:ByteString) : int =
        let limit = min (a.Length) (b.Length)
        let rec loop ix =
            if ((ix = limit) || (a.[ix] <> b.[ix])) then ix else
            loop (ix + 1)
        loop 0


    let private joinBytes (a:ByteString) (b:byte) (c:ByteString) : ByteString =
        ByteStream.write (fun dst ->
            let len = a.Length + 1 + c.Length
            ByteStream.reserve len dst
            ByteStream.writeBytes a dst
            ByteStream.writeByte b dst
            ByteStream.writeBytes c dst)

        

