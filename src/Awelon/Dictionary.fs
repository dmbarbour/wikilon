namespace Awelon
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
// node based on a matching prefix (/). Internal nodes are identified
// by secure hash. The matched prefix is stripped from symbols within
// the referenced node. Empty prefix is valid and can be useful. There
// is no special directory separator - any byte boundary will work.
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

    /// A symbol is a bytestring, it should be ASCII with no control
    /// characters or spaces. In context of inner nodes, the prefix
    /// will be stripped from a symbol. Symbol may be empty string.
    type Symbol = ByteString
    let isValidSymbolChar c = ((126uy >= c) && (c > 32uy))
    let isValidSymbol s = BS.forall isValidSymbolChar s

    /// A directory is represented by a symbol prefix. Empty prefix
    /// is valid.
    type Prefix = Symbol
    
    /// Definitions are ASCII texts with no control characters, i.e.
    /// in range 32..126. This is a restriction to enhance legibility
    /// of dictionary nodes and avoids need for sized definitions.
    ///
    /// Definitions may contain secure hash references into Stowage.
    /// To resist premature GC, a hidden `Deps` field can hold onto
    /// Stowage references via VRef or a list thereof.
    [<Struct>]
    type Def = 
        val Data : ByteString
        val private Deps : System.Object
        new(def,deps) = { Data = def; Deps = deps }
        new(def) = { Data = def; Deps = null }
        member x.KeepAlive() : unit = System.GC.KeepAlive (x.Deps)
    let isValidDefChar c = ((126uy >= c) && (c >= 32uy))
    let isValidDefStr s = BS.forall isValidDefChar s
    let inline isValidDef (d:Def) = isValidDefStr (d.Data)

    /// A dictionary node indexed for efficient lookups.
    ///
    /// The main directory structure is made very explicit here,
    /// so we can easily perform deep lookups without providing
    /// a Stowage reference. But definitions are not processed.
    /// 
    /// Definitions are encoded with an option type, with None
    /// representing deletion. Directories are indicated by their
    /// secure hash, but may be empty string (None) for deletion
    /// of a prefix.
    type Dict = 
        { dirs : CritbitTree<Dir>
          defs : CritbitTree<Def option>
        }
    and Dir = VRef<Dict> option

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

    let inline private isPrefix p s = (p = (BS.take (BS.length p) s))

    // obtain leftmost value of a CritbitTree node
    let rec private leftValN node =
        match node with
        | CritbitTree.Inner(_,l,_,_) -> leftValN l
        | CritbitTree.Leaf v -> v

    // All critbits up to prefix length will match, so any prefix is
    // certainly in the search path. But not all keys in the search 
    // path will be relevant prefixes. So we just test every key in
    // the path.
    let rec private matchDirN sym kl node =
        match node with
        | CritbitTree.Inner(cb,l,kr,r) ->
            if CritbitTree.testCritbit cb sym then
                // test for larger match in right. 
                let inR = matchDirN sym kr r
                if Option.isSome inR then inR else
                // otherwise, kl might be a match.
                if isPrefix kl sym 
                    then Some (struct(kl,leftValN l))
                    else None
            else matchDirN sym kl l
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

    /// Update the definition for a given symbol. The `None` definition
    /// will reset our definition to an undefined status. Normally this
    /// will add a `:symbol` or `~symbol` line to the dictionary, but 
    /// we can sometimes drop the `~symbol` line if the symbol obviously
    /// is not defined.
    let setSymbol (sym:Symbol) (defOpt:Def option) (dict:Dict) : Dict =
        if (Option.isNone defOpt) && (Option.isNone (matchDirectory sym dict)) 
            then { dict with defs = CritbitTree.remove sym (dict.defs) } 
            else { dict with defs = CritbitTree.add sym defOpt (dict.defs) }

    /// Set directory for a given prefix.
    ///
    /// This corresponds to a `/prefix dirHash` update. It erases all
    /// masked definitions and directories. The empty secure hash is
    /// an alias for the empty dictionary node. If locally obvious that
    /// a prefix has no entries, we'll drop the empty prefix entry. But
    /// we don't load any directories.
    let setPrefix (p:Prefix) (dir:Dir) (dict:Dict) : Dict =
        let defs' = CritbitTree.dropPrefix p (dict.defs)
        let kpdir = CritbitTree.dropPrefix p (dict.dirs)
        let noent = (Option.isNone dir) && (Option.isNone (matchDirT p kpdir))
        let dirs' = if noent then kpdir else CritbitTree.add p dir kpdir
        { dirs = dirs'; defs = defs' }


