namespace Awelon
open Stowage
open Data.ByteString

// Awelon language has a simple standard language definition, based
// on a log-structured merge-tree with radix indexing. Example:
//
//      /prefix1 secureHash1
//      /prefix2 secureHash2
//      :symbol1 definition1
//      :symbol2 definition2
//      ~symbol3
//
// We have subtrees and symbol definitions in line-oriented ASCII text.
// Each line is gives us an "update" - a prefix and subtree, a symbol
// and definition, or a symbol deletion. Only the last relevant update
// applies, e.g. prefix `/p` masks prior `:poke` and `/prod`. We'll
// want to normalize a node before saving it by erasing masked updates
// and sorting whatever remains.
//
// This module will focus on the dictionary representation - access,
// update, compaction. Efficient access may also require caching to
// avoid repeatedly loading and indexing the data, but I need to see
// whether this is a significant concern in practice. Observations on
// a dictionary, such as detecting cycles or computing types, must be
// handled separately.
//
// Garbage collection is a potential concern: we must prevent premature
// collection of Stowage-layer dependencies, especially for newer defs.
// Since I don't want to parse definitions at this layer, I'll just give
// each definition an ad-hoc dependencies object.
//
// Cached observations over the dictionary (types, evaluations, 
// fuzzy find, reverse lookup, etc.) are not supported here. But
// efficient diffs are essential to easily maintain cache.
//
module Dict =

    /// Opaque GC constraint - an Object with finalizer, or null.
    ///
    /// Each definition is accompanied by a GCDeps. The intention
    /// is to use this with Stowage Decref finalizers, to prevent
    /// premature GC of resources referenced only from memory.
    type GCDeps = System.Object

    /// A symbol fragment. Prefixes are stripped from inner nodes.
    /// The empty prefix is not uncommon.
    type Prefix = ByteString

    /// A full symbol should be a valid word or dictionary name.
    /// However, we'll often work with partial symbols because
    /// we strip the prefix.
    type Symbol = ByteString

    /// We don't parse dictionary definitions at this layer, so
    /// we just use bytestrings. Definitions must be inline, so
    /// the character LF is illegal, but this isn't checked. In
    /// valid dictionaries, definitions exclusively use ASCII
    /// without C0 or DEL. 
    ///
    /// Definitions are copied into each node, and very large
    /// definitions may be problematic. But we may indirect to
    /// a `$secureHash` resource as needed. Compaction of the
    /// dictionary might do so automatically.
    type Def = struct(ByteString * GCDeps)
    let inline defBytes (struct(b,_)) = b

    /// Dictionary Tree Structure
    ///
    /// This is an "open" dictionary structure, in the sense that all
    /// definitions 
    /// We can compute the normalized dictionary from an update stream.
    /// It's usually not essential to remember the history of updates.
    type Dict =
        { dirs : CritbitTree<Dict>           // all the prefixes
        , defs : CritbitTree<Option<Def>>    // None for deletion.
        }

    type Upd =
        | Direct of Prefix * Def    // /prefix secureHash
        | Define of Symbol * Def    // :symbol definition
        | Delete of Symbol          // ~symbol


    /// A mapping from words to definitions.
    type Defs = Trie<Def>

    /// For Hi





