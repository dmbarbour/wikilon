namespace Awelon
open Stowage
open Data.ByteString

// Awelon language does not specify a dictionary representation.
// However, there are many relevant desiderata:
//
// * lightweight structure sharing
// * lazy downloading over networks
// * efficient working set operations
// * caching for evaluation, optimization
// * support background computations
// * support real-time publish-subscribe
//
// For now, I'm going to treat cached computations and pubsub as a
// separate issue and hope it remains that way. However, 
//
// I'm leaning towards use of an LSM-trie to record definitions. This
// gives me structure sharing across versions and forks (and a lesser
// degree across similar dictionaries). And an LSM-trie over Stowage
// should permit lazy import/export.
//
// Minimally, we must have an association from words to definitions.
//
// For effective cache lookups, one option is to associate with each
// word a secure hash based on definition and transitive dependencies.
// This version string must be invalidated, but cache associations to
// the string are expired. This supports sharing cache among similar
// dictionaries. 
// 
// To manage version cache, we do need to perform a reverse lookup
// for a word's clients. We might also want to search for: labels,
// secure hash resources, natural numbers, embedded texts, etc.. and
// perhaps even a full-text search.
//
// Also, dictionaries may be hierarchical (via `@dict` suffixes).
// Thus, any of this structure can be repeated recursively.
//
// A proposed model:
//
//  trie from each defined word to its definition
//  trie from symbol to set of clients (another trie)
//  trie from symbol to version string
//  trie from @dict name to more dictionaries
// 
// We can transitively invalidate any version strings upon update,
// but it's a hassle to do so if we change any root definitions.
//
// It might be worthwhile to model a dictionary as "mid update" to
// support buffering and asynchronous processing of recent updates.
// This means we might have some set of symbols to be invalidated,
// and a set of definition updates still to be processed. However,
// this should be a non-issue in the short term.
//
// Aside: Awelon code can also compress pretty well, e.g. a list
// might have long runs of `cons] cons] cons] cons]`. For editable
// views, which could have large definitions, it is tempting to
// try compression (however, we must be careful with secure hash
// resources). 
module Dict =

    /// An Awelon definition will be recorded as a ByteString. But
    /// large definitions should be separated from the trie nodes to
    /// improve structure sharing.
    type Def = CByteString

    /// How large a definition before we use a remote reference?
    let shortDefThreshold = 360

    /// A mapping from words to definitions.
    type Defs = Trie<Def>

    /// For Hi





