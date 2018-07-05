namespace Wikilon
open Awelon
open Stowage
open Data.ByteString

// As a RESTful application, Wikilon data is kept in a Database and
// the web-apps shouldn't keep much local state. Potential exceptions
// may exist for server-push (e.g. web-sockets) and caching.  
//
// What does our DB need, then?
//
// For persistent data, I propose a simple key-value database similar
// to the Dictionary data structure. Indeed, I could directly use the
// Awelon Dict type, which would simplify debugging and full-system 
// export. Unlike conventional key-value databases, we will leverage
// a persistent data structure and maintain a system snapshot history.
//
// We will likely want some metadata:
//
// - reverse lookup indices, full text search
// - cached versions, evaluations, types
// - user models or access-control aspects
//
// Relevant concerns:
//
// Concurrent updates can be hindered by a centralized data structure. 
// This could be mitigated via transactional update model, such that we
// can detect merge-conflicts and allow most non-conflicting updates.
//
// A reverse-lookup index can be maintained in real-time, but the others
// cannot be due to cascading update issues. We should manage the indices
// in a background thread, asynchronously. Further, I need to consider the
// potential to add new indices over time, and potential for dependencies 
// between indices (e.g. version tracking depends on reverse-lookup index).
//
// For the moment, it might be best to favor an ad-hoc approach, with each
// index implemented by hand.
//
// For real-time systems, we'll also want some form of publish-subscribe
// server-push (web-sockets, COMET patterns, etc.). This could feasibly be
// treated as another index, with updates being processed asynchronously.
// Ideally, anything we can query can also be indexed.
// 
module DB =

    type Symbol = Dict.Symbol   /// Bytes, minus LF or SP
    type Def = Dict.Def         /// Bytes minus LF, with finalizers

    /// Our simplified transaction model! Upon commit, we'll compare
    /// a read-set against values in the database. If everything is
    /// okay, we'll accept the writes as non-conflicting.
    ///
    /// Reads and writes may be entire dictionaries, which allows for
    /// prefix-level updates or read dependencies. But in the normal
    /// use case, our write-set or read-set should be a simple set of
    /// symbols with local definitions.
    type TX = 
        { reads   : Dict
          writes  : Dict      
          durable : bool
        }

    module TX =
        let empty = { reads = Dict.empty; writes = Dict.empty; durable = false }
        let inline addRead' (k:Symbol) (d:Def) (tx:TX) : TX =
            { tx with reads = Dict.add k d (tx.reads) }
        let inline addRead (k:Symbol) (s:ByteString) (tx:TX) : TX =
            addRead' k (Def(s)) tx // ignore Stowage GC issues
        let inline addWrite (k:Symbol) (d:Def) (tx:TX) : TX = 
            { tx with writes = Dict.add k d (tx.writes) }
        let inline markDurable tx = { tx with durable = true }


    /// Database abstract interface.
    ///
    /// The Database allows some separation from 
    type DB =

        // Since we're working with Dicts, DB has a Stowage.
        abstract member Stowage : Stowage with get
        
        







