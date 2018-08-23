namespace Wikilon
open Awelon
open Stowage
open Data.ByteString

// As a RESTful application, Wikilon data is kept in a Database and
// the web-apps shouldn't keep much local state. Potential exceptions
// may exist for server-push (e.g. web-sockets) and caching. For the
// latter, we might benefit from caching evaluation environments for
// multiple runs.
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
// in a background thread, asynchronously and incrementally. Further, I
// should consider our potential to introduce new indices. 
//
// For the moment, it might be best to favor an ad-hoc approach, with each
// index implemented by hand.
//
// For real-time systems, we'll also want some form of publish-subscribe
// server-push (web-sockets, COMET patterns, etc.). This could feasibly be
// treated as another index, with updates being processed asynchronously.
// Ideally, anything we can query can also be indexed.
// 
module WikiState =

    type Symbol = Dict.Symbol   /// Bytes, minus LF or SP
    type Def = Dict.Def         /// Bytes minus LF, with finalizers

    /// Our simplified transaction model! Upon commit, we'll compare
    /// a read-set against values in the database. We may also check
    /// the write-set for sensible symbols and definitions, e.g. to
    /// ensure everything will parse. If it all checks out, we accept
    /// the writes as non-conflicting.
    ///
    /// Reads and writes may be entire dictionaries, which allows for
    /// prefix-level updates or read dependencies. But in the normal
    /// use case, our write-set or read-set should be a simple set of
    /// symbols with local definitions.
    ///
    /// Transactions may be rejected for reasons other than co
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

    /// A history of dictionaries.
    type DictHist = Snapshot.H<Dict>

    val dict_key = BS.fromString "dict"
    val hist_key = BS.fromString "hist"

    let getDict (db:Stowage.DB) : 


    /// Database abstract interface. A database is stateful, but has
    /// a primary value for import/export.
    ///
    /// This doesn't specify a schema, e.g. for definitions versus
    /// user models. Also, we might wish to use separate databases
    /// for sensitive data, such as passwords or user models.
    type DB =
        /// Every database has associated Stowage.
        abstract member Stowage : Stowage with get

        /// The primary state can be accessed as a value.
        abstract member Head : Dict with get

        /// We'll often look up individual symbols. Doing so might be
        /// more efficient than constructing a Dict value, depending
        /// on internal representations.
        abstract member Read : Symbol -> Def option

        /// Our DB will have a memory of historical states. However, we
        /// won't make strong guarantees about how long our historical 
        /// snapshots are preserved.
        abstract member Hist : DictHist with get

        /// Updates to our database are transactional. Transactions are
        /// modeled as pure values, see above. Upon commit, we'll accept
        /// or reject the transaction. Non-durable transactions can often
        /// return very swiftly.
        abstract member Commit : TX -> bool

    // Thoughts: Some forms of cache can be modeled as 'forgetful' DBs.
    // E.g. with memoized results that can be regenerated as needed.
    // It could feasibly be separated from our main dictionary.
        
    /// Even with a key-value DB, I still need a schema of sorts for our
    /// primary data. Durable data includes both our dictionaries and our
    /// user information. 






