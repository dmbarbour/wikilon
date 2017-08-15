namespace Stowage
open Data.ByteString

/// A key-value tree using Stowage resources.
///
/// A key-value tree modeled above Stowage essentially gives us first
/// class databases with persistence, structure sharing, and efficient
/// serialization (sharing even over a network). Tree values may be 
/// larger than memory, keeping most of the data in the Stowage layer.


/// Modeling a key-value tree above Stowage offers a simple basis for
/// first-class key-value databases as plain old values. These databases
/// offer logarithmic access and update, ordered keys, structure sharing,
/// and data persistence. Such trees may be larger than working memory.
///

// challenges of working with resources include managing reference counts
// and potential support for intermediate key-value structures. 
// 

module internal I =
    // Node structure is based on crit-bit trees. 
    type Node =
        | Leaf of ByteString                        // leaf value
        | Inner of int * ByteString * Node * Node   // inner node
        | Remote of RscHash                         // remote node


// A difficulty with this model is managing the re


[<Struct>]
type KVTree =
    val DB            : DB
    val internal Root : Option<struct (ByteString * I.Node)>
    val internal Rsc
    internal new(db:DB, root:I.Node option) = { DB = db; Root = root }

module KVTree =
    trimKey (k:Key) : Key = 


    let empty (db:DB) : KVTree = KVTree(db,None)
    let singleton (db:DB) (

