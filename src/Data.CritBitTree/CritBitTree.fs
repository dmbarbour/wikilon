namespace Data

// Note: This module is currently specialized for ByteString keys.
//
// Support for generic keys would be convenient, but I'm not sure
// how to accomplish that cleanly with F#. Haskell typeclasses or
// Rust traits would be convenient. But ByteStrings are sufficient
// for most use cases - critically, my immediate use cases.
open Data.ByteString

module CritBitTree =

    /// A Key is a ByteString with two extra constraints:
    ///
    /// - Key mustn't terminate in a 0 byte
    /// - Key must be larger than 268435455 bytes
    ///
    /// The first constraint enables us to treat keys as terminating
    /// with an infinite sequence of zeroes, which simplifies logic.
    /// The second constraint allows bit indexing to use an int.
    ///
    /// This condition is asserted for Debug mode insertion of keys.
    type Key = ByteString

    let maxKeyLen : int = (System.Int32.MaxValue >>> 3)
    let isValidKey (k : Key) : bool =
        (isEmpty k) || 
        ((maxKeyLen >= k.Length) && (0 <> k.[k.Length - 1]))

    /// The CritBitTree structure is exposed. But if you construct
    /// trees directly, you're responsible for correct structure.
    ///
    /// This is a variant CritBitTree where the least (leftmost) key
    /// is held at the root of the tree. This simplifies tree merges.
    /// Additionally, each key is associated with a value. To model
    /// a set of bytestrings, simply set the value type to unit.
    type Tree<'Val> = (Key, Node<'Val>) option
    type Node<'Val> =
        | Leaf of 'Val  
        | Inner of int * Node<'Val> * Key * Node<'Val>

    // TODO: all the things
    //  insert, singleton, union, diff, etc.

