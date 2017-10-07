namespace Stowage
open Data.ByteString

/// Stowage Database above a Memory-mapped Database
///
/// This uses the memory mapped file database LMDB under the hood,
/// and it serves as a primary implementation for Stowage.
module MDB =
    let maxSafeKeyLen = 255
    let minSafeKeyLen = 1
    let keyHashPrefix = byte '#'

    /// Safe keys must have valid size and may not start with `#`.
    let safeKey (k:ByteString) : bool =
        (maxSafeKeyLen >= k.Length) &&
        (k.Length >= minSafeKeyLen) &&
        (k.[0] <> keyHashPrefix)

    /// Mangles unsafe keys to `#SecureHash` using half of RscHash.
    let mangle (k:ByteString) : DB.Key =
        if safeKey k then k else
        let h = BS.take (RscHash.size / 2) (RscHash.hash k)
        BS.cons keyHashPrefix h


    // TODO:
    //
    // Construct LMDB version of DB.Storage
    // Direct construction of the DB

