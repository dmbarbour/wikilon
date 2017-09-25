
namespace Stowage
open Data.ByteString

/// Stowage keys are short, non-empty bytestrings used for key-value
/// lookups. Keys should not contain sensitive data because it is very
/// likely keys can be discovered via timing attacks.
///
/// As far as Stowage is concerned, keys are opaque binaries. Client
/// layers are responsible for meaning, access control, and so on.
type Key = ByteString

/// Stowage values are arbitrary bytestrings. However, values are
/// not considered entirely opaque: they may contain resource hash
/// references to other binaries (cf. RscHash.scanHashDeps).
type Val = ByteString

/// A batch of key-values to be compared or written.
type KVMap = BTree<Val>

[<AutoOpen>]
module Key =
    /// Keys are limited to 1..255 bytes. Otherwise, they're opaque.
    let minKeyLen : int = 1
    let maxKeyLen : int = 255
    let inline isValidKey (k : Key) : bool = 
        (maxKeyLen >= k.Length) && (k.Length >= minKeyLen)

[<AutoOpen>]
module Val = // types and utilities also used by implementation

    /// Values should rarely be larger than 100kB, instead using
    /// RscHash references to other values before that point. But
    /// in some rare cases, it might be useful to have values of
    /// several megabytes. For now, I'll permit values of up to
    /// 64MB. 
    let maxValLen : int = (64 * 1024 * 1024)
    let inline isValidVal (v : Val) : bool = 
        (maxValLen >= v.Length)


/// Abstract Key-Value Database




