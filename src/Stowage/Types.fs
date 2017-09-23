
namespace Stowage
open Stowage.Hash
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
/// references to other binaries (cf. RscHash, scanHashDeps).
type Val = ByteString

/// A batch of key-values to be compared or written.
type KVMap = BTree<Val>

/// A Resource is referenced by a secure hash (Stowage.Hash) of a Val.
///
/// Secure hashes can be considered bearer tokens that authorize
/// reading of the value referenced. The Stowage DB is careful to
/// avoid accidental leaks of full capabilities via timing attacks.
/// The Stowage client should similarly be careful.
type RscHash = ByteString

[<AutoOpen>]
module Key =
    /// Keys are limited to 1..255 bytes. Otherwise, they're opaque.
    let minKeyLen : int = 1
    let maxKeyLen : int = 255
    let inline isValidKey (k : Key) : bool = 
        (maxKeyLen >= k.Length) && (k.Length >= minKeyLen)

[<AutoOpen>]
module Val = // types and utilities also used by implementation

    /// Values do have a maximum size, 1GB. In practice, values that are
    /// larger than a hundred kilobytes should be fragmented, with the 
    /// pieces referenced by secure hash. But working with blocks of a few
    /// megabytes might prove convenient or efficient for some use cases.
    let maxValLen : int = (1024 * 1024 * 1024)
    let inline isValidVal (v : Val) : bool = 
        (maxValLen >= v.Length)

[<AutoOpen>]
module RscHash =
    /// Resource hashes 
    let rscHashLen = Stowage.Hash.validHashLen
    let inline rscHashByte (b : byte) : bool = Stowage.Hash.validHashByte b
    let inline hash (v:Val) : RscHash = Stowage.Hash.hash v
  

