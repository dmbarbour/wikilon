
namespace Stowage
open Stowage.Hash
open Data.ByteString

[<AutoOpen>]
module Types = // types and utilities also used by implementation

    /// Stowage keys are non-empty bytestrings of at most 255 bytes.
    /// These are only used for key-value lookups. It is also feasible
    /// to iterate or browse keys within the Stowage database.
    ///
    /// As far as Stowage is concerned, keys are opaque binaries. Client
    /// layers are responsible for meaning, access control, and so on.
    type Key = ByteString
    let minKeyLen : int = 1
    let maxKeyLen : int = 255
    let inline isValidKey (k : Key) : bool = 
        (maxKeyLen >= k.Length) && (k.Length >= minKeyLen)

    /// Stowage values are arbitrary bytestrings. However, values are
    /// not considered entirely opaque: they may contain resource hash
    /// references to other binaries (cf. RscHash, scanHashDeps).
    type Val = ByteString

    /// Values do have a maximum size, 1GB. In practice, values that are
    /// larger than a hundred kilobytes should be fragmented, with the 
    /// pieces referenced by secure hash. But working with blocks of a few
    /// megabytes might prove convenient or efficient for some use cases.
    let maxValLen : int = (1024 * 1024 * 1024)
    let inline isValidVal (v : Val) : bool = 
        (maxValLen >= v.Length)

    /// Resource is referenced by a secure hash (Stowage.Hash) of a Val.
    ///
    /// Secure hashes can be considered bearer tokens that authorize
    /// reading of the value referenced. The Stowage DB is careful to
    /// avoid accidental leaks of these capabilities, e.g. via timing
    /// attacks. The Stowage client should also be careful.
    type RscHash = ByteString
    let rscHashLen = Stowage.Hash.validHashLen
    let inline rscHashByte (b : byte) : bool = 
        Stowage.Hash.validHashByte b

   
    /// Scan a value for secure hash resource dependencies.
    ///
    /// This conservatively finds substrings that exactly match the Hash
    /// size and character set (cf. Stowage.Hash). Separate hashes using
    /// spaces, braces, punctuation, parentheses, or other characters. Or
    /// for 'weak' refs, break the hash with a slash or a dash, such that
    /// scanHashDeps won't recognize it.
    let scanHashDeps (fn : 's -> RscHash -> 's) : 's -> Val -> 's =
        let rec loop s v = 
            let hv' = Data.ByteString.dropWhile (not << rscHashByte) v
            if Data.ByteString.isEmpty hv' then s else
            let (h,v') = Data.ByteString.span rscHashByte hv'
            let s' = if (rscHashLen = h.Length) then (fn s h) else s
            loop s' v'
        loop

    /// A batch of key-values to be compared or written.
    type KVMap = Map<Key,Val>


