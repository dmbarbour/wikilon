namespace Stowage
open Data.ByteString
open Konscious.Security.Cryptography

/// A RscHash is simply a 72-character bytestring encoded using
/// an unusual base32 alphabet: bcdfghjklmnpqrstBCDFGHJKLMNPQRST.
///
/// This string is computed from the Blake2B 360-bit secure hash
/// of a binary, and is used as a name or capability to read a
/// binary resource.
///
/// Note on 10/11/2017: from 280 bits to 360 bits.
type RscHash = ByteString

module RscHash =
    /// the base32 alphabet used for Stowage hash references.
    let alphabet = "bcdfghjklmnpqrstBCDFGHJKLMNPQRST"

    /// number of bits encoded in hash
    let hashBitLen : int = 360

    let inline private hdiv n = 
        assert (0 = (hashBitLen % n))
        (hashBitLen / n)

    /// number of base32 ASCII characters in hash 
    let size : int = hdiv 5 

    /// expected length of hash in bytes
    let private hashByteLen : int = hdiv 8

    // alphabet encoded as bytes array
    let private alphabyte : byte[] = 
        let s = System.Text.Encoding.UTF8.GetBytes(alphabet)
        assert (32 = s.Length)
        s

    // table lookup for presence of data
    let private alphabool : bool[] =
        let memb x = Array.exists (int >> ((=) x)) alphabyte
        Array.init 256 memb 

    // test whether an element is valid within a UTF8 or ASCII hash.
    let isHashByte (b : byte) : bool = alphabool.[int b] 

    // encode forty bits from src to dst.
    let inline private b32e40 (dst : byte[]) (src : byte[]) off =
        let dst_off = (off * 8)
        let src_off = (off * 5)
        let inline r ix = src.[src_off + ix]
        let inline w ix v = dst.[dst_off + ix] <- alphabyte.[int v]
        // read forty bits of data
        let i4 = r 4
        let i3 = r 3
        let i2 = r 2
        let i1 = r 1
        let i0 = r 0
        // encode data into eight bytes
        do w 7 (((i4 &&& 0x1Fuy)      ))
        do w 6 (((i4 &&& 0xE0uy) >>> 5) |||
                ((i3 &&& 0x03uy) <<< 3))
        do w 5 (((i3 &&& 0x7Cuy) >>> 2))
        do w 4 (((i3 &&& 0x80uy) >>> 7) |||
                ((i2 &&& 0x0Fuy) <<< 1))
        do w 3 (((i2 &&& 0xF0uy) >>> 4) |||
                ((i1 &&& 0x01uy) <<< 4))
        do w 2 (((i1 &&& 0x3Euy) >>> 1))
        do w 1 (((i1 &&& 0xC0uy) >>> 6) |||
                ((i0 &&& 0x07uy) <<< 2))
        do w 0 (((i0 &&& 0xF8uy) >>> 3))

    // perform a base32 encoding of the Blake2 hash.
    let private b32enc (src : byte[]) : byte[] =
        assert ((45 = src.Length) && (72 = size))
        let dst = Array.zeroCreate size
        do b32e40 dst src 8
        do b32e40 dst src 7
        do b32e40 dst src 6
        do b32e40 dst src 5
        do b32e40 dst src 4
        do b32e40 dst src 3
        do b32e40 dst src 2
        do b32e40 dst src 1
        do b32e40 dst src 0
        dst

    /// basic bytestring hash
    let hash (s : ByteString) : ByteString =
        use alg = new HMACBlake2B(hashBitLen)
        let bytes = alg.ComputeHash(s.UnsafeArray, s.Offset, s.Length)
        BS.unsafeCreateA (b32enc bytes)

    /// Fold over RscHash dependencies represented within a value.
    ///
    /// Find substrings that look like hashes - appropriate size and
    /// character set, separated by non-hash characters. Useful for
    /// conservative GC of resources.
    let rec foldHashDeps (fn : 's -> RscHash -> 's) (s:'s) (v:ByteString) : 's =
        if (v.Length < size) then s else
        let hv' = BS.dropWhile (not << isHashByte) v
        let struct(h,v') = BS.span isHashByte hv'
        let s' = if (size = h.Length) then (fn s h) else s
        foldHashDeps fn s' v'

    /// Iterate through RscHash dependencies in a value.
    /// Recognizes same RscHash substrings as foldHashDeps.
    let rec iterHashDeps (fn : RscHash -> unit) (v:ByteString) : unit =
        if (v.Length < size) then () else
        let hv' = BS.dropWhile (not << isHashByte) v
        let struct(h,v') = BS.span isHashByte hv'
        if (size = h.Length)
            then fn h
        iterHashDeps fn v'


