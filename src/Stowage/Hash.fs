
(* Stowage simply uses a 280-bit Blake2b hash function, but encodes
 * this result in 56 characters of an unusual base32 alphabet:
 *
 *     bcdfghjklmnpqrstBCDFGHJKLMNPQRST 
 *
 * That is, we simply use the first sixteen consonants of the alphabet,
 * in lower then upper case. This resists accidental construction of 
 * anything otherwise meaningful to humans or machines.
 *
 * These hashes are used as references between immutable binary data,
 * enabling flexible representations. We use conservative GC, so hashes
 * must be separated by non-hash characters. 
 *)
namespace Stowage
open Data.ByteString
open Konscious.Security.Cryptography

module Hash =
    /// the base32 alphabet used for Stowage hash references.
    let alphabet = "bcdfghjklmnpqrstBCDFGHJKLMNPQRST"

    /// number of bits encoded in hash
    let hashBitLen : int = 280

    let inline private hdiv n = 
        assert (0 = (hashBitLen % n))
        (hashBitLen / n)

    /// number of base32 ASCII characters in hash 
    let validHashLen : int = hdiv 5 

    /// expected length of hash in bytes
    let hashByteLen : int = hdiv 8

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
    let validHashElem (n : int) : bool = 
        if ((0 <= n) && (n < alphabool.Length))
            then alphabool.[n]
            else false

    /// test whether a byte array currently looks like a hash
    let validHash (b : byte[]) : bool =
        if (validHashLen <> b.Length) then false else
        not (Array.exists (int >> validHashElem >> not) b)

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
        assert (hashByteLen = src.Length)
        let dst = Array.zeroCreate validHashLen
        // seven blocks of forty bits = 280 bits
        do b32e40 dst src 6
        do b32e40 dst src 5
        do b32e40 dst src 4
        do b32e40 dst src 3
        do b32e40 dst src 2
        do b32e40 dst src 1
        do b32e40 dst src 0
        assert (hashBitLen = 280)
        dst

    /// basic bytestring hash
    let hash (s : ByteString) : ByteString =
        use alg = new HMACBlake2B(hashBitLen)
        let bytes = alg.ComputeHash(s.UnsafeArray, s.Offset, s.Length)
        Data.ByteString.unsafeCreateA (b32enc bytes)

    /// compute a hash result from binary
    let hashArray (src : byte[]) : byte[] = 
        use alg = new HMACBlake2B(hashBitLen)
        b32enc (alg.ComputeHash(src))

    /// compute a hash result from stream
    let hashStream (src : System.IO.Stream) : byte[] = 
        use alg = new HMACBlake2B(hashBitLen)
        b32enc (alg.ComputeHash(src))



