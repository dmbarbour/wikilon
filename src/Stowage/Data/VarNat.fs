namespace Stowage

/// A VarNat is an efficient encoding for a natural number in 
/// base128, such that we can easily distinguish the end of 
/// the number. 
///
/// The encoding uses (0..127)*(128..255). This ensures there
/// is no interference between the final byte of the VarNat and
/// any RscHash dependencies.
module VarNat =

    /// Size of a VarNat encoding.
    let size (n : uint64) : int =
        let rec loop ct n = if (0UL = n) then ct else loop (1+ct) (n/128UL)
        loop 1 (n/128UL)

    /// Write a VarNat to an output stream.
    let write (o : System.IO.Stream) (n : uint64) : unit =
        assert(o.CanWrite)
        let rec whb n =
            if (0UL = n) then () else
            whb (n/128UL)
            o.WriteByte(byte (n%128UL))
        whb (n/128UL)
        o.WriteByte(128uy + byte(n%128UL))

    /// Read a VarNat from an input stream.
    let read (i : System.IO.Stream) : uint64 =
        assert(i.CanRead)
        let rec readLoop acc =
            let b = i.ReadByte()
            if (b < 0) then invalidOp "read varNat from empty stream"
            let acc' = ((128UL * acc) + (uint64 (b &&& 0x7F)))
            if (b < 128) then readLoop acc' else acc'
        readLoop 0UL


/// A Variable Integer
///
/// This simply translates to a VarNat with a ZigZag encoding.
/// {0 → 0; -1 → 1; 1 → 2; -2 → 3; 2 → 4; ...}
module VarInt =

    /// zig-zag conversions. 
    let zzEncode (i : int64) : uint64 = 
        if(i < 0L) then (2UL * uint64 ((-1L)-i)) + 1UL
                   else (2UL * uint64 i)
    let zzDecode (n : uint64) : int64 =
        if (0UL = (n % 2UL)) then (int64)(n/2UL)
                             else (-1L)-(int64)(n/2UL)

    // zig-zag conversions could be optimized using bit-level
    // manipulations instead of a conditional expression. But
    // it isn't very worthwhile.

    let inline size (i : int64) : int = VarNat.size (zzEncode i)
    let inline write (o : System.IO.Stream) (i : int64) = VarNat.write o (zzEncode i)
    let inline read (i : System.IO.Stream) : int64 = zzDecode (VarNat.read i)

        
