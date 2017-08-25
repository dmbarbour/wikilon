namespace Stowage
open Data.ByteString

/// A VarNat is an efficient encoding for a natural number in 
/// base128, such that we can easily distinguish the end of 
/// the number. 
///
/// The encoding uses (0..127)*(128..255). This ensures there
/// is no interference between the final byte of the VarNat and
/// any RscHash dependencies.
module EncVarNat =

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
module EncVarInt =

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

    let inline size (i : int64) : int = EncVarNat.size (zzEncode i)
    let inline write (o : System.IO.Stream) (i : int64) = EncVarNat.write o (zzEncode i)
    let inline read (i : System.IO.Stream) : int64 = zzDecode (EncVarNat.read i)

module EncByte =
    let inline size (b:byte) : int = 1
    let inline write (o:System.IO.Stream) (b:byte) : unit = o.WriteByte(b)
    let read (i:System.IO.Stream) : byte =
        let n = i.ReadByte()
        if (n < 0) then failwith "insufficient data"
        assert (n < 256)
        byte n
    let expect (i:System.IO.Stream) (b:byte) : unit =
        let r = read i
        if(r <> b) then failwith (sprintf "unexpected byte (expect %A got %A)" b r)

/// ByteString encoding:
///  (size)(data)
///
/// (size) uses EncVarNat - i.e. (0..127)*(128..255)
/// (data) is encoded raw
module EncBytes =

    let size (b:ByteString) : int =
        EncVarNat.size (uint64 b.Length) + b.Length

    let inline writeRaw (o:System.IO.Stream) (b:ByteString) : unit =
        o.Write(b.UnsafeArray, b.Offset, b.Length)

    let write (o:System.IO.Stream) (b:ByteString) : unit =
        EncVarNat.write o (uint64 b.Length)
        writeRaw o b

    let readLen (len:int) (i:System.IO.Stream) : ByteString =
        let arr = Array.zeroCreate len
        let rct = i.Read(arr, 0, arr.Length)
        if (rct <> arr.Length) then failwith "insufficient data"
        Data.ByteString.unsafeCreateA arr

    let read (i:System.IO.Stream) : ByteString =
        let len = EncVarNat.read i
        if (len > uint64 System.Int32.MaxValue) then failwith "int overflow"
        readLen (int len) i

    let toInputStream (b:ByteString) : System.IO.Stream =
        (new System.IO.MemoryStream(b.UnsafeArray, b.Offset, b.Length, false)) 
            :> System.IO.Stream

/// Resource Hash encoding: {Hash}
module EncRscHash =

    let inline size (h:RscHash) : int = 
        assert(h.Length = rscHashLen)
        2 + rscHashLen

    let cbL = 123uy
    let cbR = 125uy

    let write (o:System.IO.Stream) (h:RscHash) : unit =
        assert(h.Length = rscHashLen)
        o.WriteByte(cbL)
        EncBytes.writeRaw o h
        o.WriteByte(cbR)

    let read (i:System.IO.Stream) : RscHash =
        EncByte.expect i cbL
        let h = EncBytes.readLen rscHashLen i
        EncByte.expect i cbR
        h

/// Same as RscHash encoding, but incref and wrap Rsc upon read
module EncRsc =

    let inline size (r:Rsc) : int = EncRscHash.size (r.ID)
    let inline write (o:System.IO.Stream) (r:Rsc) : unit = 
        EncRscHash.write o (r.ID)
    let inline read (db:DB) (i:System.IO.Stream) : Rsc = 
        new Rsc(db, EncRscHash.read i, true)

/// Stowage Binary Encoding: (bytes)(sep)
///
/// The (sep) byte helps guard recognition of references in
/// data, and is added if and only if the final byte is a 
/// valid rscHashByte. The (sep) byte is simply SP (32).
///
/// If it's known that (sep) is unnecessary in context, we
/// may simply use EncBytes instead of EncBin. OTOH, the 
/// overhead for one extra separator character is minor.
/// Due to how (bytes) encodes (size), no prefix separator
/// is required.
module EncBin =

    let sep : byte = 32uy

    let sepReq (b : ByteString) : bool =
        (b.Length > 0) && (rscHashByte (b.[b.Length - 1]))

    let size (b:Binary) : int = 
        EncBytes.size b.Bytes
          + (if sepReq b.Bytes then 1 else 0)

    let write (o:System.IO.Stream) (b:Binary) : unit =
        EncBytes.write o (b.Bytes)
        if sepReq (b.Bytes) then o.WriteByte(sep)

    let read (db:DB) (i:System.IO.Stream) : Binary =
        let s = EncBytes.read i
        if sepReq s then EncByte.expect i sep
        new Binary(db, s, true)

        


