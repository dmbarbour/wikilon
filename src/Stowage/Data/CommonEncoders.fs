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
        let rec loop ct n = if (0UL = n) then ct else loop (1+ct) (n>>>7)
        loop 1 (n>>>7)

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

    let codec =
        { new Codec<uint64> with
            member __.Write o n = write o n
            member __.Read db i = read i
            member __.Compact db n = struct(n, size n)
        }


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
        let iAbs = int64 (n/2UL)
        if (0UL = (n % 2UL)) then iAbs
                             else (-1L)-(int64)(n/2UL)

    // zig-zag conversions could be optimized using bit-level
    // manipulations instead of a conditional expression. But
    // it isn't very worthwhile.

    let inline size (i : int64) : int = EncVarNat.size (zzEncode i)
    let inline write (o : System.IO.Stream) (i : int64) = EncVarNat.write o (zzEncode i)
    let inline read (i : System.IO.Stream) : int64 = zzDecode (EncVarNat.read i)

    let codec =
        { new Codec<int64> with
            member __.Write o iVal = write o iVal
            member __.Read db i = read i
            member __.Compact db iVal = struct(iVal, size iVal)
        }

module EncByte =
    let size : int = 1
    let inline write (o:System.IO.Stream) (b:byte) : unit = o.WriteByte(b)
    let read (i:System.IO.Stream) : byte =
        let n = i.ReadByte()
        if (n < 0) then failwith "insufficient data"
        assert (n < 256)
        byte n
    let expect (i:System.IO.Stream) (b:byte) : unit =
        let r = read i
        if(r <> b) then failwith (sprintf "unexpected byte (expect %A got %A)" b r)
    let codec =
        { new Codec<byte> with
            member __.Write o b = write o b
            member __.Read db i = read i
            member __.Compact db b = struct(b,size)
        }

/// ByteString encoding:
///  (size)(data)(sep)
///
/// (size) uses EncVarNat - i.e. (0..127)*(128..255)
/// (data) is encoded raw
/// (sep) is simply an SP (32), but is conditional:
///   (sep) is added iff data terminates in rscHashByte.
///
/// This construction ensures easy recognition of secure hash resource
/// references represented within the bytestring. 
module EncBytes =

    let sepReq (b:ByteString) : bool =
        (b.Length > 0) && (rscHashByte (b.[b.Length - 1]))
    
    let sep : byte = 32uy

    let size (b:ByteString) : int =
        EncVarNat.size (uint64 b.Length) 
            + b.Length 
            + (if (sepReq b) then 1 else 0)

    let inline writeRaw (o:System.IO.Stream) (b:ByteString) : unit =
        o.Write(b.UnsafeArray, b.Offset, b.Length)

    let write (o:System.IO.Stream) (b:ByteString) : unit =
        EncVarNat.write o (uint64 b.Length)
        writeRaw o b
        if (sepReq b) then EncByte.write o sep

    let readLen (len:int) (i:System.IO.Stream) : ByteString =
        let arr = Array.zeroCreate len
        let rct = i.Read(arr, 0, arr.Length)
        if (rct <> arr.Length) then failwith "insufficient data"
        BS.unsafeCreateA arr

    let read (i:System.IO.Stream) : ByteString =
        let len = EncVarNat.read i
        if (len > uint64 System.Int32.MaxValue) then failwith "int overflow"
        let b = readLen (int len) i
        if (sepReq b) then EncByte.expect i sep
        b

    let toInputStream (b:ByteString) : System.IO.Stream =
        upcast (new System.IO.MemoryStream(b.UnsafeArray, b.Offset, b.Length, false)) 

    let codec =
        { new Codec<ByteString> with
            member __.Write o b = write o b
            member __.Read db i = read i
            member __.Compact db b = struct(b,size b)
        }

/// Resource Hash encoding: {Hash}
module EncRscHash =

    let size : int = 2 + rscHashLen
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

    let codec =
        { new Codec<RscHash> with
            member __.Write o h = write o h
            member __.Read db i = read i
            member __.Compact db h = struct(h, size)
        }

/// Same as RscHash encoding, but incref and wrap Rsc upon read
module EncRsc =

    let size : int = EncRscHash.size
    let inline write (o:System.IO.Stream) (r:Rsc) : unit = 
        EncRscHash.write o (r.ID)
    let inline read (db:DB) (i:System.IO.Stream) : Rsc = 
        new Rsc(db, EncRscHash.read i, true)
    let codec =
        { new Codec<Rsc> with
            member __.Write o r = write o r
            member __.Read db i = read db i
            member __.Compact db r = struct(r, size)
        }

/// a Rsc option is convenient for 'nullable' resource references.
/// Encoding is '_' for a hole, otherwise overlaps resource encoding.
/// Consequently, it is easy to transition Rsc to RscOpt (but not the
/// other direction).
module EncRscOpt =
    let cNone = byte '_'

    let size (ro : Rsc option) : int = 
        match ro with
        | None -> 1
        | Some r -> EncRsc.size

    let write (o : System.IO.Stream) (ro : Rsc option) : unit =
        match ro with
        | None -> EncByte.write o cNone
        | Some r -> EncRsc.write o r
            
    let read (db:DB) (i:System.IO.Stream) : Rsc option =
        let b = EncByte.read i
        if(cNone = b) then None else
        if(b <> EncRscHash.cbL) then failwith "expecting resource"
        let rsc = new Rsc(db, EncRscHash.read i, true)
        EncByte.expect i (EncRscHash.cbR)
        Some rsc

    let codec =
        { new Codec<Rsc option> with
            member __.Write o ro = write o ro
            member __.Read db i = read db i
            member __.Compact db ro = struct(ro,size ro)
        } 


/// Same as bytestrings, but incref and wrap Binary upon read.
module EncBin =
    let inline size (b:Binary) : int = EncBytes.size (b.Bytes)
    let inline write (o:System.IO.Stream) (b:Binary) : unit =
        EncBytes.write o (b.Bytes)
    let inline read (db:DB) (i:System.IO.Stream) : Binary =
        new Binary(db, EncBytes.read i, true)
    let codec =
        { new Codec<Binary> with
            member __.Write o b = write o b
            member __.Read db i = read db i
            member __.Compact db b = struct(b,size b)
        }

module Codec =

    let inline write (c:Codec<'T>) (o:System.IO.Stream) (v:'T) : unit = c.Write o v
    let inline read (c:Codec<'T>) (db:DB) (i:System.IO.Stream) : 'T = c.Read db i

    let inline writeBytes (c:Codec<'T>) (v:'T) : ByteString =
        use o = new System.IO.MemoryStream()
        c.Write o v
        BS.unsafeCreateA (o.ToArray())

    let inline writeBinary (c:Codec<'T>) (db:DB) (v:'T) : Binary =
        let result = new Binary(db, writeBytes c v, true)
        System.GC.KeepAlive v
        result

    let readBytes (c:Codec<'T>) (db:DB) (b:ByteString) : 'T =
        use i = EncBytes.toInputStream b
        let result = c.Read db i
        if (i.Position <> i.Length) 
            then failwith "unread bytes"
        result

    let inline readBinary (c:Codec<'T>) (b:Binary) : 'T =
        let result = readBytes c (b.DB) (b.Bytes)
        System.GC.KeepAlive b
        result

    /// stow a value without first compacting it
    let inline stow' (c:Codec<'T>) (db:DB) (v:'T) : Rsc =
        let result = Rsc.Stow db (writeBytes c v)
        System.GC.KeepAlive v
        result

    let inline compactSz (c:Codec<'T>) (db:DB) (v:'T) : struct('T * int) =
        c.Compact db v

    let inline compact (c:Codec<'T>) (db:DB) (v:'T) : 'T = 
        let struct(v',sz) = compactSz c db v
        v'

    /// compact and stow together (common use case)
    let inline stow (c:Codec<'T>) (db:DB) (v:'T) : Rsc = 
        stow' c db (compact c db v)
        
    let inline load (c:Codec<'T>) (r:Rsc) : 'T =
        let result = readBytes c r.DB (loadRscDB r.DB r.ID)
        System.GC.KeepAlive r // prevent GC of resource during parse
        result

    // TODO: consider developing generic combinators:
    //  codec<V> to codec<V list> or codec<V list>
    //  codec<A> and codec<B> to codec<(A,B)> or codec<Choice<A,B>>

