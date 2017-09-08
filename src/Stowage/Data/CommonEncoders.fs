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

    let rec private whb (n:uint64) (dst:ByteDst) : unit =
        if (0UL = n) then () else
        whb (n >>> 7) dst
        let lob = byte (n &&& 0x7FUL)
        ByteStream.writeByte lob dst

    /// Write a VarNat to an output stream.
    let write (n:uint64) (dst:ByteDst) : unit =
        whb (n >>> 7) dst
        let lob = 0x80uy ||| byte (n &&& 0x7FUL)
        ByteStream.writeByte lob dst

    let rec private readLoop (acc:uint64) (src:ByteSrc) : uint64 =
        let b = ByteStream.readByte src
        let acc' = (acc <<< 7) + (uint64 (b &&& 0x7Fuy))
        if (0uy = (0x80uy &&& b)) 
            then acc'
            else readLoop acc' src 

    let inline read (src:ByteSrc) = readLoop 0UL src

    let codec =
        { new Codec<uint64> with
            member __.Write n dst = write n dst
            member __.Read db src = read src
            member __.Compact db n = struct(n, size n)
        }


/// A Variable Integer
///
/// This simply translates to a VarNat with a ZigZag encoding.
/// {0 → 0; -1 → 1; 1 → 2; -2 → 3; 2 → 4; ...}
module EncVarInt =

    /// zig-zag conversions. 
    let zzEncode (i : int64) : uint64 = 
        if(i < 0L) 
            then (2UL * uint64 ((-1L)-i)) + 1UL
            else (2UL * uint64 i)
    let zzDecode (n : uint64) : int64 =
        let iAbs = int64 (n/2UL)
        if (0UL = (n &&& 1UL)) 
            then iAbs
            else (-1L)-(int64)(n/2UL)

    // zig-zag conversions could be optimized using bit-level
    // manipulations instead of a conditional expression. But
    // it isn't very worthwhile.

    let inline size (i:int64) : int = EncVarNat.size (zzEncode i)
    let inline write (i:int64) (dst:ByteDst) = EncVarNat.write (zzEncode i) dst
    let inline read (src:ByteSrc) : int64 = zzDecode (EncVarNat.read src)

    let codec =
        { new Codec<int64> with
            member __.Write i dst = write i dst
            member __.Read db src = read src
            member __.Compact db i = struct(i, size i)
        }

module EncByte =
    let size : int = 1
    let inline write (b:byte) (dst:ByteDst) : unit = ByteStream.writeByte b dst
    let inline read (src:ByteSrc) : byte = ByteStream.readByte src
    let expect (b:byte) (src:ByteSrc) : unit =
        let r = read src
        if(r <> b) then raise ByteStream.ReadError
    let codec =
        { new Codec<byte> with
            member __.Write b dst = write b dst
            member __.Read db src = read src
            member __.Compact db b = struct(b,size)
        }

/// Trivial ByteString encoding.
///
/// This can only be used as the final writer for a ByteString, 
/// since it will always read the full stream. Mostly, this is
/// intended so we can use the codec for raw binary VRefs.
module EncRawBytes =

    let inline size (b:ByteString) : int = b.Length
    let inline write (b:ByteString) (dst:ByteDst) : unit =
        ByteStream.writeBytes b dst
    let inline read (src:ByteSrc) : ByteString =
        ByteStream.readRem src
    let codec : Codec<ByteString> =
        { new Codec<ByteString> with
            member __.Write b dst = write b dst
            member __.Read _ src = read src
            member __.Compact _ b = struct(b, size b)
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

    let sep : byte = 32uy
    let sepReq (b:ByteString) : bool =
        (b.Length > 0) && (rscHashByte (b.[b.Length - 1]))

    let size (b:ByteString) : int =
        EncVarNat.size (uint64 b.Length) 
            + b.Length 
            + (if (sepReq b) then 1 else 0)

    let write (b:ByteString) (dst:ByteDst) : unit =
        EncVarNat.write (uint64 b.Length) dst
        ByteStream.writeBytes b dst
        if (sepReq b) then EncByte.write sep dst

    let read (src:ByteSrc) : ByteString =
        let len = EncVarNat.read src
        let b = ByteStream.readBytes (int len) src
        if (sepReq b) then EncByte.expect sep src
        b

    let codec =
        { new Codec<ByteString> with
            member __.Write b dst = write b dst
            member __.Read db src = read src
            member __.Compact db b = struct(b,size b)
        }

/// Resource Hash encoding: {Hash}
module EncRscHash =

    let size : int = 2 + rscHashLen
    let cbL = 123uy
    let cbR = 125uy

    let write (h:RscHash) (dst:ByteDst) : unit =
        if(h.Length <> rscHashLen)
            then invalidArg "h" "not a resource hash"
        EncByte.write cbL dst
        ByteStream.writeBytes h dst
        EncByte.write cbR dst

    let read (src:ByteSrc) : RscHash =
        EncByte.expect cbL src
        let h = ByteStream.readBytes rscHashLen src
        EncByte.expect cbR src
        h

    let codec =
        { new Codec<RscHash> with
            member __.Write h dst = write h dst
            member __.Read db src = read src
            member __.Compact db h = struct(h, size)
        }

/// Same as RscHash encoding, but incref and wrap Rsc upon read
module EncRsc =

    let size : int = EncRscHash.size
    let inline write (r:Rsc) (dst:ByteDst) : unit = 
        EncRscHash.write (r.ID) dst
    let inline read (db:DB) (src:ByteSrc) : Rsc = 
        new Rsc(db, EncRscHash.read src, true)

    let codec =
        { new Codec<Rsc> with
            member __.Write rsc dst = write rsc dst
            member __.Read db src = read db src
            member __.Compact db rsc = struct(rsc, size)
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
        | Some rsc -> EncRsc.size

    let write (ro : Rsc option) (dst : ByteDst)  : unit =
        match ro with
        | None -> EncByte.write cNone dst
        | Some rsc -> EncRsc.write rsc dst
            
    let read (db:DB) (src:ByteSrc) : Rsc option =
        let b = EncByte.read src
        if(cNone = b) then None else
        let rsc = new Rsc(db, EncRscHash.read src, true)
        EncByte.expect (EncRscHash.cbR) src
        Some rsc

    let codec =
        { new Codec<Rsc option> with
            member __.Write ro dst = write ro dst
            member __.Read db src = read db src
            member __.Compact db ro = struct(ro,size ro)
        } 


/// Same as bytestrings, but incref and wrap Binary upon read.
module EncBin =
    let inline size (b:Binary) : int = EncBytes.size (b.Bytes)
    let inline write (b:Binary) (dst:ByteDst) : unit =
        EncBytes.write (b.Bytes) dst
    let inline read (db:DB) (src:ByteSrc) : Binary =
        new Binary(db, EncBytes.read src, true)
    let codec =
        { new Codec<Binary> with
            member __.Write b dst = write b dst
            member __.Read db src = read db src
            member __.Compact db b = struct(b,size b)
        }

