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

    let rec private sizeLoop ct n = 
        if (0UL = n) then ct else 
        sizeLoop (1+ct) (n>>>7)

    /// Size of a VarNat encoding.
    let size (n : uint64) : int = sizeLoop 1 (n>>>7)

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

    let read (src:ByteSrc) = readLoop 0UL src

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

/// VRef hashes are simply serialized as `{hash}`
module EncRscHash =
    let size : int = 2 + rscHashLen
    let sepL : byte = byte '{'
    let sepR : byte = byte '}'
    
    let write (h:RscHash) (dst:ByteDst) =
        assert(rscHashLen = h.Length)
        ByteStream.writeByte sepL dst
        ByteStream.writeBytes h dst
        ByteStream.writeByte sepR dst

    let read (src:ByteSrc) : RscHash =
        let l = ByteStream.readByte src
        if (sepL <> l) then raise ByteStream.ReadError
        let h = ByteStream.readBytes rscHashLen src
        let r = ByteStream.readByte src
        if (sepR <> r) then raise ByteStream.ReadError
        h

    let codec =
        { new Codec<RscHash> with
            member __.Write h dst = write h dst
            member __.Read db src = read src
            member __.Compact db h = struct(h, size)
        }

module EncVRef =
    let size : int = EncRscHash.size
    let inline write (ref:VRef<_>) (dst:ByteDst) : unit = 
        EncRscHash.write (ref.ID) dst
    let inline read (c:Codec<'V>) (db:DB) (src:ByteSrc) : VRef<'V> =
        VRef.wrap c db (EncRscHash.read src)
    let codec (c:Codec<'V>) =
        { new Codec<VRef<'V>> with
            member __.Write ref dst = write ref dst
            member __.Read db src = read c db src
            member __.Compact _ ref = struct(ref, size)
        }

module EncBRef =
    let size : int = EncVRef.size
    let read (db:DB) (src:ByteSrc) : BRef = 
        EncVRef.read (BRef.c) db src
    let write (ref:BRef) (dst:ByteDst) : unit = 
        EncVRef.write (ref) dst
    let codec : Codec<BRef> = 
        { new Codec<BRef> with
            member __.Write ref dst = write ref dst
            member __.Read db src = read db src
            member __.Compact _ ref = struct(ref, size)
        }

module EncLVRef =
    let size : int = EncVRef.size
    let inline write (ref:LVRef<_>) (dst:ByteDst) : unit =
        EncVRef.write (ref.VRef) dst
    let inline read (c:Codec<'V>) (db:DB) (src:ByteSrc) : LVRef<'V> =
        LVRef.wrap (EncVRef.read c db src)
    let codec (c:Codec<'V>) =
        { new Codec<LVRef<'V>> with
            member __.Write ref dst = write ref dst
            member __.Read db src = read c db src
            member __.Compact _ ref = struct(ref, size)
        }

module EncOption =
    let cNil : byte  = 128uy // EncVarNat 0
    let cSome : byte = 129uy // EncVarNat 1

    let write (cV:Codec<'V>) (vOpt : 'V option) (dst:ByteDst) : unit =
        match vOpt with
        | None -> EncByte.write cNil dst
        | Some v ->
            EncByte.write cSome dst
            cV.Write v dst

    let read (cV:Codec<'V>) (db:DB) (src:ByteSrc) : 'V option =
        let b0 = EncByte.read src
        if(cNil = b0) then None 
        elif(cSome <> b0) then raise ByteStream.ReadError 
        else Some (cV.Read db src)

    let compact (cV:Codec<'V>) (db:DB) (vOpt:'V option) : struct('V option * int) =
        match vOpt with
        | None -> struct(None, 1)
        | Some v ->
            let struct(v',szV) = cV.Compact db v
            struct(Some v', 1+szV)

    let codec (cV:Codec<'V>) =
        { new Codec<'V option> with
            member __.Write vOpt dst = write cV vOpt dst
            member __.Read db src = read cV db src
            member __.Compact db vOpt = compact cV db vOpt
        }

/// Arrays are encoded with size (as varnat) followed by every element
/// in sequence without separators. It's assumed that the elements are
/// distinguishable on parse and protect their own RscHash references. 
module EncArray =
    let write (cV:Codec<'V>) (a: 'V array) (dst:ByteDst) =
        EncVarNat.write (uint64 a.Length) dst
        Array.iter (fun e -> Codec.write cV e dst) a

    let read (cV:Codec<'V>) (db:DB) (src:ByteSrc) : 'V array =
        let len = int (EncVarNat.read src)
        let arr = Array.zeroCreate len
        for ix = 0 to arr.Length - 1 do
            arr.[ix] <- Codec.read cV db src
        arr

    /// compact in-place
    let compact' (cV:Codec<'V>) (db:DB) (arr:'V array) =
        let mutable szA = EncVarNat.size (uint64 arr.Length)
        for ix = 0 to (arr.Length - 1) do
            let struct(v',szV) = cV.Compact db (arr.[ix])
            arr.[ix] <- v'
            szA <- (szA + szV)
        struct(arr, szA)

    /// copying compact
    let inline compact cV db arr = compact' cV db (Array.copy arr)

    // NOTE: the Compact operation in this case is an in-place update!
    //   you may need to copy the array if that isn't your intention.
    let codec (cV:Codec<'V>) =
        { new Codec<'V array> with
            member __.Write arr dst = write cV arr dst
            member __.Read db src = read cV db src
            member __.Compact db arr = compact cV db arr
        }


/// Encode list via List.toArray. Consider `Codec.list` if you want
/// list via option.
module EncList =
    let inline write (cV:Codec<'V>) (lst: 'V list) (dst:ByteDst) : unit =
        EncArray.write cV (List.toArray lst) dst
    let inline read (cV:Codec<'V>) (db:DB) (src:ByteSrc) : 'V list =
        List.ofArray (EncArray.read cV db src)
    let inline compact (cV:Codec<'V>) (db:DB) (lst : 'V list) =
        let struct(arr,szA) = EncArray.compact' cV db (List.toArray lst)
        struct(List.ofArray arr, szA)
    let codec (cV:Codec<'V>) =
        { new Codec<'V list> with
            member __.Write lst dst = write cV lst dst
            member __.Read db src = read cV db src
            member __.Compact db lst = compact cV db lst
        }

module EncPair =
    let codec (cA:Codec<'A>) (cB:Codec<'B>) =
        { new Codec<'A * 'B> with
            member __.Write ((a,b)) dst =
                cA.Write a dst
                cB.Write b dst
            member __.Read db src =
                let a = cA.Read db src
                let b = cB.Read db src
                (a,b)
            member __.Compact db ((a,b)) =
                let struct(a',szA) = cA.Compact db a
                let struct(b',szB) = cB.Compact db b
                struct((a',b'),(szA + szB))
        }

module EncTriple =
    let codec (cA:Codec<'A>) (cB:Codec<'B>) (cC:Codec<'C>) =
        { new Codec<'A * 'B * 'C> with
            member __.Write ((a,b,c)) dst =
                cA.Write a dst
                cB.Write b dst
                cC.Write c dst
            member __.Read db src =
                let a = cA.Read db src
                let b = cB.Read db src
                let c = cC.Read db src
                (a,b,c)
            member __.Compact db ((a,b,c)) =
                let struct(a',szA) = cA.Compact db a
                let struct(b',szB) = cB.Compact db b
                let struct(c',szC) = cC.Compact db c
                struct((a',b',c'),(szA + szB + szC))
        }

