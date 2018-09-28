namespace Stowage
open Data.ByteString

/// A VarNat is an efficient encoding for a natural number in 
/// base128. The high bit in each byte is `1` to indicate there
/// are bytes remaining. This is an efficient encoding when most
/// encoded values are close to zero.
module EncVarNat =

    let rec private sizeLoop ct n = 
        if (0UL = n) then ct else 
        sizeLoop (1+ct) (n>>>7)

    /// Size of a VarNat encoding.
    let size (n : uint64) : SizeEst = uint64 (sizeLoop 1 (n>>>7))

    let rec private whb (n:uint64) (dst:ByteDst) : unit =
        if (0UL = n) then () else
        whb (n >>> 7) dst
        let b = 0x80uy ||| byte (n &&& 0x7FUL)
        ByteStream.writeByte b dst

    /// Write a VarNat to an output stream.
    let write (n:uint64) (dst:ByteDst) : unit =
        whb (n >>> 7) dst
        let b = byte (n &&& 0x7FUL)
        ByteStream.writeByte b dst

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

    let inline size (i:int64) : SizeEst = EncVarNat.size (zzEncode i)
    let inline write (i:int64) (dst:ByteDst) = EncVarNat.write (zzEncode i) dst
    let inline read (src:ByteSrc) : int64 = zzDecode (EncVarNat.read src)

    let codec =
        { new Codec<int64> with
            member __.Write i dst = write i dst
            member __.Read db src = read src
            member __.Compact db i = struct(i, size i)
        }

module EncByte =
    let size : SizeEst = 1UL
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
///  (size)(data)
///
/// (size) uses EncVarNat
/// (data) is encoded raw
///
/// This is intended for encoding bytestrings mixed with other data.
module EncBytes =
    let size (b:ByteString) : SizeEst =
        let len = uint64 b.Length
        (EncVarNat.size len) + len

    let write (b:ByteString) (dst:ByteDst) : unit =
        EncVarNat.write (uint64 b.Length) dst
        ByteStream.writeBytes b dst

    let read (src:ByteSrc) : ByteString =
        let len = EncVarNat.read src
        ByteStream.readBytes (int len) src

    let codec =
        { new Codec<ByteString> with
            member __.Write b dst = write b dst
            member __.Read db src = read src
            member __.Compact db b = struct(b,size b)
        }

/// Wrap data with a size header.
///
///    (size)(data)
///
/// This will write data twice: write to a temporary buffer to find
/// exact size, then a fast copy to the destination buffer, with the
/// size metadata. Upon read, we'll isolate the codec to the data 
/// within the sized volume. It's assumed that any RscHash references
/// in data are protected from adjacency concerns, like the `{hash}`.
module EncSized =

    let codec (c:Codec<'V>) =
        { new Codec<'V> with
            member __.Write v dst =
                let s = Codec.writeBytes c v
                EncBytes.write s dst
            member __.Read db src =
                let s = EncBytes.read src
                Codec.readBytes c db s
            member __.Compact db v =
                let struct(v',szV) = c.Compact db v
                struct(v', (EncVarNat.size szV) + szV)
        }
                

/// Raw ByteString Encoder. 
///
/// This codec is suitable for a VRef<ByteString> that consists of only
/// the bytestring and no other data. On read, it trivially consumes all 
/// remaining data from the input.
module EncBytesRaw =
    let inline size (b:ByteString) : SizeEst = uint64 b.Length
    let inline write b dst = ByteStream.writeBytes b dst
    let inline read src = ByteStream.readRem src
    let codec = 
        { new Codec<ByteString> with
            member __.Write b dst = write b dst
            member __.Read db src = read src
            member __.Compact db b = struct(b, size b)
        }

module EncPair =
    /// Codec combinator for structural pair.
    let codec' (cA : Codec<'A>) (cB : Codec<'B>) =
        { new Codec<struct('A * 'B)> with
            member __.Write (struct(a,b)) dst =
                cA.Write a dst
                cB.Write b dst
            member __.Read db src =
                let a = cA.Read db src
                let b = cB.Read db src
                struct(a,b)
            member __.Compact db (struct(a,b)) =
                let struct(a',szA) = cA.Compact db a
                let struct(b',szB) = cB.Compact db b
                struct(struct(a',b'),(szA + szB))
        }

    /// Codec combinator for a pair.
    let codec (cA : Codec<'A>) (cB : Codec<'B>) =
        { new Codec<('A * 'B)> with
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

    /// Codec combinator for structural triple.
    let codec' (cA : Codec<'A>) (cB : Codec<'B>) (cC : Codec<'C>) =
        { new Codec<struct('A * 'B * 'C)> with
            member __.Write (struct(a,b,c)) dst =
                cA.Write a dst
                cB.Write b dst
                cC.Write c dst
            member __.Read db src =
                let a = cA.Read db src
                let b = cB.Read db src
                let c = cC.Read db src
                struct(a,b,c)
            member __.Compact db (struct(a,b,c)) =
                let struct(a',szA) = cA.Compact db a
                let struct(b',szB) = cB.Compact db b
                let struct(c',szC) = cC.Compact db c
                struct(struct(a',b',c'),(szA + szB + szC))
        }

    /// Codec combinator for a normal triple.
    let codec (cA : Codec<'A>) (cB : Codec<'B>) (cC : Codec<'C>) =
        { new Codec<('A * 'B * 'C)> with
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

module EncQuad = 

    /// Codec combinator for a structural 4-tuple.
    let codec' (cA:Codec<'A>) (cB:Codec<'B>) (cC:Codec<'C>) (cD:Codec<'D>) =
        { new Codec<struct('A * 'B * 'C * 'D)> with
            member __.Write (struct(a,b,c,d)) dst =
                cA.Write a dst
                cB.Write b dst
                cC.Write c dst
                cD.Write d dst
            member __.Read db src =
                let a = cA.Read db src
                let b = cB.Read db src
                let c = cC.Read db src
                let d = cD.Read db src
                struct(a,b,c,d)
            member __.Compact db (struct(a,b,c,d)) =
                let struct(a',szA) = cA.Compact db a
                let struct(b',szB) = cB.Compact db b
                let struct(c',szC) = cC.Compact db c
                let struct(d',szD) = cD.Compact db d
                struct(struct(a',b',c',d'),(szA + szB + szC + szD))
        }

    /// Codec combinator for a 4-tuple.
    let codec cA cB cC cD =
        let get (struct(a,b,c,d)) = (a,b,c,d)
        let set ((a,b,c,d)) = struct(a,b,c,d)
        Codec.view (codec' cA cB cC cD) get set

// Option encoding: same as an array of zero or one items.
module EncOpt =
    let bNone = 0uy
    let bSome = 1uy
    do assert(bNone <> bSome)

    let write cV vOpt dst =
        match vOpt with
        | Some v ->
            ByteStream.writeByte bSome dst
            Codec.write cV v dst
        | None -> ByteStream.writeByte bNone dst

    let read cV db src =
        let b0 = ByteStream.readByte src
        if (b0 = bNone) then None else
        if (b0 <> bSome) then raise ByteStream.ReadError else
        Some (Codec.read cV db src)

    let compact cV db vOpt =
        match vOpt with
        | Some v ->
            let struct(v',szV) = Codec.compactSz cV db v
            struct(Some v', 1UL+szV)
        | None -> struct(None,1UL)

    /// Codec combinator for a simple option type.
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

    let read (cV:Codec<'V>) (db:Stowage) (src:ByteSrc) : 'V array =
        let len = int (EncVarNat.read src)
        if (0 = len) then Array.empty else
        let arr = Array.zeroCreate len
        for ix = 0 to arr.Length - 1 do
            arr.[ix] <- Codec.read cV db src
        arr

    /// compact in-place
    let compact' (cV:Codec<'V>) (db:Stowage) (arr:'V array) =
        let mutable szA = EncVarNat.size (uint64 arr.Length)
        for ix = 0 to (arr.Length - 1) do
            let struct(v',szV) = cV.Compact db (arr.[ix])
            arr.[ix] <- v'
            szA <- (szA + szV)
        struct(arr, szA)

    /// copying compact
    let inline compact cV db arr = compact' cV db (Array.copy arr)

    /// Codec with in-place compaction.
    let codec' (cV:Codec<'V>) =
        { new Codec<'V array> with
            member __.Write arr dst = write cV arr dst
            member __.Read db src = read cV db src
            member __.Compact db arr = compact' cV db arr
        }

    /// Codec with copying compaction.
    let codec (cV:Codec<'V>) =
        { new Codec<'V array> with
            member __.Write arr dst = write cV arr dst
            member __.Read db src = read cV db src
            member __.Compact db arr = compact cV db arr
        }


/// Encode a list via array.
module EncList =
    let codec (cV:Codec<'V>) =
        Codec.view (EncArray.codec' cV) (List.ofArray) (List.toArray)

/// Encode a sequence via array.
module EncSeq =
    let codec (cV:Codec<'V>) =
        Codec.view (EncArray.codec' cV) (Seq.ofArray) (Seq.toArray)

/// Encode a map via array.
module EncMap =
    let codec (cK:Codec<'K>) (cV:Codec<'V>) =
        let cKV = EncPair.codec cK cV
        Codec.view (EncArray.codec' cKV) (Map.ofArray) (Map.toArray)

/// Encode a BTree via array.
module EncCritbitTree =
    let codec (cV:Codec<'V>) =
        let cKV = EncPair.codec (EncBytes.codec) cV
        Codec.view (EncArray.codec' cKV) (CritbitTree.ofArray) (CritbitTree.toArray)

/// Encode a String via array.
module EncString =
    let codec = Codec.view (EncBytes.codec) (BS.toString) (BS.fromString)

module EncStringRaw =
    let codec = Codec.view (EncBytesRaw.codec) (BS.toString) (BS.fromString)

module EncVarNat32 =
    let codec = Codec.view (EncVarNat.codec) (uint32) (uint64)

module EncVarInt32 =
    let codec = Codec.view (EncVarInt.codec) (int) (int64)


