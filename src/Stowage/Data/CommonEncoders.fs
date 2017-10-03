namespace Stowage
open Data.ByteString

/// A VarNat is an efficient encoding for a natural number in 
/// base128, high bit from each byte to indicate whether the
/// byte is the final one.
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
///  (size)(data)
///
/// (size) uses EncVarNat
/// (data) is encoded raw
///
/// This is intended for encoding bytestrings mixed with other data.
/// It's assumed that, if the data contains any RscHash references,
/// those references should be protected from adjacency issues (e.g.
/// like how EncRscHash uses `{hash}`. 
module EncBytes =
    let size (b:ByteString) : int =
        EncVarNat.size (uint64 b.Length) + b.Length 

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

/// Raw ByteString Encoder. 
///
/// This codec is suitable for a *Ref<ByteString>, such that the binary
/// is not mixed with any other data. On read, it trivially consumes all 
/// the data from the input stream.
module EncBytesRaw =
    let inline size (b:ByteString) : int = b.Length
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


module EncOpt =
    /// Codec combinator for an Option type with choice of prefix.
    let codecP (bNone:byte) (bSome:byte) (cV:Codec<'V>) =
        assert(bSome <> bNone)
        { new Codec<'V option> with
            member __.Write vOpt dst =
                match vOpt with
                | Some v ->
                    ByteStream.writeByte bSome dst
                    cV.Write v dst
                | None -> ByteStream.writeByte bNone dst
            member __.Read db src =
                let b0 = ByteStream.readByte src
                if(b0 = bNone) then None else
                if(b0 <> bSome) then raise ByteStream.ReadError else
                Some (cV.Read db src)
            member __.Compact db vOpt =
                match vOpt with
                | Some v ->
                    let struct(v',szV) = cV.Compact db v
                    struct(Some v', 1+szV)
                | None -> struct(None,1)
        }

    /// Codec combinator for option type. This one is designed so
    /// we can transparently upgrade from option to a list or array
    /// (sized by VarNat).
    let inline codec cV = codecP (128uy) (129uy) cV


/// Arrays are encoded with size (as varnat) followed by every element
/// in sequence without separators. It's assumed that the elements are
/// distinguishable on parse and protect their own RscHash references. 
module EncArray =
    let write (cV:Codec<'V>) (a: 'V array) (dst:ByteDst) =
        EncVarNat.write (uint64 a.Length) dst
        Array.iter (fun e -> Codec.write cV e dst) a

    let read (cV:Codec<'V>) (db:Stowage) (src:ByteSrc) : 'V array =
        let len = int (EncVarNat.read src)
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
module EncBTree =
    let codec (cV:Codec<'V>) =
        let cKV = EncPair.codec (EncBytes.codec) cV
        Codec.view (EncArray.codec' cKV) (BTree.ofArray) (BTree.toArray)



