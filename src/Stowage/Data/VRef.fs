namespace Stowage
open Data.ByteString

/// A VRef is an abstract reference to a value whose binary 
/// representation is held in Stowage, addressed by secure
/// hash. The abstract nature is to separate caching, GC,
/// lazy serialization, and similar features. To simplify
/// caching, Deref also returns a serialized size estimate.
///
/// Comparison of VRefs is based on type and secure hash, so
/// we assume a stable codec per type (at least within context
/// of the comparisons). The ToString method simply returns the
/// "{secureHash}".
[<AbstractClass>]
type VRef<'V>() =
    abstract member Addr : RscHash with get
    abstract member Deref : unit -> struct('V * SizeEst) 

    override r.ToString() = sprintf "{%s}" (BS.toString r.Addr)
    override r.GetHashCode() = ByteString.Hash32 (r.Addr) |> int
    override x.Equals yobj =
        match yobj with
        | :? VRef<'V> as y -> System.Object.ReferenceEquals(x,y) || (x.Addr = y.Addr)
        | _ -> false
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? VRef<'V> as y ->
                if System.Object.ReferenceEquals(x,y) then 0 else
                ByteString.Compare (x.Addr) (y.Addr)
            | _ -> invalidArg "yobj" "comparing values of different types"

module VRef =

    /// Create VRef by packaging Codec, Stowage DB, and RscHash.
    /// Assumes caller is passing ownership, so for GC we will 
    /// call Stowage Decref on the hash when done with the VRef.
    /// This is the "simplest" VRef, without any caching.
    let wrap' (c:Codec<'V>) (db:Stowage) (h0:RscHash) : VRef<'V> = 
        assert(BS.length h0 = RscHash.size) // partial validation
        let h = BS.trimBytes h0
        { new VRef<'V>() with
            override __.Addr with get() = h
            override ref.Deref() = 
                let bytes = db.Load h
                let szest = uint64 (BS.length bytes)
                let value = Codec.readBytes c db bytes
                System.GC.KeepAlive(ref)
                struct(value,szest)
            override __.Finalize() = db.Decref h
        }

    /// As wrap', but will first Incref the RscHash to resist premature
    /// garbage collection of the referenced data.
    let inline wrap (c:Codec<'V>) (db:Stowage) (h:RscHash) : VRef<'V> =
        db.Incref h
        wrap' c db h

    /// Create VRef by eagerly Stowing a value.
    let inline stow (c:Codec<'V>) (db:Stowage) (v:'V) : VRef<'V> =
        wrap' c db (Codec.stow c db v)

    /// load the VRef's data from Stowage (ignoring the SizeEst).
    let inline load (ref:VRef<'V>) : 'V = 
        let struct(value,_) = ref.Deref()
        value

module EncRscHash =
    let cPrefix = byte '{'
    let cSuffix = byte '}'
    let size = uint64 (2 + RscHash.size)
    let write (h:RscHash) (dst:ByteDst) =
        assert(h.Length = RscHash.size)
        ByteStream.writeByte cPrefix dst
        ByteStream.writeBytes h dst
        ByteStream.writeByte cSuffix dst
    let read (src:ByteSrc) : RscHash =
        let bPrefix = ByteStream.readByte src
        if (bPrefix <> cPrefix) then raise ByteStream.ReadError
        let h = ByteStream.readBytes (RscHash.size) src
        let bSuffix = ByteStream.readByte src
        if (bSuffix <> cSuffix) then raise ByteStream.ReadError
        h


module EncVRef = 
    let size = EncRscHash.size
    let inline write (ref:VRef<_>) (dst:ByteDst) : unit = 
        EncRscHash.write (ref.Addr) dst
    let inline read (cV:Codec<'V>) (db:Stowage) (src:ByteSrc) : VRef<'V> =
        let h = EncRscHash.read src
        VRef.wrap cV db h
    let codec (cV:Codec<'V>) =
        { new Codec<VRef<'V>> with
            member __.Write ref dst = write ref dst
            member __.Read db src = read cV db src
            member __.Compact _ ref = struct(ref, size)
        }

