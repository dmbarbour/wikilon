namespace Stowage
open Data.ByteString

/// A VRef is a reference to a value represented within Stowage.
/// This packages everything needed to manage the value: a hash,
/// a Stowage database to locate it, a Codec to parse it, and a
/// finalizer to release the resource when done with it.
///
/// Note: Comparison of VRefs is based on RscHash and type only.
/// Also, consider use of LVRef for caching and delayed stowage.
/// Or CVRef to keep smaller values inline.
type VRef<'V> =
    val Codec : Codec<'V>
    val DB : Stowage
    val internal Hash : byte[]
    member x.ID with get() : RscHash = BS.unsafeCreateA (x.Hash)

    // Object overrides
    override v.Finalize() = v.DB.Decref (v.ID)
    override v.ToString() = v.ID.ToString()
    override v.GetHashCode() = v.ID.GetHashCode()
    override x.Equals yobj =
        match yobj with
        | :? VRef<'V> as y -> (x.ID = y.ID)
        | _ -> false
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? VRef<'V> as y -> compare (x.ID) (y.ID)
            | _ -> invalidArg "yobj" "cannot compare values of different types"

    // use VRef.wrap' to construct
    internal new (codec:Codec<'V>, db:Stowage, hash:byte[]) =
        assert(RscHash.size = hash.Length)
        { Codec = codec
          DB = db 
          Hash = hash
        } 

module VRef =

    /// Create VRef by packaging Codec, Stowage DB, and RscHash.
    ///
    /// Assumes client is passing ownership of the reference. You
    /// might need to explicitly increfRscDB in some cases.
    let wrap' (c:Codec<'V>) (db:Stowage) (h:RscHash) : VRef<'V> = 
        new VRef<'V>(c,db,BS.toArray h)

    /// Create VRef by packaging Codec, Stowage DB, and RscHash.
    ///
    /// This will incref the RscHash at the DB to prevent GC of the
    /// stowed data while the VRef is held in .Net runtime memory.
    let inline wrap (c:Codec<'V>) (db:Stowage) (h:RscHash) : VRef<'V> =
        db.Incref h
        wrap' c db h

    /// Create VRef by Stowing a value.
    let inline stow (c:Codec<'V>) (db:Stowage) (v:'V) : VRef<'V> =
        wrap' c db (Codec.stow c db v)

    /// load a VRef's data from Stowage, bypassing cache entirely.
    let inline load (ref:VRef<'V>) : 'V =
        let result = Codec.load (ref.Codec) (ref.DB) (ref.ID)
        System.GC.KeepAlive ref
        result

module EncRscHash =
    let cPrefix = byte '{'
    let cSuffix = byte '}'
    let size = 2 + RscHash.size
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
        EncRscHash.write (ref.ID) dst
    let inline read (cV:Codec<'V>) (db:Stowage) (src:ByteSrc) : VRef<'V> =
        let h = EncRscHash.read src
        VRef.wrap cV db h
    let codec (cV:Codec<'V>) =
        { new Codec<VRef<'V>> with
            member __.Write ref dst = write ref dst
            member __.Read db src = read cV db src
            member __.Compact _ ref = struct(ref, size)
        }

