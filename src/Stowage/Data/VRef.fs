namespace Stowage
open Data.ByteString

/// A VRef is a reference to a value represented within Stowage.
/// This packages everything needed to manage the value: a hash
/// and DB to locate it, a Codec to parse it, and a finalizer to
/// release the resource when done with it.
///
/// Note: Comparison of VRefs is based on RscHash and type only.
type VRef<'V> =
    val Codec : Codec<'V>
    val DB : DB
    val internal Hash : byte[]
    member x.ID with get() : RscHash = BS.unsafeCreateA (x.Hash)

    // Object overrides
    override v.Finalize() = decrefRscDB (v.DB) (v.ID)
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
    internal new (codec:Codec<'V>, db:DB, hash:byte[]) =
        { Codec = codec
          DB = db 
          Hash = hash
        } 

module VRef =

    /// Create VRef by packaging Codec, DB ref, and RscHash.
    ///
    /// Assumes client is passing ownership of the reference. You
    /// might need to explicitly increfRscDB in some cases.
    let wrap' (c:Codec<'V>) (db:DB) (h:RscHash) : VRef<'V> = 
        if (rscHashLen <> h.Length)
            then invalidArg "h" "not a resource hash"
        new VRef<'V>(c,db,BS.toArray h)

    /// Create VRef by packaging Codec, DB ref, and RscHash.
    ///
    /// This will incref the RscHash at the DB to prevent GC of the
    /// stowed data while the VRef is held in .Net runtime memory.
    let inline wrap (c:Codec<'V>) (db:DB) (h:RscHash) : VRef<'V> =
        increfRscDB db h
        wrap' c db h

    /// Create VRef by Stowing a value.
    let inline stow (c:Codec<'V>) (db:DB) (v:'V) : VRef<'V> =
        wrap' c db (Codec.stow c db v)

    /// load a VRef's data from Stowage, bypassing cache entirely.
    let inline load (ref:VRef<'V>) : 'V =
        let result = Codec.load (ref.Codec) (ref.DB) (ref.ID)
        System.GC.KeepAlive ref
        result

/// BRef indicates the trivial VRef<ByteString>.
///
/// Most VRefs have codecs that parse a stowed binary into structured
/// data (which may recursively contain more VRefs). But the simple
/// binary VRef - BRef - loads the bytestring without modification.
type BRef = VRef<ByteString>

module BRef =
    /// This BRef codec will blindly read the entire stream, so it
    /// is unsuitable for use with codec combinators. Indeed, there
    /// isn't much use for it outside of BRefs.
    let c : Codec<ByteString> =
        { new Codec<ByteString> with
            member __.Write b dst = ByteStream.writeBytes b dst
            member __.Read db src = ByteStream.readRem src
            member __.Compact db b = struct(b,b.Length)
        }

    let wrap' (db:DB) (h:RscHash)    : BRef = VRef.wrap' c db h
    let wrap  (db:DB) (h:RscHash)    : BRef = VRef.wrap  c db h
    let stow  (db:DB) (v:ByteString) : BRef = VRef.stow  c db v
    let load  (ref:BRef) : ByteString = VRef.load ref


