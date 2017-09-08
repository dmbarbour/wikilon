namespace Stowage

/// A VRef is a reference to a value represented within Stowage.
///
/// A stowage reference is identified by a secure hash of a binary,
/// stored within a Stowage database. The binary is interpreted via
/// Codec, which may construct further VRefs as needed.
///
/// To control certain redundant costs, a VRef includes a mutable
/// cache that may be explicitly loaded or cleared as needed. Since
/// the VRef represents immutable data, the cache is never 'invalid'.
/// (However, there is no built-in expiration feature.)
///
/// Note: all comparisons and hashcodes for VRef assume a stable Codec
/// for a given type and only peek at the RscHash values.
type VRef<'V> =
    val DB : DB
    val ID : RscHash
    val Codec : Codec<'V>
    val mutable internal Cache : 'V option

    // private 'new' within assembly; use VRef module to construct
    internal new (db,id,c) =
        { DB = db ; ID = id ; Codec = c ; Cache = None }

    // allow GC of Stowage only after GC in .Net runtime.
    override v.Finalize() = decrefRscDB (v.DB) (v.ID)

    // Object overrides
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
    



        

            
    


