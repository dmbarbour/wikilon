namespace Stowage
open Data.ByteString

/// Structured Binary Data in Memory
///
/// The Stowage Binary represents binary data that references ad-hoc
/// stowage resources (see scanHashDeps). This type exists mostly to
/// simplify interaction between GC in the .Net runtime and GC in the
/// Stowage persistence layer. On Finalize or Dispose, decrefValDeps
/// is performed to release the binary from memory.
///
/// Other than compatibility with scanHashDeps, no interpretation of
/// the binary is assumed. In general, the binary must be parsed to 
/// extract usable data. If you know a binary is just a RscHash, the
/// Rsc type will be more convenient and explicit.
///
/// NOTE: In some cases, you may need to leverage System.GC.KeepAlive
/// to guard against premature destruction via optimizer. This is most
/// likely needed after parsing the bytestring.
type Binary =
    val DB    : DB
    val Bytes : Val
    new(db:DB, bytes:Val) = { DB = db; Bytes = bytes }
    new(db:DB, bytes:Val, incref:bool) =
        if incref then increfValDeps db bytes
        new Binary(db,bytes)

    // Clone is useful in some contexts where you might use Dispose(). 
    member b.Clone() : Binary = 
        let result = new Binary(b.DB, b.Bytes, true)
        System.GC.KeepAlive b
        result

    member private b.Decref() = decrefValDeps (b.DB) (b.Bytes)
    override b.Finalize() = b.Decref()
    interface System.IDisposable with
        member b.Dispose() =
            b.Decref()
            System.GC.SuppressFinalize ref

    override x.Equals yobj =
        match yobj with
        | :? Binary as y -> (x.DB = y.DB) && (x.Bytes = y.Bytes)
        | _ -> false

    override b.GetHashCode() = b.Bytes.GetHashCode()

    static member Compare (a:Binary) (b:Binary) : int =
        let cdb = compare (a.DB) (b.DB) 
        if (0 <> cdb) then cdb else
        compare (a.Bytes) (b.Bytes)

    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Binary as y -> Binary.Compare x y
            | _ -> invalidArg "yobj" "cannot compare values of different types"

    member inline b.Length with get() = b.Bytes.Length
    member inline b.Item (ix : int) : byte = b.Bytes.Item ix
    member b.GetSlice (iniOpt : int option, finOpt : int option) : Binary =
        let slice = b.Bytes.GetSlice(iniOpt, finOpt)
        increfValDeps (b.DB) slice
        new Binary(b.DB, slice)

    override b.ToString() : string = b.Bytes.ToString()
    
/// Stowage Resource References
///
/// This is essentially a Binary specialized for a singular RscHash.
/// Upon Finalize or Dispose, we decrefRscDB. This offers a minor 
/// performance advantage over Binary. The main benefit, however, is
/// more explicit assumptions in types, and convenient Load or Stow.
/// With a Binary, you generally must parse the Bytes into something
/// useful. This isn't the case for a simple resource reference.
///
/// As with Binary, you may need to leverage System.GC.KeepAlive after
/// parsing a loaded value to prevent premature release of the Rsc.
type Rsc =
    val DB : DB
    val ID : RscHash
    new (db:DB, id:RscHash) = 
        assert(id.Length = rscHashLen)
        { DB = db; ID = (BS.trimBytes' 200 id) }
    new (db:DB, id:RscHash, incref:bool) =
        if incref then increfRscDB db id
        new Rsc(db,id)

    // Clone is useful in some contexts where you might use Dispose(). 
    member rsc.Clone() : Rsc =
        let result = new Rsc(rsc.DB, rsc.ID, true)
        System.GC.KeepAlive rsc
        result

    static member inline Stow (db:DB) (v:Val) : Rsc = 
        new Rsc(db, stowRscDB db v)

    member rsc.Load() : Val = 
        let result = loadRscDB (rsc.DB) (rsc.ID)
        System.GC.KeepAlive (rsc)
        result
    member rsc.TryLoad() : Val option = 
        let result = tryLoadRscDB (rsc.DB) (rsc.ID)
        System.GC.KeepAlive (rsc) 
        result
    static member StowBin (b:Binary) : Rsc = 
        let result = Rsc.Stow (b.DB) (b.Bytes)
        System.GC.KeepAlive (b)
        result
    member rsc.LoadBin() : Binary =
        let result = new Binary(rsc.DB, loadRscDB (rsc.DB) (rsc.ID), true)
        System.GC.KeepAlive (rsc)
        result
    member rsc.TryLoadBin() : Binary option =
        let result = 
            match tryLoadRscDB (rsc.DB) (rsc.ID) with
            | Some v -> Some (new Binary(rsc.DB, v, true))
            | None -> None
        System.GC.KeepAlive (rsc)
        result

    member private rsc.Decref() = decrefRscDB (rsc.DB) (rsc.ID)
    override rsc.Finalize() = rsc.Decref()
    interface System.IDisposable with
        member rsc.Dispose() =
            rsc.Decref()
            System.GC.SuppressFinalize rsc

    override x.Equals yobj =
        match yobj with
        | :? Rsc as y -> (x.DB = y.DB) && (x.ID = y.ID)
        | _ -> false

    override rsc.GetHashCode() = rsc.ID.GetHashCode()

    static member Compare (a:Rsc) (b:Rsc) : int =
        let cdb = compare (a.DB) (b.DB) 
        if (0 <> cdb) then cdb else
        compare (a.ID) (b.ID)

    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Rsc as y -> Rsc.Compare x y
            | _ -> invalidArg "yobj" "cannot compare values of different types"
    
    override rsc.ToString() : string = rsc.ID.ToString()

// under consideration: intermediate form, perhaps called BinRef or BigBin,
// that takes form of Binary if small, Rsc if large. 


