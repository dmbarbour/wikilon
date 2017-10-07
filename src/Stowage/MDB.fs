namespace Stowage
open Data.ByteString

/// Stowage above a memory-mapped database.
///
/// This uses the memory mapped file database LMDB under the hood.
/// This serves as the primary implementation for Stowage.
module MDB =
    /// MDB storage will support values up to 64MB.
    let maxValLen = (64 * 1024 * 1024)

    let maxSafeKeyLen = 255
    let minSafeKeyLen = 1
    let inline safeKeyByte (b:byte) : bool = 
        ((126uy >= b) && (b >= 33uy))

    /// Safe keys are limited as follows:
    ///   they must contain only ASCII bytes in 33..126
    ///   size range is limited between 1 and 255
    /// All other keys will be mangled.
    let safeKey (k:ByteString) : bool =
        (maxSafeKeyLen >= k.Length) &&
        (k.Length >= minSafeKeyLen) &&
        BS.forall safeKeyByte k

    /// Mangle unsafe keys by rewriting to (ESC)(SecureHash)
    /// where (ESC) is byte 27 and the SecureHash is just the
    /// first first 160 bits (32 chars) of RscHash.
    let mangle (k:ByteString) : DB.Key =
        if safeKey k then k else
        BS.cons 27uy (BS.take 32 (RscHash.hash k))

    module private Impl = 
        // TODO: review and transfer older implementation


        type EphID = uint64
        type private EphTable =
            val table : RCTable.Table
            val mutable decrefs : ResizeArray<EphID>
            val mutable delay : bool
            
            new() = 
                { table = new RCTable.Table()
                  decrefs = new ResizeArray<EphID>()
                  delay = false
                }

            member this.DelayDecrefs () = 
                lock this (fun () -> this.delay <- true)
            member this.PassDecrefs () =
                lock this (fun () ->
                    this.decrefs.ForEach(fun k -> this.table.Decref k)
                    this.decrefs <- new ResizeArray<EphID>()
                    this.delay <- false
                    )
            member this.Incref (k:EphID) : unit =
                lock this (fun () -> this.table.Incref k)
            member this.Decref (k:EphID) : unit =
                lock this (fun () ->
                    if this.delay 
                        then this.decrefs.Add k
                        else this.table.Decref k
                    )
            member this.Contains (k:EphID) : bool =
                lock this (fun () -> this.table.Contains k)


    


    // TODO:
    //
    // Construct LMDB version of DB.Storage
    // Direct construction of the DB

