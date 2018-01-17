namespace Stowage
open Data.ByteString

/// A "compacting" ByteString.
///
/// The idea here is that small bytestrings should be held locally
/// in memory, while large bytestrings will be remotely referenced
/// and only briefly cached in memory.
///
/// Although this is represented by CVRef<ByteString>, we will use
/// a specialized encoder to reduce overheads and ensure the "raw"
/// encoding is used for remote binaries (no size prefix), making
/// structure sharing with equivalent binaries a little easier.
type CByteString = CVRef<ByteString>

module EncCByteString =

    // Prefix is:
    //  0 for remote
    //  EncVarNat (1 + Length) for local.
    //
    // Suffix is: 0 for remote, nothing for local.

    let write (ref:CByteString) (dst:ByteDst) : unit =
        match ref with
        | Local (s,_) ->
            EncVarNat.write (1UL + uint64 s.Length) dst
            ByteStream.writeBytes s dst
        | Remote vref -> 
            EncVarNat.write (0UL) dst
            ByteStream.writeBytes (vref.ID) dst
            ByteStream.writeByte 0uy dst

    let read (db:Stowage) (src:ByteSrc) : CByteString =
        let s0 = ByteStream.bytesRem src
        let len = EncVarNat.read src
        if (0UL = len) then // Remote
            let h = ByteStream.readBytes (RscHash.size) src
            let bSuffix = ByteStream.readByte src
            if (bSuffix <> 0uy) then raise ByteStream.ReadError
            Remote (LVRef.wrap (VRef.wrap (EncBytesRaw.codec) db h))
        else // Local
            let bs = ByteStream.readBytes (int (len - 1UL)) src
            let sf = ByteStream.bytesRem src
            Local (bs, (s0 - sf))

    let remoteSize = 2 + RscHash.size
    let inline localSize s = 
        let len = BS.length s
        len + EncVarNat.size (1UL + uint64 len) 

    let compact (thresh:int) (db:Stowage) (ref:CByteString) : struct(CByteString * int) =
        match ref with
        | Local (s,szEst) ->
            if (szEst < thresh) then struct(ref, szEst) else
            let sz = localSize s
            if (sz < thresh) then struct(Local(s,sz), sz) else
            let vref = LVRef.stow (EncBytesRaw.codec) db s (s.Length)
            struct(Remote vref, remoteSize)
        | Remote _ -> struct(ref, remoteSize)

    let codec (thresh:int) =
        { new Codec<CByteString> with
            member __.Write ref dst = write ref dst
            member __.Read db src = read db src
            member __.Compact db ref = compact thresh db ref
        }

module CByteString =

    // local, load, etc. matching interface for CVRef
    let inline local (s:ByteString) : CByteString = CVRef.local s
    let inline isRemote (ref:CByteString) : bool = CVRef.isRemote ref
    let inline load (ref:CByteString) : ByteString = CVRef.load ref
    let inline load' (ref:CByteString) : ByteString = CVRef.load' ref

    /// Wrap a remote binary resource. Will decref remote resource
    /// when destroyed. Use `wrap` if you need to incref resource.
    let inline wrap' (db:Stowage) (h:RscHash) =
        Remote (LVRef.wrap (VRef.wrap' (EncBytesRaw.codec) db h))

    /// Wrap remote binary resource, with incref to prevent GC.
    let inline wrap (db:Stowage) (h:RscHash) = 
        db.Incref h
        wrap' db h

    /// Construct a compacted bytestring directly, compacting immediately
    /// if required according to the threshold. This function uses the 
    /// specialized encoder from EncCByteString.
    let stow (thresh:int) (db:Stowage) (s:ByteString) : CByteString =
        let struct(ref,_) = EncCByteString.compact thresh db (local s)
        ref

