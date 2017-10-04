namespace Stowage
open Data.ByteString

/// Compacting Value Reference
///
/// The overhead for secure hash resources is relatively high, so we don't
/// want to use them for small values. Instead, keep small values inline.
/// CVRef models this common use case. It is either a value in local memory
/// or a cacheable LVRef. Upon compaction, we heuristically move large data
/// into LVRefs while. Repeated compactions are short-circuited by keeping
/// some size metadata.
type CVRef<'V> =
    | Local of 'V * SizeEst
    | Remote of LVRef<'V>

/// Serialization for CVRef
module EncCVRef =
    // encoding is `local or {hash}.

    // overlapping the first byte with the RscHash container.
    let cLocal =
        let c = byte '`'
        assert(c <> EncRscHash.cPrefix)
        c

    let write (cV:Codec<'V>) (ref:CVRef<'V>) (dst:ByteDst) : unit =
        match ref with
        | Local (v,_) ->
            ByteStream.writeByte cLocal dst
            Codec.write cV v dst
        | Remote vref -> EncRscHash.write (vref.ID) dst
            
    let read (cV:Codec<'V>) (db:Stowage) (src:ByteSrc) : CVRef<'V> =
        let b0 = ByteStream.readByte src
        if (b0 = cLocal) then
            let s0 = ByteStream.bytesRem src
            let v = Codec.read cV db src
            let sf = ByteStream.bytesRem src
            Local (v, (s0 - sf))
        else if (b0 <> EncRscHash.cPrefix) then
            raise ByteStream.ReadError
        else
            let h = ByteStream.readBytes (RscHash.size) src
            let bf = ByteStream.readByte src
            if (bf <> EncRscHash.cSuffix) then raise ByteStream.ReadError
            Remote (LVRef.wrap (VRef.wrap cV db h))

    let compact (thresh:int) (cV:Codec<'V>) (db:Stowage) (ref:CVRef<'V>) : struct(CVRef<'V> * int) =
        match ref with
        | Local (v,szEst) ->
            if (szEst < thresh) then struct(ref, 1 + szEst) else
            let struct(v',szV) = Codec.compactSz cV db v
            if (szV < thresh) then struct(Local(v',szV), 1 + szV) else
            struct(Remote (LVRef.stow cV db v' szV), EncRscHash.size)
        | Remote _ -> struct(ref, EncRscHash.size)

    let codec (thresh:int) (cV:Codec<'V>) =
        { new Codec<CVRef<'V>> with
            member __.Write ref dst = write cV ref dst
            member __.Read db src = read cV db src
            member __.Compact db ref = compact thresh cV db ref
        }


module CVRef =

    /// Local in-memory value. 
    ///
    /// The first compaction will determine whether this remains local
    /// or is stowed remotely. Subsequent compactions of the same size
    /// will be short-circuited.
    let inline local v = Local (v, System.Int32.MaxValue)

    /// Remote value reference.
    let inline remote r = Remote r

    /// Test whether value is a Stowage reference.
    let isRemote ref =
        match ref with
        | Local _ -> false
        | Remote _ -> true

    /// Caching access to value.
    let load (ref:CVRef<'V>) : 'V =
        match ref with
        | Local (v,_) -> v
        | Remote r -> LVRef.load r

    /// Non-caching access to value.
    let load' (ref:CVRef<'V>) : 'V = 
        match ref with
        | Local (v,_) -> v
        | Remote r -> LVRef.load' r

    /// Construct a compacted value directly. 
    ///
    /// This will immediately compact the value in memory, then decide
    /// whether to keep it local or stow it to the database based on
    /// the estimated size. Even if stowed, the value remains in cache
    /// at least briefly.
    let inline stow (thresh:int) (cV:Codec<'V>) (db:Stowage) (v:'V) : CVRef<'V> =
        let struct(ref,_) = EncCVRef.compact thresh cV db (local v)
        ref

