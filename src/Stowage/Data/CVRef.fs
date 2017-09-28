namespace Stowage
open Data.ByteString

/// Compacting Value Reference
///
/// The overhead for secure hash resources is relatively high, so we don't
/// want to use them for small values. Instead, we keep those values inline.
/// CVRef models this common use case. It is either a value in local memory
/// or a remote, cacheable LVRef. Upon compaction, we can potentially move 
/// from the former to the latter.
///
/// Size metadata is also preserved to resist redundant compaction efforts,
/// which is convenient for most use-cases. 
module CVRef =

    type Ref<'V> =
        | Local of 'V * SizeEst
        | Remote of LVRef<'V>

    /// Local value without initial size estimate. 
    let inline local v = Local (v, System.Int32.MaxValue)

    /// Local compact value with serialization size estimate, generally
    /// from recently reading or compacting the value. The estimate 
    /// should be close enough to actual size for heuristics to work.
    /// If the size isn't obvious, just use `local` instead.
    let inline localSized v sz = Local (v,sz)

    /// Remote value reference.
    let inline remote r = Remote r

    /// Non-caching access to value.
    let inline load' (ref:Ref<'V>) : 'V = 
        match ref with
        | Local (v,_) -> v
        | Remote r -> LVRef.load' r

    /// Caching access to value.
    let inline load (ref:Ref<'V>) : 'V =
        match ref with
        | Local (v,_) -> v
        | Remote r -> LVRef.load r

    /// Forcibly clear cached value (if any).
    let inline clear (ref:Ref<'V>) : unit =
        match ref with
        | Local _ -> ()
        | Remote r -> LVRef.clear r

    /// Force initial stowage.
    let inline force (ref:Ref<'V>) : unit =
        match ref with
        | Local _ -> ()
        | Remote r -> LVRef.force r

    /// Primary codec for CVRef. 
    ///
    /// The size threshold determines when compaction switches from
    /// local to remote.
    let codec (threshold:int) (cV:Codec<'V>) =
        let cLocal = byte 'L'
        let cRemote = byte 'R'
        { new Codec<Ref<'V>> with
            member __.Write ref dst =
                match ref with
                | Local (v,_) ->
                    EncByte.write cLocal dst
                    cV.Write v dst
                | Remote r ->
                    EncByte.write cRemote dst
                    EncLVRef.write r dst
            member __.Read db src =
                let b0 = EncByte.read src
                if (cLocal = b0) then 
                    // compute size estimate from parse
                    let s0 = ByteStream.bytesRem src
                    let v = cV.Read db src
                    let sf = ByteStream.bytesRem src
                    Local (v, (s0 - sf))
                elif (cRemote <> b0) then raise (ByteStream.ReadError)
                else Remote (EncLVRef.read cV db src)
            member __.Compact db ref =
                match ref with
                | Remote r -> struct(ref, 1 + EncLVRef.size)
                | Local (v,szE) ->
                    if(szE < threshold) then struct(ref, 1 + szE) else
                    let struct(v',szV) = cV.Compact db v
                    if(szV < threshold) then struct(Local(v',szV), 1 + szV) else
                    let r = LVRef.stow cV db v' // background stowage
                    struct(Remote r, 1 + EncLVRef.size)
        }


type CVRef<'V> = CVRef.Ref<'V>
