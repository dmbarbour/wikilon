namespace Stowage
open Data.ByteString

/// Compacting Value Reference
///
/// The idea here is that we should be able to compact oversized data
/// into a reference while representing smaller data inline or in memory.
/// This is motivated because VRefs actually have a significant

/// The CVRef type may contain Local value inline, or a remote ref. On
/// compaction, we determine whether the local value must be rewritten
/// to the remote reference based on thresholds and a size estimate.
///
/// CVRef wraps LVRef to provide caching and latent stowage.
module CVRef =

    type Ref<'V> =
        | Local of 'V * SizeEst
        | Remote of LVRef<'V>

    /// Local value with a guaranteed compaction effort.
    let inline local v = Local (v, System.Int32.MaxValue)

    /// Local value with a size estimate. This size shouldn't
    /// be too far off from the actual serialization size.
    let inline localSz (v:'V) (sz:SizeEst) : Ref<'V> = Local (v,sz)

    /// Remote value reference.
    let inline remote r = Remote r



    /// Forcibly clear cached value (if any).
    let inline clear (ref:Ref<'V>) : unit =
        match ref with
        | Local _ -> ()
        | Remote r -> LVRef.clear r

    /// Caching access to value.
    let inline load (ref:Ref<'V>) : 'V =
        match ref with
        | Local v -> v
        | Remote r -> LVRef.load r

    /// Non-caching access to value.
    let inline load' (ref:Ref<'V>) : 'V = 
        match ref with
        | Local v -> v
        | Remote r -> LVRef.load' r

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
                | Local v ->
                    EncByte.write cLocal dst
                    cV.Write v dst
                | Remote r ->
                    EncByte.write cRemote dst
                    EncLVRef.write r dst
            member __.Read db src =
                let b0 = EncByte.read src
                if (cLocal = b0) then Local (cV.Read db src) 
                elif (cRemote <> b0) then raise (ByteStream.ReadError)
                else Remote (EncLVRef.read cV db src)
            member __.Compact db ref =
                match ref with
                | Remote r -> struct(ref, 1 + EncLVRef.size)
                | Local v ->
                    let struct(v',szV) = cV.Compact db v
                    if(szV < threshold) then struct(Local v', 1 + szV) else
                    let r = LVRef.stow cV db v'
                    struct(Remote r, 1 + EncLVRef.size)
        }


type CVRef<'V> = CVRef.Ref<'V>
