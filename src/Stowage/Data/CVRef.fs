namespace Stowage
open Data.ByteString

/// Compacting Value Reference
///
/// The overhead for secure hash resources is relatively high, so we don't
/// want to use them for small values. Instead, we keep those values inline.
/// CVRef models this common use case. It is either a value in local memory
/// or a remote, cacheable LVRef. Upon compaction, we can potentially move 
/// from the former to the latter. Size estimates are preserved to resist 
/// redundant compaction efforts.
module CVRef =

    type Ref<'V> =
        | Local of 'V * SizeEst
        | Remote of LVRef<'V>

    /// Local value without initial size estimate. 
    let inline local v = Local (v, System.Int32.MaxValue)

    /// Remote value reference.
    let inline remote r = Remote r

    let inline isRemote ref =
        match ref with
        | Local _ -> false
        | Remote _ -> true

    /// Caching access to value.
    let inline load (ref:Ref<'V>) : 'V =
        match ref with
        | Local (v,_) -> v
        | Remote r -> LVRef.load r

    /// Non-caching access to value.
    let inline load' (ref:Ref<'V>) : 'V = 
        match ref with
        | Local (v,_) -> v
        | Remote r -> LVRef.load' r

    /// Forcibly clear cached value (if any).
    let inline clear (ref:Ref<'V>) : unit =
        match ref with
        | Local _ -> ()
        | Remote r -> LVRef.clear r

    module Enc =
        // overlapping the first byte with the RscHash container.
        let cLocal =
            let c = byte '`'
            assert(c <> EncRscHash.cPrefix)
            c

        let write (cV:Codec<'V>) (ref:Ref<'V>) (dst:ByteDst) : unit =
            match ref with
            | Local (v,_) ->
                ByteStream.writeByte cLocal dst
                Codec.write cV v dst
            | Remote vref -> EncRscHash.write (vref.ID) dst
                

        let read (cV:Codec<'V>) (db:Stowage) (src:ByteSrc) : Ref<'V> =
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

        let compact (thresh:int) (cV:Codec<'V>) (db:Stowage) (ref:Ref<'V>) : struct(Ref<'V> * int) =
            match ref with
            | Local (v,szEst) ->
                if (szEst < thresh) then struct(ref, 1 + szEst) else
                let struct(v',szV) = Codec.compactSz cV db v
                if (szV < thresh) then struct(Local(v',szV), 1 + szV) else
                struct(Remote (LVRef.stow cV db v'), EncRscHash.size)
            | Remote _ -> struct(ref, EncRscHash.size)

        let compact' (thresh:int) (cV:Codec<'V>) (db:Stowage) (ref:Ref<'V>) : struct(Ref<'V> * int) =
            match ref with
            | Local (v,szEst) ->
                if (szEst < thresh) then struct(ref, 1 + szEst) else
                let struct(v',szV) = Codec.compactSz cV db v
                if (szV < thresh) then struct(Local(v',szV), 1 + szV) else
                struct(Remote (LVRef.stow' cV db v'), EncRscHash.size)
            | Remote _ -> struct(ref, EncRscHash.size)

    /// Construct directly with compaction. Stow asynchronously. (See LVRef.)
    let stow (thresh:int) (cV:Codec<'V>) (db:Stowage) (v:'V) : Ref<'V> =
        let struct(ref,_) = Enc.compact thresh cV db (local v)
        ref

    /// Construct with compaction. Eagerly stow.
    let stow' (thresh:int) (cV:Codec<'V>) (db:Stowage) (v:'V) : Ref<'V> =
        let struct(ref,_) = Enc.compact' thresh cV db (local v)
        ref

    /// Codec combinator for CVRef.
    /// 
    /// For recursive structures, however, you'll probably want to use
    /// CVRef.Enc.read / write / compact directly to build your own codec.
    let codec (thresh:int) (cV:Codec<'V>) = 
        { new Codec<Ref<'V>> with
            member __.Write ref dst = Enc.write cV ref dst
            member __.Read db src = Enc.read cV db src
            member __.Compact db ref = Enc.compact thresh cV db ref
        }

    /// Codec combinator with eager compaction. 
    let codec' (thresh:int) (cV:Codec<'V>) = 
        { new Codec<Ref<'V>> with
            member __.Write ref dst = Enc.write cV ref dst
            member __.Read db src = Enc.read cV db src
            member __.Compact db ref = Enc.compact' thresh cV db ref
        }

type CVRef<'V> = CVRef.Ref<'V>
