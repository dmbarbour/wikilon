namespace Stowage

/// Abstract encoder-decoder type for data in Stowage.
///
/// This uses the stream-oriented read and write from Data.ByteString
/// to avoid constructing too many intermediate buffers.
///
/// Stowage data structures frequently supports a "compaction" step 
/// that rewrites large, stable fragments of a value as VRefs. This 
/// is accessible through the Stowage codec, and compaction returns
/// an estimated write size to aid heuristics.
///
/// Compaction is a special step in context of stowage, whereby the
/// components of a large value are replaced by value refs that can
/// be loaded, cached, and efficiently serialized via secure hashes.
/// For heuristic compaction, a size estimate is also returned.
type Codec<'T> =
    abstract member Write : 'T -> Data.ByteString.ByteDst -> unit
    abstract member Read  : DB -> Data.ByteString.ByteSrc -> 'T
    abstract member Compact : DB -> 'T -> struct('T * int)

module Codec =

    let inline write (c:Codec<'T>) (v:'T) (dst:ByteDst) : unit = c.Write v dst

    let inline read (c:Codec<'T>) (db:DB) (src:ByteSrc) : 'T = c.Read db src

    let inline compactSz (c:Codec<'T>) (db:DB) (v:'T) : struct('T * int) = c.Compact db v

    let inline compact (c:Codec<'T>) (db:DB) (v:'T) : 'T =
        let struct(v',_) = compactSz c db v
        v'

    let writeBytes (c:Codec<'T>) (v:'T) : ByteString =
        ByteStream.write (write c v)

    /// Read full bytestring as value, or raise ByteStream.ReadError
    let readBytes (c:Codec<'T>) (db:DB) (b:ByteString) : 'T =
        ByteStream.read (read c db) b

    /// Read full bytestring as value, or return None.
    let tryReadBytes (c:Codec<'T>) (db:DB) (b:ByteString) : 'T option =
        let src = new ByteSrc(b)
        try let result = read c db src
            if(ByteStream.eos src) 
                then Some result 
                else None
        with 
        | ByteStream.ReadError -> None

    /// Stow a value without compacting it first.
    ///   Note: you'll have a reference to the resulting RscHash.
    let inline stow' (c:Codec<'T>) (db:DB) (v:'T) : RscHash =
        stowRscDB db (writeBytes c v)

    /// Compact and stow 


    let stow (c:Codec


    /// stow a value without first compacting it
    let inline stow' (c:Codec<'T>) (db:DB) (v:'T) : Rsc =
        let result = Rsc.Stow db (writeBytes c v)
        System.GC.KeepAlive v
        result

    let inline compactSz (c:Codec<'T>) (db:DB) (v:'T) : struct('T * int) =
        c.Compact db v

    let inline compact (c:Codec<'T>) (db:DB) (v:'T) : 'T = 
        let struct(v',sz) = compactSz c db v
        v'

    /// compact and stow together (common use case)
    let inline stow (c:Codec<'T>) (db:DB) (v:'T) : Rsc = 
        stow' c db (compact c db v)
        
    let inline load (c:Codec<'T>) (r:Rsc) : 'T =
        let result = readBytes c r.DB (loadRscDB r.DB r.ID)
        System.GC.KeepAlive r // prevent GC of resource during parse
        result

    // TODO: consider developing generic combinators:
    //  codec<V> to codec<V list> or codec<V list>
    //  codec<A> and codec<B> to codec<(A,B)> or codec<Choice<A,B>>





