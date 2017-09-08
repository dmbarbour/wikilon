namespace Stowage
open Data.ByteString

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
    abstract member Write : 'T -> ByteDst -> unit
    abstract member Read  : DB -> ByteSrc -> 'T
    abstract member Compact : DB -> 'T -> struct('T * int)

module Codec =

    let inline write (c:Codec<'T>) (v:'T) (dst:ByteDst) : unit = c.Write v dst

    let inline read (c:Codec<'T>) (db:DB) (src:ByteSrc) : 'T = c.Read db src

    let inline compactSz (c:Codec<'T>) (db:DB) (v:'T) : struct('T * int) = c.Compact db v

    let inline compact (c:Codec<'T>) (db:DB) (v:'T) : 'T =
        let struct(v',_) = compactSz c db v
        v'

    let inline writeBytes (c:Codec<'T>) (v:'T) : ByteString =
        ByteStream.write (write c v)

    /// Read full bytestring as value, or raise ByteStream.ReadError
    let inline readBytes (c:Codec<'T>) (db:DB) (b:ByteString) : 'T =
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
    ///
    /// Note: You'll have a reference to the resulting RscHash, so
    /// you'll need to use decrefRscDB later (or wrap into a VRef).
    let inline stow (c:Codec<'T>) (db:DB) (v:'T) : RscHash =
        let result = stowRscDB db (writeBytes c v)
        System.GC.KeepAlive v // prevent GC of value during write
        result

    /// A frequent composition of compaction and stowage.
    let inline compactAndStow (c:Codec<'T>) (db:DB) (v:'T) : RscHash =
        stow c db (compact c db v)

    /// Load a stowed value from RscHash.
    let inline load (c:Codec<'T>) (db:DB) (h:RscHash) : 'T =
        readBytes c db (loadRscDB db h)

    // TODO: develop codec-combinators.
    //   e.g. combine two codecs into a pair or choice of codecs
    //   wrap a codec to form a codec for a list or choice of values


