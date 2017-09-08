namespace Stowage

/// Abstract encoder/decoder/compaction type for data in Stowage.
///
/// This uses stream-oriented encoders and decoders, and there must
/// be no backtracking when reading. 
///
/// Compaction is a special step in context of stowage, whereby the
/// components of a large value are replaced by value refs that can
/// be loaded, cached, and efficiently serialized via secure hashes.
/// For heuristic compaction, a size estimate is also returned.
type Codec<'T> =
    abstract member Write : System.IO.Stream -> 'T -> unit
    abstract member Read  : DB -> System.IO.Stream -> 'T
    abstract member Compact : DB -> 'T -> struct('T * int)

