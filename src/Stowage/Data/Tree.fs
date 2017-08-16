namespace Stowage
open Data.ByteString

module internal I =

        // potential binary representation in stowage:
        //  (0)  (nothing, empty bytestring)
        //  (1)RemoteHash(32)
        //  (size+1)bytes of size(126)
        //
        // Here (size+1) would be encoded as a variable nat:
        //
        //   0..127 for high digits
        //   128..255 for the final digit
        //
        // This would avoid interference with conservative GC,
        // in case the binary contains resource references. Also,
        // this encoding could be separate from the decision of
        // how large of local binary we permit.

    let maxBinaryLocal = 1023
    compactBinary (db:DB) (b:Binary)

    type Node =
        | NLeaf of VRef
        | NInner of int * VRef * Node * Node
        | NRemote of Rsc

    roughNodeMemSize (n:Node) : int64 = 32L +
        match n with
        | NLeaf b -> (int64) b.Length
        | NInner (_,b,n1,n2) -> (int64) b.Length 
                              + roughNodeMemSize n1
                              + roughNodeMemSize n2 

    type Root = Option<struct(Binary * Node)>




/// A simple key-value tree using Stowage resources.
///
/// A key-value tree modeled above Stowage essentially gives us first
/// class databases with persistence, structure sharing, and efficient
/// serialization. Because most of the data is in the stowage layer, it
/// is feasible for such trees to grow much larger than memory.
///
/// We use a variant of the crit-bit tree structure, tweaked to simplify
/// merge of trees (by keeping the least key with the parent node). The
/// compaction operation is also separate, so adding a bunch of keys will
/// gradually modify a tree in memory without pushing every intermediate
/// node to disk.

that keeps the
/// least key in the parent node (for more efficient merge) and uses an
/// explicit compaction step.
/// 
/// This specific tree happens to be a variant of the crit-bit tree, which
/// has history-independent structure modulo compaction. This means the
/// tree structure will be equivalent, after compaction, for a given set of 
[<Struct>]
type KVTree =
    val internal Root : I.Root
    internal new() = { Root = None }
    internal new(root:RscHash) = { Root = I.loadRoot root }
    // TODO: custom equality, custom comparison



