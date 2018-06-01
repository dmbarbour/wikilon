namespace Awelon
open Data.ByteString
open Stowage

// An experimental alternative representation for Dict, aiming for
// improved performance. This affects the in-memory model, caching,
// and compaction. The serialized representation is compatible with
// the original model. 
module Dict2 =
    type Symbol = Dict.Symbol
    type Prefix = Dict.Prefix
    type Def = Dict.Def
    type DictEnt = Dict.DictEnt


    let cSP = 32uy

    type DefUpd = Def option    // None to delete, Some to define

    [<Struct>]
    type Dict = 
        { vu  : DefUpd option   // optional update for empty symbol 
          cs  : Map<byte,Child> // all children, using SP for proto
        }
    and Child = (struct(Prefix * CVRef<Dict>))
        // consider use of a specialized Map type; I feel we could
        // optimize for small maps with byte keys. A simple array
        // would be a viable option, for example.



