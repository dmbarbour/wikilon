namespace Awelon
open Data.ByteString

// ParseExt is a 'default' textual projection for Awelon with all
// the bells and whistles we can implement that *don't* depend on
// special knowledge about a dictionary. This is intended both as
// an initial scaffolding for programming Awelon, and a fallback 
// or 'safe mode' textual projection for debugging (if needed).
//
// In older versions of Awelon, I might have called this `CLAW` as
// a command language for Awelon. However, 
//
module ParseExt =

    // SUPPORTABLE NOW (based on developed techniques):
    //  - local variables
    //  - monadic sequences
    //  - simple list literals
    //  - labeled data (records, variants)
    //  - basic numeric notations
    //  - raw vs preprocessed literals
    //  - lightweight qualified namespaces

    type NatRep = uint64
    type IntRep = int64
    type RatRep = (struct(IntRep * NatRep))
    type DecRep = (struct(IntRep * int16)) // 3141 -3 => 3.141
    type SciRep = (struct(DecRep * IntRep)) // Decimal*10^Exponent

    type Number =
        | Nat of NatRep
        | Int of IntRep
        | Rat of RatRep
        | Dec of DecRep
        | Sci of SciRep


    // Potential Future Extensions:
    //
    //  - pattern matching (parser combinators)
    //  - concise type descriptors

