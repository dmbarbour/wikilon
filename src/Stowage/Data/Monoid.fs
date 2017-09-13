
namespace Stowage

type Monoid<'M> =
    abstract member Zero : 'M
    abstract member Plus : 'M -> 'M -> 'M

/// Monoid singletons on demand.
type MonoidSource<'M when 'M :> Monoid<'M> and 'M:(new: unit -> 'M)> private () =
    static let zero : 'M = (new 'M() :> Monoid<'M>).Zero
    static member Instance = (zero :> Monoid<'M>)

