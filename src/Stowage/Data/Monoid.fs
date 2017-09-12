
namespace Stowage

type Monoid<'M> =
    abstract member Zero : 'M
    abstract member Plus : 'M -> 'M -> 'M

type DefaultMonoid<'M when 'M :> Monoid<'M> and 'M:(new: unit -> 'M)> private () =
    static let zero : 'M = (new 'M() :> Monoid<'M>).Zero
    static member Zero = zero
    static member Monoid = (zero :> Monoid<'M>)

