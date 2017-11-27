namespace Stowage

/// A Finger Tree in Stowage
///
/// The Finger Tree is the 'swiss army knife' of data structures. It 
/// supports efficient O(1) operations at each edge. Monoidal summaries
/// can support efficient random access, priority, etc.
///
/// This finger tree assumes relatively cheap computation for measures,
/// and chooses to recompute rather than cache for local data.
module FingerTree =

    /// Branches for the 2-3 tree structure.
    type B<'V> =
        | B2 of 'V * 'V
        | B3 of 'V * 'V * 'V

    /// A node is the main element we compact for Stowage.
    [<Struct>]
    type Node<'V, 'M> =
        val M : 'M
        val B : CVRef<B<'V>>
        new(m,b) = { M = m; B = b }
        interface IMeasured<'M> with
            member node.Measure = node.M

    /// Digits at each edge of a finger tree.
    type D<'V> =
        | D1 of 'V
        | D2 of 'V * 'V
        | D3 of 'V * 'V * 'V
        | D4 of 'V * 'V * 'V * 'V

    let private measureD (p:'M -> 'M -> 'M) (m:'V -> 'M) (d:D<'V>) : 'M =
        match d with
        | D1 (v1) -> m v1
        | D2 (v1,v2) -> p (m v1) (m v2)
        | D3 (v1,v2,v3) -> p (m v1) (p (m v2) (m v3))
        | D4 (v1,v2,v3,v4) -> p (m v1) (p (m v2) (p (m v3) (m v4)))

    let inline private measure (v:'V) : 'M = (v :> IMeasured<'M>).Measure 

    type Tree<'V, 'M when 'V :> IMeasured<'M> and 'M :> Monoid<'M> and 'M:(new:unit -> 'M)> =
        | Empty
        | Single of 'V
        | Many of D<'V> * 'M * Tree<Node<'V,'M>,'M> * D<'V>
            // note: measure in Many is cache only for central FingerTree.
        member t.Monoid with get() = MonoidSource<'M>.Instance 
        interface IMeasured<'M> with
            member t.Measure : 'M =
                match t with
                | Empty -> t.Monoid.Zero
                | Single v -> measure v
                | Many (pre,mt,t,suf) ->
                    let p = t.Monoid.Plus
                    let m d = measureD p measure d
                    p (m pre) (p mt (m suf))


