namespace Stowage

/// A Finger Tree in Stowage
///
/// The Finger Tree is the 'swiss army knife' of data structures. It 
/// supports efficient O(1) operations at each edge. Monoidal summaries
/// can support efficient random access, priority, etc.
module FingerTree =

    type NodeRef<'V> =
        | Local of 'V
        | Remote of LVRef<'V>

    type Node<'V, 'M> =
        | Branch2 of 'M * NodeRef<'V * 'V>
        | Branch3 of 'M * NodeRef<'V * 'V * 'V>
        interface IMeasured<'M> with
            member node.Measure : 'M =
                match node with
                | Branch2 (m,_) -> m
                | Branch3 (m,_) -> m

    type Digit<'V> =
        | One of 'V
        | Two of 'V * 'V
        | Three of 'V * 'V * 'V
        | Four of 'V * 'V * 'V * 'V

    let private measureDigit (p:'M -> 'M -> 'M) (m:'V -> 'M) (d:Digit<'V>) : 'M =
        match d with
        | One v1 -> m v1
        | Two (v1,v2) -> p (m v1) (m v2)
        | Three (v1,v2,v3) -> p (m v1) (p (m v2) (m v3))
        | Four (v1,v2,v3,v4) -> p (m v1) (p (m v2) (p (m v3) (m v4)))

    let inline private measure (v:'V) : 'M = (v :> IMeasured<'M>).Measure 

    type FingerTree<'V, 'M when 'V :> IMeasured<'M> and 'M :> Monoid<'M> and 'M:(new:unit -> 'M)> =
        | Empty
        | Single of 'V
        | Many of Digit<'V> * 'M * FingerTree<Node<'V,'M>,'M> * Digit<'V>
            // note: measure in Many is cache only for central FingerTree.
        interface IMeasured<'M> with
            member t.Measure : 'M =
                let monoid = MonoidSource<'M>.Instance
                match t with
                | Empty -> monoid.Zero
                | Single v -> measure v
                | Many (pre,mt,t,suf) ->
                    let p = monoid.Plus
                    let dv = measureDigit (p) (measure)
                    p (dv pre) (p mt (dv suf))


