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


    type FingerTree<'V, 'M when 'V :> IMeasured<'M> and 'M :> Monoid<'M> and 'M:(new:unit -> 'M)> =
        | Empty
        | Single of 'V
        | Many of 'M * Digit<'V> * FingerTree<Node<'V,'M>,'M> * Digit<'V>
        interface IMeasured<'M> with
            member tree.Measure : 'M =
                match tree with
                | Empty -> DefaultMonoid<'M>.Zero
                | Single v -> v.Measure
                | Many (m,_,_,_) -> m



