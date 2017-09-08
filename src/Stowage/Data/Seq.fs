namespace Stowage
open Data.ByteString

/// A finger-tree based sequence, modeled above Stowage.
/// 
/// Finger-trees are an interesting persistent data structure that offer
/// O(lg(min(K,N-K))) access to the Kth of N elements. This reduces to
/// O(1) access to the first or last few elements. Similar logarithmic 
/// properties apply to splitting or concatenation of the finger-trees.
///
/// Consequently, finger-trees can be used for: queues, stacks, dequeues,
/// lists or vectors, ropes, and many things that can be built above a
/// sequence.
///
/// Finger trees do have weaknesses: weak memory locality, and history
/// dependent structure. They're useful despite these limitations, but in
/// some cases a more specialized structure might be better.



