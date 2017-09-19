namespace Stowage

/// Value differences for key-indexed values.
type VDiff<'V> =
    | InL of 'V         // value in left
    | InR of 'V         // value in right
    | InB of 'V * 'V    // two values with equality failure

