
Kyle Blake has developed abcc, which does some typechecking based on an infinite terms model.

However, for AO, I don't really need infinite terms. I'd like to look into Bidirectional Typechecking.

An interesting thought: While ABC and AO don't have 'nominative' types per se, they do have sealed values that can act like `newtype` declarations, e.g. `{:foo}` as a discretionary sealer. An interesting possibility is to leverage value sealing to help name datatypes, e.g. a typechecker or linter could notice when a sealer is used for incompatible data structures then raise a warning.

This behavior or intention could be made more explicit with annotations, e.g. `{&type:foo}` could indicate that values with the same name should have some common properties, which might be described elsewhere within the dictionary. It's certainly an interesting possibility to think about.
