
Hashes for Names
================

Paul Chiusano is, for Unison project, vigorously pursuing [hash-based identities](http://unisonweb.org/2015-05-18/let-blocks.html#a-idhashingaa-quick-preview-of-unisons-nameless-hashing). He's doing it carefully, too! Hashes are normalized with respect to:

* rendered variable names
* order of bindings in a letrec block
* linking of names

For Awelon Bytecode, I've been thinking about use of hash based resources for separate compilation, i.e. a resource might be identified by `{#secureHashOfBytecode}` possibly together with a decryption key (though I could separate these features with an unsealer). The idea of normalizing seems very nice. How might we go about accomplishing this?

1. One option, I suppose, is to fully expand the program and hash the expanded form. This would probably work well enough, but would make verification of hashes expensive for large programs.
2. Another option is to automatically hash on certain boundaries, e.g. within blocks and between blocks. Unfortunately, this would lead to a high density of links and fine-grained resource bindings.

A third option is to forget the whole normalization concern. Developers may use hashed resource identifiers in bytecode, but there is no guarantee of stability for linking. Stability instead comes from maintenance practices. This is my original design, and has simplicity advantages. Fundamentally, we'll always have multiple hashes for a given function, so hashes cannot be associated tightly with meaning.

At the moment, I lean towards this third option. Normalization can always be performed above a less constrained model.

 




