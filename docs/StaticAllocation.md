

# Statically Allocated Functional Language

The work by Mycroft and Sharp on Statically Allocated Functional Language (SAFL) is most tempting to incorporate into the designs of Awelon project, potentially to redesign Awelon Bytecode a small bit. 

SAFL has a simple design constraint to support static allocation: that recursion (within a mutually recursive group of functions) is only permitted in a tail-call context. This is combined with acyclic static scope for functions outside a local group. It's an interesting design constraint.

Funloft programming language also has a statically computable, maximum memory usage. 

How can static allocation be realized for a streamable code? Should it be? An interesting alternative, perhaps, is to specify that certain subprograms require static space. 
