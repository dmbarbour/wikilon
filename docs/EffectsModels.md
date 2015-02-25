
# Effects Models for Wikilon and Awelon Project

Awelon Bytecode is designed to support object capability model for side effects. At the bytecode layer, a token such as `{foo}` calls into the environment to request an arbitrary effect. ABC compilers or ABC to ABC optimizers assume use of these tokens protects causal commutativity and spatial idempotence. 

At the dictionary layer, I've permitted two kinds of tokens: annotations (prefix `&`) and discretionary sealer/unsealer pairs (prefix `:` and `.` respectively). Annotations aren't allowed to have any internally-observable effect on their input, but can be useful for performance, debugging, validation. Discretionary sealers serve a role similar to `newtype`. But neither is truly a side-effect like.

But capabilities don't seem ideal for imperative code. Under the causal commutativity and spatial idempotence constraints, I'm forced either to linearize a central power token (e.g. `[{!secret}]kf` used in the earlier `aoi` REPL) or somehow control aliasing of state. I might work around this by modeling capabilities indirectly using cryptographic text, e.g. "zszfqkmtjbxymdmfftbpgxmnxbtyqtxz", but I'd rather avoid using this approach more than absolutely necessary. If I can avoid embedding cryptographic text in the Wikilon dictionary, perhaps making an exception for toplevel configurations of hosted applications (via abstract virtual machines), that would be preferable.

An alternative to capabilities is to favor monadic effects, much like Haskell. 

Approaches to modeling monadic effects in dynamic languages [have been explored](http://eighty-twenty.org/2015/01/25/monads-in-dynamically-typed-languages/). And very generic approaches are also applicable, e.g. modeling a free monad or a delimited continuations monad. When imperative effects are modeled monadically, it should be easy to reuse imperative algorithms in non-imperative contexts, e.g. by capturing effects and modeling them functionally. Well... maybe not 'easy', but at least much easier than doing the same with capabilities. I wonder if a free monad might offer effective support for specializing code via staged programming or partial evaluation.

Usefully, we may implement delimited continuations or monadic effects at the top level by having them call into our powerblock, thus supporting a simple portability layer (albeit, in only one direction) between the different effects models.

Capabilities for RDP are a better fit for Awelon project. Usefully, a lot of imperative code modeled using monadic effects could also be used in RDP systems, e.g. for processing a collection or embedding in a state resource. But even for this role, it might be worth exploring a more functional approach, e.g. something based on arrows that protects causal commutativity and supports staging, shifting most capabilities to the toplevel of the code.

## Kinds of Effects?

For Wikilon hosted applications, I think the main effects will relate to networking and state. Other potential effects include: capturing failure in a subprogram, and possible uniqueness sources, secure-random numbers, etc.. And annotations, of course, for performance and validation.

Reflective effects to observe or manipulate a dictionary associated with an abstract virtual machine might also be useful. 

State resources may require special attention. Do I want something like CRDTs to allow machines to connect to one another via publish-subscribe?


