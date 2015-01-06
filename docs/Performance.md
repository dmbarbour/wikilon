
A strong desire is that Wikilon should have competitive performance. I don't really expect to match C anytime soon, e.g. not without a lot of very good optimizers, and the ability to leverage register-sized number types (via annotation or inference). But I would like to at least be competitive with other scripting languages.

I believe the ABC or AO interpreter will be the main bottleneck.

I can probably speed up ABC interpreter a lot, e.g. by recognizing common functions - swaps, fixpoints, etc. - and replacing long sequences of bytecodes with dedicated Haskell functions. Even better if I use something like ABCD to make this happen, such that I don't need to spend repeated efforts on recognizers. 

A related possibility is to recognize common data structures or data types - e.g. matrices or vectors, lists, streams - and common functions on them. The possibility exists to translate these structures into a more compact form, and to optimize functions on them even further.

A third possibility is to support compilation.

Compilation has the advantage of being potentially more generically applicable compared to recognizing common functions (though we can still benefit from that). It allows optimization of code independent of the domain. And possibly some specializations on data type. However, generating code in the target language is an issue, and so is loading the code. 

Unfortunately, Haskell doesn't make it easy to compile and load code into the current process.

There is the 'plugins' library, but that seems buggy and slow when I tried it before, and probably requires very concrete knowledge of the data and monad types to be effective. I might try to use the GHC API directly, or push development of a new plugins package. Another possibility is to target LLVM, and leverage Haskell's libraries to load LLVM code into the runtime (Stephen Diehl has a tutorial on this subject). 

Cross-compilation is an interesting possibility, especially for cases involving GPGPU, FPGA, JavaScript on browsers, etc.. I think it's a worthy area to explore, but is somewhat independent of the Wikilon performance concern. A separate process can't directly interact with the Wikilon memcache or filesystem.

So, long term, I'll probably be trying the plugins approach again. 

Short term, I should perhaps focus on recognizing common subprograms and optimizing them by hand, and perhaps focusing on just a few common data types for efficient collections-oriented programming.