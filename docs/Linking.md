
# Linking in ABC and AO

## Scenario as of 2016 September

I currently use two linking concepts that are very similer. 

At the ABC layer, I use `{'resourceId}` to bind an stowed value. The resource ID may involve a secure hash of the target or an HMAC for security. It may be necessary to identify a shared fragment of a larger resource. Either way, we assume stowed value resources are immutable. In general, they're also garbage collected.

At the AO layer, I use `{%word}` to link definitions within the local dictionary and permit `{%word@source}` to identify a word in an external dictionary. In the general case, dictionaries are mutable. However, it is possible to specify immutable dictionaries (e.g. by secure hash). Words are frequently human-meaningful and self-documenting. Words may also have *associative* structure, e.g. `word.doc` would document our `word`.

Can I do better? I believe so. I also hope so. 

## Import Sources (Accepted)

For compact code, I propose that instead of `{%word@secureHash}` we use `{%word@source}` then define a tagged value such as `@source ["Hash" "secureHash"]`. This has several benefits:

* `source` is word of predictable size, structure, and aesthetics
* link data can be richly structured (multi-homed, security, etc.) 
* great potential for metaprogramming, construct `source` as value
* trivial to re-route sources without rewriting the entire dictionary

This seems like an obvious improvement. Also, it lets me go back to small, 64-byte tokens.

## Unified Linking Model (Accepted)

An obvious possibility is to unify the AO and ABC linking models. What changes?

Pros:
* Human-meaningful symbols will in distributed ABC objects simplify debugging.
* A shared `word` may include `word.doc` explanations, `word.test` examples.
* Development tools can leverage associated `word.render` or `word.type` hints.
* Effectively, we unify ABC and AO. There is no need for a separate AO layer.
* Linking of both mutable and immutable structure is feasible at global scales.

Cons:
* For evaluation with dynamic linking, I must track dictionary contexts.
* Potential difficulty specifying *where* stowage is applied.
* Dictionary-level security and garbage collection become more critical.
* May desire to explicitly associate attributes when generating value stowage.
* Link cycles are difficult to prevent, but remain relatively easy to detect.

I believe the potential metadata benefits for human comprehension and tooling/rendering of ABC artifacts weighs strongly in favor of this unification. Those features are very important for my Awelon project vision.

Further, I believe that some of the challenges raised are worthy even in the separate model. I've already been contemplating memory management and security for my [application models](ApplicationModel.md). I wonder if I can do better by shifting many such concerns into a link layer?

## Secure Sources

Okay, so assuming I have `{%word@source}` then I also define word `source` to tell the linker where to find the dictionary. In the simplest case, the source might simply describe a URL or a secure hash where.




Information security, then, would be achieved by constructing *fine-grained* dictionaries that contain no more data than is necessary for computation and human comprehension. The original stowage may effectively consist of single-element dictionaries.








I believe the best answer is to simply insist that a `{%foo@resourceId}` grants read-access to that *entire* resource. This is necessary if we're going to validate a resource identifier by secure hash, and for convenient access to attributes like `foo.doc` or `foo.example`.




* ABC resources become more debuggable. This is both because `{%word@source}` may include the human-meaning allows us to find `{%word.doc@source}`. 
* ABC resources become more debuggable





* dictionaries can directly reference the ABC stowage layer
* 

Unifying AO and ABC linking could simplify sharing and export of ABC resources, 


and potential metadata documentation of stowed values. ABC resources could use lightweight, immutable dictionaries.







In general, du


 I can construct new resources via *stowage*. For security and validation, a resource id may involve a secure hash or contain an HMAC. Either way, I assume ABC resources are immutable.

At the AO level, I permit tokens of form `{%word@source}` to identify a word from an external dictionary. I

