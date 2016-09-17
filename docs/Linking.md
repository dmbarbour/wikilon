
# Linking in ABC and AO

## Scenario as of 2016 September

I currently use two linking concepts that are very similer. 

At the ABC layer, I use `{'resourceId}` to bind an stowed value. The resource ID may involve a secure hash of the target or an HMAC for security. It may be necessary to identify a shared fragment of a larger resource. Either way, we assume stowed value resources are immutable. In general, they're also garbage collected.

At the AO layer, I use `{%word}` to link definitions within the local dictionary and permit `{%word@source}` to identify a word in an external dictionary. In the general case, dictionaries are mutable. However, it is possible to specify immutable dictionaries (e.g. by secure hash). Words are frequently human-meaningful and self-documenting. Words may also have *associative* structure, e.g. `word.doc` would document our `word`.

Can I do better? I believe so. I also hope so. 

## Unified Linking Model? (Accepted)

An obvious possibility is to unify the AO and ABC linking models. What changes?

Pros:
* Human-meaningful symbols will in distributed ABC objects simplify debugging.
* A shared `word` may include `word.doc` explanations, `word.test` examples.
* Development tools can leverage associated `word.render` or `word.type` hints.
* Effectively, we unify ABC and AO. There is no need for a separate AO layer.
* Linking of both mutable and immutable structure is feasible at global scales.

Cons:
* Dictionary-level security and garbage collection become more critical.
* May desire to explicitly associate attributes when generating value stowage.
* Link cycles are difficult to prevent, but remain relatively easy to detect.

I believe the potential metadata benefits for human comprehension and tooling/rendering of ABC artifacts weighs strongly in favor of this unification. Those features are very important for my Awelon project vision.

Further, I believe that some of the challenges raised are worthy even in the separate model. I've already been contemplating memory management and security for my [application models](ApplicationModel.md). I wonder if I can do better by shifting many such concerns into a link layer?

