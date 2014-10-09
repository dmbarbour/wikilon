
One question I've been asking myself is: how should I prevent users/guests from accidentally or maliciously modifying 'important' words? e.g. the words where everything breaks if you change them? We can do a lot with unit tests, and requiring tests pass and programs typecheck, and so on. But a malicious gest might remove the broken tests and such. 

The best idea I've had so far is this: define a directive-word like `frozen!word` to indicate that the behavior for a word should not change without approval. Changing a frozen directive, in either direction, would require the same approval. Ideally, very few words are frozen. This gives wiki administrators - and users, too - some extra alerts about frozen words, without really preventing them from proposing edits or editing the definitions within their own sessions.

Beyond this, if a user session does break something - e.g. the web-app with which they're editing the dictionary - it might be useful to have an easy fallback. I propose that adding `!` prefix to a URL for a web-app should select the transactional view of it, i.e. rather than the session view, while still working with the session's state.

# Bad Ideas

To avoid revisiting mistakes, or at least escape them quickly...

* `word@transactionId` is too verbose and hinders exponential decay
* `@frozen word def` - defining words as frozen from the start, hinders transaction merge since we'd need to know all the dependencies of def. Also, the best implementation - compiling the word down to to ABC, and installing that ABC - is easy to do by hand.
