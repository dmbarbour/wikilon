
While developing Wikilon, I've occasionally returned to the idea of 'frozen' code, to support certain features or stability properties. A primary example is that modeling [embedded literal objects](EmbeddedLiteralObjects.md) might benefit from words that are protected from mutation. Or if we have a nice, working version of some application, we might wish to freeze it so we don't accidentally break it later.

Now, I don't want to completely prohibit mutation of frozen code. For embedded literal objects, I might want to refactor, optimize, clean up non-essential histories, and so on. For frozen applications, we might wish to occasionally edit to fix a bug, albeit only after a lot of careful testing.

My current idea is to use directive words:

        @define frozen!foo

A simple policy, then, could validate that frozen words aren't modified or renamed to non-frozen words, and generally go through special validation processes. A similar policy might explicitly recognize ELO words, such that we don't need to explicitly mark them as frozen.

This would occur at the same layer as policies that require typechecking of AO code and so on. The dictionary model itself will remain ignorant of these policies.
