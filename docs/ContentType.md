
# RESTful Design and Content Type

A common recommendation for RESTful design is to leverage 'Content-Type' to provide both multiple views of a single resource and API versioning. Some sites suggest URLs for versioning, but other people make [excellent arguments against it](http://stackoverflow.com/a/975394/482149). 

I'm not sure how I feel about all of this quite yet. I'm not yet experienced enough with web development to have an opinion. At the very least, it shouldn't hurt to give it a try.

Mime-types seem dreadfully arbitrary to me. When do we use `application/` vs. `text/`? It seems we're supposed to use `text` for content that is directly read or written by humans, and `application` otherwise. So let's give these ideas a try:

* `text/vnd.org.awelon.aodict` - a very specialized import/export format
* `text/vnd.org.awelon.abcdef` - ABC code used within a definition, allows `{%word}` dependencies on an implicit dictionary, and should have definition type `∃v.∀e.e→([v→[a→b]]*(v*e))` in context of a dictionary.
* `application/vnd.org.awelon.abcval` - ABC code of type `∀e.e→(val*e)` for some value type. Must not contain tokens other than annotations and discretionary sealers/unsealers.
* `application/vnd.org.awelon.abc` - purely functional ABC code of some arbitrary type. Must not contain tokens other than annotations and discretionary sealers/unsealers.

Note that I won't be using `application/octet-stream` for ABC content. I might later use extensions or options such as `+d` for ABCD extensions, `+rsc` for `{#resourceId}`, and `+crypto` for common cryptographic sealers/unsealers.

I might try `application/json` as an encoding of dictionaries, though I'm not particularly inclined towards it. Seems easy enough to receive the `aodict` format then process it in JavaScript. Or maybe YAML, or XML. But for now, I'd like to stick with very lightweight representations.

I'll probably want to use `multipart/form-data` for all form inputs.
