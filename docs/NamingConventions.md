
Naming Conventions for Words
============================

Wikilon is a fresh start. I'd like to consider naming conventions compared to the old AO.

One consideration is that I'm now keeping everything in a Word-based Trie. So common prefixes will be easier to look up. But this is a rather minor performance consideration in the long run. 

Another consideration is that I can discriminate in the UI based on suffixes and prefixes. 

* words with common prefix (or suffix) given a common type
* words with common prefix (or suffix) rendered with CSS styles
* prefix or suffix hiding when rendering words

So, let's consider a particular case: documentation for a word 'foo'.

Should this be `foo.doc` or `doc.foo`? Or maybe `doc:foo` or `foo-doc`?

After experimenting with this a bit, I'm starting to think the `doc:foo` approach might be preferable. It places all the documentation in a browseable directory by prefix, and all the docs should have a similar type. I'd also like something like `blurb:foo` or `shortdoc:foo`, something that fits into just one line.
