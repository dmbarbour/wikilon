
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

Or maybe I should try to use link relations?

        [{%doc.foo}]{&rel:doc}%

Probably not. If I want interactive documentation, I can't be adding a dependency on `doc.foo`. 

For now, I'm favoring `foo.doc` as the convention for documentation on `foo`. Though, use of a hyphenated word foo-doc or some other structure is also tempting. This shifts type or role information into the suffix, while the prefix is namespace or similar.

