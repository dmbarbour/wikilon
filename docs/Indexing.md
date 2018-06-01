# Indexing Dictionaries

I need to index dictionaries for effective use. 

Which indices are most useful?

* word → version
* version → evaluated definition
* version → optimized/compiled forms
* version → type of word
* Reverse Lookup: word → client words 
* Undefined or cyclic words.
* Type Search for Editing: types → words or word-versions
 * find badly typed words, too

A challenge for indexing is that Wikilon will have many versions of dictionaries, both for different users and version histories. It's important (for security and privacy) to ensure our index lookup is specific to a dictionary. Fortunately, our word→version and word→clients indices should have a fixed, predictable overhead per dictionary (in both space and time). We can maintain various indices together with our dictionaries. 

Version identifiers - an easy solution is to take a secure hash using Stowage.RscHash, albeit tweaked a little to avoid undesirable interaction with Stowage GC. It would be sufficient to combine the code for a definition with the versions of each dependent word.

Undefined or cyclic words could be recorded as similar to adding an `UNDEFINED` symbol for reverse lookup. Or more generally, we could record a set of "problem" words, together with a short description of the problem. But we cannot generally compute cycles locally. It would be most convenient to recognize cycles when computing a version at each word.

