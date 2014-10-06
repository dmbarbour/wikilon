If I plan to keep a lot of history in Wikilon (for version control purposes), it may be useful to reduce the size of this as much as possible. It may be useful to apply compression for serialized forms within acid-state, i.e. to reduce the amount of information stored on disk and the amount in-memory in case of lazy processing.

My initial idea was to use compressed ABC for transactional updates. This would allow me to leverage compression also used for ABC resources and streams, and thus improve all three.

I like the idea of a private dictionary compression. Phrases such as:

        0xFD a b c 

Could index into a private dictionary, global within the Wikilon instance, with up to many million items (I propose 7 million for base192). Such a dictionary could greatly optimize internal storage for highly repetitive elements across versions of the dictionary, and should compose effectively with other forms of compression that operate at different scales (base16, LZSS, ABCD, ABC resources). 

The dictionary in this proposal would have special restrictions on mutation. In particular, I could not mutate a definition without first performing a checkpoint with the item removed, e.g. to garbage collect elements that prove ineffective at compression.

The difficulty I'm having is how to serialize while assuming a private dictionary, especially in contexts such as *acid-state* or the *cereal* package. The important part is that I should not include the dictionary in the compressed form. One viable option is to explicitly include an external compression context:

* Compress using the pair `(Dict,Value)`
* Decompress to an alternative form, `Dict → Value`

But might be a bit tricky. I could use `Either` types or something to make it work, but that seems like a hack.

RESOLUTION: Optimize later.

For now, I'll leave this idea of private dictionary compression alone, and focus on the other four layers of compression. Good support for LZSS should be a big advantage without nearly as much hassle. Safecopy should allow me to transparently change serialization formats later on.
