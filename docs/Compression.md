Stream compression such as gzip and zlib are very useful for compressing large ABC subprograms by finding local patterns. ABC is repetitive enough to make this worthwhile even with a small window. I'd prefer to use one simple compression algorithm for most streaming purposes, even if it isn't ideal.  Unfortunately, this technique is not useful for patterns that are large but low-frequency. In those cases, I need something more like dictionary compression.

With use of VCache now available, support for dictionary compression could be modeled indirectly, e.g. in terms of capturing content in a VRef. I already anticipate something similar for quoting large values, i.e. such that I can load very large objects (e.g. maps, trees) lazily from disk. Adding support to load sequences of more arbitrary (non-value) code shouldn't be difficult, requiring only a little information about type.

So, on the representation side, there is no difficulty with global dictionary compression across lots of smaller ABC components.

The challenge is how to decide what should be compressed, and how to compress new content. I'll need to think about this a fair bit. One possibility is to process all ABC through some machines (e.g. an LZW variant with exponential decay, or a series of them) that build a 'global commons' dictionary that might later be mined for ABCD options. 

Compression within a dictionary is also an interesting possibility. It might be worthwhile in cases where developers have very large words, e.g. modeling ad-hoc data. But it might be better to focus on improving the languages and dictionary so the languages manage their own dictionary compression (e.g. some reflective manipulations of the dictionary, and perhaps some content-addressing/searches in the dictionary to combine identical structures - "create or find an anonymous word for this code: ..." where anonymous words are something like `#abcBase16#`, i.e. something that humans wouldn't type out by hand.



