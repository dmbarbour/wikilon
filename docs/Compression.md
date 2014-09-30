If I plan to keep a lot of history in Wikilon (for version control purposes), it would be useful to reduce the size of this as much as possible. 

Anyhow, it might be useful to apply compression for the serialized forms with acid-state (or external file). 

I think applying the simple ABC stream compression would be a good start. It includes an LZSS compression, albeit with a small 2k window, plus a binary compression pass. This would be enough to compress most words, excepting only those that use embedded literal objects or large texts.

Okay, so that's a start. To this, we can add compression by ABCD (which uses a *global* dictionary for frequent, correct, highly useful sequences of ABC) and ABC resources (which allows separate compilation and linking, albeit to the tune of 99 characters - compressing to 53 binary - per link). These can both offer very high levels of compression in the right situation.

What else can we do?

Well, one idea is to apply a dictionary compression prior to the ABC streaming pass, using something like:

        0xFD a b c

A header byte (which is invalid UTF-8) plus three dictionary index bytes in base192. This could allow me to build a large (up to seven million elements) internal yet Wikilon instance specific dictionary for efficient storage purposes across all input histories, texts, and so on. In addition, this dictionary could be useful to mine for ABCD candidates.

Unlike the mutable AO dictionary, I cannot edit a word in this dictionary after I use it. I might be able to GC a word that goes unused for a long enough time, but that will be rare due to keeping histories. So I should be conservative. My idea for this is to pursue just a few candidate words at a time, i.e. to build the 7M dictionary very gradually based on observed frequencies on a relatively large scale. And I should require a minimum size before compression, of perhaps 16 bytes in the original UTF-8 encoding.

Such a dictionary might also be very useful for stream compression, e.g. using `0xFC a b c DynLen Value` when first defining a word then `0xFD a b c` to reuse it. (The undefined elements of UTF-8 are proving excellent for this sort of feature!)

(Note: We MUST NOT share the full internal dictionary, since that could be a big security hole that would be difficult to patch. Similarly, we cannot allow external entities to query for individual words. But sharing a word when we first use it within a given stream/session is fine because we're already sharing the content of that word.)

If accepted, this gives us five layers of compression:

* ABC base16 (`bdfghjkmnpqstxyz`) to `0xF8 (0..253) (3..256 octets)` 
* LZSS compression plus literals aggregation (to limit expansion)
* Wikilon local dictionary (`0xFD a b c`; base 192 - 7M elements)
* ABCD (global standardized dictionary; extended UTF-8 range)
* ABC resources `{#secureHashCiphertextSecureHashBytecode}` if reused

If applied correctly, these five layers of compression should be mostly orthogonal to one another, operating at different scales in space/time. They should stack and augment one another effectively, though not perfectly. And worst-case expansion is effectively limited.

