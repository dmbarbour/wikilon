If I plan to keep a lot of history in Wikilon (for version control purposes), it would be useful to reduce the size of this as much as possible. 

Anyhow, it might be useful to apply compression for the serialized forms with acid-state (or external file). 

I think applying the simple ABC stream compression would be a good start. It includes an LZSS compression, albeit with a small 2k window, plus a binary compression pass. This would be enough to compress most words, excepting only those that use embedded literal objects or large texts.

Okay, so that's a start. To this, we can add compression by ABCD (which uses a *global* dictionary for frequent, correct, highly useful sequences of ABC) and ABC resources (which allows separate compilation and linking, albeit to the tune of 99 characters - compressing to 53 binary - per link). These can both offer very high levels of compression in the right situation.

What else can we do?

Well, one idea is to apply a *private* dictionary compression prior to the ABC streaming pass, using something like:

        0xFD a b c

Here the header byte is invalid UTF-8, allowing us to escape the UTF-8 stream to access a dictionary. The three dictionary index bytes in base192 will follow. This allows me to build a large dictionary, up to seven million elements. Words should have minimum ABC size 12 to 16 to ensure effective compression of 70% or higher; in practice, I expect many will be much larger.

Unlike the mutable AO dictionary, I cannot edit a word in this dictionary after I use it. I might be able to GC a word that goes unused for a long enough time, but that will be rare due to keeping histories. So I should be conservative. My idea for this is to pursue just a few candidate words at a time, i.e. to build the 7M dictionary very gradually based on observed frequencies on a relatively large scale. And I should require a minimum size before compression, of perhaps 16 bytes in the original UTF-8 encoding.

Such a dictionary might also be very useful for stream compression, e.g. using `0xFC a b c DynLen Value` to define a new word idempotently, then `0xFD a b c` to reuse it. (The undefined elements of UTF-8 are proving excellent for this sort of feature!) In this streaming case, we can assume base 256 on read even if we write in base 192. 

(Note: We MUST NOT directly share a Wikilon internal dictionary, because said dictionary may encode some private information. But it's always okay to share a word for which we're about to send a definition anyway, since that does not change how much security sensitive information the client receives. They only learn a little extra about the frequency with which a given substream shows up.)

If accepted, this gives us five layers of compression:

* ABC base16 (`bdfghjkmnpqstxyz`) to `0xF8 (0..253) (3..256 octets)` 
* LZSS compression plus literals aggregation (to limit expansion)
* Wikilon private dictionary (`0xFD a b c`; base 192 - 7M elements)
* ABCD (global standardized dictionary; extended UTF-8 range)
* ABC resources `{#secureHashCiphertextSecureHashBytecode}` if reused

If applied correctly, these five layers of compression should be mostly orthogonal to one another, operating at different scales in space/time. They should stack and augment one another effectively, though not perfectly. And worst-case expansion is effectively limited.
