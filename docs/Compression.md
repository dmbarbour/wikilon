Stream compression such as gzip or zlib aren't very good for patterns that have a low frequency but high impact. It may be worthwhile to create a new variation of dictionary compression to address these other patterns, and provide eventual suggestions for ABCD.

RESOLUTION: Optimize later.

For now, I'll leave this idea of private dictionary compression alone, and focus on the other four layers of compression. Good support for LZSS should be a big advantage without nearly as much hassle. Safecopy should allow me to transparently change serialization formats later on.
