
-- | While Awelon Bytecode may be naively interpreted, doing so is
-- not efficient. Wikilon will pre-process bytecode to support many
-- performance tweaks:
--
-- * bytecode held in relatively compact bytestrings
-- * fast slicing for texts, blocks, tokens in bytecode
-- * quoted values may be computed once ahead of time
-- * faster copy via copyable annotations for values
-- * larger-than-memory values supportable via VCache
-- * shared structure for large values via VCache
-- * ABCD-like extended dictionary of accelerated ops
-- 
-- Consequently, Wikilon ABC is a bit specialized regarding the
-- value types involved. This module focuses on 'true' value
-- encodings. Though, I'm also interested in type-oriented 
-- value encodings.
module Wikilon.ABC 
    (
    ) where

