
-- | This module provides a simple import and export format for Awelon
-- dictionaries, emitting an entire dictionary as a large file. This
-- dictionary uses the following format:
--
--   %over [{%dupd} {%swap}][]
--   %dupd [rw{%dup}wl][]
--   %swap [rwrwzwlwl][]
--   %dup [r^zlwl][]
--   %doc.dup "(Copyable x) ⇒ x -- x x
--    ~[{%docString}]
--
-- Or more generally:
--
--   %word bytecode
--   %anotherWord more bytecode
--   %multiLineWord bytecode continues
--    on another line, each LF escaped
--    by following SP
--
-- The bytecode in this case has models a (v→fn,v) pair such that a
-- single operator `$` will compile the value into a function. The
-- split logic between compiler and value helps model an abstract
-- syntax, structured views and editors.
--
-- This export model is intended for easy extension into streamable
-- models, i.e. by providing different prefixes for extended sets of
-- commands.
--
-- In the basic case, however, we'll simply export words such that
-- all hyperstatic dependencies are addressed first, for easy stream
-- processing.
module Wikilon.Dict.Export
    ( encode
    , encodeWords
    -- , decode
    -- , decodeList
    ) where

import Control.Category
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import Wikilon.Dict


-- | Encode the entire dictionary for export into a bytestring.
encode :: Dict -> LBS.ByteString
encode d = encodeWords d (keys d)

-- | Encode a subset of the dictionary, specified by a list of root
-- words. All transitive dependencies of these words are included in
-- the export.
encodeWords :: Dict -> [Word] -> LBS.ByteString
encodeWords = error "TODO: Wikilon.Dict.Export.encodeWords"

-- | Attempt to decode a complete dictionary from a bytestring. This
-- may fail if the provided dictionary has some bad properties.
-- decode :: LBS.ByteString -> Either String Dict
-- decode = decodeList >>> error "TODO: Wikilon.Dict.Export.decode"

-- | Attempt to decode a list of (Word,ABC) pairs from a string that
-- allegedly has the export format. This will IGNORE all lines that
-- do not start with `%`. But see decodeM for more options.
-- decodeList :: 




