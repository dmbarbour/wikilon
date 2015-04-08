
-- | This module provides a simple import and export format for Awelon
-- dictionaries, emitting an entire dictionary as a large file. This
-- dictionary uses the following format:
--
--   @over [{%dupd} {%swap}][]
--   @dupd [rw{%dup}wl][]
--   @swap [rwrwzwlwl][]
--   @dup [r^zlwl][]
--   @doc.dup "(Copyable x) ⇒ x -- x x
--    ~[{%docString}]
--
-- Or more generally:
--
--   @word bytecode
--   @anotherWord more bytecode
--   @multiLineWord bytecode continues
--    on another line, each LF escaped
--    by following SP
--
-- The type of each definition is: ∀s.s→∃v.((v→fn)*(v*s)). See the
-- Wikilon.Dict module for more information.
--
-- This export format is intended for easy extension into streamable
-- update models, leveraging new prefixes for alternative commands
-- such as indicating time or origin, renaming words, or deleting
-- words. 
module Wikilon.Dict.Export
    ( encode
    , encodeWords
    -- , decode
    -- , decodeList
    ) where

import Control.Monad
import Control.Monad.State (State)
import Data.Monoid
import qualified Control.Monad.State as State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import Data.Set (Set)
import qualified Data.Set as Set
import Wikilon.Dict

-- | Encode the entire dictionary for export into a bytestring.
encode :: Dict -> LBS.ByteString
encode d = encodeWords d (wordsInDict d)

-- | Encode a subset of the dictionary, specified by a list of root
-- words. All transitive dependencies of these words are included in
-- the export. If a word is not defined, it will be silently skipped.
encodeWords :: Dict -> [Word] -> LBS.ByteString
encodeWords d ws = 
    let action = mapM_ (_enw d) ws in
    let s0 = EnSt mempty mempty in
    let s' = State.execState action s0 in
    BB.toLazyByteString (en_out s')

data EnSt = EnSt
    { en_out :: BB.Builder  -- the output stream
    , en_rec :: Set Word    -- the words recorded
    } 
type ENC a = State EnSt a

enc_print :: BB.Builder -> ENC () 
enc_print bb = State.modify $ \ s ->
    let o' = en_out s <> bb in
    s { en_out = o' }

enc_record :: Word -> ENC Bool
enc_record w = 
    State.gets en_rec >>= \ s0 ->
    let b = Set.notMember w s0 in
    let s' = Set.insert w s0 in
    State.modify (\ s -> s { en_rec = s' }) >>
    return b

_enw :: Dict -> Word -> ENC ()
_enw d w = 
    -- skip words already recorded
    enc_record w >>= \ bNewWord ->
    when bNewWord $
        -- record all dependencies first
        mapM_ (_enw d) (deps d w) >> 
        case lookupBytes d w of
            Nothing -> return ()
            Just bytes -> enc_print $
                BB.char8 '@' <> BB.byteString (wordToUTF8 w) <> BB.char8 ' ' 
                <> _esc bytes <> BB.char8 '\n'

_esc :: LBS.ByteString -> BB.Builder
_esc = mconcat . fmap _esc' . LBS.toChunks

-- every LF is printed as LF SP.
_esc' :: BS.ByteString -> BB.Builder
_esc' bs = case BS.elemIndex 10 bs of
    Nothing -> BB.byteString bs
    Just ix -> 
        let ct = ix + 1 in
        BB.byteString (BS.take ct bs) <>
        BB.char8 ' ' <> 
        _esc' (BS.drop ct bs)


-- | Attempt to decode a complete dictionary from a bytestring. This
-- may fail if the provided dictionary has some bad properties.
-- decode :: LBS.ByteString -> Either String Dict
-- decode = decodeList >>> error "TODO: Wikilon.Dict.Export.decode"

-- | Attempt to decode a list of (Word,ABC) pairs from a string that
-- allegedly has the export format. This will IGNORE all lines that
-- do not start with `%`. But see decodeM for more options.
-- decodeList :: 




