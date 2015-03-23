
-- | Wikilon's Text model is UTF8 plus compression for ABC Base16.
--
-- Sequences of 6..512 base16 characters in `bcdfghkmnpqstxyz` are
-- compressed down to half as many bytes, plus a two byte header.
-- This encoding is meant for large binary values, e.g. for streaming
-- video or audio data. (In general, binary representations are not
-- encouraged for ABC systems.)
--
-- At the moment, I'm just lazily decoding the Base16 as part of the
-- uncons operation. 
--
-- For stable encoding of Base16, I'll probably want to work backward
-- such that elements near the end of the text are encoded first. This
-- allows adding elements at the beginning of the text without changing
-- how later parts of the text are aligned.
--
-- todo eventually: optimize these functions. probably not critical.
module Wikilon.Text
    ( Text(..)
    , uncons
    ) where

import Control.Exception (assert)
import Control.Arrow (second)
import Control.Applicative
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified ABC.Base16 as B16

-- | UTF8 text with mixed ABC Base16 binary. 
newtype Text = Text LBS.ByteString

-- | O(1) uncons operation.
uncons :: Text -> Maybe (Char, Text)
uncons (Text t) | LBS.null t = Nothing
                | (0xf8 == LBS.head t) = uncons (Text (_expandHead (LBS.tail t)))
                | otherwise = second Text <$> LazyUTF8.uncons t

_expandHead :: LBS.ByteString -> LBS.ByteString
_expandHead t = case LBS.uncons t of
    Nothing -> error "invalid text in Wikilon"
    Just (l,t') ->
        let len = (fromIntegral l) + 3 in
        assert (len <= 256) $
        let (tL, tR) = LBS.splitAt len t' in
        assert (len == LBS.length tL) $
        LBS.append (_expandData tL) tR

-- todo: optimize this to directly allocate a buffer
_expandData :: LBS.ByteString -> LBS.ByteString
_expandData = LBS.pack . B16.encode . LBS.unpack 




