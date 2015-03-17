
-- | A few text utilities.
module Wikilon.Text
    ( Text
    , textLines
    ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8

-- For large texts, I'll just use a lazy UTF8 bytestring.
type Text = LazyUTF8.ByteString

-- | A lossless 'lines' function. The original structure may be
-- recovered by simply adding a '\n' before each segment in the
-- list result, then concatenating.
textLines :: Text -> (Text, [Text])
textLines txt =
    case LBS.elemIndex 10 txt of
        Nothing -> (txt, [])
        Just idx ->
            let (ln1,txt') = LBS.splitAt idx txt in
            let p = textLines (LBS.drop 1 txt') in
            (ln1, (fst p : snd p))



