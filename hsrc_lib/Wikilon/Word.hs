
module Wikilon.Word
    ( Word(..), textToWord, wordToText
    , wordToUTF8
    ) where

import qualified Data.ByteString as B
import qualified Wikilon.Base16 as B16
import qualified Codec.Binary.UTF8.Generic as UTF8

-- | A word is a bytestring, with UTF8 encoding and Base16
-- compression. The latter is to support large names that
-- contain hashes or unique transaction IDs. The Base16 is
-- the normal one for Awelon project (bdfghjkmnpqstxyz).
newtype Word = Word { unWord :: B.ByteString } deriving (Ord,Eq)

wordToText :: Word -> String
wordToText = UTF8.toString . B16.decompress . B.unpack . unWord

textToWord :: String -> Word
textToWord = Word . B.pack . B16.compress . UTF8.fromString 

wordToUTF8 :: Word -> B.ByteString
wordToUTF8 = B.pack . B16.decompress . B.unpack . unWord

-- Show a Word
instance Show Word where 
    showsPrec _ = showString . wordToText
