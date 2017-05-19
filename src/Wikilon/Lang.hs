{-# LANGUAGE CPP, BangPatterns, GeneralizedNewtypeDeriving #-}
-- | The Awelon language, as used by Wikilon.
--
-- Awelon is semantically and syntactically simple. This module will
-- provide concrete parsers and serialization. Interpretation is left
-- to another module.
--
-- Note: for performance, Wikilon assumes a valid UTF-8 encoding and
-- all validation and parsing functions operate at the byte level.
-- But do validate UTF-8 at another layer, as needed.
module Wikilon.Lang
    ( Word(..), Anno(..), NS(..), Text(..)
    , Prog(..), Op(..)
    , encode, encodeBB
    -- , parseProg, decode, decodeP
    , validWord, validAnno, validNS, validText
    , inlinableText
    ) where

import Prelude hiding (Word)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BB
import qualified Data.ByteString.UTF8 as U8
import qualified Data.ByteString.Lazy.UTF8 as LU8
import qualified Data.List as L
import qualified Data.Array.Unboxed as A
import Data.String
import Data.Word (Word8)
import Data.Monoid

-- | Words are the primary user-definable unit of Awelon. A word is
-- identified by an ideally small UTF-8 bytestring.
newtype Word = Word { wordBytes :: BS.ByteString } 
    deriving (Ord, Eq)

-- | Annotations are generally runtime or interpreter defined. They
-- are essentially words wrapped within parentheses, such as (par)
-- to request a parallel evaluation. At this point, the parentheses
-- have been stripped.
newtype Anno = Anno { annoWord :: Word } 
    deriving (Ord, Eq)

-- | Awelon supports namespaces for hierarchical dictionaries. The
-- form `foo@dict` would mean we're using the word `foo` as defined
-- in a child dictionary `dict`. Any operation may be qualified with
-- a namespace - words, blocks, texts, and even namespace themselves
-- may be hierarchical. The namespace must be a valid word.
newtype NS = NS { nsWord :: Word } 
    deriving (Ord, Eq)

-- | Awelon supports embedded texts, both inline and multi-line.
--
-- For multi-line case, we use `LF SP` to escape a linefeed if the
-- next line is non-empty. `LF LF` is okay for an empty line. And 
-- `LF "` terminates a text. LF is the only special case, no other
-- escapes are supported. Multi-line texts start with `" LF` while
-- inline texts start with just `"` and run to the final `"`. 
--
-- The `Text` type should only contain the data, not the escapes
-- or initial or final quotes.
newtype Text = Text { textData :: LBS.ByteString } 
    deriving (Ord, Eq, Monoid) 

-- | A basic operation is a word, annotation, text, or block. In
-- addition, any operation may be modified with a namespace, which
-- is potentially hierarchical.
--
-- While this representation is acceptable for parsing, it is not
-- good for efficient interpretation. We still need to bind each
-- word to its definition (or at least support efficient lookup),
-- evaluate definitions and analyze arity, recognize accelerators,
-- track reference attributes (unique, (nc), (nd)), and accelerate
-- numeric, array, sum, and record data types.
--
data Op = OpWord  Word  -- ^ just a word
        | OpAnno  Anno  -- ^ (annotation)
        | OpBlock Prog  -- ^ [block of code]
        | OpText  Text  -- ^ "embedded text"
        | OpNS Op NS    -- ^ Op@NS
    deriving (Ord, Eq)

-- | An Awelon program consists of a sequence of operations. These
-- operations can be understood as manipulating an implicit stack,
-- similar to Forth. However, Awelon evaluates by rewriting and the
-- output will be another program.
newtype Prog = Prog { progOps :: [Op] } deriving (Ord, Eq, Monoid)

-- | This serializes a program into modest sized chunks.
--
-- This encoder doesn't do anything fancy with word separators. It
-- simply adds a little whitespace between each operation, which is
-- aesthetically acceptable even if not optimal.
encode :: Prog -> LBS.ByteString
encode = toLBS . encodeBB where
    toLBS = BB.toLazyByteStringWith strat LBS.empty
    strat = BB.untrimmedStrategy 240 BB.smallChunkSize

-- | Access the bytestring builder to integrate serialization.
encodeBB :: Prog -> BB.Builder
encodeBB = mconcat . L.intersperse ws . fmap opBB . progOps where
    ws = BB.word8 32

opBB :: Op -> BB.Builder
opBB (OpWord w) = BB.byteString (wordBytes w)
opBB (OpAnno a) = BB.word8 40 <> BB.byteString (wordBytes (annoWord a)) <> BB.word8 41
opBB (OpBlock p) = BB.word8 91 <> encodeBB p <> BB.word8 93
opBB (OpText t) 
    | inlinableText t = BB.word8 34 <> BB.lazyByteString (textData t) <> BB.word8 34
    | otherwise = BB.word8 34 <> BB.word8 10 
                    <> mlTextBB (textData t) 
                    <> BB.word8 10 <> BB.word8 34
opBB (OpNS op ns) = opBB op <> BB.word8 64 <> BB.byteString (wordBytes (nsWord ns))

-- | We cannot inline a text that contains double quote (34) or a
-- linefeed (10).
inlinableText :: Text -> Bool
inlinableText (Text s) = not (LBS.elem 10 s || LBS.elem 34 s)

-- | Render a multi-line text, starting on a new line. We'll add a
-- space at the start of each non-empty line. Empty lines won't use
-- an extra escape.
mlTextBB :: LBS.ByteString -> BB.Builder
mlTextBB s = case LBS.elemIndex 10 s of
    Nothing -> BB.lazyByteString s
    Just 0  -> BB.word8 10 <> mlTextBB (LBS.drop 1 s)
    Just ix -> let (line,s') = LBS.splitAt (ix+1) s in
               BB.word8 32 <> BB.lazyByteString line <> mlTextBB s'

-- | 


#if 0

-- | Scan for Parse Errors (assuming valid UTF-8)
--
-- Perform a quick check for parse errors within an Awelon program.
-- A scan returns multiple values:
--
--  - accepted: valid program prefix (in bytes)
--  - parsed:   valid prefix modulo block balance (in bytes)
--  - scanned:  how many bytes were processed
--  - length:   number of bytes in input (max scan)
--
-- These, together with the initial input, can be used to quickly
-- diagnose most parse errors. For example, if our 'parsed' halts
-- on a `"` character, and 'scanned' is equal to program length,
-- we have a text that has not terminated. Similarly, if 'accepted'
-- halts on a `[` character, and 'parsed' is equal to 'length', we
-- have a block imbalance.
data Scan = Scan
    { s_accepted :: {-# UNPACK #-} !Int64
    , s_parsed   :: {-# UNPACK #-} !Int64
    , s_scanned  :: {-# UNPACK #-} !Int64
    , s_length   :: {-# UNPACK #-} !Int64
    }

scan :: LBS.ByteString -> Scan
scan = addEOF . scanF where
    addEOF s = if (s_scanned s /= s_length s) then s {- stopped before EOF -} else
        let parsed' = s_scanned s in
        let accept' = if (0 == s_depth s) then parsed' else s_accept s in
        s { s_parsed = parsed', s_accepted = accept' }

-- | Check if scan indicates fully valid input.
scanOK :: Scan -> Bool
scanOK s = ((s_accepted s) == (s_length s))

-- add logical SP to the input, assuming in toplevel
scanSP :: Scan -> Scan
scanSP s =
    let parsed' = s_scanned s in
    let accept' = if (0 == s_depth s) then parsed' else s_accept s in
    s { s_accepted = accept', s_parsed = parsed' }
    let accept' = s_scanned s in
| (0 == s_depth s) = s { s_accepted = s_scanned s, s_parsed = s_
    
              
        let parsed' = if (s_scanned s == s_length s) then s_scanned s else s_parsed s in
        let accept' = if (0 == s_depth s) then  

if (s_scanned s /= s_length s) then s else
        let parsed' = s_scanned s in
        let accept' = if (0 == s_depth s) then parsed'

        | (s_scanned s /= s_length s) = s

-- adjust scan result for a logical end of input
scanEOF :: Scan -> Scan
scanEOF s | (s_scanned s == s_length s) = s { s_parsed
    if (s_scanned s != s_length s) then

 | (s_scanned s != s_length s) = s
          | otherwise = 
scanSP s | 

 
    assumeEOF s | (s_scanned s == s_length s)
   assumeEOF = acceptEOF . parseEOF


-- only different from 'accepted' if we have an imbalanced block
-- accepted, how much was parsed, and how much is leftover.
-- 
--
-- The scan result indicates how much of the input was accepted, how
-- much more would be accepted if open blocks are balanced, and any
-- remaining input that was not successfully parsed.
--
data Scan = Scan
    { s_accepted :: {-# UNPACK #-} !Int64 -- ^ accepted fragment of input
    , s_parsed   :: {-# UNPACK #-} !Int64 -- ^ accepted modulo block balance
    , s_leftover :: !LBS.ByteString       -- ^ text that wasn't parsed
    }

scanOK :: Scan -> Bool
scanOK s = (LBS.null (s_leftover s)) 
        && (s_parsed s == s_accepted s)

-- | Scan program text, assuming full input has been provided.
scan :: LBS.ByteString -> Scan
scan = undefined


-- | Scan a program text fragment. This variation assumes we might
-- addend more to the input stream, hence does not accept words at
-- the end of stream because they might be part of a larger word.
scanF :: LBS.ByteString -> Scan
scanF = undefined



-- TODO:
--   Parser
--    parse error reporting
--    qualified namespaces, blocks
--
-- A streaming parser seems difficult in context of namespaces and
-- blocks. But 

-- First issue: do I want a streaming parser?
--  It's unnecessary for now, though it wouldn't hurt.
--  
-- 
-- If we eschew a streaming parser, we'll still need to report where
-- errors occur. A viable option is to report 
#endif



instance Show Prog where 
    showsPrec _ = showString . LU8.toString . encode 

--instance IsString Prog where
--    fromString = decodeP . LU8.fromString

instance IsString Word where fromString = Word . U8.fromString

validWord :: Word -> Bool
validWord = validWordToken . wordBytes

validWordToken :: BS.ByteString -> Bool
validWordToken s = not (BS.null s) && BS.all validWordByte s

invalidUTF8Bytes :: [Word8]
invalidUTF8Bytes = [192,193] ++ [245..255]

-- Word blacklist is: @#[]()<>{}\/,;|&='"`, SP, C0 (0-31), and DEL.
-- We can also forbid a few bytes never permitted in UTF-8.
wordBytesArray :: A.UArray Word8 Bool
wordBytesArray = A.listArray (0,255) $ fmap accept [0..255] where
    accept = flip L.notElem blacklist
    blacklist = [32,34,35,38,39,40,41,44,47,59,60,61,62
               ,64,91,92,93,96,123,124,125,127] 
            ++ [0..31] ++ invalidUTF8Bytes

validWordByte :: Word8 -> Bool
validWordByte = (A.!) wordBytesArray

instance IsString Anno where fromString = Anno . fromString

validAnno :: Anno -> Bool
validAnno = validWord . annoWord


instance IsString NS where fromString = NS . fromString

validNS :: NS -> Bool
validNS = validWord . nsWord


instance IsString Text where fromString = Text . LU8.fromString

-- blacklist for normal characters in a text.
textBytesArray :: A.UArray Word8 Bool
textBytesArray = A.listArray (0,255) $ fmap accept [0..255] where
    accept = flip L.notElem blacklist
    blacklist = [0..9] ++ [11..31] ++ [127] ++ invalidUTF8Bytes

validTextByte :: Word8 -> Bool
validTextByte = (A.!) textBytesArray

-- | This only tests for forbidden characters (C0 except LF, DEL).
-- The assumption is the text has a valid UTF-8 encoding.
validText :: Text -> Bool
validText = LBS.all validTextByte . textData


