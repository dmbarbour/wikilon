{-# LANGUAGE CPP #-}
-- | The Awelon language, as used by Wikilon.
--
-- Awelon is semantically and syntactically simple. This module will
-- provide concrete parsers and a reference interpreter, and other
-- basic utilities. The reference interpreter is slow, but Wikilon
-- will develop several more with growing levels of sophistication.
--
-- Note: for performance, Wikilon assumes a valid UTF-8 encoding and
-- all validation and parsing functions operate at the byte level.
-- But do validate UTF-8 at another layer, as needed.
module Wikilon.Lang
    ( W(..), Anno(..), NS(..), Text(..)
    , Prog(..), Op(..)
    , validWord, validAnno, validNS
    ) where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as U8
import qualified Data.ByteString.Lazy.UTF8 as LU8
import qualified Data.List as L
import qualified Data.Array.Unboxed as A
import Data.String
import Data.Word (Word8)

-- | Words are the primary user-definable unit of Awelon.
newtype W = W { wordBytes :: BS.ByteString } deriving (Ord, Eq)

-- | Annotations are generally runtime or interpreter defined. They
-- are essentially words wrapped within parentheses, such as (par)
-- to request a parallel evaluation. At this point, the parentheses
-- have been stripped.
newtype Anno = Anno { annoWord :: W } deriving (Ord, Eq)

-- | Awelon supports namespaces for hierarchical dictionaries. The
-- form `foo@dict` would mean we're using the word `foo` as defined
-- in a child dictionary `dict`. Any operation may be qualified with
-- a namespace - words, blocks, texts, and even namespace themselves
-- may be hierarchical. The namespace must be a valid word.
newtype NS = NS { nsWord :: W } deriving (Ord, Eq)

-- | Awelon supports embedded texts, both inline and multi-line.
--
-- For multi-line case, we use `LF SP` to escape a linefeed if the
-- next line is non-empty. `LF LF` is okay for an empty line. And 
-- `LF "` terminates a text. LF is the only special case, no other
-- escapes are supported. Multi-line texts start with `" LF` while
-- inline texts start with just `"` and run to the final `"`. 
--
-- When rendering, we use a heuristic: if a text is large, or it
-- contains double quote or LF, we'll render using the multi-line
-- format.
--
-- The `Text` type should only contain the data, not the escapes
-- or initial or final quotes.
newtype Text = Text { textData :: LBS.ByteString } deriving (Ord, Eq) 

-- | A basic operation is a word, annotation, text, or block. In
-- addition, any operation may be modified with a namespace, which
-- is potentially hierarchical.
--
-- Note that this is a rather naive representation. It doesn't offer
-- support for natural numbers. It doesn't support linking a word's
-- definition or precomputing arity. It doesn't enable easy tracking
-- of substructural metadata during evaluation. Etc..
data Op = OpWord  W     -- ^ just a word
        | OpAnno  Anno  -- ^ (annotation)
        | OpBlock Prog  -- ^ [block of code]
        | OpText  Text  -- ^ "embedded text"
        | OpNS Op NS    -- ^ Op@NS
    deriving (Ord, Eq)

-- | An Awelon program consists of a sequence of operations. These
-- operations can be understood as manipulating an implicit stack,
-- similar to Forth. However, Awelon evaluates by rewriting and the
-- output will be another program.
newtype Prog = Prog [Op] deriving (Ord, Eq)


#if 0

-- | An operation in Awelon is a token, text, or block.
--
-- Tokens include annotations with parentheses, e.g. (par), and all
-- words including natural numbers or primitives. Texts include only
-- the contained data, i.e. excluding linefeed escapes. Blocks are
-- simply first-class subprograms.
--
-- Any operation can be qualified with a namespace, to reference a
-- subordinate dictionary. E.g. `foo@xy@zzy` would be represented as
-- `foo@xy` under NS `zzy`

-- Additionally, any operation may be attributed with a namespace.
--
-- Tokens include words and annotations.
data Op = OpTok BS.ByteString 
        | OpTxt LBS.ByteString
        | OpBlk [Op]
        | OpNS Op BS.ByteString 

-- | An Awelon program consists of a seq

-- | An Awelon operation consists of a word, annotation, 
-- Additionally, an operation may be wrapped into a qualified namespace.
data Op = Tok | A Anno | T Text | B [Op] | NS 

okTextByte :: Word8 -> Bool    
okTextByte = (A.!) textBytesArray

binaryName :: LBS.ByteString -> BS.ByteString


-- parseSuffix :: LBS.ByteString -> LBS.ByteString


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

instance IsString W where fromString = W . U8.fromString

validWord :: W -> Bool
validWord = validWordToken . wordBytes

validWordToken :: BS.ByteString -> Bool
validWordToken s = not (BS.null s) && BS.all validWordByte s

c0 :: [Word8]
c0 = [0..31]

invalidUTF8Bytes :: [Word8]
invalidUTF8Bytes = [192,193] ++ [245..255]

-- Word blacklist is: @#[]()<>{}\/,;|&='"`, SP, C0 (0-31), and DEL.
-- We can also forbid a few bytes never permitted in UTF-8.
wordBytesArray :: A.UArray Word8 Bool
wordBytesArray = A.listArray (0,255) $ fmap accept [0..255] where
    accept = flip L.notElem blacklist
    blacklist = [32,34,35,38,39,40,41,44,47,59,60,61,62
               ,64,91,92,93,96,123,124,125,127] 
            ++ (c0 ++ invalidUTF8Bytes)

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
    blacklist = c0 ++ invalidUTF8Bytes

validTextByte :: Word8 -> Bool
validTextByte = (A.!) textBytesArray

-- | This only tests for obvious byte-level errors.
validText :: Text -> Bool
validText = LBS.all validTextByte . textData


