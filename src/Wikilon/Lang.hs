
-- | The Awelon language, as used by Wikilon.
--
-- Awelon is semantically and syntactically simple. This module will
-- provide concrete parsers and a reference interpreter. However, the
-- interpreter provided here is not very efficient.
module Wikilon.Lang
    ( Scan(..), scan, scanF
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int


-- | Scan for Parse Errors
--
-- When input cannot be trusted, scan it first. Awelon language is
-- simple enough that a quick linear scan can quickly discover the
-- first error. The Scan data structure provides some context to
-- highlight the error.
--
-- The different fragments are:
--
--   accepted: the valid program prefix
--   parsed:   valid prefix modulo missing ']'
--    balance: how many missing ']'
--   scanned:  cursor location at error
--
-- For example, the 'accepted' fragment might slice just before a
-- toplevel block, while 'parsed' slices just before a text, and
-- 'scanned' slices into the middle of the text. One can use this
-- information to highlight errors, or for stream processing.
--
data Scan = Scan
    { s_accepted :: {-# UNPACK #-} !Int64 -- ^ accepted fragment of input
    , s_parsed   :: {-# UNPACK #-} !Int64 -- ^ accepted modulo balance
    , s_balance  :: {-# UNPACK #-} !Int64 -- ^ block balance (missing ']').
    , s_scanned  :: {-# UNPACK #-} !Int64 -- ^ where error was noticed
    }

-- | Scan program text, assuming full input has been provided.
scan :: LBS.ByteString -> Scan
scan = scanSP . scanF -- pretend one extra space at end of input

-- | On whitespace, we accept the program scanned so far, but only if
-- we're on the program toplevel (i.e. not within a block).
scanSP :: Scan -> Scan
scanSP s | (0 == s_balance s) = s { s_accepted = s_parsed s }
         | otherwise = s

-- | Scan a program text fragment. This variation assumes we might
-- addend more to the input stream, hence does not accept words at
-- the end of stream because they might be part of a larger word.
scanF :: LBS.ByteString -> Scan
scanF = undefined


-- Parser will silently drop the remaining input upon any error.




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

{-
typedef struct wikrt_parse_data { 
    size_t accepted; // valid program prefix
    size_t parsed;   // accepted modulo balance
    size_t balance;  // count of unmatched '['
    size_t scanned;  // where is error noticed?
} wikrt_parse_data;
-}


