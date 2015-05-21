

-- | This module provides a simple regex patterns suitable for
-- HTML \<input\> fields. Naturally, there are some limits on
-- how precise we can be with regular expressions. However, I
-- think we could get a lot of benefit from just a little effort
-- here.
module Wikilon.WAI.RegexPatterns 
    ( abc
    , aoWord
    , aoWordList
    , aoDict
    ) where

sp :: String
sp = "\\x20|\\n"

abcOp :: String
abcOp = sp ++ "|[lrwzvcLRWZVC\\x5E%+\\x2D*/Q\\x3E$?'okfDFMK#0-9]"

abcOps :: String
abcOps = "(?:" ++ abcOp ++ ")*"

abcText :: String
abcText = "\\x22(?:.*\\n)(?:\\x20.*\\n)*\\x7E"

abcTok :: String
abcTok = "[{]" ++ aoWord ++ "[}]"

abcBlockEnds :: String
abcBlockEnds = "\\x5B|\\x5D"

abcSegment :: String
abcSegment = abcOps ++ "|" ++ abcText ++ "|" ++ abcTok ++ "|" ++ abcBlockEnds

abc :: String
abc = "(?:" ++ abcSegment ++ ")*"

aoWord :: String
aoWord = "[a-zA-Z0-9\\x2D._~!$'*+=:@\\u00A0-\\uFFFC]+"

-- a list of zero or more words
aoWordList :: String
aoWordList = "(?:[, ]*" ++ aoWord ++ ")*"

aoDictEnt :: String
aoDictEnt = "\\x40" ++ aoWord ++ sp ++ abc

aoDict :: String
aoDict = "(?:" ++ aoDictEnt ++ "\\n)*"

