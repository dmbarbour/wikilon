
-- | The `cereal` package doesn't include useful parser combinators,
-- and clearly isn't intended for that use case. However, I'm using
-- them that way for easy integration with acid-state. And it isn't
-- all that difficult to add a few useful combinators.
--
-- Usefully, `cereal` already favors UTF-8 for character encodings,
-- so I can focus on character level parsing and get the behavior
-- I want.
module Wikilon.ParseUtils
    ( tryCommit
    , optionMaybe
    , many, many1, manyC
    , manyTil, manyTilEnd
    , satisfy, satisfyMsg, char, eof
    ) where

import Control.Applicative ((<$>),(<|>),(<*>))
import qualified Data.Serialize as C
import qualified Data.List as L

-- | tryCommit is a two-phase parser. The first phase, which
-- returns the second-phase parser, is allowed to fail, in which
-- case Nothing is returned. Otherwise, the second phase is
-- required to succeed, i.e. we are committed to it.
--
-- This is used to limit backtracking, e.g. for the `many` or 
-- `optional` combinators.
tryCommit :: C.Get (C.Get a) -> C.Get (Maybe a)
tryCommit runPhase1 = do
    maybePhase2 <- (Just <$> runPhase1) <|> return Nothing
    case maybePhase2 of
        Just runPhase2 -> Just <$> runPhase2
        Nothing -> return Nothing

-- | parse a value that satisfies some refinement condition.
satisfy :: (C.Serialize a) => (a -> Bool) -> C.Get a
satisfy = satisfyMsg "unexpected value"

-- | satisfy with a given error message on fail
satisfyMsg :: (C.Serialize a) => String -> (a -> Bool) -> C.Get a 
satisfyMsg errMsg test = 
    C.get >>= \ a ->
    if test a then return a 
              else fail errMsg

-- | match a particular character
char :: Char -> C.Get Char
char c = satisfyMsg eMsg (== c) where
    eMsg = '\'' : c : "' expected"

optionMaybe :: C.Get a -> C.Get (Maybe a)
optionMaybe op = tryCommit (return <$> op)

-- | parse zero or more entries.
many :: C.Get a -> C.Get [a]
many = manyC . (return <$>)

-- | parse one or more entries.
many1 :: C.Get a -> C.Get [a]
many1 g = (:) <$> g <*> many g

-- | parse many elements with partial commit (e.g. for LL1 parsers).
--
-- Allowing failure in the middle of parsing an element helps control
-- how far the parser can backtrack.
manyC :: C.Get (C.Get a) -> C.Get [a]
manyC gg = loop [] where
    tryA = tryCommit gg
    loop as = 
        tryA >>= \ mba ->
        case mba of
            Nothing -> return (L.reverse as)
            Just a -> loop (a:as)

-- | parse EOF, i.e. fail if not at end of input
eof :: C.Get ()
eof = (peek >> fail emsg) <|> return () where
    peek = C.lookAhead C.getWord8
    emsg = "end of input expected"

-- | parseManyTil: try to parse many inputs til we reach a terminal
manyTil :: C.Get a -> C.Get end -> C.Get [a]
manyTil a e = fst <$> manyTilEnd a e

-- | parse many inputs until terminal; return the terminal, too
--
-- Note that parseManyTil* doesn't have the same difficulty as 
-- parseMany since we need to lookahead on elements.
manyTilEnd :: C.Get a -> C.Get end -> C.Get ([a],end)
manyTilEnd getElem atEnd = loop [] where
    loop as = (atEnd >>= \ end -> return (L.reverse as, end))
          <|> (getElem >>= \ a -> loop (a:as))



