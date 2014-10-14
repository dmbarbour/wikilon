
-- | The `binary` package doesn't include useful parser combinators,
-- and clearly isn't intended for that use case. However, I'm using
-- them that way, so I've added a few...
module Wikilon.ParseUtils
    ( tryCommit
    , optionMaybe
    , many, many1, manyC
    , manyTil, manyTilEnd
    , satisfy, satisfyMsg, char, eof
    ) where

import Control.Applicative ((<$>),(<|>),(<*>))
import Data.Binary
import Data.Binary.Get
import qualified Data.List as L

-- | tryCommit is a two-phase parser. The first phase, which
-- returns the second-phase parser, is allowed to fail, in which
-- case Nothing is returned. Otherwise, the second phase is
-- required to succeed, i.e. we are committed to it.
--
-- This is used to limit backtracking, e.g. for the `many` or 
-- `optional` combinators.
tryCommit :: Get (Get a) -> Get (Maybe a)
tryCommit runPhase1 = do
    maybePhase2 <- (Just <$> runPhase1) <|> return Nothing
    case maybePhase2 of
        Just runPhase2 -> Just <$> runPhase2
        Nothing -> return Nothing

-- | parse a value that satisfies some refinement condition.
satisfy :: (Binary a) => (a -> Bool) -> Get a
satisfy = satisfyMsg "unexpected value"

-- | satisfy with a given error message on fail
satisfyMsg :: (Binary a) => String -> (a -> Bool) -> Get a 
satisfyMsg errMsg test = 
    get >>= \ a ->
    if test a then return a 
              else fail errMsg

-- | match a particular character
char :: Char -> Get Char
char c = satisfyMsg eMsg (== c) where
    eMsg = '\'' : c : "' expected"

optionMaybe :: Get a -> Get (Maybe a)
optionMaybe op = tryCommit (return <$> op)

-- | parse zero or more entries.
many :: Get a -> Get [a]
many = manyC . (return <$>)

-- | parse one or more entries.
many1 :: Get a -> Get [a]
many1 g = (:) <$> g <*> many g

-- | parse many elements with partial commit (e.g. for LL1 parsers).
--
-- Allowing failure in the middle of parsing an element helps control
-- how far the parser can backtrack.
manyC :: Get (Get a) -> Get [a]
manyC gg = loop [] where
    tryA = tryCommit gg
    loop as = 
        tryA >>= \ mba ->
        case mba of
            Nothing -> return (L.reverse as)
            Just a -> loop (a:as)

-- | parse EOF, i.e. fail if not at end of input
eof :: Get ()
eof = 
    isEmpty >>= \ bEmpty ->
    if bEmpty then return () else 
    fail "end of input expected"

{- -- older version for `cereal` (which lacks a sensible isEmpty)
eof = (peek >> fail emsg) <|> return () where
    peek = lookAhead getWord8
    emsg = "end of input expected"
-}

-- | parseManyTil: try to parse many inputs til we reach a terminal
manyTil :: Get a -> Get end -> Get [a]
manyTil a e = fst <$> manyTilEnd a e

-- | parse many inputs until terminal; return the terminal, too
--
-- Note that parseManyTil* doesn't have the same difficulty as 
-- parseMany since we need to lookahead on elements.
manyTilEnd :: Get a -> Get end -> Get ([a],end)
manyTilEnd getElem atEnd = loop [] where
    loop as = (atEnd >>= \ end -> return (L.reverse as, end))
          <|> (getElem >>= \ a -> loop (a:as))



