
-- | A Claw-based Read Eval Print Loop.
--
-- How to model this?
--
-- One option is to GET a stateless REPL resource, with commands 
-- provided via query strings. Really, an entire session could be
-- provided this way. This has advantages: stateless, simple, 
-- bookmarks. A disadvantage is that renaming won't impact words
-- used in the stateless GET; bookmarks may fail over time due to
-- name changes.
--
-- An interesting alternative is to model persistent REPL sessions as
-- a dictionary application. Usefully, I could try branching sessions.
-- Each post becomes a word or small set of words in the dictionary,
-- and reverse-lookup (plus a few naming conventions) become a simple
-- basis for discovering threads. This would involve POSTing updates
-- to a session such that we may add words to our dictionary.
--
-- These two techniques should combine nicely. Persistent, branching
-- REPL sessions essentially become a functional forum that users can
-- browse and share, and whose presence provides continuous testing
-- and typechecking and documentation. The stateless sessions offer
-- easy external sharing of ad-hoc content.
--
-- I'm interested in having a REPL session essentially become an iPython
-- notebook, but I'm thinking I'll want to figure out how to model each
-- notebook as a dictionary app first (so I can easily preserve rendering
-- of a post as a notebook). So for now, our stateless REPL will only
-- accept one command string and an optional parent thread. Ideally, posts
-- could be given any render-able structure (including a dictionary app)
-- and automatically be rendered appropriately.
--
-- I will use both techniques for now, such that I have both stateful
-- REPL sessions recorded in the dictionary and stateless extensions
-- to ad-hoc sessions.
--
module Wikilon.WAI.Pages.ClawRepl
    ( dictClawRepl
    --, formDictClawRepl
    ) where


import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString.Builder as BB
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Network.Wai as Wai
import Database.VCache

import Awelon.ClawCode


import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import qualified Wikilon.Store.Dict as Dict
import qualified Wikilon.Store.Branch as Branch
import Wikilon.Store.Root

import Wikilon.WAI.Pages.DictWord.Rename
import Wikilon.WAI.Pages.DictWord.AODef
import Wikilon.WAI.Pages.DictWord.ClawDef

dictClawRepl :: WikilonApp
dictClawRepl = app where
    app = routeOnMethod [(HTTP.methodGet, onGet),(HTTP.methodPost, onPost)]
    onGet = branchOnOutputMedia [(mediaTypeTextHTML, clawReplPage)]
    onPost = branchOnOutputMedia [(mediaTypeTextHTML, clawReplPost)]

clawReplPage :: WikilonApp
clawReplPage = toBeImplementedLater "clawRepl: return evaluation results"

clawReplPost :: WikilonApp
clawReplPost = toBeImplementedLater "clawRepl: persistent sessions via dictionary"

-- THOUGHTS: I might also want:
--  a resource to extract the entire session
--  a resource to extract the result as ABC (quoted)
--
-- But I think these might need different URIs.
--
