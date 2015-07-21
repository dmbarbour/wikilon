
-- | Wikilon's REPL (Read Eval Print Loop).
--
-- The plan is to model this in two parts:
--
-- (a) stateless, GET-based, bookmarkable, view oriented
-- (b) stateful, POST-based, sessions, functional forums
--
-- These elements would then be integrated by allowing the GET-based
-- REPL to extend a POST-based session. The stateful REPL is modeled
-- as a dictionary application, using the dictionary itself as the
-- storage for the sessions. This simplifies integration of tools; a
-- stateful session becomes an implicit test.
--
-- Wikilon's REPL is purely functional. Every post is a pure function
-- that manipulates the implicit session environment. Every session is
-- a pure function operating on an implicit 'void' environment or type.
-- Using assertions, a session or post might also be understood as a
-- constraint model on the environment in addition to a transformation
-- on it.
--
-- Each post is modeled as a separate word. This potentially allows
-- posts themselves to be dictionary applications or embedded objects,
-- e.g. a spreadsheet or iPython notebook style app or a table. For
-- the short term, however, I'll be focusing on claw code REPLs.
--
module Wikilon.WAI.Pages.REPL
    ( dictRepl
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

dictRepl :: WikilonApp
dictRepl = app where
    app = routeOnMethod [(HTTP.methodGet, onGet),(HTTP.methodPost, onPost)]
    onGet = branchOnOutputMedia [(mediaTypeTextHTML, clawReplPage)]
    onPost = branchOnOutputMedia [(mediaTypeTextHTML, clawReplPost)]

replPage :: WikilonApp
replPage = toBeImplementedLater "clawRepl: return evaluation results"

replPost :: WikilonApp
replPost = toBeImplementedLater "clawRepl: persistent sessions via dictionary"

-- THOUGHTS: I might also want:
--  a resource to extract the entire session
--  a resource to extract the result as ABC (quoted)
--
-- But I think these might need different URIs.
--
