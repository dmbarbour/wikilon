
-- | A Claw-based Read Eval Print Loop.
--
-- I'm not sure how I should model this... 
--
-- One option is to GET a REPL resource, with commands provided via
-- the query string. Really, an entire session could be provided
-- this way. This has many advantages: stateless, simple, bookmarks.
--
-- An alternative, interesting option is to treat REPL as a threaded
-- tree, recording actions like user posts to a bulletin board. Each
-- thread would carry a computational context, with different threads
-- branching in different directions. 
--
-- For this particular page, I'm going to focus on the GET-based
-- REPL. 
module Wikilon.WAI.Pages.ClawRepl
    ( dictClawRepl
    , formDictClawRepl
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
    app = routeOnMethod [(HTTP.methodGet, onGet)]
    onGet = branchOnOutputMedia [(mediaTypeTextHTML, clawReplPage)]

clawReplPage :: WikilonApp
clawReplPage = dictApp $ \ w dn 

