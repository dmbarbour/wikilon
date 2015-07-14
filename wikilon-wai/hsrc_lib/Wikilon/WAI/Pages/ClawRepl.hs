
module Wikilon.WAI.Pages.ClawRepl
    ( dictClawRepl
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
dictClawRepl = toBeImplementedLater "Claw REPL"

