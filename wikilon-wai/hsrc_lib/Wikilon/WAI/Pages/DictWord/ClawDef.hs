{-# LANGUAGE OverloadedStrings #-}

module Wikilon.WAI.Pages.DictWord.ClawDef
    ( dictWordClawDef
    , getDictWordClawDef
    , putDictWordClawDef
    , dictWordClawDefEdit
    , formDictWordClawDefEdit
    ) where



import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Media as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai
import Database.VCache

import Wikilon.Time
import Wikilon.Dict.Word

import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import Wikilon.WAI.RecvFormPost
import qualified Wikilon.WAI.RegexPatterns as Regex
import qualified Wikilon.Store.Dict as Dict
import Wikilon.Store.Branch (BranchName)
import qualified Wikilon.Store.Branch as Branch
import Wikilon.Store.Root

-- | Endpoint that restructs users to Claw format for put and get.
dictWordClawDef :: WikilonApp
dictWordClawDef = app where
    app = routeOnMethod [(HTTP.methodGet, onGet),(HTTP.methodPut, onPut)]
    onGet = branchOnOutputMedia [(mediaTypeClaw, getDictWordClawDef)]
    onPut = branchOnInputMedia [(mediaTypeClaw, putDictWordClawDef)]

getDictWordClawDef :: WikilonApp
getDictWordClawDef = toBeImplementedLater "view claw def for word (if possible)"

putDictWordClawDef :: WikilonApp
putDictWordClawDef = toBeImplementedLater "set word as having a claw definition"

dictWordClawDefEdit :: WikilonApp
dictWordClawDefEdit = toBeImplementedLater "Claw Editor Page"

formDictWordClawDefEdit :: HTML
formDictWordClawDefEdit = "TODO"

