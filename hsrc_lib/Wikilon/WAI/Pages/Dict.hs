
-- | Pages for a single dictionary. This might turn into another
-- aggregation module because I want a lot of diverse operations
-- on full dictionaries.
module Wikilon.WAI.Pages.Dict
    ( dictResource
    , dictWords
    , module Wikilon.WAI.Pages.AODict
    ) where

import Data.Monoid
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Media as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai
import Database.VCache

import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import Wikilon.Branch (BranchName)
import Wikilon.Root
import Wikilon.Time


import Wikilon.WAI.Pages.AODict
        
-- The full 'dictionary resource' will include access to
-- words, histories, issues, subscriptions, etc.. Since
-- there is a lot here, I'll need to use subdirectory URIs
-- for current words and so on.
dictResource :: WikilonApp
dictResource = justGET dictFrontPage

dictFrontPage :: WikilonApp
dictFrontPage = toBeImplementedLater "Dictionary"

-- our 
dictWords :: WikilonApp
dictWords = app where
    app = routeOnMethod
        [(HTTP.methodGet, onGet)
        ,(HTTP.methodPut, onPut)]
    onGet = branchOnOutputMedia
        [(mediaTypeTextHTML, dictWordsPage)
        ,(mediaTypeAODict, exportAODict)]
    onPut = branchOnInputMedia
        [(mediaTypeAODict, importAODict)]


-- what should the dict words page have?
--  access to recently changed words?
--  ability to delete words?
dictWordsPage :: WikilonApp
dictWordsPage = toBeImplementedLater "Dict words!"




