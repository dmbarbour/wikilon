

module Wikilon.WAI.Pages.DictWord
    ( dictWord
    , dictWordRename
    , formDictWordRename
    ) where


import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Builder as BB
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai
import Database.VCache

import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import Wikilon.Dict.Word
import Wikilon.Dict (Dict)
import qualified Wikilon.Dict as Dict
import Wikilon.Branch (BranchName)
import qualified Wikilon.Branch as Branch
import Wikilon.Root

import qualified Wikilon.Dict.Type as Dict
import qualified Data.VCache.Trie as Trie
import qualified Data.VCache.Trie.Type as Trie
import qualified Data.Array.IArray as A

dictWord :: WikilonApp
dictWord = error "todo" 

dictWordRename :: WikilonApp
dictWordRename = error "todo"

formDictWordRename :: HTML
formDictWordRename = error "todo"

