{-# LANGUAGE ViewPatterns, OverloadedStrings #-}


-- | just a module that gathers all the other page modules
-- 
module Wikilon.WAI.Pages
    ( module Wikilon.WAI.Pages.FrontPage
    , module Wikilon.WAI.Pages.DictList
    , module Wikilon.WAI.Pages.Dict
    , module Wikilon.WAI.Pages.Admin
    ) where

import Wikilon.WAI.Pages.FrontPage
import Wikilon.WAI.Pages.DictList
import Wikilon.WAI.Pages.Dict
import Wikilon.WAI.Pages.Admin

