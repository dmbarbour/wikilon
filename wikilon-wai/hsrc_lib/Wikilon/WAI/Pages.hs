{-# LANGUAGE ViewPatterns, OverloadedStrings #-}


-- | just a module that gathers all the other page modules
-- 
module Wikilon.WAI.Pages
    ( module Wikilon.WAI.Pages.FrontPage
    , module Wikilon.WAI.Pages.DictList
    , module Wikilon.WAI.Pages.Dict
    , module Wikilon.WAI.Pages.Admin
    , module Wikilon.WAI.Pages.ClawCode
    , module Wikilon.WAI.Pages.REPL
    ) where

import Wikilon.WAI.Pages.FrontPage
import Wikilon.WAI.Pages.DictList
import Wikilon.WAI.Pages.Dict
import Wikilon.WAI.Pages.Admin
import Wikilon.WAI.Pages.ClawCode
import Wikilon.WAI.Pages.REPL
