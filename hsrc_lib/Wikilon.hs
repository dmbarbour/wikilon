-- | Wikilon is a persistent web server implemented above Warp and 
-- VCache. Wikilon hosts multiple Awelon project dictionaries, and
-- will also host many abstract virtual machines.
--
-- Wikilon, in the long term, aims to be highly configurable for
-- ad-hoc purposes and to easily support users in construction,
-- sharing, and integration of software artifacts.
-- 
module Wikilon
    ( module Wikilon.Root
    , module Wikilon.WAI
    ) where

import Wikilon.Root
import Wikilon.WAI 
