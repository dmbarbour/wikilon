
-- | The Wikilon runtime type and monad. 
--
-- At the moment, this is just passing some extra resources around in
-- an IO monad, plus an option to perform transactional operations.
--
-- 
-- 
module Wikilon.RT
    ( 
    ) where

import Control.Monad.Trans.Reader
import Wikilon.DB

data RTE = RTE
    { rt_read   :: ByteString -> IO ByteString
    , rt_write  :: ByteString -> ByteString -> IO ()
    , rt_load   :: Hash -> IO (Maybe ByteString)
    , rt_stow   :: ByteString -> IO Hash
    , rt_trace  :: ByteString -> IO ()
    }
newtype RT a = RT (ReaderT RTE IO a)

-- Adding some form of checkpoint model to our RTE seems useful.


