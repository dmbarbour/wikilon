{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}

module PortDB (Port(..), GetPort(..), SetPort(..)) where

import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Data.Data
import Data.Acid
import Data.SafeCopy

newtype Port = Port Int deriving (Data,Typeable)

setPort :: Int -> Update Port ()
setPort = put . Port

getPort :: Query Port Int
getPort = ask >>= \ (Port p) -> return p

$(deriveSafeCopy 1 'base ''Port) 
$(makeAcidic ''Port ['setPort, 'getPort])

