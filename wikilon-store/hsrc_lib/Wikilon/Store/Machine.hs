{-# LANGUAGE DeriveDataTypeable #-}

-- | Wikilon hosts a set of abstract virtual machines, or AVMs. These
-- machines interact by messaging each other through a virtual network.
-- Messaging is capability-secure at a fine granularity, allowing the
-- machine to define its own capabilities.
--
-- Each machine intrinsically has:
--
-- * a State, which is just an ABC value
-- * a Step function which is ABC code
-- * a Signal context for host-layer messages
-- * a Self function changes contexts into capabilities
-- * a global identifier based on a public key
--
-- Rough model:
--
--    type Step m = (InMsg m * State m) → (OutMsg List * State m)
--    type InMsg m = ∃a . (Context m a, a)
--    type OutMsg = ∃a . (Capability a, a)
--    type family Context m :: * → *
--    type Capability a = (∃m . Context m a, GlobalID)
--    type Self = ∀a . (Context m a → Capability a)
--    type Signal m = Context m SigMsg
--    type SigMsg = Init Mode 
--                | YouAre Self 
--                | SetStdOut (Capability Text)
--                | etc.
-- 
-- For Awelon project, a primary goal is to ensure that long-running
-- behaviors have a stateful and human meaningful representation. In
-- context of Wikilon, this means *live programming* and some direct
-- manipulation:
--
-- * machines are bound to a word in a branching dictionary
-- * updates to the word in the branch affect Step and Signal
-- * machines support direct state observation and manipulation
-- 
-- Fortunately, these features can be modeled directly using Wikilon's
-- own machine interface for subscriptions and a simple wrapper around
-- user-defined state and behavior. 
--
module Wikilon.Store.Machine
    ( AVM(..)
    , LiveAVM(..)
    , Bind(..)
    , MachineName(..)
    , PublicPrivateKey(..)
    ) where

import Data.Typeable (Typeable)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8

-- import Database.VCache

import Wikilon.Dict.Word
import Wikilon.Store.Code
import Wikilon.Store.Time
import Wikilon.Store.Branch


-- | Our basic, opaque machine model.
--
-- The step function must be a pure function of the form:
--
--    (InMsg,State)→([OutMsg],State)
--
-- Note: I doubt I will directly persist AVMs using this model. It
-- seems unsuitable for performance because state updates faster than
-- signal or step.
--
data AVM = AVM 
    { avm_step   :: !ABC
    , avm_signal :: !Value
    , avm_state  :: !Value
    }

-- | A liveness layer treats our primary machine behavior as state.
-- Message 'in the left' are passed transparently to the primary
-- machine, while those 'in the right' are intercepted.
-- 
-- The liveness layer adds the following features:
--
-- * bind step function and signal to a dictionary
-- * direct update to the state or step functions
-- * direct observation and manipulation of state
-- * subscriptions to state for reactive views
-- 
-- Anything here could be done just as easily if directly by AVM.
-- But modeling it explicitly allows an immediate implementation.
--
data LiveAVM = LiveAVM
    { lavm_inner  :: !AVM
    , lavm_bind   :: !Bind
    -- something more for subscriptions
    }

-- | binding associated with live AVMs
data Bind = Bind
    { bind_branch :: !BranchName
    , bind_word   :: !Word
    , bind_time   :: !T
    } 

-- | Each toplevel machine has a secure, globally unique identity via
-- public key cryptography. Separately, I'll be favoring a secret for
-- cryptographic capabilities.
data MachineName = MachineName 
    { m_pki_full :: !PublicPrivateKey
    , m_pki_hash :: !UTF8.ByteString -- in ABC base16
    , m_cap_key  :: !BS.ByteString
    } deriving (Typeable)

-- public and private key, and the algorithm name
-- probably favoring elliptic curve cryptography
data PublicPrivateKey = PublicPrivateKey
    { m_key_public    :: !BS.ByteString
    , m_key_private   :: !BS.ByteString
    , m_pki_algorithm :: !UTF8.ByteString
    }

-- THOUGHTS:
--
-- I'd like to optimize representations of machines for storage or 
-- persistence. Relevantly, I might keep the stable parts separate
-- from the main state for a machine. 


