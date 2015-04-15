{-# LANGUAGE DeriveDataTypeable #-}

-- | Wikilon hosts a set of abstract virtual machines, or AVMs. These
-- machines interact by messaging each other through a virtual network.
-- Messaging is capability-secure at a fine granularity, allowing the
-- machine to define its own capabilities.
--
-- Each machine intrinsically has:
--
-- * a global identifier based on a public key
-- * an internal secret for cryptographic capabilities
-- * a state, which is just an ABC value
-- * a signal context for host-layer signals
-- * a behavior function, which is ABC code
-- 
-- Wikilon aims to support live programming. Thus, it may be necessary
-- to update our signal context and behavior function at runtime. To
-- make this easy, we'll bind each machine's behavior to a branch of 
-- the dictionary and recompute the behavior function as needed. When
-- not bound to a dictionary, we could instead use a capability.
--
-- We may also wish to support direct view and manipulation of machine
-- state, i.e. similar to operating on a filesystem, i.e. for ad-hoc
-- debugging and administration.
--
-- Thoughts:
--
-- Rather than capturing machine states in Wikilon, it might be better
-- to capture the entire network state, including a set of machines.
-- In this case, we might also want to separate the stable elements of
-- the network (names, bindings, behaviors) from the unstable elements.
--
module Wikilon.Machine
    ( AVM
    , Bind
    , MachineName
    , MachineMeta
    ) where

import Data.Typeable (Typeable)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8

import Database.VCache

import Wikilon.ABC
import Wikilon.Time
import Wikilon.Branch
import Wikilon.Dict.Word


-- | The underlying abstract virtual machine type.
type AVM = AVM0

-- versioned AVM state data
data AVM0 = AVM0
    { avm_step   :: !ABC
    , avm_signal :: !Value
    , avm_state  :: !Value
    } deriving (Typeable)

-- | Content to support live programming. The machine's behavior is
-- specified in terms of an ABC word that computes a (Step*Signal) 
-- pair.
--
-- If not bound to a dictionary, we might instead provide capability
-- to update the step and signal functions via message.
--
-- Aside: I am contemplating instead binding to an ABC subprogram.
-- My hesitation to do so, however, is that it would require some
-- additional interfaces to easily edit our machine's behavior.
-- With Wikilon, it may be easier to just create a new word for 
-- each active machine that must be edited independently.
--
type Bind = Bind0

-- versioned live program binding
data Bind0 = Bind0
    { bind_branch :: !BranchName
    , bind_word   :: !Word
    , bind_time   :: !T
    } deriving (Typeable)

-- | Each toplevel machine has a secure, globally unique identity via
-- public key cryptography. Separately, I'll be favoring a secret for
-- cryptographic capabilities.
type MachineName = Name0

-- versioned name data
data Name0 = Name0 
    { m_pki_full :: !PublicPrivateKey
    , m_pki_hash :: !UTF8.ByteString -- in ABC base16
    , m_cap_key  :: !BS.ByteString
    } deriving (Typeable)

-- public and private key, and the algorithm name
-- probably favoring elliptic curve cryptography
type PublicPrivateKey = PPK0
data PPK0 = PPK0
    { m_key_public    :: !BS.ByteString
    , m_key_private   :: !BS.ByteString
    , m_pki_algorithm :: !UTF8.ByteString
    }

-- | Collected metadata for a machine. Mostly, this information is
-- very stable.
type MachineMeta = Meta0

-- versioned collection of metadata
data Meta0 = Meta0
    { m_name :: !MachineName
    , m_bind :: !(Maybe Bind)
    }

-- | The machine in context of Wikilon.
data Machine = Machine
    { m_meta  :: !(VRef MachineMeta)
    , m_state :: !AVM
    }


