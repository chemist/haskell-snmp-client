{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Network.Snmp.Client.Types
-- Copyright   : (c) Anton Kondrachkov 2015
-- License     : BSD3
--
-- Maintainer  : Anton Kondrachkov <simplex91@gmail.com>
-- Stability   : Experimental
--
--------------------------------------------------------------------------------

module Network.Snmp.Client.Types
(
  OID
, OIDS
, Community(..)
, AuthType(..)
, PrivType(..)
, PrivAuth(..)
, Value(..)
, V1
, V2
, V3
, Version(..)
, Request(..)
, ErrorStatus
, ErrorIndex
, Suite(..)
, Coupla(..)
, SnmpException(..)

, Login
, Password
, Usm(..)
, AuthInfo(..)
, PrivInfo(..)
, Hostname(..)
, Port(..)
, Timeout(..)
, RetryCount(..)

, SnmpManager(..)
, Client(..)

, sec
, msec
, Construct(..)
) where

import           BasicPrelude
import qualified Prelude

import           Data.Word
import           Network.Protocol.Snmp

-- | SnmpV3 Usm user name.
type Login = ByteString

-- | SnmpV3 User Security Model info.
data Usm
    = Usm
    { username :: !Login
      -- ^ User name.
    , auth     :: !(Maybe AuthInfo)
      -- ^ Auth info.
    , priv     :: !(Maybe PrivInfo)
      -- ^ Priv info.
    , level    :: !PrivAuth
      -- ^ Security level [NoAuthNoPriv, AuthNoPriv, AuthPriv].
    }
    deriving (Show, Eq)

-- | SnmpV3 Usm auth information.
-- Used for AuthNoPriv and AuthPriv security levels.
data AuthInfo
    = AuthInfo
    { authType     :: !AuthType
    , authPassword :: !Password
    } deriving (Eq, Ord, Show)

-- | SnmpV3 Usm private information.
-- Used for PDU ecnryption with AuthPriv security level.
data PrivInfo
    = PrivInfo
    { privType     :: !PrivType
    , privPassword :: !Password
    } deriving (Eq, Ord, Show)

newtype Hostname
    = Hostname ByteString
      -- ^ Hostname or IP address octets.
  deriving (Eq, Ord, Show)
newtype Port
    = Port Word16
      -- ^ Port (1-65535), default 161.
  deriving (Eq, Ord, Show)

newtype Timeout
    = Timeout Int
      -- ^ Timeout (msec). Time to wait for response.
  deriving (Eq, Ord, Show)

newtype RetryCount
    = RetryCount Int
      -- ^ Retries. How many times engine should resend request on timeout.
  deriving (Eq, Ord, Show)

-- | SnmpManager class.
-- Provides an interface to request data from Snmp manager.
class SnmpManager a where
    managerAddress :: a -> IO Hostname
    managerVersion :: a -> IO Version
    managerTimeout :: a -> IO Timeout
    managerRetries :: a -> IO RetryCount

    reqSuite :: a -> Request -> Suite -> IO Suite
    reqSuiteTR :: a -> Request -> Timeout -> RetryCount -> Suite -> IO Suite

-- | Existential type for Snmp clients.
data Client = forall a. (Typeable a, Show a, SnmpManager a) => Client a

instance Show Client where
    show (Client a) = Prelude.show a
    {-# INLINE show #-}

instance SnmpManager Client where
    managerAddress (Client a) = managerAddress a
    {-# INLINE managerAddress #-}
    managerVersion (Client a) = managerVersion a
    {-# INLINE managerVersion #-}
    managerTimeout (Client a) = managerTimeout a
    {-# INLINE managerTimeout #-}
    managerRetries (Client a) = managerRetries a
    {-# INLINE managerRetries #-}

    reqSuite (Client a) = reqSuite a
    {-# INLINE reqSuite #-}
    reqSuiteTR (Client a) = reqSuiteTR a
    {-# INLINE reqSuiteTR #-}

msec :: Int -> Timeout
msec = Timeout . (* 1000)

sec :: Int -> Timeout
sec = Timeout . (* 1000000)

