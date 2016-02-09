{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Network.Snmp.Client.Config
-- Copyright   : (c) Anton Kondrachkov 2015
-- License     : BSD3
--
-- Maintainer  : Anton Kondrachkov <simplex91@gmail.com>
-- Stability   : Experimental
--
--------------------------------------------------------------------------------

module Network.Snmp.Client.Config (
  Config(..)
) where

import           BasicPrelude
import           Network.Snmp.Client.Types

data Config
    = ConfigV1
    { hostname  :: !Hostname
    , port      :: !Port
    , timeout   :: !Timeout
    , retries   :: !RetryCount
    , community :: !Community
    }
    | ConfigV2
    { hostname  :: !Hostname
    , port      :: !Port
    , timeout   :: !Timeout
    , retries   :: !RetryCount
    , community :: !Community
    }
    | ConfigV3
    { hostname :: !Hostname
    , port     :: !Port
    , timeout  :: !Timeout
    , retries  :: !RetryCount
    , security :: !Usm
    }
    deriving (Show, Eq)

instance Construct (Version -> Config) where
    initial Version1 = ConfigV1
        { hostname  = Hostname "localhost"
        , port      = Port 161
        , timeout   = sec 5
        , retries   = RetryCount 1
        , community = Community "public"
        }
    initial Version2 = ConfigV2
        { hostname  = Hostname "localhost"
        , port      = Port 161
        , timeout   = sec 5
        , retries   = RetryCount 1
        , community = Community "public"
        }
    initial Version3 = ConfigV3
        { hostname = Hostname "localhost"
        , port     = Port 161
        , timeout  = sec 5
        , retries  = RetryCount 1
        , security = Usm "guest" Nothing Nothing NoAuthNoPriv
        }
    {-# INLINE initial #-}
