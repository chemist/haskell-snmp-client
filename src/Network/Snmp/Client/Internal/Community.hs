{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Network.Snmp.Client.Internal.Community
-- Copyright   : (c) Anton Kondrachkov 2015
-- License     : BSD3
--
-- Maintainer  : Anton Kondrachkov <simplex91@gmail.com>
-- Stability   : Experimental
--
--------------------------------------------------------------------------------

module Network.Snmp.Client.Internal.Community
    ( setRCS
    ) where

import           BasicPrelude

import           Network.Protocol.Snmp (Community, Packet, Request, Suite)
import qualified Network.Protocol.Snmp as Snmp

-- | Set RequestID, Community and Suite to Packet.
setRCS :: Request -> Community -> Suite -> Packet -> Packet
setRCS r c s = Snmp.setRequest r . Snmp.setCommunityP c . Snmp.setSuite s
{-# INLINE setRCS #-}
