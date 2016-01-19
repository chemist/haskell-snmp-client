{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Network.Snmp.Client.Simple
-- Copyright   : (c) Anton Kondrachkov 2015
-- License     : BSD3
--
-- Maintainer  : Anton Kondrachkov <simplex91@gmail.com>
-- Stability   : Experimental
--
--------------------------------------------------------------------------------

module Network.Snmp.Client.Simple (
  client
  ) where


import           BasicPrelude
import           Network.Snmp.Client.Api
import           Network.Snmp.Client.Config
import           Network.Snmp.Client.Simple.Community
import           Network.Snmp.Client.Simple.Usm

client :: Config -> IO Client
-- client ConfigV1{..} = communityClient v1 hostname port timeout retries community
client ConfigV1{..} =
    Client <$> communityClient v1 hostname port timeout retries community
client ConfigV2{..} =
    Client <$> communityClient v2 hostname port timeout retries community
client ConfigV3{..} =
    Client <$> usmClient hostname port timeout retries security
