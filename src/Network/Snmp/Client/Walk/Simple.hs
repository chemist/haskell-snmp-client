--------------------------------------------------------------------------------
-- |
-- Module      : Network.Snmp.Client.Walk.Simple
-- Copyright   : (c) Anton Kondrachkov 2016
-- License     : BSD3
--
-- Maintainer  : Anton Kondrachkov <simplex91@gmail.com>
-- Stability   : Experimental
--
--------------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Snmp.Client.Walk.Simple (
  walk
, bulkwalk
) where

import           BasicPrelude
import qualified Data.DList                     as DL
import           Network.Snmp.Client.Api
import           Network.Snmp.Client.Types
import qualified Network.Snmp.Client.Walk.Pipes as Walk
import qualified Pipes.Prelude                  as Pipes

walk :: (SnmpManager c, MonadIO io) => c -> OIDS -> io Suite
walk c oids =  Pipes.fold collect [] (Suite . reverse) (Walk.walk c oids)
  where
    collect xs x = x:xs

bulkwalk :: (SnmpManager c, MonadIO io) => c -> OIDS -> io Suite
bulkwalk c oids = Pipes.fold collect DL.empty (Suite . DL.toList) (Walk.bulkwalk c oids)
  where
    collect acc xs = DL.append acc (DL.fromList xs)

