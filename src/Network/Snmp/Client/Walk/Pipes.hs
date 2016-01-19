--------------------------------------------------------------------------------
-- |
-- Module      : Network.Snmp.Client.Walk.Pipes
-- Copyright   : (c) Anton Kondrachkov 2015
-- License     : BSD3
--
-- Maintainer  : Anton Kondrachkov <simplex91@gmail.com>
-- Stability   : Experimental
--
--------------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
module Network.Snmp.Client.Walk.Pipes (
  walk
, bulkwalk
) where

import           BasicPrelude
import           Network.Snmp.Client.Api
import           Network.Snmp.Client.Internal
import           Network.Snmp.Client.Types
import qualified Pipes

walk :: (MonadIO m, SnmpManager c)
     => c
     -> OIDS
     -> Pipes.Producer Coupla m ()
walk _ [] = return ()
walk c (x:xs) = gowalk x >> walk c xs
  where
    gowalk oi = do
        Suite [start] <- liftIO $ get c [oi]
        Suite [next] <- liftIO $ getnext c [oi]
        case value start of
             EndOfMibView -> return ()
             NoSuchObject -> go next
             NoSuchInstance -> go next
             _ -> Pipes.yield start >> go next
    go bind@(Coupla oi val) =
        case (isUpLevel oi x, val) of
            (True, _) -> return ()
            (_, EndOfMibView) -> return ()
            _ -> do
                Pipes.yield bind
                Suite [next] <- liftIO $ getnext c [oi]
                go next

bulkwalk :: (MonadIO m, SnmpManager c)
         => c
         -> OIDS
         -> Pipes.Producer [Coupla] m ()
bulkwalk _ [] = return ()
bulkwalk c (x:xs) = gowalk x >> bulkwalk c xs
  where
    filterBinds = takeWhile (\(Coupla oi' _) -> not $ isUpLevel oi' x)
    gowalk oi = do
        Suite binds <- liftIO $ getbulk c [oi]
        let Coupla nextOid nextValue = last binds
        case (isUpLevel nextOid x, nextValue) of
            (True, _) -> void $ Pipes.yield (filterBinds binds)
            (_, EndOfMibView) -> void $ Pipes.yield (filterBinds binds)
            _ -> Pipes.yield binds >> gowalk nextOid
