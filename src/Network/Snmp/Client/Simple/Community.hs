{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Network.Snmp.Client.Simple.Community
-- Copyright   : (c) Anton Kondrachkov 2015
-- License     : BSD3
--
-- Maintainer  : Anton Kondrachkov <simplex91@gmail.com>
-- Stability   : Experimental
--
--------------------------------------------------------------------------------

module Network.Snmp.Client.Simple.Community where

import           BasicPrelude
import qualified Prelude

import           Network.Protocol.Snmp
import           Network.Snmp.Client.Internal
import           Network.Snmp.Client.Types

import           Control.Concurrent.MVar
import           Data.IORef

v1 :: Packet
v1 = initial Version1

v2 :: Packet
v2 = initial Version2


data CommunityClient = CommunityClient
  { socket    :: MVar UdpSocket
  , ridRef    :: IORef RequestId

  , address   :: Address
  , community :: Community

  , timeout   :: Timeout
  , retries   :: RetryCount

  , packet    :: Packet
  }

instance SnmpManager CommunityClient where
    managerAddress = return . Hostname . encodeUtf8 . show . address
    managerVersion = return . getVersion . packet
    managerTimeout = return . timeout
    managerRetries = return . retries

    reqSuiteTR = talkClient
    {-# INLINE reqSuiteTR #-}
    reqSuite cl@CommunityClient{timeout, retries} r = talkClient cl r timeout retries
    {-# INLINE reqSuite #-}

instance Show CommunityClient where
    show CommunityClient{..} =
        "CommunityClient {"
        <> "type: simple" <> ", "
        <> "host: " <> Prelude.show address
        <> "}"

communityClient :: Packet
                -> Hostname
                -> Port
                -> Timeout
                -> RetryCount
                -> Community
                -> IO CommunityClient
communityClient pkt h p to rc community = do
    -- Resolve hostname and make socket.
    Just address <- resolve h p
    socketVar <- newMVar =<<  newSocket
    -- Make RequestID reference.
    ridRef <- newIORef =<< uniqID

    return CommunityClient
            { socket = socketVar
            , ridRef = ridRef
            , address = address
            , community = community
            , timeout = to
            , retries = rc
            , packet = pkt
            }
  where
    newSocket = do
        s <- udpSocket
        bindPort s (Port 0)
        return s

withSocket :: MVar UdpSocket -> (UdpSocket -> IO a) -> IO a
withSocket sockVar = bracket (takeMVar sockVar) (putMVar sockVar)
{-# INLINE withSocket #-}

talkClient :: CommunityClient -> Request -> Timeout -> RetryCount -> Suite -> IO Suite
talkClient CommunityClient{..} r to rc suite =
    withSocket socket $ \ sock -> retryOnTimeout to rc $ do
        rid <- sendReq sock p'
        (returnResult =<< receiveResp sock rid) `catch` fixErrorV1 r suite
  where
    p' = setRCS r community suite packet
    sendReq s p = do
        rid <- succCounter ridRef
        let pkt = setRid rid p
        sendMessage s (Message pkt address)
        return rid
    receiveResp s rid = do
        (Message resp source) <- receiveMessage s 1500
        if (source == address) && (getRid resp == rid)
            then return resp
            else receiveResp s rid

returnResult :: Packet -> IO Suite
returnResult resp
    | status /= 0 = throwIO $ SnmpException status
    | otherwise = return $ getSuite resp
  where status = getErrorStatus resp
{-# INLINE returnResult #-}

fixErrorV1 :: Request -> Suite -> SnmpException -> IO Suite
fixErrorV1 GetRequest{} suite (SnmpException 2) =
    return $ makeSuiteDefault NoSuchObject suite
fixErrorV1 GetNextRequest{} suite (SnmpException 2) =
    return $ makeSuiteDefault EndOfMibView suite
fixErrorV1 _ _ err = throwIO err
{-# INLINE fixErrorV1 #-}

