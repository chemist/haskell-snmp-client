{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Network.Snmp.Client.Simple.Usm
-- Copyright   : (c) Anton Kondrachkov 2015
-- License     : BSD3
--
-- Maintainer  : Anton Kondrachkov <simplex91@gmail.com>
-- Stability   : Experimental
--
--------------------------------------------------------------------------------

module Network.Snmp.Client.Simple.Usm
    ( usmClient
    ) where

import           BasicPrelude
import qualified Prelude

import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.IORef
import           System.Random

import           Network.Protocol.Snmp         (Packet, RequestId,
                                                getErrorStatus, getRid,
                                                getSuite)

import           Network.Snmp.Client.Exception
import           Network.Snmp.Client.Internal
import           Network.Snmp.Client.Types

-- | Cache for USM security.
-- Used for encryption.
data UsmCache
    = UsmCache
    { authCache  :: !(Maybe (AuthType, Key))
      -- ^ Auth key for MD5/SHA packet signation.
    , privCache  :: !(Maybe (PrivType, Key))
      -- ^ Priv key for DES/AES PDU encryption.
    , packet     :: !Packet
      -- ^ Packet skeleton with filled security fields.
    , engineInfo :: !EngineInfo
      -- ^ Snmp Engine information.
    }
    deriving (Eq, Show)

data UsmClient
    = UsmClient
    { socketVar     :: !(MVar UdpSocket)
    , address       :: !Address

    , timeout       :: !Timeout
    , retries       :: !RetryCount
    , security      :: !Usm
    , ridRef        :: !(IORef RequestId)
    , saltRef       :: !(IORef UsmSalt)
    , cacheVar      :: !(TVar (Maybe UsmCache))

    , maxPacketSize :: !Int
    }
    deriving Typeable

instance Show UsmClient where
    show UsmClient{..} =
        "UsmClient {"
        <> " host: " <> Prelude.show address
        <> "}"

usmClient :: Hostname -> Port -> Timeout -> RetryCount -> Usm -> IO UsmClient
usmClient host port to rc usm@Usm{..} = do
    Just addr <- resolve host port  -- TODO: Make proper error handling!
    socketVar <- newMVar =<< newSock
    ridRef <- newIORef =<< uniqID
    saltRef <- newIORef =<< newSalt priv
    cacheVar <- newTVarIO Nothing

    return UsmClient
        { socketVar = socketVar
        , address = addr
        , timeout = to
        , retries = rc
        , security = usm
        , ridRef = ridRef
        , saltRef = saltRef
        , cacheVar = cacheVar
        , maxPacketSize = defaultPacketSize
        }
  where
    newSock = do
        sock <- udpSocket
        bindPort sock (Port 0)
        return sock

instance SnmpManager UsmClient where
    managerAddress = return . Hostname . encodeUtf8 . show . address
    managerVersion _ = return Version3
    managerTimeout = return . timeout
    managerRetries = return . retries

    reqSuiteTR = talkClient
    {-# INLINE reqSuiteTR #-}
    reqSuite cl@UsmClient{timeout, retries} r =
        reqSuiteTR cl r timeout retries
    {-# INLINE reqSuite #-}

withSocketVar :: MVar UdpSocket -> (UdpSocket -> IO a) -> IO a
withSocketVar sockVar = bracket (takeMVar sockVar) (putMVar sockVar)
{-# INLINE withSocketVar #-}

talkClient :: UsmClient -> Request -> Timeout -> RetryCount -> Suite -> IO Suite
talkClient UsmClient{..} r to rc suite =
    withSocketVar socketVar $ \ sock -> do
        cacheMaybe <- readTVarIO cacheVar
        cache <- case cacheMaybe of
                    Just c -> return c
                    Nothing -> do
                        c <- initUsmCache sock
                        atomically $ writeTVar cacheVar (Just c)
                        return c
        let pkt = setRS r suite (packet cache)
        result <- retryOnTimeout to rc $ do
            rid <- sendReq sock cache pkt
            receiveResp sock cache rid
        updateCacheVar cacheVar result
        returnResult result
  where
    sendReq s c p = do
        rid <- succCounter ridRef
        encoded <- encP c saltRef (setRequestIdP rid p)
        sendMessage s (Message encoded address)
        return rid

    receiveResp s c rid = do
        (Message encoded source) <- receiveMessage s maxPacketSize
        if getRequestIdP encoded == rid && source == address
            then return $! decP c encoded
            else receiveResp s c rid

    initUsmCache s =
        makeCache security <$> retryOnTimeout timeout retries (obtainEngineInfo s address ridRef)

    updateCacheVar cv p = atomically $ do
        cacheMaybe <- readTVar cv
        up cacheMaybe
      where
        newei = getEngineInfoP p
        writeCache = writeTVar cv $! Just (makeCache security newei)

        up Nothing = writeCache
        up (Just cache) =
            let oldei = engineInfo cache
            in if checkEngineInfo oldei newei
                then return ()
                else writeCache

checkEngineInfo :: EngineInfo -> EngineInfo -> Bool
checkEngineInfo old new =
    (boots old == boots new)
    && (time new) - (time old) < 150

returnResult :: Packet -> IO Suite
returnResult resp
    | isReport resp = throwIO $ SnmpEngineException $! (firstS . getSuite) resp
    | status /= 0 = throwIO $ SnmpException status
    | otherwise = return $ getSuite resp
  where status = getErrorStatus resp
        isReport p =
            case getRequestP p of
                Report{} -> True
                _ -> False

succSalt :: IORef UsmSalt -> IO UsmSalt
succSalt ref = atomicModifyIORef' ref checkSalt
  where
    checkSalt (Salt32 x)
        | x < maxBound = (Salt32 $! succ x, Salt32 $! succ x)
        | otherwise = (Salt32 0, Salt32 0)
    checkSalt (Salt64 x)
        | x < maxBound = (Salt64 $! succ x, Salt64 $! succ x)
        | otherwise = (Salt64 $! 0, Salt64 $! 0)
{-# INLINE succSalt #-}

encP :: UsmCache -> IORef UsmSalt -> Packet -> IO Packet
encP UsmCache{..} saltRef pkt = do
    p' <- encrypt privCache pkt
    return $! sign authCache p'
  where
    encrypt (Just (pt, k)) p = do
        salt <- succSalt saltRef
        return $! encryptPacket pt salt k p
    encrypt Nothing p = return p
    sign (Just (at, k)) p = signPacket at k p
    sign Nothing p = p
{-# INLINE encP #-}

decP :: UsmCache -> Packet -> Packet
decP UsmCache{..} = decrypt privCache
  where
    decrypt (Just (pt, k)) = decryptPacket pt k
    decrypt Nothing = id
{-# INLINE decP #-}

obtainEngineInfo :: UdpSocket -> Address -> IORef RequestId -> IO EngineInfo
obtainEngineInfo sock dest ridRef = do
    rid <- succCounter ridRef
    sendReq (dummyPacket rid)
    resp <- receiveResp rid
    return $! getEngineInfoP resp
  where
    sendReq p = sendMessage sock (Message p dest)
    receiveResp rid = do
        (Message resp source) <- receiveMessage sock defaultPacketSize
        if source == dest && rid == getRid resp
            then return resp
            else receiveResp rid

newSalt :: Maybe PrivInfo -> IO UsmSalt
newSalt (Just (PrivInfo DES _)) = (Salt32 . abs) <$> randomIO
newSalt (Just (PrivInfo AES _)) = (Salt64 . abs) <$> randomIO
newSalt _ = return $ Salt32 0
{-# INLINE newSalt #-}

makeCache :: Usm -> EngineInfo -> UsmCache
makeCache usm@Usm{..} ei =
    UsmCache { authCache = newAuthCache auth
             , privCache = newPrivCache auth priv
             , packet = (setUsmP usm . setEngineInfoP ei) v3Packet
             , engineInfo = ei
             }
  where
    newAuthCache (Just ai) = Just $! (authType ai, makeAuthKey ai ei)
    newAuthCache Nothing = Nothing
    newPrivCache (Just ai) (Just pi') = Just $! (privType pi', makePrivKey ai pi' ei)
    newPrivCache _ Nothing = Nothing
    newPrivCache _ _ = error "incorrect security settings"

