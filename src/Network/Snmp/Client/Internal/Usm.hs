{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Network.Snmp.Client.Internal.Usm
-- Copyright   : (c) Anton Kondrachkov 2015
-- License     : BSD3
--
-- Maintainer  : Anton Kondrachkov <simplex91@gmail.com>
-- Stability   : Experimental
--
--------------------------------------------------------------------------------

module Network.Snmp.Client.Internal.Usm
    ( UsmSalt (..)
    , EngineInfo (..)
    , Key
    , encryptPacket
    , decryptPacket
    , signPacket
    , makeAuthKey
    , makePrivKey
    , getContextEngineIdP
    , setContextEngineIdP
    , setUsmP
    , getEngineInfoP
    , setEngineInfoP
    , setRequestIdP
    , getRequestIdP
    , getRequestP
    , setRS
    , v3Packet
    , dummyPacket
    , defaultPacketSize
    ) where

import           BasicPrelude
import           Data.Binary
import qualified Data.ByteString.Lazy      as BL

import           Network.Protocol.Snmp     (ContextEngineID, EngineBootId,
                                            EngineId, EngineTime, Header, Key,
                                            PDU (..), Packet, aesDecrypt,
                                            aesEncrypt, desDecrypt, desEncrypt)
import qualified Network.Protocol.Snmp     as Snmp

import           Network.Snmp.Client.Types


-- | Usm encryption salt.
data UsmSalt
    = Salt32 !Int32
      -- ^ 32bit salt for DES.
    | Salt64 !Int64
      -- ^ 64bit salt for AES.
    deriving (Eq, Show)

-- | SNMP Engine info.
-- Should be filled (retrieved) before sending requests.
data EngineInfo
    = EngineInfo
    { engineID        :: !EngineId
      -- ^ SnmpV3 Authorative Engine ID.
    , contextEngineID :: !ContextEngineID
      -- ^ SnmpV3 Context Engine ID.
    , boots           :: !EngineBootId
      -- ^ SnmpV3 Authorative Engine Boots.
    , time            :: !EngineTime
      -- ^ SnmpV3 Authorative Engine Time.
    }
    deriving (Eq, Show)

-- | Encrypts packet with specified algorithm.
-- PrivType and UsmSalt should match!
encryptPacket :: PrivType -> UsmSalt -> Key -> Packet -> Packet
encryptPacket pt s key packet =
    Snmp.setPrivParametersP salt' . Snmp.setPDU (CryptedPDU encrypted) $ packet
  where
    header = Snmp.getHeader packet :: Header V3
    pdu = BL.toStrict . encode $ (Snmp.getPDU packet :: PDU V3)
    eib = Snmp.getAuthoritiveEngineBoots header
    et = Snmp.getAuthoritiveEngineTime header
    encryptPDU :: PrivType -> UsmSalt -> (ByteString, Snmp.Salt)
    encryptPDU DES (Salt32 salt) = desEncrypt key eib salt pdu
    encryptPDU AES (Salt64 salt) = aesEncrypt key eib et salt pdu
    encryptPDU _ _ = error "incorrect arguments"
    (encrypted, salt') = encryptPDU pt s
{-# INLINE encryptPacket #-}

-- | Decrypts packet with specified algorithm.
decryptPacket :: PrivType -> Key -> Packet -> Packet
decryptPacket pt key packet =
    Snmp.setPDU (decryptPDU pt pdu) packet
  where
    header = Snmp.getHeader packet :: Header V3
    pdu = Snmp.getPDU packet :: PDU V3
    salt = Snmp.getPrivacyParameters header
    eib = Snmp.getAuthoritiveEngineBoots header
    et = Snmp.getAuthoritiveEngineTime header
    decryptPDU :: PrivType -> PDU V3 -> PDU V3
    decryptPDU DES (CryptedPDU x) = decode . BL.fromStrict . desDecrypt key salt $ x :: PDU V3
    decryptPDU AES (CryptedPDU x) = decode . BL.fromStrict . aesDecrypt key salt eib et $ x :: PDU V3
    decryptPDU _ _ = pdu
{-# INLINE decryptPacket #-}

-- | Signs packet.
signPacket :: AuthType -> Key -> Packet -> Packet
signPacket = Snmp.signPacket
{-# INLINE signPacket #-}

-- | Gets context engine ID from packet.
getContextEngineIdP :: Packet -> ContextEngineID
getContextEngineIdP p = Snmp.getContextEngineID pdu
  where
    pdu = Snmp.getPDU p :: PDU V3
{-# INLINE getContextEngineIdP #-}

-- | Sets context engine ID to packet.
setContextEngineIdP :: ContextEngineID -> Packet -> Packet
setContextEngineIdP ceid packet = Snmp.setPDU newpdu packet
  where
    pdu = Snmp.getPDU packet :: PDU V3
    newpdu = Snmp.setContextEngineID ceid pdu
{-# INLINE setContextEngineIdP #-}

-- | Sets USM info to packet.
setUsmP :: Usm -> Packet -> Packet
setUsmP Usm{username, level} =
      Snmp.setReportableP True
    . Snmp.setPrivAuthP level
    . Snmp.setUserNameP username
    . Snmp.setAuthenticationParametersP Snmp.cleanPass
{-# INLINE setUsmP #-}

-- | Sets EngineInfo to packet.
setEngineInfoP :: EngineInfo -> Packet -> Packet
setEngineInfoP EngineInfo{..} p =
    (Snmp.setHeader header . Snmp.setPDU pdu) p
  where
    header = ( Snmp.setAuthoritiveEngineId engineID
             . Snmp.setAuthoritiveEngineBoots boots
             . Snmp.setAuthoritiveEngineTime time
             ) (Snmp.getHeader p :: Header V3)
    pdu = Snmp.setContextEngineID contextEngineID (Snmp.getPDU p :: PDU V3)
{-# INLINE setEngineInfoP #-}

-- | Gets EngineInfo from packet.
getEngineInfoP :: Packet -> EngineInfo
getEngineInfoP p = EngineInfo { engineID = Snmp.getEngineIdP p
                              , contextEngineID = getContextEngineIdP p
                              , boots = Snmp.getEngineBootsP p
                              , time = Snmp.getEngineTimeP p
                              }
{-# INLINE getEngineInfoP #-}

-- | Set RequestID to packet.
setRequestIdP :: Snmp.RequestId -> Packet -> Packet
setRequestIdP rid = Snmp.setIDP (Snmp.ID rid) . Snmp.setRid rid
{-# INLINE setRequestIdP #-}

-- | Get RequestID from packet.
getRequestIdP :: Packet -> Snmp.RequestId
getRequestIdP = (\(Snmp.ID i) -> i) . Snmp.getID . header
  where
    header p = Snmp.getHeader p :: Snmp.Header V3
{-# INLINE getRequestIdP #-}

getRequestP :: Packet -> Snmp.Request
getRequestP = Snmp.getRequest
{-# INLINE getRequestP #-}

-- | Set Request and Suite to packet.
setRS :: Request -> Suite -> Packet -> Packet
setRS r s = Snmp.setRequest r . Snmp.setSuite s
{-# INLINE setRS #-}

-- | Make authentication key.
makeAuthKey :: AuthInfo -> EngineInfo -> Key
makeAuthKey AuthInfo{..} EngineInfo{engineID} =
    Snmp.passwordToKey authType authPassword engineID
{-# INLINE makeAuthKey #-}

-- | Make private key.
makePrivKey :: AuthInfo -> PrivInfo -> EngineInfo -> Key
makePrivKey AuthInfo{authType} PrivInfo{privPassword} EngineInfo{engineID} =
    Snmp.passwordToKey authType privPassword engineID
{-# INLINE makePrivKey #-}

-- | Empty SnmpV3 skeleton packet.
v3Packet :: Packet
v3Packet = initial Snmp.Version3
{-# INLINE v3Packet #-}

-- | Make dummy packet for EngineID Discovery procedure.
dummyPacket :: Snmp.RequestId -> Packet
dummyPacket rid =
    ( Snmp.setIDP (Snmp.ID rid)
    . Snmp.setMaxSizeP (Snmp.MaxSize defaultPacketSize)
    . Snmp.setReportableP False
    . Snmp.setPrivAuthP NoAuthNoPriv
    . Snmp.setRid rid
    ) v3Packet
{-# INLINE dummyPacket #-}

-- | Default packet size for SnmpV3 messages.
defaultPacketSize :: (Integral a) => a
defaultPacketSize = 1500
{-# INLINE defaultPacketSize #-}

