{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Network.Snmp.Client.Internal.Socket
-- Copyright   : (c) Anton Kondrachkov 2015
-- License     : BSD3
--
-- Maintainer  : Anton Kondrachkov <simplex91@gmail.com>
-- Stability   : Experimental
--
--------------------------------------------------------------------------------

module Network.Snmp.Client.Internal.Socket where

import           BasicPrelude
import           Control.Monad.Catch
import           Data.Binary
import qualified Data.ByteString.Lazy       as BL

import           Pipes                      (await, yield)
import qualified Pipes
import qualified System.Socket              as Sock
import qualified System.Socket.Family.Inet6 as Inet6

import           Network.Protocol.Snmp      (Packet)

import           Network.Snmp.Client.Types

type UAddrInfo = Sock.AddressInfo Inet6.Inet6 Sock.Datagram Sock.UDP
type UdpSocket = Sock.Socket Inet6.Inet6 Sock.Datagram Sock.UDP
type Address = Inet6.SocketAddressInet6

-- | Resolve ip address by hostname and port.
-- Works like getHostByName.
--
-- Throws `AddressInfoException`.
--
-- > addressMaybe <- resolve (Hostname "localhost") (Port 80)
resolve :: Hostname -> Port -> IO (Maybe Address)
resolve (Hostname h) (Port p) =
    takeFirstAddress <$> (Sock.getAddressInfo
        (Just h)
        (Just $ fromShow p)
        Sock.aiV4Mapped
        :: IO [UAddrInfo])
  where
    takeFirstAddress :: [UAddrInfo] -> Maybe Address
    takeFirstAddress xs
        | null xs = Nothing
        | otherwise = Just . Sock.socketAddress . head $ xs

-- | Make IpV4/IpV6 UDP socket.
--
-- Throws `SocketException`.
--
-- > sock <- udpSocket
udpSocket :: IO UdpSocket
udpSocket = do
    sock <- Sock.socket
    Sock.setSocketOption sock (Inet6.V6Only False)
    return sock

-- | Bind socket to specified port.
-- Sets ReuseAddress option to socket before binding.
--
-- Throws `SocketException`.
--
-- > bindPort sock (Port 8080)
bindPort :: UdpSocket -> Port -> IO ()
bindPort s (Port p) =
    Sock.setSocketOption s (Sock.ReuseAddress True) >> Sock.bind s addr
  where
    addr = Inet6.SocketAddressInet6 Inet6.any (Inet6.Port p) mempty 0

--------------------------------------------------------------------------------
-- | Message type. Used to communicate with Snmp agent.
data Message
    = Message !Packet !Address
    deriving (Eq, Show, Typeable)

-- | Send message via socket.
--
-- > sendMessage socket (Message packet destination)
sendMessage :: UdpSocket -> Message -> IO ()
sendMessage s (Message packet dest) =
    void $ Sock.sendTo s p mempty dest
  where
    p = (BL.toStrict . encode) packet
{-# INLINE sendMessage #-}

-- | Receive message via socket.
--
-- Throws `SnmpException` and `SocketException`.
--
-- > Message packet sender <- receiveMessage socket 1500
receiveMessage :: UdpSocket -> Int -> IO Message
receiveMessage s size =
    toMsg <$> Sock.receiveFrom s size mempty
  where
    decPacket = decode . BL.fromStrict
    toMsg (raw, source) = Message (decPacket raw) source
{-# INLINE receiveMessage #-}

-- | Make @Address@ with localhost:port.
--
-- address = localhost (Port 8080)
localhost :: Port -> Address
localhost (Port p) = Inet6.SocketAddressInet6 Inet6.any (Inet6.Port p) mempty 0

--------------------------------------------------------------------------------

receiver :: (MonadIO m, MonadThrow m) => UdpSocket -> Int -> Pipes.Producer Message m ()
receiver s size = forever $ liftIO (receiveMessage s size) >>= yield

sender :: (MonadIO m, MonadThrow m) => UdpSocket -> Pipes.Consumer Message m ()
sender s = forever (liftIO . sendMessage s =<< await)
