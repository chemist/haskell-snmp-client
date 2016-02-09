{-# LANGUAGE NoImplicitPrelude #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Network.Snmp.Client.Internal
-- Copyright   : (c) Anton Kondrachkov 2015
-- License     : BSD3
--
-- Maintainer  : Anton Kondrachkov <simplex91@gmail.com>
-- Stability   : Experimental
--
--------------------------------------------------------------------------------

module Network.Snmp.Client.Internal
    ( oidFromBS
    , succCounter
    , predCounter
    , firstS
    , lastS
    , isUpLevel
    , uniqID
    , makeSuiteDefault
    , reverseSuite

    , retryN
    , retryOnTimeout

    , module Network.Snmp.Client.Internal.Socket
    , module Network.Snmp.Client.Internal.Community
    , module Network.Snmp.Client.Internal.Usm
    )
    where

import           BasicPrelude

import qualified Data.ByteString.Char8                  as C
import           Data.IORef                             (IORef,
                                                         atomicModifyIORef')
import           Network.Info
import           System.Timeout

import           Network.Snmp.Client.Exception
import           Network.Snmp.Client.Internal.Community
import           Network.Snmp.Client.Internal.Socket
import           Network.Snmp.Client.Internal.Usm
import           Network.Snmp.Client.Types

oidFromBS :: ByteString -> OID
oidFromBS = mapMaybe (\x -> fst <$> C.readInteger x) . C.split '.'
{-# INLINE oidFromBS #-}

makeSuiteDefault :: Value -> Suite -> Suite
makeSuiteDefault v (Suite couplaList) =
    Suite $ fmap (\(Coupla oi _) -> Coupla oi v) couplaList
{-# INLINE makeSuiteDefault #-}

reverseSuite :: Suite -> Suite
reverseSuite (Suite l) = Suite $ reverse l
{-# INLINE reverseSuite #-}

succCounter :: (Bounded a, Integral a) => IORef a -> IO a
succCounter ref = atomicModifyIORef' ref check
  where
    check x
        | x < maxBound  = (succ x, succ x)
        | otherwise = (0, 0)
{-# INLINE succCounter #-}

predCounter :: (Bounded a, Integral a) => IORef a -> IO a
predCounter ref = atomicModifyIORef' ref check
  where
    check x
        | x > 0 = (pred x, pred x)
        | otherwise = (maxBound, maxBound)
{-# INLINE predCounter #-}

firstS :: Suite -> Coupla
firstS (Suite xs) = head xs
{-# INLINE firstS #-}

lastS :: Suite -> Coupla
lastS (Suite xs) = last xs
{-# INLINE lastS #-}

isUpLevel :: OID -> OID -> Bool
isUpLevel new old = let baseLength = length old
                    in old /= take baseLength new
{-# INLINE isUpLevel #-}

uniqID :: IO Int32
uniqID = do
    nf <- getNetworkInterfaces
    let zeroMac = MAC 0 0 0 0 0 0
        zeroIp = IPv4 0
        ipToW (IPv4 x) = x
    return $ abs . fromIntegral $ case filter (\x -> ipv4 x /= zeroIp && mac x /= zeroMac ) nf of
        [] -> 1000000 -- i cant find ip address
        [x] -> ipToW (ipv4 x)
        x:_ -> ipToW (ipv4 x)

retryOnTimeout :: Timeout -> RetryCount -> IO a -> IO a
retryOnTimeout (Timeout to) retries f = check =<< retryN retries action
  where
    action = timeout to f
    check :: Maybe a -> IO a
    check (Just a) = return a
    check Nothing = throwIO $ TimeoutException to
{-# INLINE retryOnTimeout #-}

retryN :: (Monad m) => RetryCount -> m (Maybe a) -> m (Maybe a)
retryN (RetryCount 0) f = f
retryN (RetryCount n) f = do
    result <- f
    case result of
        Nothing -> retryN (RetryCount (n - 1)) f
        Just r -> return $ Just r
{-# INLINE retryN #-}
