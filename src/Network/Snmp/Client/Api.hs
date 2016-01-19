{-# LANGUAGE NoImplicitPrelude #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Network.Snmp.Client.Api
-- Copyright   : (c) Anton Kondrachkov 2015
-- License     : BSD3
--
-- Maintainer  : Anton Kondrachkov <simplex91@gmail.com>
-- Stability   : Experimental
--
--------------------------------------------------------------------------------


module Network.Snmp.Client.Api
  ( Client(..)
  , SnmpManager
  , get
  , get'
  , getnext
  , getnext'
  , getbulk
  , getbulk'
  , set
  , set'
  ) where

import           BasicPrelude
import           Network.Snmp.Client.Types

toSuite :: OIDS -> Suite
toSuite = Suite . fmap (`Coupla` Zero)
{-# INLINE toSuite #-}

commonReq :: (MonadIO m, SnmpManager c)
          => Request
          -> c
          -> OIDS
          -> m Suite
commonReq r c oids = liftIO $ reqSuite c r suite
  where suite = toSuite oids
{-# INLINE commonReq #-}

commonReqTR :: (MonadIO m, SnmpManager c)
           => Request
           -> c
           -> Timeout
           -> RetryCount
           -> OIDS
           -> m Suite
commonReqTR r c to rc oids = liftIO $ reqSuiteTR c r to rc suite
  where suite = toSuite oids
{-# INLINE commonReqTR #-}

get :: (MonadIO m, SnmpManager c)
    => c
    -> OIDS
    -> m Suite
get = commonReq (GetRequest 0 0 0)
{-# INLINE get #-}

get' :: (MonadIO m, SnmpManager c)
     => c
     -> Timeout
     -> RetryCount
     -> OIDS
     -> m Suite
get' = commonReqTR (GetRequest 0 0 0)
{-# INLINE get' #-}

getnext :: (MonadIO m, SnmpManager c)
        => c
        -> OIDS
        -> m Suite
getnext = commonReq (GetNextRequest 0 0 0)
{-# INLINE getnext #-}

getnext' :: (MonadIO m, SnmpManager c)
         => c
         -> Timeout
         -> RetryCount
         -> OIDS
         -> m Suite
getnext' = commonReqTR (GetNextRequest 0 0 0)
{-# INLINE getnext' #-}

getbulk :: (MonadIO m, SnmpManager c)
        => c
        -> OIDS
        -> m Suite
getbulk = commonReq (GetBulk 0 0 30)
{-# INLINE getbulk #-}

getbulk' :: (MonadIO m, SnmpManager c)
         => c
         -> Timeout
         -> RetryCount
         -> OIDS
         -> m Suite
getbulk' = commonReqTR (GetBulk 0 0 30)
{-# INLINE getbulk' #-}

set :: (MonadIO m, SnmpManager c)
    => c
    -> Suite
    -> m Suite
set c s = liftIO $ reqSuite c (SetRequest 0 0 0) s
{-# INLINE set #-}

set' :: (MonadIO m, SnmpManager c)
     => c
     -> Timeout
     -> RetryCount
     -> Suite
     -> m Suite
set' c to rc s = liftIO $ reqSuiteTR c (SetRequest 0 0 0) to rc s
{-# INLINE set' #-}
