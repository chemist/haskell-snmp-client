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
  , getTR
  , getnext
  , getnextTR
  , getbulk
  , getbulkTR
  , set
  , setTR
  ) where

import           BasicPrelude
import           Network.Snmp.Client.Types

get :: (MonadIO m, SnmpManager c)
    => c
    -> OIDS
    -> m Suite
get = commonReq (GetRequest 0 0 0)
{-# INLINE get #-}

getTR :: (MonadIO m, SnmpManager c)
     => c
     -> Timeout
     -> RetryCount
     -> OIDS
     -> m Suite
getTR = commonReqTR (GetRequest 0 0 0)
{-# INLINE getTR #-}

getnext :: (MonadIO m, SnmpManager c)
        => c
        -> OIDS
        -> m Suite
getnext = commonReq (GetNextRequest 0 0 0)
{-# INLINE getnext #-}

getnextTR :: (MonadIO m, SnmpManager c)
         => c
         -> Timeout
         -> RetryCount
         -> OIDS
         -> m Suite
getnextTR = commonReqTR (GetNextRequest 0 0 0)
{-# INLINE getnextTR #-}

getbulk :: (MonadIO m, SnmpManager c)
        => c
        -> ErrorStatus  -- NonRepeaters
        -> ErrorIndex  -- MaxRepetitions
        -> OIDS
        -> m Suite
getbulk c nr mr = commonReq (GetBulk 0 nr mr) c
{-# INLINE getbulk #-}

getbulkTR :: (MonadIO m, SnmpManager c)
         => c
         -> ErrorStatus  -- NonRepeaters
         -> ErrorIndex  -- MaxRepetitions
         -> Timeout
         -> RetryCount
         -> OIDS
         -> m Suite
getbulkTR c nr mr = commonReqTR (GetBulk 0 nr mr) c
{-# INLINE getbulkTR #-}

set :: (MonadIO m, SnmpManager c)
    => c
    -> Suite
    -> m Suite
set c s = liftIO $ reqSuite c (SetRequest 0 0 0) s
{-# INLINE set #-}

setTR :: (MonadIO m, SnmpManager c)
     => c
     -> Timeout
     -> RetryCount
     -> Suite
     -> m Suite
setTR c to rc s = liftIO $ reqSuiteTR c (SetRequest 0 0 0) to rc s
{-# INLINE setTR #-}

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

