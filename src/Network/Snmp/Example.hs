{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Snmp.Example (
-- * Usage
-- ** Imports
-- $imports

-- ** Create config
-- $config

-- ** Describe oids
-- $oids

-- ** Describe requests
-- $request

-- ** Do IO
-- $send
) where

import           BasicPrelude

import           Network.Snmp.Client
import           Network.Snmp.Client.Simple
import           Network.Snmp.Client.Walk.Pipes

import           Pipes                          (await, (>->))
import qualified Pipes

{- $extensions
> {-# LANGUAGE OverloadedStrings #-}
-}

{- $imports
> -- imports here
> import Data.ByteString (ByteString)
> import Control.Exception (bracket, try)
>
> import Network.Snmp.Client
> import Network.Snmp.Client.Simple
-}

{- $config
> -- First you must create config
> -- For SNMPv1
> conf1 :: Config
> conf1 = (initial Version1) { hostname = Hostname "salt"
>                            , community = Community "helloall"
>                            }
> -- For SNMPv2
> conf2 :: Config
> conf2 = (initial Version2) { hostname = Hostname "salt"
>                            , community = Community "helloall"
>                            }
>
> -- For SNMPv3
> conf3 :: Config
> conf3 = (initial Version3) { hostname = Hostname "salt"
>                            , security = Usm { username = "salt"
>                                             , auth = Just (AuthInfo MD5 "hellohello")
>                                             , priv = Just (PrivInfo AES "hellohello")
>                                             , level = AuthPriv
>                                             }
>                            }
>
-}
-- For SNMPv1
conf1 :: Config
conf1 = (initial Version1) { hostname = Hostname "salt"
                           , community = Community "helloall"
                           }
-- For SNMPv2
conf2 :: Config
conf2 = (initial Version2) { hostname = Hostname "salt"
                           , community = Community "helloall"
                           }

-- For SNMPv3
conf3 :: Config
conf3 = (initial Version3) { hostname = Hostname "salt"
                           , security = Usm { username = "salt"
                                            , auth = Just (AuthInfo MD5 "hellohello")
                                            , priv = Just (PrivInfo AES "hellohello")
                                            , level = AuthPriv
                                            }
                           }


{- $send
> -- do io
> client3 :: IO ()
> client3 = bracket (client conf3)
>                   print
>                   requests
>
> client2 :: IO ()
> client2 = bracket (client conf2)
>                   print
>                   requests
>
> client1 :: IO ()
> client1 = bracket (client conf1)
>                   print
>                   requests
-}
client3 :: IO ()
client3 = bracket (client conf3)
                  print
                  requests

client2 :: IO ()
client2 = bracket (client conf2)
                  print
                  requests

client1 :: IO ()
client1 = bracket (client conf1)
                  print
                  requests

{- $oids
> -- Describe oids which you need
> root, eth0, tabl, ipAddr, zeroDotZero :: [Integer]
> root = [1,3,6,1,2,1,2,2,1,2]
> eth0 = [1,3,6,1,2,1,2,2,1,2,1]
> tabl = [31,1,1,1,1]
> ipAddr = [1,3,6,1,2,1,4,22,1,3,3,192,168,3,1]
> zeroDotZero = [1,3,6,1,2,1,2,2,1,22,20]
>
> oi, sysUptime, memory, sysContact, bad, testOid :: ByteString
> oi = ".1.3.6.1.2.1.1.9.1.2.1"
> sysUptime = "1.3.6.1.2.1.25.1.1.0"
> memory = "1.3.6.1.2.1.25.2"
> sysContact = "1.3.6.1.2.1.1.4.0"
> bad = "1.4.6.1.2.1.1.4"
> testOid = "1.3.6.1.2.1.25.1.1.0"
-}
root, eth0, tabl, ipAddr, zeroDotZero :: [Integer]
root = [1,3,6,1,2,1,2,2,1,2]
eth0 = [1,3,6,1,2,1,2,2,1,2,1]
tabl = [31,1,1,1,1]
ipAddr = [1,3,6,1,2,1,4,22,1,3,3,192,168,3,1]
zeroDotZero = [1,3,6,1,2,1,2,2,1,22,20]

oi, sysUptime, memory, sysContact, bad, testOid, inet :: ByteString
oi = ".1.3.6.1.2.1.1.9.1.2.1"
sysUptime = "1.3.6.1.2.1.25.1.1.0"
memory = "1.3.6.1.2.1.25.2"
sysContact = "1.3.6.1.2.1.1.4.0"
bad = "1.4.6.1.2.1.1.4"
testOid = "1.3.6.1.2.1.25.1.1.0"
inet = ".1.3.6.1"

{- $request
> requests :: Client -> IO ()
> requests snmp = do
>     print "get request"
>     putStr . show =<< get snmp [oidFromBS testOid]
>     putStr . show =<< get snmp [oidFromBS sysUptime, oidFromBS oi, zeroDotZero]
>     print "getbulk request"
>     putStr . show =<< getbulk snmp [oidFromBS sysUptime]
>     print "getnext request"
>     putStr . show =<< getnext snmp [oidFromBS sysUptime]
>     print "walk memory"
>     Pipes.runEffect $ walk snmp [oidFromBS memory] >-> forever (await >>= print)
>     print "bulkwalk memory"
>     Pipes.runEffect $ bulkwalk snmp [oidFromBS memory] >-> forever (await >>= print)
>     print "get sysContact"
>     putStr . show =<< get snmp [oidFromBS sysContact]
>     print "set sysContact"
>     putStr . show =<< (try $ set snmp (Suite [Coupla (oidFromBS sysContact) (String "hello all")]) :: IO (Either SomeClientException Suite))
>     print "get sysContact"
>     putStr . show =<< get snmp [oidFromBS sysContact]
>
> main :: IO ()
> main = do
>     client1
>     client2
>     client3
-}
requests :: Client -> IO ()
requests snmp = do
    print "get request"
    putStr . show =<< get snmp [oidFromBS testOid]
    putStr . show =<< get snmp [oidFromBS sysUptime, oidFromBS oi, zeroDotZero]
    print "getbulk request"
    putStr . show =<< getbulk snmp [oidFromBS sysUptime]
    print "getnext request"
    putStr . show =<< getnext snmp [oidFromBS sysUptime]
    print "walk memory"
    Pipes.runEffect $ walk snmp [oidFromBS memory] >-> forever (await >>= print)
    print "bulkwalk memory"
    Pipes.runEffect $ bulkwalk snmp [oidFromBS memory] >-> forever (await >>= print)
    print "get sysContact"
    putStr . show =<< get snmp [oidFromBS sysContact]
    print "set sysContact"
    putStr . show =<< (try $ set snmp (Suite [Coupla (oidFromBS sysContact) (String "hello all")]) :: IO (Either SomeClientException Suite))
    print "get sysContact"
    putStr . show =<< get snmp [oidFromBS sysContact]

main :: IO ()
main = do
    client1
    client2
    client3

