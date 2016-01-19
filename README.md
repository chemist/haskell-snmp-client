# Haskell SNMP Client
Haskell Snmp client implementation. Supports v1, v2c and v3 protocol versions.

## Example
```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BasicPrelude

import           Pipes

import           Network.Snmp.Client
import           Network.Snmp.Client.Simple
import           Network.Snmp.Client.Walk.Pipes

host1, host3 :: Hostname
host1 = Hostname "localhost"
host3 = Hostname "localhost"

security3 :: Usm
security3 = Usm { username = "guest"
                , auth = Just (AuthInfo MD5 "myauthpassword")
                , priv = Nothing
                , level = AuthNoPriv
                }

main :: IO ()
main = do
    let conf1 = (initial Version1) { hostname = host1 }
        conf3 = (initial Version3) { hostname = host3
                                   , security = security3
                                   }
        ifTable = fmap oidFromBS ["1.3.6.1.2.1.2.2"]
        sysObjectID = fmap oidFromBS ["1.3.6.1.2.1.1.2.0"]

    -- Make SnmpV1 client.
    cl1 <- client conf1
    -- Make SnmpV3 client.
    cl3 <- client conf3

    print =<< get cl1 sysObjectID
    print =<< getnext cl1 sysObjectID

    print =<< get cl3 sysObjectID
    print =<< getnext cl3 sysObjectID

    Pipes.runEffect $ walk cl1 ifTable >-> forever (await >>= print)
    Pipes.runEffect $ walk cl3 ifTable >-> forever (await >>= print)
```
