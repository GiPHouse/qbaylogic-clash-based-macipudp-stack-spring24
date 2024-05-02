{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Arp.ArpTable where

-- base
import Prelude

import Data.List qualified as L

-- clash-prelude
import Clash.Prelude hiding ( repeat )
import Clash.Prelude qualified as C

-- hedgehog
import Hedgehog

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

-- clash-protocols
import Protocols
import Protocols.Df hiding ( fst, snd )

-- Me
import Clash.Cores.Arp.ArpTable
import Clash.Cores.Arp.ArpTypes
import Clash.Cores.Ethernet.EthernetTypes


ip1, ip2 :: IPAddress
ip1 = IPAddress $ 0xDE :> 0xAD :> 0xBE :> 0xEF :> Nil
ip2 = IPAddress $ 0xDE :> 0xAD :> 0xBE :> 0xEE :> Nil

arpEntry1, arpEntry2 :: ArpEntry
arpEntry1 = ArpEntry {
  _arpIP = ip1,
  _arpMac = MacAddress $ C.repeat 0xFF
}
arpEntry2 = ArpEntry {
  _arpIP = ip2,
  _arpMac = MacAddress $ C.repeat 0xAA
}

-- TODO test takes way too long.
prop_arp_table_expiration :: Property
prop_arp_table_expiration = property $
  do L.map fst (sample_lazy (bundle bwdOut)) L.!! (natToNum @(10^12 `Div` DomainPeriod System + 2)) === Just (ArpEntryFound (_arpMac arpEntry2))
     L.map fst (sample_lazy (bundle bwdOut)) L.!! (natToNum @(10^12 `Div` DomainPeriod System + 3)) === Just ArpEntryNotFound
    where
      fwdIn :: [(Maybe IPAddress, Data ArpEntry)]
      fwdIn = [(Nothing, NoData),
              (Nothing, Data arpEntry2)] L.++ L.repeat (Just ip2, NoData)

      bwdOut :: (Signal System (Maybe ArpResponse), Signal System Ack)
      (bwdOut, _) = toSignals ckt (unbundle $ fromList fwdIn, ())
        where
          -- ARP entries expire after 1 second.
          ckt = exposeClockResetEnable (arpTable d1) systemClockGen systemResetGen enableGen

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1))
  $(testGroupGenerator)
