{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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


createDomain vSystem
  { vName="TestDom10Hz"
  , vPeriod=100_000_000_000
  , vActiveEdge=Rising
  , vResetKind=Asynchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

ip1, ip2 :: IPAddress
ip1 = IPAddress $ 0xDE :> 0xAD :> 0xBE :> 0xEF :> Nil
ip2 = IPAddress $ 0xDE :> 0xAD :> 0xBE :> 0xEE :> Nil

arpEntry1, arpEntry2 :: ArpEntry
arpEntry1 = ArpEntry {
  _arpIP = ip1,
  _arpMac = MacAddress (C.repeat 0xFF)
}
arpEntry2 = ArpEntry {
  _arpIP = ip2,
  _arpMac = MacAddress (C.repeat 0xAA)
}

-- | Tests proper expiration and overwriting of ARP entries in the table.
prop_arp_table :: Property
prop_arp_table = property $
  do L.map fst (sampleN 14 (bundle bwdOut)) === expectedBwdOut
    where
      fwdIn :: [(Maybe IPAddress, Data ArpEntry)]
      fwdIn = [ (Nothing, NoData)
              , (Nothing, Data arpEntry2)
              , (Just ip2, Data arpEntry1)]
              L.++ L.repeat (Just ip1, NoData)

      bwdOut :: (Signal TestDom10Hz (Maybe ArpResponse), Signal TestDom10Hz Ack)
      (bwdOut, _) = toSignals ckt (unbundle $ fromList fwdIn, ())
        where
          -- ARP entries expire after 1 second, so after 10 clock cycles for a 10 Hz clock.
          ckt = exposeClockResetEnable (arpTable d1) clockGen resetGen enableGen

      expectedBwdOut :: [Maybe ArpResponse]
      expectedBwdOut = [ Nothing
                       , Nothing
                       , Just (ArpEntryFound (_arpMac arpEntry2))]
                       L.++ L.replicate (natToNum @(10^12 `Div` DomainPeriod TestDom10Hz))
                                        (Just (ArpEntryFound (_arpMac arpEntry1)))
        L.++ [Just ArpEntryNotFound]

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1))
  $(testGroupGenerator)
