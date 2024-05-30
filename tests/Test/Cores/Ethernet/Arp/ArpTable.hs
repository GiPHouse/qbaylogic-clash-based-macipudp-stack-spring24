{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cores.Ethernet.Arp.ArpTable where

import Prelude

import Data.List qualified as L

import Clash.Prelude hiding ( repeat )
import Clash.Prelude qualified as C

import Hedgehog

import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

import Protocols
import Protocols.Df qualified as Df

import Clash.Cores.Ethernet.Arp.ArpTable
import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes


createDomain vSystem
  { vName="TestDom10Hz"
  , vPeriod=100_000_000_000
  , vActiveEdge=Rising
  , vResetKind=Asynchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

-- These IP addresses are such that there are no CRC collisions between them
-- This allows us to fill up the entire ARP table if we set the depth to 2,
-- In order to test that all entries expire like they should.
ip1, ip2, ip3, ip4 :: IPv4Address
ip1 = IPv4Address $ 0xDE :> 0xAD :> 0xBE :> 0xEF :> Nil
ip2 = IPv4Address $ 0xDE :> 0xAD :> 0xBE :> 0xEE :> Nil
ip3 = IPv4Address $ 0xDE :> 0xAD :> 0xBE :> 0x00 :> Nil
ip4 = IPv4Address $ 0xDE :> 0xAD :> 0xBE :> 0x11 :> Nil

arpEntry1, arpEntry2, arpEntry3, arpEntry4 :: ArpEntry
arpEntry1 = ArpEntry {
  _arpIP = ip1,
  _arpMac = MacAddress (C.repeat 0x01)
}
arpEntry2 = ArpEntry {
  _arpIP = ip2,
  _arpMac = MacAddress (C.repeat 0x02)
}
arpEntry3 = ArpEntry {
  _arpIP = ip3,
  _arpMac = MacAddress (C.repeat 0x04)
}
arpEntry4 = ArpEntry {
  _arpIP = ip4,
  _arpMac = MacAddress (C.repeat 0x08)
}

-- | Tests proper insertion, lookup and expiration of ARP entries in the ARP table.
prop_arp_table :: Property
prop_arp_table = property $
  do L.map fst (sampleN 32 (bundle bwdOut)) === expectedBwdOut
    where
      fwdIn :: [(Maybe IPv4Address, Df.Data ArpEntry)]
      fwdIn = [ (Nothing, Df.NoData)
              , (Nothing, Df.Data arpEntry1)
              , (Just ip1, Df.Data arpEntry2)
              , (Just ip1, Df.Data arpEntry3)
              , (Just ip1, Df.Data arpEntry4)]
              L.++ L.cycle
              [(Just ip1, Df.NoData)
              , (Just ip2, Df.NoData)
              , (Just ip3, Df.NoData)
              , (Just ip4, Df.NoData)]

      bwdOut :: (Signal TestDom10Hz (Maybe ArpResponse), Signal TestDom10Hz Ack)
      (bwdOut, _) = toSignals ckt (unbundle $ fromList fwdIn, ())
        where
          -- The ARP table holds 4 entries.
          -- ARP entries should expire after 1-2 seconds,
          -- depending on the value of the timer when they are inserted
          ckt = exposeClockResetEnable (arpTable d2 d2) clockGen resetGen enableGen

      expectedBwdOut :: [Maybe ArpResponse]
      expectedBwdOut
        = [ Nothing
          , Nothing
          , Nothing
          , Nothing -- 2 clock cycle delay due to CRC and blockram
          , Just (ArpEntryFound (_arpMac arpEntry1))
          , Just (ArpEntryFound (_arpMac arpEntry1))
          , Just (ArpEntryFound (_arpMac arpEntry1))
          , Just (ArpEntryFound (_arpMac arpEntry1))
          , Just (ArpEntryFound (_arpMac arpEntry2))
          , Just (ArpEntryFound (_arpMac arpEntry3))
          , Just (ArpEntryFound (_arpMac arpEntry4))
          , Just (ArpEntryFound (_arpMac arpEntry1))
          , Nothing -- A second has passed: arp table is now decreasing the timer of entries
          , Nothing
          , Nothing
          , Nothing
          , Nothing -- Clock cycle delay due to state change
          , Just (ArpEntryFound (_arpMac arpEntry3))
          , Just (ArpEntryFound (_arpMac arpEntry4))
          , Just (ArpEntryFound (_arpMac arpEntry1))
          , Just (ArpEntryFound (_arpMac arpEntry2))
          , Just (ArpEntryFound (_arpMac arpEntry3))
          , Nothing -- Another second has passed. After this sweep, all entries should be invalid.
          , Nothing
          , Nothing
          , Nothing
          , Nothing
          , Just ArpEntryNotFound
          , Just ArpEntryNotFound
          , Just ArpEntryNotFound
          , Just ArpEntryNotFound
          , Just ArpEntryNotFound
          ]

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1))
  $(testGroupGenerator)
