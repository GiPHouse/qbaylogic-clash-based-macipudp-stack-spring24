{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.ARP.ArpTable where

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
import Clash.Cores.ARP.ArpTable
import Clash.Cores.ARP.ArpTypes
import Clash.Cores.Ethernet.EthernetTypes


clk :: Clock System
clk = systemClockGen

rst :: Reset System
rst = systemResetGen

en :: Enable System
en = enableGen

ip1 :: IPAddress
ip1 = IPAddress $ 0xDE :> 0xAD :> 0xBE :> 0xEF :> Nil

ip2 :: IPAddress
ip2 = IPAddress $ 0xDE :> 0xAD :> 0xBE :> 0xEE :> Nil

arpEntry1 :: ARPEntry
arpEntry1 = ARPEntry {
  _arpIP = ip1,
  _arpMac = MacAddress $ C.repeat 0xFF
}

arpEntry2 :: ARPEntry
arpEntry2 = ARPEntry {
  _arpIP = ip2,
  _arpMac = MacAddress $ C.repeat 0xAA
}

fwdIn :: [(Maybe IPAddress, Data ARPEntry)]
fwdIn = [(Nothing, NoData),
         (Nothing, Data arpEntry1),
         (Just ip1, NoData),
         (Just ip1, NoData),
         (Just ip1, NoData),
         (Just ip1, NoData),
         (Just ip1, NoData),
         (Just ip1, Data arpEntry2),
         (Just ip2, NoData),
         (Just ip2, Data arpEntry1),
         (Just ip1, NoData),
         (Nothing, NoData)] L.++ L.repeat (Nothing, NoData)

bwdOut :: (Signal System (Maybe ARPResponse), Signal System Ack)
(bwdOut, _) = toSignals ckt (unbundle $ fromList fwdIn, ())
  where
    -- ARP entries expire after 4 clock cycles.
    ckt = exposeClockResetEnable (arpTable d4) clk rst en

expectedBwdOut :: [Maybe ARPResponse]
expectedBwdOut =
  [ Nothing
  , Nothing
  , Just (ARPEntryFound (_arpMac arpEntry1))
  , Just (ARPEntryFound (_arpMac arpEntry1))
  , Just (ARPEntryFound (_arpMac arpEntry1))
  , Just (ARPEntryFound (_arpMac arpEntry1))
  , Just ARPEntryNotFound
  , Just ARPEntryNotFound
  , Just (ARPEntryFound (_arpMac arpEntry2))
  , Just (ARPEntryFound (_arpMac arpEntry2))
  , Just (ARPEntryFound (_arpMac arpEntry1))
  , Nothing]

prop_arp_table :: Property
prop_arp_table = property $
  do L.map fst (sampleN 12 (bundle bwdOut)) === expectedBwdOut

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
