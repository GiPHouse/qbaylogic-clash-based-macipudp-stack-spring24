{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cores.Arp.ArpManager where

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
import Protocols.Df hiding ( fst, snd )

import Clash.Cores.Arp.ArpManager
import Clash.Cores.Arp.ArpTypes
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.IP.IPv4Types


createDomain vSystem
  { vName="TestDom10Hz"
  , vPeriod=100_000_000_000
  , vActiveEdge=Rising
  , vResetKind=Asynchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

ip1, ip2 :: IPv4Address
ip1 = IPv4Address $ 0xDE :> 0xAD :> 0xBE :> 0xEF :> Nil
ip2 = IPv4Address $ 0xDE :> 0xAD :> 0xBE :> 0xEE :> Nil

mac1, mac2 :: MacAddress
mac1 = MacAddress (C.repeat 0x01)
mac2 = MacAddress (C.repeat 0x02)

-- | Test proper outgoing requests, relaying and timeouts of the ARP manager.
--   Manual test, because circuits containing ArpLookup cannot be automatically tested.
prop_arp_manager :: Property
prop_arp_manager = property $
  do L.zip expectedBwdOut expectedFwdOut === sampleN 32 (bundle (bwdOut, bundle fwdOut))
    where
      fwdIn :: [Maybe IPv4Address]
      fwdIn = [ Nothing
              , Nothing
              , Just ip1
              , Just ip1
              , Nothing
              , Just ip1
              , Just ip1
              , Just ip1
              , Just ip1
              ] L.++ L.repeat (Just ip2)

      bwdIn :: [(Maybe ArpResponse, Ack)]
      bwdIn = L.map (, Ack True) $
              [ Nothing
              , Nothing
              , Nothing
              , Just (ArpEntryFound mac1)
              , Nothing
              , Nothing
              , Just ArpEntryNotFound
              , Nothing
              , Just (ArpEntryFound mac1)
              , Nothing
              , Just ArpEntryNotFound]
              L.++ L.replicate 20 Nothing
              L.++ [Just ArpEntryNotFound]

      bwdOut :: Signal TestDom10Hz (Maybe ArpResponse)
      fwdOut :: (Signal TestDom10Hz (Maybe IPv4Address), Signal TestDom10Hz (Data ArpLite))
      (bwdOut, fwdOut) = toSignals ckt inp
        where
          -- We wait 1-2 seconds for ARP replies
          ckt = exposeClockResetEnable (arpManagerC @TestDom10Hz d2) clockGen resetGen enableGen
          inp = (unbundle $ fromList fwdIn, unbundle $ fromList bwdIn)

      expectedBwdOut :: [Maybe ArpResponse]
      expectedBwdOut
        = [ Nothing
          , Nothing
          , Nothing
          , Just (ArpEntryFound mac1) -- Relaying to client circuit from ARP table
          , Nothing
          , Nothing
          , Nothing
          , Nothing
          , Just (ArpEntryFound mac1)] -- Relaying to client circuit from ARP reply
          L.++ L.replicate 22 Nothing
          L.++
          [Just ArpEntryNotFound] -- We were waiting for an ARP reply, but we timed out.

      expectedFwdOut :: [(Maybe IPv4Address, Data ArpLite)]
      expectedFwdOut
        = [ (Nothing, NoData)
          , (Nothing, NoData)
          , (Just ip1, NoData) -- Relay IP lookup to ARP table
          , (Just ip1, NoData)
          , (Nothing, NoData)
          , (Just ip1, NoData)
          , (Just ip1, Data (ArpLite broadcastMac ip1 True)) -- We received ArpEntryNotFound from the ARP table, so send request
          , (Just ip1, NoData)
          , (Just ip1, NoData) -- Received ArpEntryFound
          , (Just ip2, NoData)
          , (Just ip2, Data (ArpLite broadcastMac ip2 True))] -- We received ArpEntryNotFound from the ARP table, so send request
          L.++ L.replicate 21 (Just ip2, NoData) -- We were waiting for an ARP reply, but we timed out.

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1))
  $(testGroupGenerator)
