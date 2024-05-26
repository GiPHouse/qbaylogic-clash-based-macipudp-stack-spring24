{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Arp.ArpReceiver where

import Clash.Prelude

import Data.List qualified as L

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

-- arp
import Clash.Cores.Arp.ArpReceiver
import Clash.Cores.Arp.ArpTypes

-- ethernet
import Clash.Cores.Ethernet.DepacketizeToDf ( depacketizeToDfC )
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.PacketStream
import Test.Cores.Ethernet.Depacketizer
import Test.Cores.Ethernet.Packetizer
import Test.Cores.Ethernet.Util ( fullPackets )

-- ip
import Clash.Cores.IP.IPv4Types

-- protocols hedgehog
import Protocols.Hedgehog


ourMac :: MacAddress
ourMac = MacAddress (0xDE :> 0xAD :> 0xBE :> 0xEF :> 0x01 :> 0x02 :> Nil)

ourIP :: IPv4Address
ourIP = IPv4Address (0x33 :> 0x44 :> 0x55 :> 0x66 :> Nil)

-- | generates 1/4 well-formed ARP requests intended for us,
-- 1/4 well-formed ARP requests intended for us but aborted,
-- 1/2 non-ARP request packet streams (with varying length and content).
-- All generated streams consist of full packets (i.e. end with a fragment with _last == True).
genPackets :: forall
    (dataWidth :: Nat)
  .  KnownNat dataWidth
  => 1 <= dataWidth
  => Gen [PacketStreamM2S dataWidth EthernetHeader]
genPackets = do
  arpPkt <- genArpPacket
  abort <- Gen.bool
  pickRandomPacket <- Gen.bool
  if pickRandomPacket
  then
    fmap fullPackets (Gen.list (Range.linear 1 100) genPackets')
  else
    let stream = packetizeFromDfModel toEthernetHdr id [arpPkt] :: [PacketStreamM2S dataWidth EthernetHeader]
    in pure $ L.init stream L.++ [(L.last stream){_abort = abort}]

  where
    genArpPacket =
      newArpPacket <$>
      (MacAddress <$> genVec Gen.enumBounded) <*>
      (IPv4Address <$> genVec Gen.enumBounded) <*>
      Gen.constant ourMac <*>
      Gen.constant ourIP <*>
      Gen.bool

    genVec gen = sequence (repeat gen)

    genPackets' =
        PacketStreamM2S <$>
        genVec Gen.enumBounded <*>
        Gen.maybe Gen.enumBounded <*>
        genEthernetHdr <*>
        Gen.enumBounded

    genEthernetHdr :: Gen EthernetHeader
    genEthernetHdr =
        EthernetHeader <$>
        Gen.constant ourMac <*>
        (MacAddress <$> genVec Gen.enumBounded) <*>
        Gen.constant arpEtherType

    toEthernetHdr ArpPacket{..} = EthernetHeader {
        _macDst = _tha,
        _macSrc = _sha,
        _etherType = arpEtherType
      }

arpDepacketizerPropertyGenerator
  :: forall (dataWidth :: Nat)
   . 1 <= dataWidth
  => SNat dataWidth
  -> Property
arpDepacketizerPropertyGenerator SNat =
  propWithModelSingleDomain
    @System
    defExpectOptions
    genPackets
    (exposeClockResetEnable model)
    (exposeClockResetEnable @System $ depacketizeToDfC const)
    (===)
    where
      model :: [PacketStreamM2S dataWidth EthernetHeader] -> [ArpPacket]
      model = depacketizeToDfModel const

arpReceiverPropertyGenerator
  :: forall (dataWidth :: Nat)
   . 1 <= dataWidth
  => SNat dataWidth
  -> Property
arpReceiverPropertyGenerator SNat =
  propWithModelSingleDomain
    @System
    defExpectOptions
    genPackets
    (exposeClockResetEnable model)
    (exposeClockResetEnable @System $ arpReceiverC $ pure ourIP)
    (===)
    where
      model :: [PacketStreamM2S dataWidth EthernetHeader] -> ([ArpEntry],[ArpLite])
      model ethStr = (entries, lites)
        where
          arpDf = L.filter (validArp ourIP) (depacketizeToDfModel const ethStr)
          (arpRequests, arpEntries) = L.partition (isRequest ourIP)  arpDf

          validArp ip ArpPacket{..} =
                _htype == 1
                && _ptype == 0x0800
                && _hlen  == 6
                && _plen  == 4
                &&(_oper == 1 && (_tpa == ip || _tpa == _spa) || _oper == 2)

          isRequest ip ArpPacket{..} = _oper == 1 && _tpa == ip

          entries = (\p -> ArpEntry (_sha p) (_spa p)) <$> arpEntries
          lites = (\p -> ArpLite (_sha p) (_spa p) False) <$> arpRequests

-- | headerBytes < dataWidth
prop_arp_depacketizer_d3:: Property
prop_arp_depacketizer_d3 = arpDepacketizerPropertyGenerator d3

-- | headerBytes mod dataWidth ~ 0
prop_arp_depacketizer_d7:: Property
prop_arp_depacketizer_d7 = arpDepacketizerPropertyGenerator d7

-- | headerBytes ~ 11
prop_arp_depacketizer_d11:: Property
prop_arp_depacketizer_d11 = arpDepacketizerPropertyGenerator d11

-- | headerBytes ~ datWidth - 1
prop_arp_depacketizer_d27:: Property
prop_arp_depacketizer_d27 = arpDepacketizerPropertyGenerator d27

-- | headerBytes ~ dataWidth
prop_arp_depacketizer_d28:: Property
prop_arp_depacketizer_d28 = arpDepacketizerPropertyGenerator d28

-- | headerBytes mod dataWidth ~ 0
prop_arp_receiver_d1 :: Property
prop_arp_receiver_d1 = arpReceiverPropertyGenerator d1

-- | dataWidth < headerBytes
prop_arp_receiver_d9 :: Property
prop_arp_receiver_d9 = arpReceiverPropertyGenerator d9

-- | dataWidth ~ 11
prop_arp_receiver_d11 :: Property
prop_arp_receiver_d11 = arpReceiverPropertyGenerator d11

-- | dataWidth mod 2 ~ 0
prop_arp_receiver_d12 :: Property
prop_arp_receiver_d12 = arpReceiverPropertyGenerator d12

-- | dataWidth ~ prime
prop_arp_receiver_d17 :: Property
prop_arp_receiver_d17 = arpReceiverPropertyGenerator d17

-- | dataWidth ~ headerBytes
prop_arp_receiver_d28 :: Property
prop_arp_receiver_d28 = arpReceiverPropertyGenerator d28

-- | dataWidth > headerBytes
prop_arp_receiver_d29 :: Property
prop_arp_receiver_d29 = arpReceiverPropertyGenerator d29

tests :: TestTree
tests =
    localOption (mkTimeout 10_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
