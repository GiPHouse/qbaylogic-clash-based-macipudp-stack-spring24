{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.Arp where

import Prelude

import Clash.Prelude qualified as C

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.Packetizers ( depacketizeToDfC )
import Protocols.Hedgehog

import Clash.Cores.Ethernet.Arp ( arpReceiverC, arpTransmitter )
import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes

import Test.Protocols.Extra.PacketStream.Extra ( fullPackets )
import Test.Protocols.Extra.PacketStream.Packetizers

import Data.List qualified as L


ourMac :: MacAddress
ourMac = MacAddress (0xDE C.:> 0xAD C.:> 0xBE C.:> 0xEF C.:> 0x01 C.:> 0x02 C.:> C.Nil)

ourIP :: IPv4Address
ourIP = IPv4Address (0x33 C.:> 0x44 C.:> 0x55 C.:> 0x66 C.:> C.Nil)

genVec :: (C.KnownNat n, 1 C.<= n) => Gen a -> Gen (C.Vec n a)
genVec gen = C.sequence (C.repeat gen)

genArpLite :: Gen ArpLite
genArpLite = ArpLite <$>
          (MacAddress <$> genVec Gen.enumBounded) <*>
          (IPv4Address <$> genVec Gen.enumBounded) <*>
          Gen.enumBounded

arpTransmitterPropertyGenerator
  :: forall (dataWidth :: C.Nat)
   . 1 C.<= dataWidth
  => C.SNat dataWidth
  -> Property
arpTransmitterPropertyGenerator C.SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (Gen.list (Range.linear 1 100) genArpLite)
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System (arpTransmitter (pure ourMac) (pure ourIP)))
    (===)
    where
      model :: [ArpLite] -> [PacketStreamM2S dataWidth EthernetHeader]
      model = packetizeFromDfModel toEthernetHdr toArpPkt

      toEthernetHdr arpLite = EthernetHeader {
        _macDst = _targetMac arpLite,
        _macSrc = ourMac,
        _etherType = arpEtherType
      }

      toArpPkt arpLite = newArpPacket ourMac ourIP (_targetMac arpLite) (_targetIPv4 arpLite) (_isRequest arpLite)

-- | headerBytes mod dataWidth ~ 0
prop_arp_transmitter_d1 :: Property
prop_arp_transmitter_d1 = arpTransmitterPropertyGenerator C.d1

-- | dataWidth < headerBytes
prop_arp_transmitter_d15 :: Property
prop_arp_transmitter_d15 = arpTransmitterPropertyGenerator C.d11

-- | dataWidth ~ headerBytes
prop_arp_transmitter_d28 :: Property
prop_arp_transmitter_d28 = arpTransmitterPropertyGenerator C.d28

-- | dataWidth > headerBytes
prop_arp_transmitter_d29 :: Property
prop_arp_transmitter_d29 = arpTransmitterPropertyGenerator C.d29

testsTransmitter :: TestTree
testsTransmitter =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)

-- | generates 1/4 well-formed ARP requests intended for us,
-- 1/4 well-formed ARP requests intended for us but aborted,
-- 1/2 non-ARP request packet streams (with varying length and content).
-- All generated streams consist of full packets (i.e. end with a fragment with _last == True).
genPackets :: forall
    (dataWidth :: C.Nat)
  .  C.KnownNat dataWidth
  => 1 C.<= dataWidth
  => Gen [PacketStreamM2S dataWidth EthernetHeader]
genPackets = do
  arpPkt <- genArpPacket
  abort <- Gen.bool
  pickRandomPacket <- Gen.bool
  if pickRandomPacket
  then
    C.fmap fullPackets (Gen.list (Range.linear 1 100) genPackets')
  else
    let stream = packetizeFromDfModel toEthernetHdr C.id [arpPkt] :: [PacketStreamM2S dataWidth EthernetHeader]
    in C.pure C.$ L.init stream L.++ [(L.last stream){_abort = abort}]

  where
    genArpPacket =
      newArpPacket C.<$>
      (MacAddress C.<$> genVec Gen.enumBounded) C.<*>
      (IPv4Address C.<$> genVec Gen.enumBounded) C.<*>
      Gen.constant ourMac C.<*>
      Gen.constant ourIP C.<*>
      Gen.bool

    genPackets' =
        PacketStreamM2S C.<$>
        genVec Gen.enumBounded C.<*>
        Gen.maybe Gen.enumBounded C.<*>
        genEthernetHdr C.<*>
        Gen.enumBounded

    genEthernetHdr :: Gen EthernetHeader
    genEthernetHdr =
        EthernetHeader C.<$>
        Gen.constant ourMac C.<*>
        (MacAddress C.<$> genVec Gen.enumBounded) C.<*>
        Gen.constant arpEtherType

    toEthernetHdr ArpPacket{..} = EthernetHeader {
        _macDst = _tha,
        _macSrc = _sha,
        _etherType = arpEtherType
      }

arpDepacketizerPropertyGenerator
  :: forall (dataWidth :: C.Nat)
   . 1 C.<= dataWidth
  => C.SNat dataWidth
  -> Property
arpDepacketizerPropertyGenerator C.SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    genPackets
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System C.$ depacketizeToDfC C.const)
    (===)
    where
      model :: [PacketStreamM2S dataWidth EthernetHeader] -> [ArpPacket]
      model = depacketizeToDfModel C.const

arpReceiverPropertyGenerator
  :: forall (dataWidth :: C.Nat)
   . 1 C.<= dataWidth
  => C.SNat dataWidth
  -> Property
arpReceiverPropertyGenerator C.SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    genPackets
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System C.$ arpReceiverC C.$ C.pure ourIP)
    (===)
    where
      model :: [PacketStreamM2S dataWidth EthernetHeader] -> ([ArpEntry],[ArpLite])
      model ethStr = (entries, lites)
        where
          arpDf = L.filter (validArp ourIP) (depacketizeToDfModel C.const ethStr)
          (arpRequests, arpEntries) = L.partition (isRequest ourIP)  arpDf

          validArp ip ArpPacket{..} =
                _htype C.== 1
                C.&& _ptype C.== 0x0800
                C.&& _hlen  C.== 6
                C.&& _plen  C.== 4
                C.&&(_oper C.== 1 C.&& (_tpa C.== ip C.|| _tpa C.== _spa) C.|| _oper C.== 2)

          isRequest ip ArpPacket{..} = _oper C.== 1 C.&& _tpa C.== ip

          entries = (\p -> ArpEntry (_sha p) (_spa p)) C.<$> arpEntries
          lites = (\p -> ArpLite (_sha p) (_spa p) C.False) C.<$> arpRequests

-- | headerBytes < dataWidth
prop_arp_depacketizer_d3:: Property
prop_arp_depacketizer_d3 = arpDepacketizerPropertyGenerator C.d3

-- | headerBytes mod dataWidth ~ 0
prop_arp_depacketizer_d7:: Property
prop_arp_depacketizer_d7 = arpDepacketizerPropertyGenerator C.d7

-- | headerBytes ~ 11
prop_arp_depacketizer_d11:: Property
prop_arp_depacketizer_d11 = arpDepacketizerPropertyGenerator C.d11

-- | headerBytes ~ datWidth - 1
prop_arp_depacketizer_d27:: Property
prop_arp_depacketizer_d27 = arpDepacketizerPropertyGenerator C.d27

-- | headerBytes ~ dataWidth
prop_arp_depacketizer_d28:: Property
prop_arp_depacketizer_d28 = arpDepacketizerPropertyGenerator C.d28

-- | headerBytes mod dataWidth ~ 0
prop_arp_receiver_d1 :: Property
prop_arp_receiver_d1 = arpReceiverPropertyGenerator C.d1

-- | dataWidth < headerBytes
prop_arp_receiver_d9 :: Property
prop_arp_receiver_d9 = arpReceiverPropertyGenerator C.d9

-- | dataWidth ~ 11
prop_arp_receiver_d11 :: Property
prop_arp_receiver_d11 = arpReceiverPropertyGenerator C.d11

-- | dataWidth mod 2 ~ 0
prop_arp_receiver_d12 :: Property
prop_arp_receiver_d12 = arpReceiverPropertyGenerator C.d12

-- | dataWidth ~ prime
prop_arp_receiver_d17 :: Property
prop_arp_receiver_d17 = arpReceiverPropertyGenerator C.d17

-- | dataWidth ~ headerBytes
prop_arp_receiver_d28 :: Property
prop_arp_receiver_d28 = arpReceiverPropertyGenerator C.d28

-- | dataWidth > headerBytes
prop_arp_receiver_d29 :: Property
prop_arp_receiver_d29 = arpReceiverPropertyGenerator C.d29

testsReceiver :: TestTree
testsReceiver =
    localOption (mkTimeout 10_000_000 {- 12 seconds -})
  C.$ localOption (HedgehogTestLimit (C.Just 1_000))
  $(testGroupGenerator)
