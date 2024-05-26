{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Arp.ArpTransmitter where

import Prelude

import Clash.Prelude
import Clash.Prelude qualified as C

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

import Protocols.Hedgehog

import Clash.Cores.Ethernet.Arp ( arpTransmitter )
import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes
import Protocols.Extra.PacketStream

import Test.Cores.Ethernet.Packetizer


genVec :: (KnownNat n, 1 <= n) => Gen a -> Gen (Vec n a)
genVec gen = sequence (C.repeat gen)

genArpLite :: Gen ArpLite
genArpLite = ArpLite <$>
          (MacAddress <$> genVec Gen.enumBounded) <*>
          (IPv4Address <$> genVec Gen.enumBounded) <*>
          Gen.enumBounded

arpTransmitterPropertyGenerator
  :: forall (dataWidth :: Nat)
   . 1 <= dataWidth
  => SNat dataWidth
  -> Property
arpTransmitterPropertyGenerator SNat =
  propWithModelSingleDomain
    @System
    defExpectOptions
    (Gen.list (Range.linear 1 100) genArpLite)
    (exposeClockResetEnable model)
    (exposeClockResetEnable @System (arpTransmitter (pure ourMac) (pure ourIP)))
    (===)
    where
      ourMac = MacAddress (0xDE :> 0xAD :> 0xBE :> 0xEF :> 0x01 :> 0x02 :> Nil)
      ourIP = IPv4Address (0x33 :> 0x44 :> 0x55 :> 0x66 :> Nil)

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
prop_arp_transmitter_d1 = arpTransmitterPropertyGenerator d1

-- | dataWidth < headerBytes
prop_arp_transmitter_d15 :: Property
prop_arp_transmitter_d15 = arpTransmitterPropertyGenerator d11

-- | dataWidth ~ headerBytes
prop_arp_transmitter_d28 :: Property
prop_arp_transmitter_d28 = arpTransmitterPropertyGenerator d28

-- | dataWidth > headerBytes
prop_arp_transmitter_d29 :: Property
prop_arp_transmitter_d29 = arpTransmitterPropertyGenerator d29

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
