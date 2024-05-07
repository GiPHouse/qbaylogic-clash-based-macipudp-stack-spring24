{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Arp.ArpTransmitter where

-- base
import Prelude

-- clash-prelude
import Clash.Prelude hiding ( concat )
import Clash.Prelude qualified as C

-- hedgehog
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

-- clash-protocols
import Protocols.Hedgehog

-- Me
import Clash.Cores.Ethernet.PacketStream

import Clash.Cores.Arp.ArpTransmitter
import Clash.Cores.Arp.ArpTypes
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.IP.IPv4Types

import Test.Cores.Ethernet.Packetizer


genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

arpTransmitterPropertyGenerator
  :: forall (dataWidth :: Nat) .
  ( 1 <= dataWidth)
  => SNat dataWidth
  -> Property
arpTransmitterPropertyGenerator SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (Gen.list (Range.linear 1 100) genIPAddress)
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System (arpTransmitter (pure myMac) (pure myIP)))
    (===)
    where
      genIPAddress = IPAddress <$> genVec Gen.enumBounded

      model :: [IPAddress] -> [PacketStreamM2S dataWidth EthernetHeader]
      model = packetizeFromDfModel toEthernet toArp

      myMac = MacAddress (0xDE :> 0xAD :> 0xBE :> 0xEF :> 0x01 :> 0x02 :> Nil)
      myIP = IPAddress (0x33 :> 0x44 :> 0x55 :> 0x66 :> Nil)
      toEthernet _ = EthernetHeader {
        _macDst = broadcastMac,
        _macSrc = myMac,
        _etherType = arpEtherType
      }
      toArp tpa = newArpRequest ((myMac, myIP), tpa)


-- | n mod dataWidth ~ 1
prop_arp_transmitter_d1 :: Property
prop_arp_transmitter_d1 = arpTransmitterPropertyGenerator d1

-- | n mod dataWidth ~ 3
prop_arp_transmitter_d5 :: Property
prop_arp_transmitter_d5 = arpTransmitterPropertyGenerator d5

-- | n mod dataWidth ~ 0
prop_arp_transmitter_d7 :: Property
prop_arp_transmitter_d7 = arpTransmitterPropertyGenerator d7

-- | dataWidth < header byte size
prop_arp_transmitter_d11 :: Property
prop_arp_transmitter_d11 = arpTransmitterPropertyGenerator d11

-- | dataWidth ~ header byte size
prop_arp_transmitter_d28 :: Property
prop_arp_transmitter_d28 = arpTransmitterPropertyGenerator d28

-- | dataWidth > header byte size
prop_arp_transmitter_d29 :: Property
prop_arp_transmitter_d29 = arpTransmitterPropertyGenerator d29

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
