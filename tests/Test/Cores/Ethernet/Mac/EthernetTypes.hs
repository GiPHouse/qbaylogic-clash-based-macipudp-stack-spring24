{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}

module Test.Cores.Ethernet.Mac.EthernetTypes where

-- base
import Prelude

-- clash-prelude
import Clash.Prelude ( type (<=) )
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
import Protocols
import Protocols.Hedgehog

-- util module
import Test.Cores.Ethernet.Util

-- ethernet modules
import Clash.Cores.Ethernet.IP.IPv4Types ( IPv4Address(..) )
import Clash.Cores.Ethernet.Mac.EthernetTypes
import Protocols.Extra.PacketStream


-- | Test the IPv4Address to EthernetHeader converter.
toEthernetTest
  :: forall (dataWidth :: C.Nat).
  (
    1 <= dataWidth
    )
  => C.SNat dataWidth
  -> MacAddress
  -> Property
toEthernetTest C.SNat macSrc =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets (Gen.list (Range.linear 1 100) (genPackets genIpAddr)))
    (C.exposeClockResetEnable (model macSrc))
    (C.exposeClockResetEnable @C.System ckt)
    (===)
  where
    ckt :: C.HiddenClockResetEnable C.System
      => Circuit (PacketStream C.System dataWidth IPv4Address) (PacketStream C.System dataWidth EthernetHeader)
    ckt = toEthernetC (C.fromList (repeat macSrc))

    model :: MacAddress -> [PacketStreamM2S dataWidth IPv4Address] -> [PacketStreamM2S dataWidth EthernetHeader]
    model src = map $ fmap (toEthernet src)

    hardCodedMac :: MacAddress
    hardCodedMac = MacAddress (0x8C C.:> 0x8C C.:> 0xAA C.:> 0xC8 C.:> 0x2B C.:> 0xEE C.:> C.Nil)

    toEthernet :: MacAddress -> IPv4Address -> EthernetHeader
    toEthernet src _ = EthernetHeader {
      _macDst = hardCodedMac,
      _macSrc = src,
      _etherType = 0x0800 -- IPv4 EtherType
    }

    -- This generates the packets
    genPackets :: Gen IPv4Address -> Gen (PacketStreamM2S dataWidth IPv4Address)
    genPackets genMeta =
      PacketStreamM2S <$>
      genVec Gen.enumBounded <*>
      Gen.maybe Gen.enumBounded <*>
      genMeta <*>
      Gen.enumBounded

    genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
    genVec gen = sequence (C.repeat gen)

    genIpAddr :: Gen IPv4Address
    genIpAddr = IPv4Address <$> C.sequence (C.repeat @4 Gen.enumBounded)

-- Test dataWidth's of 1, 5, 6 and 7. 5, 6 and 7 are for the edge cases because a MacAddress is 6 bytes long.
prop_toethernet_d1 :: Property
prop_toethernet_d1  = toEthernetTest C.d1 (MacAddress (C.repeat 0x1))

prop_toethernet_d5 :: Property
prop_toethernet_d5  = toEthernetTest C.d5 (MacAddress (C.repeat 0x1))

prop_toethernet_d6 :: Property
prop_toethernet_d6  = toEthernetTest C.d6 (MacAddress (C.repeat 0x1))

prop_toethernet_d7 :: Property
prop_toethernet_d7  = toEthernetTest C.d7 (MacAddress (C.repeat 0x1))


tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
