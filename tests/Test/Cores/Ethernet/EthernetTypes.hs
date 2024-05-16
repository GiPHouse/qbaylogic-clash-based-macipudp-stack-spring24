{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}

module Test.Cores.Ethernet.EthernetTypes where

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
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.PacketStream




genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

model
  :: forall (dataWidth :: C.Nat)
   . C.KnownNat dataWidth
  => 1 <= dataWidth
  => MacAddress
  -> [PacketStreamM2S dataWidth IPv4Address]
  -> [PacketStreamM2S dataWidth EthernetHeader]
model macSrc = map $ fmap (toEthernet macSrc)



-- | Test the IPv4Address to EthernetHeader converter.
toEthernetTest
  :: MacAddress
  -> Property
toEthernetTest macSrc =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets (Gen.list (Range.linear 1 100) genPackets))
    (C.exposeClockResetEnable (model macSrc))
    (C.exposeClockResetEnable @C.System ckt)
    (===)
  where
    ckt :: C.HiddenClockResetEnable C.System
      => Circuit (PacketStream C.System 1 IPv4Address) (PacketStream C.System 1 EthernetHeader)
    ckt = toEthernetC (C.fromList (repeat macSrc))

    -- This generates the packets
    genPackets =
      PacketStreamM2S <$>
      genVec Gen.enumBounded <*>
      Gen.maybe Gen.enumBounded <*>
      genVec Gen.enumBounded <*>
      Gen.enumBounded


prop_toethernet_d1 :: Property
prop_toethernet_d1  = toEthernetTest (MacAddress (C.repeat 0x1))

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
