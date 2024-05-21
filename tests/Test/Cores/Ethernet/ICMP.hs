{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.ICMP where

-- base
import Prelude

-- clash-prelude
import Clash.Prelude hiding ( concatMap )
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
import Clash.Cores.Ethernet.EthernetTypes
import Clash.Cores.Ethernet.ICMP
import Clash.Cores.Ethernet.PacketStream

import Test.Cores.Ethernet.Depacketizer ( depacketizerModel )
import Test.Cores.Ethernet.Util

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

icmpReceiverPropertyGenerator
  :: forall (dataWidth :: Nat).
  ( KnownNat dataWidth
  , 1 <= dataWidth
  )
  => SNat dataWidth
  -> Property
icmpReceiverPropertyGenerator _ =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets (Gen.list (Range.linear 1 100) genPackets))
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System icmpReceiverC)
    (===)
    where
      model :: [PacketStreamM2S dataWidth EthernetHeader] -> [PacketStreamM2S dataWidth (MacAddress, IcmpHeader)]
      model = depacketizerModel const
      genPackets =
          PacketStreamM2S <$>
          genVec Gen.enumBounded <*>
          Gen.maybe Gen.enumBounded <*>
          genVec Gen.enumBounded <*>
          Gen.enumBounded

-- | n mod dataWidth ~ 1
prop_icmp_receiver_d1 :: Property
prop_icmp_receiver_d1 = icmpReceiverPropertyGenerator d1

-- | n mod dataWidth ~ 3
prop_mac_depacketizer_d3 :: Property
prop_mac_depacketizer_d3 = icmpReceiverPropertyGenerator d3

-- | n mod dataWidth ~ 0
prop_mac_depacketizer_d7 :: Property
prop_mac_depacketizer_d7 = icmpReceiverPropertyGenerator d7

-- | dataWidth < header byte size
prop_mac_depacketizer_d9 :: Property
prop_mac_depacketizer_d9 = icmpReceiverPropertyGenerator d9

-- | dataWidth ~ header byte size
prop_mac_depacketizer_d14 :: Property
prop_mac_depacketizer_d14 = icmpReceiverPropertyGenerator d14

-- | dataWidth > header byte size
prop_mac_depacketizer_d15 :: Property
prop_mac_depacketizer_d15 = icmpReceiverPropertyGenerator d15


tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
